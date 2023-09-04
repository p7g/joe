from collections import ChainMap
from collections.abc import Iterable
from contextlib import contextmanager
from itertools import chain
from typing import TypeAlias

from llvmlite import binding as llvm_binding
from llvmlite import ir

from joe import ast, eval, typed_ast, unparse

# NEED:
# - a way to get the representation of a type, ideally not coupled to llvm
# - convert a concrete type to an interface type; this involves creating a
#   struct with the object's data and a pointer to the vtable for that class
#   and interface
#     - there are 2 facets here; type representation and code generation
#     - though actually the type representation is the same for all
#       implementations of the interface


def _method_to_llvm_type(
    ir_module: ir.Module,
    this_type: ir.Type | None,
    bound_method: eval.BoundMethod | eval.BoundConstructor,
    static: bool,
    in_progress: dict[str, ir.Type] | None = None,
) -> ir.FunctionType:
    parameters: list[ir.Type] = (
        [this_type] if this_type is not None and not static else []
    )
    for param in bound_method.get_parameter_types():
        parameters.append(_type_to_llvm(ir_module, param, in_progress=in_progress))
    return_type = (
        ir.VoidType()
        if isinstance(bound_method, eval.BoundConstructor)
        else _type_to_llvm(
            ir_module, bound_method.get_return_type(), in_progress=in_progress
        )
    )
    return ir.FunctionType(return_type, parameters)


def _type_to_llvm(
    ir_module: ir.Module,
    eval_type: eval.BoundType,
    *,
    in_progress: dict[str, ir.Type] | None = None,
) -> ir.Type:
    name = eval_type.type_constructor.name()
    if name == "joe.prelude.void":
        return ir.VoidType()
    elif name == "joe.prelude.Byte":
        return ir.IntType(8)
    elif name in ("joe.prelude.Integer", "joe.prelude.Unsigned"):
        return ir.IntType(32)
    elif name in ("joe.prelude.Long", "joe.prelude.UnsignedLong"):
        return ir.IntType(64)
    elif name == "joe.prelude.Double":
        return ir.DoubleType()
    elif name == "joe.prelude.Boolean":
        return ir.IntType(1)
    elif name == "joe.prelude.String":
        return ir.PointerType(ir.IntType(8))
    elif name == "joe.prelude.Pointer":
        elem_type = eval_type.environment.get_type("T")
        return ir.PointerType(_type_to_llvm(ir_module, elem_type))

    # FIXME: Abstract over type representation
    # FIXME: this should return the representation of an instance of the type
    # i.e. an interface will add an identified type for the vtable, and return
    # a literal struct type holding a void pointer and a pointer to the vtable
    # a class will return a pointer to an identified struct that has all the
    # fields of the class (TODO: single final field)
    # TODO: embed final class fields
    if isinstance(eval_type.type_constructor.decl_ast, ast.InterfaceDecl):
        type_name = eval_type.name()
        if type_name in ir_module.context.identified_types:
            return ir_module.context.get_identified_type(type_name).as_pointer()
        interface_type = ir_module.context.get_identified_type(type_name)
        in_progress2 = {**(in_progress or {}), type_name: interface_type}
        vtable_fields = []
        for method in eval_type.type_constructor.decl_ast.members:
            if isinstance(method, ast.MethodDecl):
                assert not method.type_parameters
                bound_method = eval_type.get_method(method.name.name, [])
                method_type = _method_to_llvm_type(
                    ir_module,
                    ir.PointerType(ir.IntType(8)),
                    bound_method,
                    bound_method.static,
                    in_progress=in_progress2,
                )
                vtable_fields.append(ir.PointerType(method_type))
        vtable_type = ir_module.context.get_identified_type(
            ir_module.get_unique_name(f"{type_name}$$vtable")
        )
        vtable_type.set_body(*vtable_fields)
        interface_type.set_body(
            ir.PointerType(ir.IntType(8)), ir.PointerType(vtable_type)
        )
        return interface_type
    else:
        type_name = eval_type.name()
        if type_name in ir_module.context.identified_types:
            return ir_module.context.get_identified_type(type_name).as_pointer()
        elif in_progress is not None and type_name in in_progress:
            return in_progress[type_name].as_pointer()
        fields = []
        class_type = ir_module.context.get_identified_type(type_name)
        in_progress2 = {**(in_progress or {}), type_name: class_type}
        # FIXME: O(n^2)
        for field in eval_type.type_constructor.decl_ast.members:
            if isinstance(field, ast.FieldDecl):
                fields.append(
                    _type_to_llvm(
                        ir_module,
                        eval_type.get_field(field.name.name),
                        in_progress=in_progress2,
                    )
                )
        class_type.set_body(*fields)
        return ir.PointerType(class_type)


class CompileContext:
    __slots__ = (
        "ir_module",
        "_malloc",
        "_free",
        "_target_machine",
    )

    def __init__(self, target_machine: llvm_binding.TargetMachine) -> None:
        # All code goes in one module for simplicity in accessing globals like
        # interface implementations
        self._target_machine = target_machine
        self.ir_module = ir.Module()
        self.ir_module.triple = self._target_machine.triple
        self.ir_module.data_layout = str(self._target_machine.target_data)
        self._malloc = self._free = None

    def get_malloc(self) -> ir.Function:
        if self._malloc is None:
            malloc_type = ir.FunctionType(
                ir.PointerType(ir.IntType(8)), [ir.IntType(64)]
            )
            self._malloc = ir.Function(self.ir_module, malloc_type, "malloc")
        return self._malloc

    def get_free(self) -> ir.Function:
        if self._free is None:
            free_type = ir.FunctionType(ir.VoidType(), [ir.PointerType(ir.IntType(8))])
            self._free = ir.Function(self.ir_module, free_type, "free")
        return self._free

    def get_target_data(self) -> llvm_binding.TargetData:
        return self._target_machine.target_data


# For each type encountered, emit the type declaration
# For every method called, compile it
# For every interface used, compile all the class implementations
#   - maybe track those as types are seen
#
# All this should be cached so anything is only ever compiled once
#
# We can see the whole program, so it's fine to just not emit things that
# aren't used.

Scope: TypeAlias = ChainMap[str, ir.Value]


# FIXME: implicit return at end of function for void functions
class MethodCompiler(ast.AstVisitor):
    __slots__ = (
        "ctx",
        "method",
        "this_type",
        "llvm_function_type",
        "llvm_function",
        "ir_builder",
        "scope",
        "variable_types",
        "_prev_expr",
    )

    def __init__(self, ctx: CompileContext, method: eval.BoundMethod) -> None:
        self.ctx = ctx
        self.method = method
        self.this_type = (
            None
            if method.static or isinstance(method.self_type, eval.BoundTypeConstructor)
            else _type_to_llvm(ctx.ir_module, method.self_type)
        )
        self.llvm_function_type = _method_to_llvm_type(
            ctx.ir_module, self.this_type, method, method.static
        )
        self.llvm_function = ir.Function(
            ctx.ir_module, self.llvm_function_type, method.name()
        )
        self.ir_builder = ir.IRBuilder(self.llvm_function.append_basic_block("entry"))
        object_scope = {}
        object_variable_types = {}
        type_constructor = (
            method.self_type.type_constructor
            if isinstance(method.self_type, eval.BoundType)
            else method.self_type
        )
        for field_ast in type_constructor.decl_ast.members:
            if not isinstance(field_ast, ast.FieldDecl) or not field_ast.static:
                continue
            object_variable_types[field_ast.name.name] = type_constructor.get_field(
                field_ast.name.name
            )
            object_scope[field_ast.name.name] = self._get_static_field_var(
                type_constructor,
                field_ast.name.name,
            )
        if not method.static:
            assert isinstance(method.self_type, eval.BoundType)
            for field_ast in method.self_type.type_constructor.decl_ast.members:
                if not isinstance(field_ast, ast.FieldDecl):
                    continue
                object_variable_types[field_ast.name.name] = method.self_type.get_field(
                    field_ast.name.name
                )
                # FIXME: do this as-needed
                object_scope[field_ast.name.name] = self.ir_builder.gep(
                    self.llvm_function.args[0],  # this
                    [
                        ir.Constant(ir.IntType(32), 0),
                        ir.Constant(
                            ir.IntType(32),
                            method.self_type.get_field_index(field_ast.name.name),
                        ),
                    ],
                    inbounds=True,
                )
        self.scope = ChainMap[str, ir.Value]({}, object_scope)
        self.variable_types = ChainMap[str, eval.BoundType]({}, object_variable_types)
        for (param_name, param_type), ir_param in zip(
            chain(
                () if method.static else (("this", method.self_type),),
                (
                    (
                        param.name.name,
                        eval.evaluate_type(method.environment, param.type),
                    )
                    for param in method.decl_ast.parameters
                ),
            ),
            self.llvm_function.args,
            strict=True,
        ):
            ir_param.name = param_name
            param_var = self.ir_builder.alloca(_type_to_llvm(ctx.ir_module, param_type))
            self.ir_builder.store(ir_param, param_var)
            self.scope[param_name] = param_var
            self.variable_types[param_name] = param_type
        self._prev_expr = None
        self._free = None

    def _get_static_field_var(
        self,
        type_constructor: eval.BoundTypeConstructor,
        name: str,
    ) -> ir.Value:
        type_ = type_constructor.get_field(name)
        var_name = f"{type_constructor.name()}.{name}"
        try:
            return self.ctx.ir_module.get_global(var_name)
        except KeyError:
            global_var = ir.GlobalVariable(
                self.ctx.ir_module,
                _type_to_llvm(self.ctx.ir_module, type_),
                var_name,
            )
            global_var.linkage = "internal"
            return global_var

    def _compile_type(self, type_: ast.Type) -> ir.Type:
        self._prev_type = None
        type_.accept(self)
        assert self._prev_type
        return self._prev_type

    def visit_type(self, type_: ast.Type) -> None:
        real_type = eval.evaluate_type(self.method.environment, type_)
        self._prev_type = _type_to_llvm(self.ctx.ir_module, real_type)

    def _compile_expr(self, expr: ast.Expr) -> ir.Value:
        type_, value = self._compile_expr_with_type(expr)
        return value

    def _comment(self, text: str) -> None:
        self.ir_builder.comment(text.replace("\n", " "))

    def _compile_expr_with_type(
        self, expr: ast.Expr
    ) -> tuple[eval.BoundType, ir.Value]:
        return ExpressionCompiler(self).compile_expression(expr)

    @contextmanager
    def _push_scope(self):
        self.scope = self.scope.new_child()
        yield
        self.scope = self.scope.parents

    def _compile_block(self, statements: Iterable[ast.Statement]) -> None:
        for statement in statements:
            if self.ir_builder.block.is_terminated:
                break
            statement.accept(self)

    def visit_method_decl(self, method_decl: ast.MethodDecl) -> None:
        self.visit_type(method_decl.return_type)
        self.visit_identifier(method_decl.name)
        for type_parameter in method_decl.type_parameters:
            self.visit_type_parameter(type_parameter)
        for parameter in method_decl.parameters:
            self.visit_parameter(parameter)
        self._compile_block(method_decl.body)

        # Add implicit return if needed
        last_block = self.ir_builder.block
        if last_block and not last_block.is_terminated:
            self.ir_builder.position_at_end(last_block)
            self._comment("implicit return")
            if method_decl.return_type.name.name != "void":
                self.ir_builder.ret(
                    ir.Constant(self.llvm_function_type.return_type, None)
                )
            else:
                self.ir_builder.ret_void()

    def visit_constructor_decl(self, constructor_decl: ast.ConstructorDecl) -> None:
        self.visit_identifier(constructor_decl.name)
        for parameter in constructor_decl.parameters:
            self.visit_parameter(parameter)
        self._compile_block(constructor_decl.body)

        # Add implicit return if needed
        if self.ir_builder.block and not self.ir_builder.block.is_terminated:
            self._comment("implicit return")
            self.ir_builder.ret_void()

    def visit_return_statement(self, return_statement: ast.ReturnStatement) -> None:
        if return_statement.expr:
            self.ir_builder.ret(self._compile_expr(return_statement.expr))
        else:
            self.ir_builder.ret_void()

    def visit_delete_statement(self, delete_expr: ast.DeleteStatement) -> None:
        free = self.ctx.get_free()
        self.ir_builder.call(
            free,
            [
                self.ir_builder.bitcast(
                    self._compile_expr(delete_expr.expr), ir.PointerType(ir.IntType(8))
                )
            ],
        )

    def visit_expr_statement(self, expr_statement: ast.ExprStatement) -> None:
        self._compile_expr(expr_statement.expr)

    def visit_if_statement(self, if_statement: ast.IfStatement) -> None:
        debug_str = f"if ({unparse.unparse(if_statement.condition)})"
        self._comment(debug_str)

        condition = self._compile_expr(if_statement.condition)

        then_block = self.llvm_function.append_basic_block()
        else_block = self.llvm_function.append_basic_block()
        after_block = self.llvm_function.append_basic_block()

        self.ir_builder.cbranch(condition, then_block, else_block)

        with self._push_scope():
            self.ir_builder.position_at_end(then_block)
            self._comment("if true")
            self._compile_block(if_statement.then)

        # i.e. if there is a return
        assert self.ir_builder.block
        if not self.ir_builder.block.is_terminated:
            self.ir_builder.branch(after_block)

        with self._push_scope():
            self.ir_builder.position_at_end(else_block)
            self._comment("else")
            self._compile_block(if_statement.else_)

        assert self.ir_builder.block
        if not self.ir_builder.block.is_terminated:
            self.ir_builder.branch(after_block)

        self.ir_builder.position_at_end(after_block)
        self._comment(f"END {debug_str}")

    def visit_while_statement(self, while_statement: ast.WhileStatement) -> None:
        body_block = self.llvm_function.append_basic_block()
        condition_block = self.llvm_function.append_basic_block()
        after_block = self.llvm_function.append_basic_block()

        debug_str = f"while ({unparse.unparse(while_statement.condition)})"
        self._comment(debug_str)
        self.ir_builder.branch(condition_block)

        self.ir_builder.position_at_end(condition_block)
        condition = self._compile_expr(while_statement.condition)
        self.ir_builder.cbranch(condition, body_block, after_block)

        with self._push_scope():
            self.ir_builder.position_at_end(body_block)
            self._comment("while body")
            self._compile_block(while_statement.body)

        assert self.ir_builder.block
        if not self.ir_builder.block.is_terminated:
            self.ir_builder.branch(condition_block)

        self.ir_builder.position_at_end(after_block)
        self._comment(f"END {debug_str}")

    def visit_for_statement(self, for_statement: ast.ForStatement) -> None:
        condition_block = self.llvm_function.append_basic_block()
        update_block = self.llvm_function.append_basic_block()
        body_block = self.llvm_function.append_basic_block()
        after_block = self.llvm_function.append_basic_block()

        debug_str = unparse.unparse(for_statement).splitlines()[0]
        self._comment(debug_str)
        with self._push_scope():
            if for_statement.init:
                self._comment("for init")
                for_statement.init.accept(self)

            if for_statement.condition:
                self.ir_builder.branch(condition_block)
                self.ir_builder.position_at_end(condition_block)
                self._comment("for condition")
                condition = self._compile_expr(for_statement.condition)
                self.ir_builder.cbranch(condition, body_block, after_block)
            else:
                self.ir_builder.branch(body_block)

            self.ir_builder.position_at_end(body_block)
            self._comment("for body")
            self._compile_block(for_statement.body)

            assert self.ir_builder.block
            if not self.ir_builder.block.is_terminated:
                self.ir_builder.branch(update_block)

            self.ir_builder.position_at_end(update_block)
            self._comment("for update")
            if for_statement.update:
                self._compile_expr(for_statement.update)
            self.ir_builder.branch(condition_block)

            self.ir_builder.position_at_end(after_block)

    def visit_variable_decl_statement(
        self, variable_decl_statement: ast.VariableDeclStatement
    ) -> None:
        assert variable_decl_statement.type or variable_decl_statement.expr
        if variable_decl_statement.expr:
            type_, expr = self._compile_expr_with_type(variable_decl_statement.expr)
        else:
            # FIXME: this type of validation shouldn't happen here
            assert variable_decl_statement.type
            type_ = eval.evaluate_type(
                self.method.environment, variable_decl_statement.type
            )
            expr = ir.Constant(_type_to_llvm(self.ctx.ir_module, type_), ir.Undefined)
        location = self.ir_builder.alloca(
            _type_to_llvm(self.ctx.ir_module, type_),
            name=variable_decl_statement.name.name,
        )
        self.ir_builder.store(expr, location)
        self.scope[variable_decl_statement.name.name] = location
        self.variable_types[variable_decl_statement.name.name] = type_


class ExpressionCompiler(typed_ast.TypedAstVisitor):
    __slots__ = ("method_compiler", "_prev_expr")

    def __init__(self, method_compiler: MethodCompiler) -> None:
        # FIXME: weakref?
        self.method_compiler = method_compiler
        self._prev_expr: ir.Value | None = None

    @property
    def ir_builder(self) -> ir.IRBuilder:
        return self.method_compiler.ir_builder

    @property
    def scope(self) -> Scope:
        return self.method_compiler.scope

    def compile_expression(self, expr: ast.Expr) -> tuple[eval.BoundType, ir.Value]:
        typed_expr = eval.evaluate_expr(
            self.method_compiler.method.environment,
            self.method_compiler.variable_types,
            expr,
        )
        return typed_expr.type, self._compile_expression(typed_expr)

    def _compile_expression(self, expr: typed_ast.Expr) -> ir.Value:
        self._prev_expr = None
        expr.accept(self)
        assert self._prev_expr is not None
        return self._prev_expr

    def visit_literal_int(self, literal_int: typed_ast.LiteralInt) -> None:
        self._prev_expr = ir.Constant(
            _type_to_llvm(self.method_compiler.ctx.ir_module, literal_int.type),
            literal_int.value,
        )

    def visit_literal_float(self, literal_float: typed_ast.LiteralFloat) -> None:
        self._prev_expr = ir.Constant(
            _type_to_llvm(self.method_compiler.ctx.ir_module, literal_float.type),
            literal_float.value,
        )

    def visit_literal_string(self, literal_string: typed_ast.LiteralString) -> None:
        # FIXME: deal with escapes, and do so somewhere else
        # Also maybe literal strings should actually call some prelude method,
        # like String.fromBytes(), which could instantiate different string
        # variants depending on the length
        str_const_type = ir.ArrayType(ir.IntType(8), len(literal_string.value) + 1)
        str_var = self.ir_builder.alloca(str_const_type)
        self.ir_builder.store(
            ir.Constant(
                str_const_type, bytearray(literal_string.value, "utf8") + b"\0"
            ),
            str_var,
        )
        self._prev_expr = self.ir_builder.bitcast(
            str_var,
            _type_to_llvm(self.method_compiler.ctx.ir_module, literal_string.type),
        )

    def visit_literal_bool(self, literal_bool: typed_ast.LiteralBool) -> None:
        self._prev_expr = ir.Constant(
            _type_to_llvm(self.method_compiler.ctx.ir_module, literal_bool.type),
            literal_bool.value,
        )

    def visit_identifier_expr(self, identifier_expr: typed_ast.IdentifierExpr) -> None:
        self._prev_expr = self.ir_builder.load(self.scope[identifier_expr.name.name])

    def _get_this(self) -> ir.Value:
        return self.ir_builder.load(self.scope["this"])

    def visit_this_expr(self, this_expr: typed_ast.ThisExpr) -> None:
        self._prev_expr = self._get_this()

    def visit_integer_cast_expr(
        self, integer_cast_expr: typed_ast.IntegerCastExpr
    ) -> None:
        desired_type = _type_to_llvm(
            self.method_compiler.ctx.ir_module, integer_cast_expr.type
        )
        actual_type = _type_to_llvm(
            self.method_compiler.ctx.ir_module, integer_cast_expr.expr.type
        )
        assert isinstance(desired_type, ir.IntType) and isinstance(
            actual_type, ir.IntType
        )

        int_expr = self._compile_expression(integer_cast_expr.expr)
        if actual_type.width > desired_type.width:
            int_expr = self.ir_builder.trunc(int_expr, desired_type)
        elif actual_type.width < desired_type.width:
            int_expr = self.ir_builder.sext(int_expr, desired_type)
        self._prev_expr = int_expr

    def _struct_field_ptr(self, dot_expr: typed_ast.DotExpr) -> ir.Value:
        struct_ptr = self._compile_expression(dot_expr.expr)
        field_index = dot_expr.expr.type.get_field_index(dot_expr.name.name)
        return self.ir_builder.gep(
            struct_ptr,
            [ir.Constant(ir.IntType(32), 0), ir.Constant(ir.IntType(32), field_index)],
            inbounds=True,
        )

    def _get_static_field_var(
        self,
        static_dot_expr: typed_ast.StaticDotExpr,
    ) -> ir.Value:
        return self.method_compiler._get_static_field_var(
            static_dot_expr.type_constructor, static_dot_expr.name.name
        )

    def visit_binary_expr(self, binary_expr: typed_ast.BinaryExpr) -> None:
        if binary_expr.operator is ast.BinaryOperator.AND:
            left = self._compile_expression(binary_expr.left)
            prev_block = self.ir_builder.block
            left_is_false = self.ir_builder.icmp_signed(
                "==",
                left,
                ir.Constant(
                    _type_to_llvm(
                        self.method_compiler.ctx.ir_module, binary_expr.left.type
                    ),
                    0,
                ),
            )
            else_block = self.ir_builder.append_basic_block()
            after_block = self.ir_builder.append_basic_block()
            self.ir_builder.cbranch(left_is_false, else_block, after_block)

            self.ir_builder.position_at_end(else_block)
            right = self._compile_expression(binary_expr.right)
            right_is_false = self.ir_builder.icmp_signed(
                "==",
                right,
                ir.Constant(
                    _type_to_llvm(
                        self.method_compiler.ctx.ir_module, binary_expr.right.type
                    ),
                    0,
                ),
            )
            self.ir_builder.branch(after_block)

            self.ir_builder.position_at_end(after_block)
            result_is_false = self.ir_builder.phi(
                _type_to_llvm(self.method_compiler.ctx.ir_module, binary_expr.type)
            )
            result_is_false.add_incoming(left_is_false, prev_block)
            result_is_false.add_incoming(right_is_false, else_block)
            self._prev_expr = self.ir_builder.not_(result_is_false)
            return
        elif binary_expr.operator is ast.BinaryOperator.OR:
            left = self._compile_expression(binary_expr.left)
            prev_block = self.ir_builder.block
            left_is_false = self.ir_builder.icmp_signed(
                "==",
                left,
                ir.Constant(
                    _type_to_llvm(
                        self.method_compiler.ctx.ir_module, binary_expr.left.type
                    ),
                    0,
                ),
            )
            else_block = self.ir_builder.append_basic_block()
            after_block = self.ir_builder.append_basic_block()
            self.ir_builder.cbranch(left_is_false, else_block, after_block)
            self.ir_builder.position_at_end(else_block)
            right = self._compile_expression(binary_expr.right)
            right_is_false = self.ir_builder.icmp_signed(
                "==",
                right,
                ir.Constant(
                    _type_to_llvm(
                        self.method_compiler.ctx.ir_module, binary_expr.right.type
                    ),
                    0,
                ),
            )
            self.ir_builder.branch(after_block)
            self.ir_builder.position_at_end(after_block)
            result_is_false = self.ir_builder.phi(
                _type_to_llvm(self.method_compiler.ctx.ir_module, binary_expr.type)
            )
            result_is_false.add_incoming(left_is_false, prev_block)
            result_is_false.add_incoming(right_is_false, else_block)
            self._prev_expr = self.ir_builder.not_(result_is_false)
            return
        elif binary_expr.operator is ast.BinaryOperator.ASSIGN:
            new_value = self._compile_expression(binary_expr.right)
            self._prev_expr = new_value
            if isinstance(binary_expr.left, typed_ast.IdentifierExpr):
                self.ir_builder.store(new_value, self.scope[binary_expr.left.name.name])
            elif isinstance(binary_expr.left, typed_ast.DotExpr):
                field_ptr = self._struct_field_ptr(binary_expr.left)
                self.ir_builder.store(new_value, field_ptr)
            elif isinstance(binary_expr.left, typed_ast.StaticDotExpr):
                global_var = self._get_static_field_var(binary_expr.left)
                self.ir_builder.store(new_value, global_var)
            elif isinstance(binary_expr.left, typed_ast.IndexExpr):
                array_ptr = self._compile_expression(binary_expr.left.expr)
                index = self._compile_expression(binary_expr.left.index)
                # TODO: Use method on Array instead of inlining
                array_buf = self.ir_builder.load(
                    self.ir_builder.gep(
                        array_ptr,
                        [
                            ir.Constant(ir.IntType(32), 0),
                            ir.Constant(
                                ir.IntType(32),
                                binary_expr.left.expr.type.get_field_index("_elements"),
                            ),
                        ],
                    )
                )
                self.ir_builder.store(
                    new_value,
                    self.ir_builder.gep(
                        array_buf,
                        [self.ir_builder.sext(index, ir.IntType(64))],
                    ),
                )
            else:
                raise NotImplementedError(binary_expr.left)
            return

        left = self._compile_expression(binary_expr.left)
        right = self._compile_expression(binary_expr.right)
        if binary_expr.operator is ast.BinaryOperator.PLUS:
            self._prev_expr = self.ir_builder.add(left, right)
        elif binary_expr.operator is ast.BinaryOperator.MINUS:
            self._prev_expr = self.ir_builder.sub(left, right)
        elif binary_expr.operator is ast.BinaryOperator.TIMES:
            self._prev_expr = self.ir_builder.mul(left, right)
        elif binary_expr.operator is ast.BinaryOperator.DIVIDE:
            self._prev_expr = self.ir_builder.sdiv(left, right)
        elif binary_expr.operator is ast.BinaryOperator.MODULO:
            self._prev_expr = self.ir_builder.srem(left, right)
        elif binary_expr.operator is ast.BinaryOperator.EQUALS:
            self._prev_expr = self.ir_builder.icmp_signed("==", left, right)
        elif binary_expr.operator is ast.BinaryOperator.NOT_EQUALS:
            self._prev_expr = self.ir_builder.icmp_signed("!=", left, right)
        elif binary_expr.operator is ast.BinaryOperator.LESS_THAN:
            self._prev_expr = self.ir_builder.icmp_signed("<", left, right)
        elif binary_expr.operator is ast.BinaryOperator.LESS_THAN_OR_EQUAL:
            self._prev_expr = self.ir_builder.icmp_signed("<=", left, right)
        elif binary_expr.operator is ast.BinaryOperator.GREATER_THAN:
            self._prev_expr = self.ir_builder.icmp_signed(">", left, right)
        elif binary_expr.operator is ast.BinaryOperator.GREATER_THAN_OR_EQUAL:
            self._prev_expr = self.ir_builder.icmp_signed(">=", left, right)
        else:
            raise NotImplementedError(binary_expr.operator)

    def visit_unary_expr(self, unary_expr: typed_ast.UnaryExpr) -> None:
        value = self._compile_expression(unary_expr.expr)
        if unary_expr.operator is ast.UnaryOperator.NOT:
            self._prev_expr = self.ir_builder.not_(value)
        elif unary_expr.operator is ast.UnaryOperator.MINUS:
            self._prev_expr = self.ir_builder.neg(value)
        else:
            raise NotImplementedError(unary_expr.operator)

    def visit_index_expr(self, index_expr: typed_ast.IndexExpr) -> None:
        array_ptr = self._compile_expression(index_expr.expr)
        index = self._compile_expression(index_expr.index)

        get_method_type = index_expr.expr.type.get_method("get", [])
        func = self._get_compiled_method(get_method_type)
        self._prev_expr = self.ir_builder.call(
            func, [array_ptr, self.ir_builder.sext(index, ir.IntType(64))]
        )

    def visit_new_expr(self, new_expr: typed_ast.NewExpr) -> None:
        # FIXME: if there's an error make sure to free the memory
        malloc = self.method_compiler.ctx.get_malloc()
        obj_ptr_ty = _type_to_llvm(self.method_compiler.ctx.ir_module, new_expr.type)
        assert isinstance(obj_ptr_ty, ir.PointerType)
        obj_ty = obj_ptr_ty.pointee
        size = obj_ty.get_abi_size(self.method_compiler.ctx.get_target_data())
        mem = self.ir_builder.call(malloc, [ir.Constant(ir.IntType(64), size)])
        mem = self.ir_builder.bitcast(mem, obj_ptr_ty)

        constructor = new_expr.type.get_constructor()
        if constructor:
            function = self._get_compiled_method(constructor)
            args = [mem, *map(self._compile_expression, new_expr.arguments)]
            self.ir_builder.call(function, args)

        self._prev_expr = mem

    def visit_new_array_expr(self, new_array_expr: typed_ast.NewArrayExpr) -> None:
        malloc = self.method_compiler.ctx.get_malloc()
        element_ty = new_array_expr.type.environment.get_type("T")
        element_llvm_ty = _type_to_llvm(self.method_compiler.ctx.ir_module, element_ty)
        length = self.ir_builder.sext(
            self._compile_expression(new_array_expr.size), ir.IntType(64)
        )
        size = self.ir_builder.mul(
            length,
            ir.Constant(
                ir.IntType(64),
                element_llvm_ty.get_abi_size(
                    self.method_compiler.ctx.get_target_data()
                ),
            ),
        )
        buf = self.ir_builder.bitcast(
            self.ir_builder.call(malloc, [size]), element_llvm_ty.as_pointer()
        )
        array_type = _type_to_llvm(
            self.method_compiler.ctx.ir_module, new_array_expr.type
        )
        array = self.ir_builder.bitcast(
            self.ir_builder.call(
                malloc,
                [
                    ir.IntType(64)(
                        array_type.pointee.get_abi_size(
                            self.method_compiler.ctx.get_target_data()
                        )
                    )
                ],
            ),
            array_type,
        )
        self.ir_builder.store(
            buf,
            self.ir_builder.gep(
                array,
                [
                    ir.IntType(32)(0),
                    ir.IntType(32)(new_array_expr.type.get_field_index("_elements")),
                ],
                inbounds=True,
            ),
        )
        self.ir_builder.store(
            length,
            self.ir_builder.gep(
                array,
                [
                    ir.IntType(32)(0),
                    ir.IntType(32)(new_array_expr.type.get_field_index("length")),
                ],
                inbounds=True,
            ),
        )
        self._prev_expr = array

    def visit_dot_expr(self, dot_expr: typed_ast.DotExpr) -> None:
        # TODO: evaluate_expr, basically annotate the AST nodes with their
        # types, and maybe we can even resolve where names come from ahead of
        # time. that way the backend doesn't need to all implement the business
        # logic of looking up in locals, then falling back to globals.
        #
        # evaluate_expr will also do type checking and semantic validation like
        # preventing `var x;`
        #
        # This is needed so we can figure out how to even access attributes of
        # the object.
        self._prev_expr = self.ir_builder.load(self._struct_field_ptr(dot_expr))

    def visit_static_dot_expr(self, static_dot_expr: typed_ast.StaticDotExpr) -> None:
        self._prev_expr = self.ir_builder.load(
            self._get_static_field_var(static_dot_expr)
        )

    def _get_compiled_method(self, method: eval.BoundMethod) -> ir.Function:
        try:
            function = self.method_compiler.ctx.ir_module.get_global(method.name())
        except KeyError:
            # Look up class type, get method, compile it
            method_compiler = MethodCompiler(self.method_compiler.ctx, method)
            method.decl_ast.accept(method_compiler)
            function = method_compiler.llvm_function
            assert function is self.method_compiler.ctx.ir_module.get_global(
                method.name()
            )
        return function

    def visit_call_expr(self, call_expr: typed_ast.CallExpr) -> None:
        function = intrinsics.get_intrinsic(self.method_compiler.ctx, call_expr.method)
        if function is None:
            function = self._get_compiled_method(call_expr.method)

        args = []
        if not call_expr.method.static:
            if call_expr.expr:
                args.append(self._compile_expression(call_expr.expr))
            else:
                args.append(self._get_this())
        for arg in call_expr.arguments:
            args.append(self._compile_expression(arg))

        self._prev_expr = self.ir_builder.call(function, args)


from joe import intrinsics  # noqa: E402
