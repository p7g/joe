from collections import ChainMap
from itertools import chain
from typing import TypeAlias

from llvmlite import ir

from joe import ast, eval, typed_ast

# NEED:
# - a way to get the representation of a type, ideally not coupled to llvm
# - convert a concrete type to an interface type; this involves creating a
#   struct with the object's data and a pointer to the vtable for that class
#   and interface
#     - there are 2 facets here; type representation and code generation
#     - though actually the type representation is the same for all
#       implementations of the interface


def _method_to_llvm_type(
    ir_module: ir.Module, this_type: ir.Type | None, bound_method: eval.BoundMethod
) -> ir.FunctionType:
    parameters: list[ir.Type] = [this_type] if this_type is not None else []
    for param in bound_method.get_parameter_types():
        parameters.append(_type_to_llvm(ir_module, param))
    return ir.FunctionType(
        _type_to_llvm(ir_module, bound_method.get_return_type()),
        parameters,
    )


def _type_to_llvm(ir_module: ir.Module, eval_type: eval.BoundType) -> ir.Type:
    name = eval_type.type_constructor.name()
    if name == "joe.prelude.void":
        return ir.VoidType()
    elif name == "joe.prelude.Integer":
        return ir.IntType(32)
    elif name == "joe.prelude.Double":
        return ir.DoubleType()
    elif name == "joe.prelude.Boolean":
        return ir.IntType(1)

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
        vtable_fields = []
        for method in eval_type.type_constructor.decl_ast.members:
            if isinstance(method, ast.MethodDecl):
                assert not method.type_parameters
                bound_method = eval_type.get_method(method.name.name, [])
                method_type = _method_to_llvm_type(
                    ir_module, ir.PointerType(ir.IntType(8)), bound_method
                )
                vtable_fields.append(ir.PointerType(method_type))
        vtable_type = ir_module.context.get_identified_type(
            ir_module.get_unique_name(f"{type_name}$$vtable")
        )
        vtable_type.set_body(*vtable_fields)
        return ir.PointerType(
            ir.LiteralStructType([ir.PointerType(ir.IntType(8)), vtable_type])
        )
    else:
        type_name = eval_type.name()
        if type_name in ir_module.context.identified_types:
            return ir_module.context.get_identified_type(type_name).as_pointer()
        fields = []
        # FIXME: O(n^2)
        for field in eval_type.type_constructor.decl_ast.members:
            if isinstance(field, ast.FieldDecl):
                fields.append(
                    _type_to_llvm(ir_module, eval_type.get_field(field.name.name))
                )
        class_type = ir_module.context.get_identified_type(type_name)
        class_type.set_body(*fields)
        return ir.PointerType(class_type)


class CompileContext:
    __slots__ = (
        "emitted_types",
        "emitted_methods",
        "emitted_interface_impls",
        "ir_module",
    )

    def __init__(self) -> None:
        # This is keyed by BoundMethod.name()
        self.emitted_methods: dict[str, ir.FunctionType] = {}
        # Globals holding the implementation of an interface
        self.emitted_interface_impls: dict[tuple[str, str], ir.GlobalValue] = {}
        # All code goes in one module for simplicity in accessing globals like
        # interface implementations
        self.ir_module = ir.Module()


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
        self.this_type = _type_to_llvm(ctx.ir_module, method.self_type)
        self.llvm_function_type = _method_to_llvm_type(
            ctx.ir_module, self.this_type, method
        )
        self.llvm_function = ir.Function(
            ctx.ir_module, self.llvm_function_type, method.name()
        )
        self.ir_builder = ir.IRBuilder(self.llvm_function.append_basic_block("entry"))
        self.scope = ChainMap[str, ir.Value]()
        self.variable_types = ChainMap[str, eval.BoundType]()
        for (param_name, param_type), ir_param in zip(
            chain(
                (("this", method.self_type),),
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

    def _compile_expr_with_type(
        self, expr: ast.Expr
    ) -> tuple[eval.BoundType, ir.Value]:
        return ExpressionCompiler(self).compile_expression(expr)

    def visit_method_decl(self, method_decl: ast.MethodDecl) -> None:
        super().visit_method_decl(method_decl)
        if self.ir_builder.block and not self.ir_builder.block.is_terminated:
            if method_decl.return_type.name.name != "void":
                self.ir_builder.ret(
                    ir.Constant(self.llvm_function_type.return_type, None)
                )
            else:
                self.ir_builder.ret_void()

    def visit_return_statement(self, return_statement: ast.ReturnStatement) -> None:
        if return_statement.expr:
            self.ir_builder.ret(self._compile_expr(return_statement.expr))
        else:
            self.ir_builder.ret_void()

    def visit_expr_statement(self, expr_statement: ast.ExprStatement) -> None:
        self._compile_expr(expr_statement.expr)

    def visit_if_statement(self, if_statement: ast.IfStatement) -> None:
        condition = self._compile_expr(if_statement.condition)

        then_block = self.llvm_function.append_basic_block()
        else_block = self.llvm_function.append_basic_block()
        after_block = self.llvm_function.append_basic_block()

        self.ir_builder.cbranch(condition, then_block, else_block)

        self.ir_builder.position_at_end(then_block)
        for statement in if_statement.then:
            statement.accept(self)
        self.ir_builder.branch(after_block)

        self.ir_builder.position_at_end(else_block)
        for statement in if_statement.else_:
            statement.accept(self)
        self.ir_builder.branch(after_block)

        self.ir_builder.position_at_end(after_block)

    def visit_while_statement(self, while_statement: ast.WhileStatement) -> None:
        condition_block = self.llvm_function.append_basic_block()
        body_block = self.llvm_function.append_basic_block()
        after_block = self.llvm_function.append_basic_block()

        self.ir_builder.branch(condition_block)

        self.ir_builder.position_at_end(condition_block)
        condition = self._compile_expr(while_statement.condition)
        self.ir_builder.cbranch(condition, body_block, after_block)

        self.ir_builder.position_at_end(body_block)
        for statement in while_statement.body:
            statement.accept(self)
        self.ir_builder.branch(condition_block)

        self.ir_builder.position_at_end(after_block)

    def visit_for_statement(self, for_statement: ast.ForStatement) -> None:
        condition_block = self.llvm_function.append_basic_block()
        update_block = self.llvm_function.append_basic_block()
        body_block = self.llvm_function.append_basic_block()
        after_block = self.llvm_function.append_basic_block()

        self.scope = self.scope.new_child()
        if for_statement.init:
            for_statement.init.accept(self)

        if for_statement.condition:
            self.ir_builder.branch(condition_block)
            self.ir_builder.position_at_end(condition_block)
            condition = self._compile_expr(for_statement.condition)
            self.ir_builder.cbranch(condition, body_block, after_block)
        else:
            self.ir_builder.branch(body_block)

        self.ir_builder.position_at_end(body_block)
        for statement in for_statement.body:
            statement.accept(self)

        self.ir_builder.branch(update_block)
        self.ir_builder.position_at_end(update_block)
        if for_statement.update:
            self._compile_expr(for_statement.update)
        self.ir_builder.branch(condition_block)

        self.ir_builder.position_at_end(after_block)
        self.scope = self.scope.parents

    def compile_variable_decl_statement(
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
            expr = ir.Constant(type_, ir.Undefined)
        location = self.ir_builder.alloca(_type_to_llvm(self.ctx.ir_module, type_))
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
        self._prev_expr = ir.Constant(
            ir.ArrayType(ir.IntType(8), len(literal_string.value) + 1),
            bytearray(literal_string.value, "utf8") + b"\0",
        )

    def visit_literal_bool(self, literal_bool: typed_ast.LiteralBool) -> None:
        self._prev_expr = ir.Constant(
            _type_to_llvm(self.method_compiler.ctx.ir_module, literal_bool.type),
            literal_bool.value,
        )

    def visit_identifier_expr(self, identifier_expr: typed_ast.IdentifierExpr) -> None:
        self._prev_expr = self.ir_builder.load(self.scope[identifier_expr.name.name])

    def visit_this_expr(self, this_expr: typed_ast.ThisExpr) -> None:
        self._prev_expr = self.ir_builder.load(self.scope["this"])

    def visit_dot_expr(self, dot_expr: typed_ast.DotExpr) -> None:
        obj = dot_expr.expr.accept(self)
        type_ = dot_expr.type
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

        field_index = type_.get_field_index(dot_expr.name.name)
        self._prev_expr = self.ir_builder.load(
            self.ir_builder.gep(
                obj,
                [
                    ir.Constant(ir.IntType(32), 0),
                    ir.Constant(ir.IntType(32), field_index),
                ],
                inbounds=True,
            )
        )

    def visit_call_expr(self, call_expr: typed_ast.CallExpr) -> None:
        def cstr(bytes_: bytes) -> ir.Value:
            ty = ir.ArrayType(ir.IntType(8), len(bytes_))
            ptr = self.ir_builder.alloca(ty)
            self.ir_builder.store(ty(bytearray(bytes_)), ptr)
            return self.ir_builder.bitcast(ptr, ir.IntType(8).as_pointer())

        if call_expr.name.name == "print":
            function = ir.Function(
                self.method_compiler.ctx.ir_module,
                ir.FunctionType(
                    ir.VoidType(),
                    [ir.IntType(8).as_pointer()],
                    var_arg=True,
                ),
                "printf",
            )
            args = [cstr(b"%s: %d\n\0"), cstr(b"hello\0")]
        else:
            try:
                function = self.method_compiler.ctx.ir_module.get_global(
                    call_expr.method.name()
                )
            except KeyError:
                # Look up class type, get method, compile it
                method_compiler = MethodCompiler(
                    self.method_compiler.ctx, call_expr.method
                )
                call_expr.method.decl_ast.accept(method_compiler)
                function = method_compiler.llvm_function
                assert function is self.method_compiler.ctx.ir_module.get_global(
                    call_expr.method.name()
                )

            args = []
        args.append(self._compile_expression(call_expr.expr))
        for arg in call_expr.arguments:
            args.append(self._compile_expression(arg))

        self._prev_expr = self.ir_builder.call(function, args)
