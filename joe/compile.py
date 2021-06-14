import dataclasses
import os
import typing as t
from contextlib import contextmanager

from joe import ast, codegen, typesys
from joe.context import GlobalContext
from joe.exc import JoeUnreachable
from joe.parse import ModulePath
from joe.source import JoeTypeError
from joe.typevisitor import MethodExprTypeVisitor


# FIXME: name mangling for int[] is not great
def _mangle_ident(*parts: str) -> str:
    ident = "__joe"
    for part in parts:
        ident += f"{len(part)}{part}"
    return f"{ident}E"


def _mangle_class_name(class_ty: typesys.Class) -> str:
    # FIXME: Properly mangle class and function IDs to support generics
    return _mangle_ident(*ModulePath.from_class_path(class_ty.id.name))


_type_to_ident_table = str.maketrans(
    {
        "*": "_ptr",
        " ": "_",
        "[": "_array",
        "]": "_",
    }
)


# def _array_type_name(ctx: GlobalContext, typ: ty.ArrayType) -> str:
#     return _mangle_ident(_type_str(ctx, typ.element_type) + "_array")


def _type_str(ctx: GlobalContext, typ: typesys.Type) -> str:
    return _ty_to_ctype(ctx, typ).render().translate(_type_to_ident_table)


def _ty_to_ctype(ctx: GlobalContext, typ: typesys.Type) -> codegen.CType:
    if isinstance(typ, typesys.IntType):
        return codegen.CNamedType("int")
    elif isinstance(typ, typesys.VoidType):
        return codegen.CNamedType("void")
    # elif isinstance(typ, ty.ArrayType):
    #     ctx.array_types.add(typ)
    #     return codegen.CStructType(name=_array_type_name(ctx, typ))
    elif isinstance(typ, typesys.ClassInstance):
        return codegen.CStructType(_mangle_class_name(typ.class_))
    elif isinstance(typ, typesys.FunctionInstance):
        return codegen.CFuncType(
            return_type=_ty_to_ctype(ctx, typ.function.return_type),
            parameter_types=[
                _ty_to_ctype(ctx, p) for p in typ.function.formal_parameters
            ],
        )
    else:
        raise NotImplementedError(typ)


# Global types include all classes, named types (fully-qualified)
#   -> possible to distinguish a.b.C from property access by checking for a
#      value called `a` in scope
# Module-level types extend global types including imported ones

# Separate scope for variables
# Method scope is top-level, includes class fields, then parameters, then
# locals.


class CodeGenerator:
    def __init__(self, indent_str: str = "    "):
        self._indent_level = 0
        self._buf: t.List[str] = []
        self._indent_str = indent_str

    def __indent(self) -> t.Generator[None, None, None]:
        self._indent_level += 1
        yield
        self._indent_level -= 1

    indent = t.cast(
        t.Callable[["CodeGenerator"], t.ContextManager[None]],
        contextmanager(__indent),
    )

    def emit(self, *lines: str) -> None:
        if not lines:
            self._buf.append("")
        else:
            self._buf.extend(
                self._indent_level * self._indent_str + line for line in lines
            )

    def get(self) -> str:
        return os.linesep.join(self._buf)


class ModuleCodeGenerator(CodeGenerator):
    def __init__(
        self,
        modules: t.List[ast.Module],
        main_method: t.Optional[str] = None,
        indent_str: str = "    ",
    ):
        super().__init__(indent_str=indent_str)

        # FIXME: hack
        self.emit("#include <stdio.h>")
        self.emit("#include <stdlib.h>")

        self.ctx = GlobalContext()
        self.ctx.populate_from_modules(modules)
        self._modules = modules
        # self._generated_array_types: t.Dict[ty.ArrayType, codegen.CStruct] = {}
        self._main_method = main_method

    def generate(self):
        for mod in self._modules:
            self.emit(f"/* start module {mod.name} */")
            self._generate_class(mod)
            self.emit(f"/* end module {mod.name} */")
            self.emit()

    def _generate_class(self, mod: ast.Module) -> None:
        class_path = mod.name
        class_path_parts = ModulePath.from_class_path(class_path)
        class_type = self.ctx.type_scope[class_path]
        assert isinstance(class_type, typesys.Class)

        # forward-declare static methods
        for meth_name, meth in class_type.methods.items():
            if not meth.static:
                continue
            self.emit()
            c_type = codegen.CFunc(
                name=_mangle_ident(
                    *ModulePath.from_class_path(class_type.id.name),
                    meth_name,
                ),
                return_type=_ty_to_ctype(self.ctx, meth.return_type),
                parameters=[
                    codegen.CParam(
                        name="",
                        type=_ty_to_ctype(self.ctx, p_type),
                    )
                    for p_type in meth.formal_parameters
                ],
                body=[],
            )
            c_type.emit_forward_decl(self)

        # Generate data struct type
        data_type = codegen.CStruct(
            name=_mangle_ident(*class_path_parts, "data")
        )
        # for name, field_ty in class_type.instance_type.fields.items():
        #     if isinstance(field_ty, ty.ArrayType):
        #         self._get_or_create_array_type(field_ty)
        #         self.emit()

        for name, field_ty in class_type.members.items():
            data_type.fields.append(
                codegen.CStructField(
                    name=name, type=_ty_to_ctype(self.ctx, field_ty)
                )
            )

        data_type.emit(self)

        # Forward declaration for object struct type
        object_type = codegen.CStruct(name=_mangle_ident(*class_path_parts))
        object_type.emit_forward_decl(self)

        # Generate vtable struct type
        vtable_type = codegen.CStruct(
            name=_mangle_ident(*class_path_parts, "vtable")
        )
        for name, meth in class_type.methods.items():
            if not meth.static:
                vtable_type.fields.append(self._method_field(class_type, name))
        vtable_type.emit(self)

        # Generate object type struct
        object_type.fields.extend(
            [
                codegen.CStructField(
                    name="data", type=data_type.type.as_pointer()
                ),
                codegen.CStructField(
                    name="vtable", type=vtable_type.type.as_pointer()
                ),
            ]
        )
        object_type.emit(self)

        # Generate method implementations
        for name, meth in class_type.methods.items():
            if meth.static:
                continue
            self.emit()
            self._method(
                class_type=class_type,
                meth_name=name,
                method=next(
                    m for m in mod.class_decl.methods if m.name.value == name
                ),
            )

        # Generate vtable variable
        self.emit()
        decl = codegen.CVarDecl(
            name=vtable_type.name,
            type=codegen.CStructType(name=vtable_type.name),
            value=codegen.CArrayLiteral(
                elements=[
                    codegen.CVariable(
                        _mangle_ident(
                            *ModulePath.from_class_path(class_type.id.name),
                            meth_name,
                        )
                    )
                    for meth_name, meth in class_type.methods.items()
                    if not meth.static
                ],
            ),
        )
        decl.emit(self)

        # emit static methods
        for meth_name, meth in class_type.methods.items():
            if not meth.static:
                continue
            self.emit()
            self._method(
                class_type=class_type,
                meth_name=meth_name,
                method=next(
                    m
                    for m in mod.class_decl.methods
                    if m.static and m.name.value == meth_name
                ),
                static=True,
            )

        # generate main method
        if self._main_method:
            class_path, meth_name = self._main_method.rsplit(".")
            cls_ty = self.ctx.type_scope[class_path]
            if not isinstance(cls_ty, typesys.Class):
                # FIXME: Another exception type?
                raise Exception(f"Invalid class for main method: {class_path}")
            meth_ty = cls_ty.get_method(meth_name)
            if meth_ty is None:
                raise Exception(f"No such method: {meth_name}")
            if (
                not isinstance(meth.return_type, typesys.VoidType)
                or not meth_ty.static
                or meth_ty.formal_parameters
            ):
                raise Exception(
                    f"Invalid signature for main method: {meth_name}"
                )
            main_method = _mangle_ident(
                *ModulePath.from_class_path(self._main_method)
            )
            main_func = codegen.CFunc(
                name="main",
                return_type=codegen.CNamedType("int"),
                parameters=[],
                body=[
                    codegen.CExprStmt(
                        codegen.CCallExpr(
                            target=codegen.CVariable(main_method),
                            arguments=[],
                        )
                    ),
                    codegen.CReturnStmt(codegen.CInteger(0)),
                ],
            )
            self.emit()
            main_func.emit(self)

    def _method_field(
        self, class_type: typesys.Class, meth_name: str
    ) -> codegen.CStructField:
        meth_ty = class_type.methods[meth_name]
        assert not meth_ty.static
        c_type = _ty_to_ctype(self.ctx, typesys.FunctionInstance(meth_ty, []))

        assert isinstance(c_type, codegen.CFuncType)
        c_type.parameter_types.insert(
            0, _ty_to_ctype(self.ctx, typesys.ClassInstance(class_type, []))
        )

        return codegen.CStructField(name=meth_name, type=c_type)

    def _method(
        self,
        class_type: typesys.Class,
        meth_name: str,
        method: ast.Method,
        static: bool = False,
    ) -> None:
        meth_ty = class_type.methods[meth_name]

        method_cg = MethodCodeGenerator(
            self.ctx,
            class_ty=class_type,
            meth_ty=meth_ty,
            method=method,
            indent_str=self._indent_str,
        )

        method_cg.generate()
        self._buf += method_cg._buf

    # FIXME: need to generate array types for all in GlobalContext at the end
    # def _get_or_create_array_type(
    #     self, typ: ty.ArrayType
    # ) -> codegen.CStructType:
    #     if typ in self._generated_array_types:
    #         return self._generated_array_types[typ].type

    #     element_type = typ.element_type
    #     struct = codegen.CStruct(name=_array_type_name(self.ctx, typ))
    #     self._generated_array_types[typ] = struct
    #     struct.fields.extend(
    #         [
    #             codegen.CStructField(
    #                 name="elements",
    #                 type=_ty_to_ctype(self.ctx, element_type).as_pointer(),
    #             ),
    #             codegen.CStructField(
    #                 name="length", type=codegen.CNamedType("int")
    #             ),
    #         ]
    #     )
    #     struct.emit(self)
    #     return struct.type


class MethodCodeGenerator(CodeGenerator):
    def __init__(
        self,
        ctx: GlobalContext,
        class_ty: typesys.Class,
        meth_ty: typesys.Function,
        method: ast.Method,
        indent_str: str = "    ",
    ):
        super().__init__(indent_str)
        self.ctx = ctx
        self._class_ty = class_ty
        self._method = method
        self._method_ty = meth_ty
        vis = MethodExprTypeVisitor(ctx.type_scope, class_ty, meth_ty, method)
        vis.visit(method)
        self._node_types = vis.expr_types
        self._internal_locals: t.List[t.Tuple[str, codegen.CType]] = []

    def _unique_local(self, typ: codegen.CType) -> str:
        idx = len(self._internal_locals)
        name = f"__joe_local_{idx}"
        self._internal_locals.append((name, typ))
        return name

    def _var_name(self, base_name: str) -> str:
        return _mangle_ident(
            *ModulePath.from_class_path(self._class_ty.id.name),
            self._method.name.value,
            base_name,
        )

    def generate(self) -> None:
        meth_name = _mangle_ident(
            *ModulePath.from_class_path(self._class_ty.id.name),
            self._method.name.value,
        )
        c_type = codegen.CFunc(
            name=meth_name,
            return_type=_ty_to_ctype(self.ctx, self._method_ty.return_type),
            parameters=[
                codegen.CParam(
                    name=self._var_name(p_node.name.value),
                    type=_ty_to_ctype(self.ctx, p_type),
                )
                for p_node, p_type in zip(
                    self._method.parameters, self._method_ty.formal_parameters
                )
            ],
            body=[],
        )

        if not self._method.static:
            c_type.parameters.insert(
                0,
                codegen.CParam(
                    name="self",
                    type=_ty_to_ctype(
                        self.ctx, typesys.ClassInstance(self._class_ty, [])
                    ),
                ),
            )

        for stmt in self._method.body:
            c_type.body.append(self._compile_stmt(stmt))

        internal_locals: t.List[codegen.CStmt] = []
        for loc_name, loc_ty in self._internal_locals:
            internal_locals.append(codegen.CVarDecl(name=loc_name, type=loc_ty))
        internal_locals.extend(c_type.body)
        c_type.body = internal_locals
        c_type.emit(self)

    def _compile_stmt(self, stmt: ast.Stmt) -> codegen.CStmt:
        if isinstance(stmt, ast.ExprStmt):
            expr = self._compile_expr(stmt.expr)
            return codegen.CExprStmt(expr)
        elif isinstance(stmt, ast.ReturnStmt):
            if stmt.expr:
                cexpr: t.Optional[codegen.CExpr] = self._compile_expr(stmt.expr)
            else:
                cexpr = None
            return codegen.CReturnStmt(cexpr)
        raise NotImplementedError(stmt)

    def _compile_expr(self, expr: ast.Expr) -> codegen.CExpr:
        if isinstance(expr, ast.IdentExpr):
            typ = self._node_types[expr]
            if isinstance(typ, typesys.FunctionInstance):
                name = _mangle_ident(
                    *ModulePath.from_class_path(self._class_ty.id.name),
                    typ.function.id.name,
                )
            elif expr.name in self._class_ty.members:
                return codegen.CFieldAccess(
                    struct_value=codegen.CFieldAccess(
                        struct_value=codegen.CVariable("self"),
                        field_name="data",
                        pointer=False,
                    ),
                    field_name=expr.name,
                    pointer=True,
                )
            else:
                name = self._var_name(expr.name)
            return codegen.CVariable(name)
        elif isinstance(expr, ast.IntExpr):
            return codegen.CInteger(expr.value)
        elif isinstance(expr, ast.CallExpr):
            target_ty = self._node_types[expr.target]
            assert isinstance(target_ty, typesys.FunctionInstance)

            args: t.List[codegen.CExpr] = []

            # if expr.target is a DotExpr and target_ty is not static, use a
            # temporary for the left side of the dot and pass it as the first
            # argument to the call.
            # if expr.target is an identifier expression and target_ty is not
            # static, assert current function is not static and pass self as
            # first argument.

            exprs: t.List[codegen.CExpr] = []
            if (
                isinstance(expr.target, ast.DotExpr)
                and not target_ty.function.static
            ):
                # Put expr.target.left in a temporary
                tmp_local = codegen.CVariable(
                    self._unique_local(
                        _ty_to_ctype(
                            self.ctx, self._node_types[expr.target.left]
                        )
                    )
                )
                exprs.append(
                    codegen.CAssignmentExpr(
                        tmp_local,
                        self._compile_expr(expr.target.left),
                    )
                )
                args.append(tmp_local)
                ctarget = codegen.CFieldAccess(
                    struct_value=codegen.CFieldAccess(
                        struct_value=tmp_local,
                        field_name="vtable",
                        pointer=False,
                    ),
                    field_name=expr.target.name,
                    pointer=True,
                )
            elif (
                isinstance(expr.target, ast.IdentExpr)
                and not target_ty.function.static
            ):
                if self._method.static:
                    raise JoeTypeError(
                        expr.location,
                        "Can't call non-static method from static method",
                    )
                args.append(codegen.CVariable("self"))
                ctarget = self._compile_expr(expr.target)
            else:
                ctarget = self._compile_expr(expr.target)

            args.extend(self._compile_expr(arg) for arg in expr.arguments)
            exprs.append(codegen.CCallExpr(ctarget, args))

            return codegen.CParens(codegen.CSeqExpr(exprs))
        elif isinstance(expr, ast.AssignExpr):
            ctarget = self._compile_expr(expr.target)
            cvalue = self._compile_expr(expr.value)
            assert isinstance(ctarget, codegen.CAssignmentTarget)
            return codegen.CAssignmentExpr(target=ctarget, value=cvalue)
        elif isinstance(expr, ast.PlusExpr):
            cleft = self._compile_expr(expr.left)
            cright = self._compile_expr(expr.right)
            left_ty = self._node_types[expr.left]
            right_ty = self._node_types[expr.right]
            if isinstance(left_ty, typesys.DoubleType) ^ isinstance(
                right_ty, typesys.DoubleType
            ):
                to_cast = (
                    cleft if isinstance(left_ty, typesys.DoubleType) else cright
                )
                casted = codegen.CCast(
                    to_cast, _ty_to_ctype(self.ctx, typesys.DoubleType())
                )
                if isinstance(left_ty, typesys.DoubleType):
                    cleft = casted
                else:
                    cright = casted
            return codegen.CBinExpr(
                left=cleft, right=cright, op=codegen.BinOp.Add
            )
        elif isinstance(expr, ast.DotExpr):
            cleft = self._compile_expr(expr.left)
            left_ty = self._node_types[expr.left]
            result_ty = self._node_types[expr]

            assert isinstance(left_ty, typesys.ClassInstance)
            if (
                isinstance(result_ty, typesys.FunctionInstance)
                and result_ty.function.static
            ):
                return codegen.CVariable(
                    name=_mangle_ident(
                        *ModulePath.from_class_path(left_ty.class_.id.name),
                        expr.name,
                    )
                )
            else:
                mem = left_ty.get_member(expr.name)
                if mem is not None:
                    return codegen.CFieldAccess(
                        struct_value=codegen.CFieldAccess(
                            struct_value=cleft,
                            field_name="data",
                            pointer=False,
                        ),
                        field_name=expr.name,
                        pointer=True,
                    )
                meth = left_ty.get_method(expr.name)
                if meth is not None:
                    return codegen.CFieldAccess(
                        struct_value=codegen.CFieldAccess(
                            struct_value=cleft,
                            field_name="vtable",
                            pointer=False,
                        ),
                        field_name=expr.name,
                        pointer=True,
                    )
                raise JoeUnreachable()
        elif isinstance(expr, ast.NewExpr):
            new_ty = self.ctx.type_scope[expr.path]
            obj_ty = self._node_types[expr]
            assert isinstance(new_ty, typesys.Class)
            assert isinstance(obj_ty, typesys.ClassInstance)
            cnew_ty = _ty_to_ctype(self.ctx, obj_ty)
            tmp_local = codegen.CVariable(self._unique_local(cnew_ty))
            new_exprs: t.List[codegen.CExpr] = [
                # Allocate the object data struct
                codegen.CAssignmentExpr(
                    target=codegen.CFieldAccess(
                        struct_value=tmp_local,
                        field_name="data",
                        pointer=False,
                    ),
                    value=codegen.CCallExpr(
                        target=codegen.CVariable("malloc"),
                        arguments=[
                            codegen.CCallExpr(
                                target=codegen.CVariable("sizeof"),
                                arguments=[
                                    codegen.CTypeExpr(
                                        codegen.CStructType(
                                            _mangle_ident(
                                                *ModulePath.from_class_path(
                                                    new_ty.id.name
                                                ),
                                                "data",
                                            ),
                                        ),
                                    )
                                ],
                            ),
                        ],
                    ),
                ),
                # Assign the vtable
                codegen.CAssignmentExpr(
                    target=codegen.CFieldAccess(
                        struct_value=tmp_local,
                        field_name="vtable",
                        pointer=False,
                    ),
                    value=codegen.CRef(
                        codegen.CVariable(
                            _mangle_ident(
                                *ModulePath.from_class_path(new_ty.id.name),
                                "vtable",
                            )
                        )
                    ),
                ),
            ]
            unqualified_name = ModulePath.from_class_path(new_ty.id.name)[-1]
            constructor = obj_ty.get_method(unqualified_name)
            if constructor is not None:
                constructor_args = [tmp_local] + [
                    self._compile_expr(arg) for arg in expr.arguments
                ]

                # Call the constructor
                new_exprs.append(
                    codegen.CCallExpr(
                        target=codegen.CFieldAccess(
                            struct_value=codegen.CFieldAccess(
                                struct_value=tmp_local,
                                field_name="vtable",
                                pointer=False,
                            ),
                            field_name=ModulePath.from_class_path(
                                new_ty.id.name
                            )[-1],
                            pointer=True,
                        ),
                        arguments=constructor_args,
                    ),
                )
            # Leave the object as the result of the sequence expression
            new_exprs.append(tmp_local)
            return codegen.CEmitOnce(
                first_emit=codegen.CParens(codegen.CSeqExpr(new_exprs)),
                after=tmp_local,
            )
        else:
            raise NotImplementedError(expr)
