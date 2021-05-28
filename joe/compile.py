import dataclasses
import os
import typing as t
from contextlib import contextmanager

from joe import ast, codegen, types_ as ty
from joe.context import GlobalContext
from joe.parse import ModulePath
from joe.source import JoeTypeError


# FIXME: name mangling for int[] is not great
def _mangle_ident(*parts: str) -> str:
    ident = "__joe"
    for part in parts:
        ident += f"{len(part)}{part}"
    return f"{ident}E"


def _mangle_class_name(class_ty: ty.ClassType) -> str:
    return _mangle_ident(*ModulePath.from_class_path(class_ty.name))


_type_to_ident_table = str.maketrans(
    {
        "*": "_ptr",
        " ": "_",
        "[": "_array",
        "]": "_",
    }
)


def _array_type_name(ctx: GlobalContext, typ: ty.ArrayType) -> str:
    return _mangle_ident(_type_str(ctx, typ.element_type) + "_array")


def _type_str(ctx: GlobalContext, typ: ty.Type) -> str:
    return _ty_to_ctype(ctx, typ).render().translate(_type_to_ident_table)


def _ty_to_ctype(ctx: GlobalContext, typ: ty.Type) -> codegen.CType:
    if isinstance(typ, ty.IntType):
        return codegen.CNamedType("int")
    elif isinstance(typ, ty.VoidType):
        return codegen.CNamedType("void")
    elif isinstance(typ, ty.ArrayType):
        ctx.array_types.add(typ)
        return codegen.CStructType(name=_array_type_name(ctx, typ))
    elif isinstance(typ, ty.ClassType):
        return codegen.CStructType(_mangle_class_name(typ))
    elif isinstance(typ, ty.MethodType):
        return codegen.CFuncType(
            return_type=_ty_to_ctype(ctx, typ.return_type),
            parameter_types=[_ty_to_ctype(ctx, p.type) for p in typ.parameters],
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
        self._generated_array_types: t.Dict[ty.ArrayType, codegen.CStruct] = {}
        self._main_method = main_method

    def generate(self):
        for mod in self._modules:
            self.emit(f"/* start module {mod.name} */")
            self._generate_class(mod)
            self.emit(f"/* end module {mod.name} */")
            self.emit()

    def _generate_class(self, mod: ast.Module) -> None:
        # FIXME: generate static methods
        class_path = mod.name
        class_path_parts = ModulePath.from_class_path(class_path)
        class_type = self.ctx.types[mod.class_decl]
        assert isinstance(class_type, ty.ClassType)

        # Generate data struct type
        data_type = codegen.CStruct(
            name=_mangle_ident(*class_path_parts, "data")
        )
        for name, field_ty in class_type.instance_type.fields.items():
            if isinstance(field_ty, ty.ArrayType):
                self._get_or_create_array_type(field_ty)
                self.emit()

        for name, field_ty in class_type.instance_type.fields.items():
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
        for name in class_type.instance_type.methods:
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
        for name in class_type.instance_type.methods:
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
                            *ModulePath.from_class_path(class_type.name),
                            meth_name,
                        )
                    )
                    for meth_name in class_type.instance_type.methods
                ],
            ),
        )
        decl.emit(self)

        # emit static methods
        for meth_name in class_type.static_methods:
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
        self, class_type: ty.ClassType, meth_name: str
    ) -> codegen.CStructField:
        meth_ty = class_type.instance_type.methods[meth_name]
        c_type = _ty_to_ctype(self.ctx, meth_ty)

        assert isinstance(c_type, codegen.CFuncType)
        c_type.parameter_types.insert(0, _ty_to_ctype(self.ctx, class_type))

        return codegen.CStructField(name=meth_name, type=c_type)

    def _method(
        self,
        class_type: ty.ClassType,
        meth_name: str,
        method: ast.Method,
        static: bool = False,
    ) -> None:
        if static:
            meth_ty = class_type.static_methods[meth_name]
        else:
            meth_ty = class_type.instance_type.methods[meth_name]

        method_cg = MethodCodeGenerator(
            self.ctx,
            class_ty=class_type,
            meth_ty=meth_ty,
            method=method,
            indent_str=self._indent_str,
            static=static,
        )

        method_cg.generate()
        self._buf += method_cg._buf

    # FIXME: need to generate array types for all in GlobalContext at the end
    def _get_or_create_array_type(
        self, typ: ty.ArrayType
    ) -> codegen.CStructType:
        if typ in self._generated_array_types:
            return self._generated_array_types[typ].type

        element_type = typ.element_type
        struct = codegen.CStruct(name=_array_type_name(self.ctx, typ))
        self._generated_array_types[typ] = struct
        struct.fields.extend(
            [
                codegen.CStructField(
                    name="elements",
                    type=_ty_to_ctype(self.ctx, element_type).as_pointer(),
                ),
                codegen.CStructField(
                    name="length", type=codegen.CNamedType("int")
                ),
            ]
        )
        struct.emit(self)
        return struct.type


class MethodCodeGenerator(CodeGenerator):
    def __init__(
        self,
        ctx: GlobalContext,
        class_ty: ty.ClassType,
        meth_ty: ty.MethodType,
        method: ast.Method,
        static: bool,
        indent_str: str = "    ",
    ):
        super().__init__(indent_str)
        self.ctx = ctx
        self._class_ty = class_ty
        self._method = method
        self._method_ty = meth_ty
        self._scope = t.ChainMap[str, ty.Type](
            ctx.classes,
            class_ty.static_methods,
            class_ty.instance_type.methods,
            class_ty.instance_type.fields,
            dict((p.name, p.type) for p in meth_ty.parameters),
            {},  # locals
        )
        self._static = static
        self._internal_locals: t.List[t.Tuple[str, codegen.CType]] = []

    def _unique_local(self, typ: codegen.CType) -> str:
        idx = len(self._internal_locals)
        name = f"__joe_local_{idx}"
        self._internal_locals.append((name, typ))
        return name

    def _var_name(self, base_name: str) -> str:
        return _mangle_ident(
            *ModulePath.from_class_path(self._class_ty.name),
            self._method.name.value,
            base_name,
        )

    def generate(self) -> None:
        meth_name = _mangle_ident(
            *ModulePath.from_class_path(self._class_ty.name),
            self._method.name.value,
        )
        c_type = codegen.CFunc(
            name=meth_name,
            return_type=_ty_to_ctype(self.ctx, self._method_ty.return_type),
            parameters=[
                codegen.CParam(
                    name=self._var_name(p.name),
                    type=_ty_to_ctype(self.ctx, p.type),
                )
                for p in self._method_ty.parameters
            ],
            body=[],
        )

        if not self._static:
            c_type.parameters.insert(
                0,
                codegen.CParam(
                    name="self", type=_ty_to_ctype(self.ctx, self._class_ty)
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
            expr, _expr_ty = self._compile_expr(stmt.expr)
            return codegen.CExprStmt(expr)
        elif isinstance(stmt, ast.ReturnStmt):
            cexpr: t.Optional[codegen.CExpr]
            if stmt.expr:
                cexpr, ret_ty = self._compile_expr(stmt.expr)
                if ret_ty != self._method_ty.return_type:
                    raise JoeTypeError(stmt.location, "Invalid return type")
            else:
                cexpr = None
            return codegen.CReturnStmt(cexpr)
        raise NotImplementedError(stmt)

    def _compile_expr(self, expr: ast.Expr) -> t.Tuple[codegen.CExpr, ty.Type]:
        if isinstance(expr, ast.IdentExpr):
            # FIXME: hack
            if expr.name == "println":
                typ: ty.Type = ty.MethodType(
                    return_type=ty.VoidType(),
                    class_type=ty.ClassType(
                        name="",
                        instance_type=ty.ObjectType(
                            methods={},
                            fields={},
                        ),
                        static_methods={},
                    ),
                    parameters=[ty.Parameter(name="", type=ty.IntType())],
                    name="println",
                    static=True,
                )
                name = "println"
            else:
                typ = self._scope[expr.name]
                if isinstance(typ, ty.MethodType):
                    name = _mangle_ident(
                        *ModulePath.from_class_path(self._class_ty.name),
                        typ.name,
                    )
                elif expr.name in self._class_ty.instance_type.fields:
                    expr_ty = self._class_ty.instance_type.fields[expr.name]
                    return (
                        codegen.CFieldAccess(
                            struct_value=codegen.CFieldAccess(
                                struct_value=codegen.CVariable("self"),
                                field_name="data",
                                pointer=False,
                            ),
                            field_name=expr.name,
                            pointer=True,
                        ),
                        expr_ty,
                    )
                else:
                    name = self._var_name(expr.name)
            return codegen.CVariable(name), typ
        elif isinstance(expr, ast.IntExpr):
            return codegen.CInteger(expr.value), ty.IntType()
        elif isinstance(expr, ast.CallExpr):
            ctarget, target_ty = self._compile_expr(expr.target)
            if not isinstance(target_ty, ty.MethodType):
                raise NotImplementedError()

            args: t.List[codegen.CExpr] = []

            if not target_ty.static:
                if target_ty.bound_to:
                    args.append(target_ty.bound_to)
                else:
                    if self._method_ty.static:
                        raise JoeTypeError(
                            expr.location,
                            "Can't call non-static method from static method",
                        )
                    args.append(codegen.CVariable("self"))

            # FIXME: hack
            if target_ty.name == "println":
                ctarget = codegen.CVariable("printf")
                args.append(codegen.CStringLiteral("%d\n"))

            if len(expr.arguments) != len(target_ty.parameters):
                raise JoeTypeError(
                    expr.location, "Incorrect number of arguments"
                )

            for param, arg in zip(target_ty.parameters, expr.arguments):
                cparam, param_ty = self._compile_expr(arg)
                if param_ty != param.type:
                    raise JoeTypeError(arg.location, "Invalid parameter type")
                args.append(cparam)

            return codegen.CCallExpr(ctarget, args), target_ty.return_type
        elif isinstance(expr, ast.AssignExpr):
            if not isinstance(expr.target, ast.IdentExpr):
                raise JoeTypeError(
                    expr.location, "Can't assign to field on that type"
                )
            ctarget, target_ty = self._compile_expr(expr.target)
            cvalue, value_ty = self._compile_expr(expr.value)
            if value_ty != target_ty:
                raise JoeTypeError(expr.location, "Incorrect type")
            assert isinstance(ctarget, codegen.CAssignmentTarget)
            return (
                codegen.CAssignmentExpr(target=ctarget, value=cvalue),
                value_ty,
            )
        elif isinstance(expr, ast.PlusExpr):
            cleft, left_ty = self._compile_expr(expr.left)
            cright, right_ty = self._compile_expr(expr.right)
            if left_ty != ty.IntType() or right_ty != ty.IntType():
                loc = (
                    expr.left.location
                    if left_ty != ty.IntType()
                    else expr.right.location
                )
                raise JoeTypeError(loc, "Can only add integers")
            return (
                codegen.CBinExpr(
                    left=cleft, right=cright, op=codegen.BinOp.Add
                ),
                ty.IntType(),
            )
        elif isinstance(expr, ast.DotExpr):
            cleft, left_ty = self._compile_expr(expr.left)
            if not isinstance(left_ty, (ty.ClassType, ty.ObjectType)):
                raise JoeTypeError(
                    expr.location, "Can't access field of non-class non-object"
                )
            if isinstance(left_ty, ty.ClassType):
                if expr.name not in left_ty.static_methods:
                    raise JoeTypeError(expr.location, "No such field")
                return (
                    codegen.CVariable(
                        name=_mangle_ident(
                            *ModulePath.from_class_path(left_ty.name), expr.name
                        )
                    ),
                    left_ty.static_methods[expr.name],
                )
            else:
                if expr.name in left_ty.fields:
                    return (
                        codegen.CFieldAccess(
                            struct_value=codegen.CFieldAccess(
                                struct_value=cleft,
                                field_name="data",
                                pointer=False,
                            ),
                            field_name=expr.name,
                            pointer=True,
                        ),
                        left_ty.fields[expr.name],
                    )
                elif expr.name in left_ty.methods:
                    return (
                        codegen.CFieldAccess(
                            struct_value=codegen.CFieldAccess(
                                struct_value=cleft,
                                field_name="vtable",
                                pointer=False,
                            ),
                            field_name=expr.name,
                            pointer=True,
                        ),
                        dataclasses.replace(
                            left_ty.methods[expr.name], bound_to=cleft
                        ),
                    )
                else:
                    raise JoeTypeError(expr.location, "No such field")
        elif isinstance(expr, ast.NewExpr):
            new_ty = self._scope.get(expr.path)
            if new_ty is None or not isinstance(new_ty, ty.ClassType):
                raise JoeTypeError(expr.location, "No such class")
            cnew_ty = _ty_to_ctype(self.ctx, new_ty)
            tmp_local = codegen.CVariable(self._unique_local(cnew_ty))
            exprs: t.List[codegen.CExpr] = [
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
                                                    new_ty.name
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
                                *ModulePath.from_class_path(new_ty.name),
                                "vtable",
                            )
                        )
                    ),
                ),
            ]
            unqualified_name = ModulePath.from_class_path(new_ty.name)[-1]
            if unqualified_name in new_ty.instance_type.methods:
                constructor_ty = new_ty.instance_type.methods[unqualified_name]

                if len(expr.arguments) != len(constructor_ty.parameters):
                    raise JoeTypeError(
                        expr.location,
                        "Wrong number of arguments to constructor",
                    )

                constructor_args: t.List[codegen.CExpr] = [tmp_local]
                for param, arg in zip(
                    constructor_ty.parameters, expr.arguments
                ):
                    cparam, param_ty = self._compile_expr(arg)
                    if param_ty != param.type:
                        raise JoeTypeError(
                            arg.location, "Wrong constructor arg type"
                        )
                    constructor_args.append(cparam)

                # Call the constructor
                exprs.append(
                    codegen.CCallExpr(
                        target=codegen.CFieldAccess(
                            struct_value=codegen.CFieldAccess(
                                struct_value=tmp_local,
                                field_name="vtable",
                                pointer=False,
                            ),
                            field_name=ModulePath.from_class_path(new_ty.name)[
                                -1
                            ],
                            pointer=True,
                        ),
                        arguments=constructor_args,
                    ),
                )
            # Leave the object as the result of the sequence expression
            exprs.append(tmp_local)
            return (
                codegen.CEmitOnce(
                    first_emit=codegen.CParens(codegen.CSeqExpr(exprs)),
                    after=tmp_local,
                ),
                new_ty.instance_type,
            )
        else:
            raise NotImplementedError(expr)
