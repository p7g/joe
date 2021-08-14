import typing as t
from collections import namedtuple
from functools import partial
from patina import Option, None_
from typing_extensions import TypeGuard

from joe import ast, cnodes, mangle, objects, typesys
from joe.context import GlobalContext, TypeContext, NullType
from joe.exc import JoeUnreachable
from joe.parse import ModulePath
from joe.scopevisitor import ScopeVisitor, escape_name
from joe.typevisitor import Arrays, MethodExprTypeVisitor
from joe.visitor import Visitor


def prefix_ident(name: str) -> str:
    return f"__joe_{name}"


def escape_attribute_name(name: str) -> str:
    return prefix_ident(f"{len(name)}{name}")


def _get_type_name(ctx: TypeContext, ty: typesys.Type) -> str:
    if isinstance(ty, typesys.BottomType):
        path = "void"
        arguments = []
    else:
        assert isinstance(ty, typesys.Instance)
        class_info = ctx.get_class_info(ty.type_constructor)
        if class_info is None:
            path = ctx.get_primitive_name(ty.type_constructor)
        else:
            path = class_info.id.name
        arguments = [_get_type_name(ctx, arg) for arg in ty.arguments]
    return mangle.mangle_name(path, arguments)


def get_type_name(ctx: TypeContext, ty: typesys.Instance) -> str:
    return prefix_ident(_get_type_name(ctx, ty))


def get_class_member_name(
    ctx: TypeContext, ty: typesys.Instance, member: str
) -> str:
    class_name = get_type_name(ctx, ty)
    # FIXME: name mangling logic is leaking
    return f"{class_name}{len(member)}{member}E"


def get_class_method_impl_name(
    ctx: TypeContext,
    obj_ty: typesys.Instance,
    meth_name: str,
) -> str:
    ci = get_obj_class_info(ctx, obj_ty)
    assert ci is not None
    meth_attr = ci.get_attribute(meth_name)
    assert isinstance(meth_attr, objects.Method)

    member_name = (
        _get_type_name(ctx, typesys.Instance(meth_attr.class_info.type, []))
        + f"{len(meth_name)}{meth_name}"
    )
    member_name += mangle.type_suffix(
        [_get_type_name(ctx, arg) for arg in meth_attr.parameter_types]
    )
    return prefix_ident(member_name + "E")


def get_class_data_name(ctx: TypeContext, obj: typesys.Instance) -> str:
    name = get_type_name(ctx, obj)
    return f"{name}_data"


def get_class_vtable_name(ctx: TypeContext, obj: typesys.Instance) -> str:
    name = get_type_name(ctx, obj)
    return f"{name}_vtable"


def get_class_vtable(ctx: TypeContext, ci: objects.ClassInfo) -> cnodes.CExpr:
    name = get_class_vtable_name(ctx, typesys.Instance(ci.type, []))
    return cnodes.CRef(cnodes.CVariable(name))


def get_ctype(ctx: TypeContext, typ: typesys.Type) -> cnodes.CType:
    if isinstance(
        typ, typesys.Instance
    ) and typ.type_constructor == ctx.get_type_constructor("int"):
        return cnodes.CNamedType("int")
    elif isinstance(
        typ, typesys.Instance
    ) and typ.type_constructor == ctx.get_type_constructor("double"):
        return cnodes.CNamedType("double")
    elif isinstance(
        typ, typesys.Instance
    ) and typ.type_constructor == ctx.get_type_constructor("boolean"):
        return cnodes.CNamedType("int")
    elif isinstance(typ, typesys.BottomType):
        return cnodes.CNamedType("void")
    elif isinstance(typ, typesys.Instance):
        if typ.type_constructor.is_function:
            return cnodes.CFuncType(
                return_type=get_ctype(ctx, typ.arguments[-1]),
                parameter_types=[get_ctype(ctx, p) for p in typ.arguments[:-1]],
            )
        elif typ.type_constructor == Arrays.get_type_constructor():
            ctx.add_array_type(typ)
            return cnodes.CStructType(get_array_name(ctx, typ))
        else:
            return cnodes.CNamedType(get_type_name(ctx, typ))
    raise NotImplementedError(typ)


def get_obj_class_info(
    ctx: TypeContext, obj_ty: typesys.Instance
) -> objects.ClassInfo:
    ci = ctx.get_class_info(obj_ty.type_constructor)
    assert ci is not None
    return ci


def get_class_method(
    ctx: TypeContext,
    obj: cnodes.CExpr,
    obj_ty: typesys.Instance,
    name: str,
    *,
    force_static_dispatch: bool = False,
) -> cnodes.CExpr:
    ci = get_obj_class_info(ctx, obj_ty)

    meth = ci.get_attribute(name)
    assert isinstance(meth, objects.Method)

    if meth.final or ci.final or force_static_dispatch:
        assert isinstance(meth.type, typesys.Instance)
        return cnodes.CVariable(get_class_method_impl_name(ctx, obj_ty, name))
    else:
        # FIXME: ensure obj doesn't have side-effects
        _data, vtable = cast_as_parent(ctx, obj, obj_ty, meth.class_info)
        return get_struct_field(
            vtable, escape_attribute_name(name), pointer=True
        )


def obj_as_parent(
    type_ctx: TypeContext,
    class_info: objects.ClassInfo,
    parent_class_info: objects.ClassInfo,
    obj: cnodes.CExpr,
) -> cnodes.CExpr:
    return make_struct_literal(
        get_ctype(
            type_ctx,
            typesys.Instance(parent_class_info.type, []),
        ),
        list(
            cast_as_parent(
                type_ctx,
                obj,
                typesys.Instance(class_info.type, []),
                parent_class_info,
            )
        ),
    )


def get_self(
    class_info: objects.ClassInfo, is_constructor: bool
) -> cnodes.CAssignmentTarget:
    slf = cnodes.CVariable("self")
    if is_constructor and optimize_single_field(class_info):
        return cnodes.CArrayIndex(slf, cnodes.CInteger(0))
    return slf


def get_struct_field(
    struct: cnodes.CExpr, name: str, pointer: bool = False
) -> cnodes.CAssignmentTarget:
    return cnodes.CFieldAccess(
        struct_value=struct,
        field_name=name,
        pointer=pointer,
    )


def get_object_data(
    ctx: TypeContext, obj: cnodes.CExpr, obj_ty: typesys.Instance
) -> cnodes.CExpr:
    if not is_array_type(obj_ty):
        ci = ctx.get_class_info(obj_ty.type_constructor)
        assert ci is not None
        if ci.final:
            return obj
    return get_struct_field(obj, "data")


def get_object_vtable(
    ctx: TypeContext, obj: cnodes.CExpr, obj_ty: typesys.Instance
) -> cnodes.CExpr:
    ci = ctx.get_class_info(obj_ty.type_constructor)
    assert ci is not None
    if ci.final:
        return get_class_vtable(ctx, ci)
    return get_struct_field(obj, "vtable")


def optimize_single_field(ci: objects.ClassInfo) -> bool:
    # TODO: Maybe we can still do the optimization if the superclass has no
    # fields?
    return (
        ci.final
        and ci.field_count == 1
        and not ci.superclass
        and next(ci.fields()).final
    )


def get_class_field(
    ctx: TypeContext,
    obj: cnodes.CExpr,
    obj_ty: typesys.Instance,
    name: str,
) -> cnodes.CExpr:
    ci = get_obj_class_info(ctx, obj_ty)
    if optimize_single_field(ci):
        only_field = next(ci.fields())
        assert name == only_field.name
        data = get_object_data(ctx, obj, obj_ty)
        if only_field.final:
            return data
        else:
            return cnodes.CArrayIndex(data, cnodes.CInteger(0))

    attr = get_obj_class_info(ctx, obj_ty).get_attribute(name)
    assert isinstance(attr, objects.Field)

    data2, _vtable = cast_as_parent(
        ctx,
        obj,
        obj_ty,
        attr.class_info,
    )

    # like: obj.data->field_name
    return get_struct_field(data2, escape_attribute_name(name), pointer=True)


def get_local_name(name: str) -> str:
    return f"__joe_{name}"


def is_array_type(ty: typesys.Type) -> TypeGuard[typesys.Instance]:
    return (
        isinstance(ty, typesys.Instance)
        and ty.type_constructor == Arrays.get_type_constructor()
    )


def get_array_length(expr: cnodes.CExpr) -> cnodes.CExpr:
    return get_struct_field(expr, "length")


def get_array_element(
    expr: cnodes.CExpr, index: cnodes.CExpr
) -> cnodes.CAssignmentTarget:
    return cnodes.CArrayIndex(
        array_value=get_struct_field(expr, "data"),
        index_value=index,
    )


def make_assign_stmt(
    dest: cnodes.CAssignmentTarget, value: cnodes.CExpr
) -> cnodes.CStmt:
    return cnodes.CExprStmt(cnodes.CAssignmentExpr(dest, value))


def make_struct_literal(
    ty: cnodes.CType, elements: t.List[cnodes.CExpr]
) -> cnodes.CExpr:
    return cnodes.CCast(
        value=cnodes.CArrayLiteral(elements),
        new_type=ty,
    )


def get_array_name(ctx: TypeContext, array_ty: typesys.Type) -> str:
    assert is_array_type(array_ty)
    return prefix_ident(
        mangle.mangle_name(
            "joe.0virtual.Array", [_get_type_name(ctx, array_ty.arguments[0])]
        )
    )


def is_null(ty: typesys.Instance) -> TypeGuard[NullType]:
    return isinstance(ty, NullType)


NULL = cnodes.CVariable("NULL")


def make_null(ctx: TypeContext, ci: objects.ClassInfo) -> cnodes.CExpr:
    if optimize_single_field(ci):
        raise NotImplementedError()
    elif ci.final:
        return NULL
    else:
        return make_struct_literal(
            get_ctype(ctx, typesys.Instance(ci.type, [])),
            [NULL, NULL],
        )


def make_array_struct(
    ctx: TypeContext, array_ty: typesys.Type
) -> cnodes.CStruct:
    assert is_array_type(array_ty)
    int_tycon = ctx.get_type_constructor("int")
    assert int_tycon is not None
    return cnodes.CStruct(
        get_array_name(ctx, array_ty),
        fields=[
            cnodes.CStructField(
                name="data",
                type=get_ctype(ctx, array_ty.arguments[0]).as_pointer(),
            ),
            cnodes.CStructField(
                name="length",
                type=get_ctype(ctx, typesys.Instance(int_tycon, [])),
            ),
        ],
    )


def make_malloc(type_: cnodes.CType, n: cnodes.CExpr = None) -> cnodes.CExpr:
    size_of_type = cnodes.CCallExpr(
        target=cnodes.CVariable("sizeof"),
        arguments=[cnodes.CTypeExpr(type_)],
    )
    expr: cnodes.CExpr = size_of_type
    if n is not None:
        expr = cnodes.CBinExpr(size_of_type, n, cnodes.BinOp.Multiply)
    return cnodes.CCallExpr(
        target=cnodes.CVariable("malloc"),
        arguments=[expr],
    )


def make_free(expr: cnodes.CExpr) -> cnodes.CStmt:
    return cnodes.CExprStmt(
        expr=cnodes.CCallExpr(
            target=cnodes.CVariable("free"),
            arguments=[expr],
        )
    )


def cast_as_parent(
    ctx: TypeContext,
    obj: cnodes.CExpr,
    obj_ty: typesys.Instance,
    parent: objects.ClassInfo,
) -> t.Tuple[cnodes.CExpr, cnodes.CExpr]:
    """Get the data and vtable structs for `parent` from `obj`.

    `obj` should be an instance of `parent` or a descendant of it. It is the
    responsibility of the caller to use vtable only if the object has a vtable
    (i.e. it's not final). `obj` will be used in both the returned data and
    vtable expressions, so it should not have side-effects.
    """

    dist = 0
    cur = obj_ty
    while True:
        if cur.type_constructor == parent.type:
            break
        dist += 1
        sup = cur.type_constructor.super
        assert not isinstance(sup, typesys.TopType), "parent not in ancestors"
        cur = sup

    ci = ctx.get_class_info(obj_ty.type_constructor)
    assert ci is not None

    data: cnodes.CExpr = get_object_data(ctx, obj, obj_ty)
    vtable: cnodes.CExpr = get_object_vtable(ctx, obj, obj_ty)

    if dist > 0:
        for i in range(dist):
            data = get_struct_field(data, "parent", pointer=i == 0)
            vtable = get_struct_field(vtable, "parent", pointer=i == 0)
        data = cnodes.CRef(data)
        vtable = cnodes.CRef(vtable)

    return data, vtable


class CompileContext:
    def __init__(
        self, global_ctx: GlobalContext, code_unit: cnodes.CCodeUnit
    ) -> None:
        self.global_ctx = global_ctx
        self.code_unit = cnodes.CCodeUnit()

    @property
    def types(self) -> TypeContext:
        return self.global_ctx.type_ctx


class ClassCompileContext:
    def __init__(
        self,
        compile_context: CompileContext,
        class_info: objects.ClassInfo,
    ) -> None:
        self.compile_context = compile_context
        self.class_info = class_info

    @property
    def types(self) -> TypeContext:
        return self.compile_context.types

    @property
    def global_ctx(self) -> GlobalContext:
        return self.compile_context.global_ctx

    @property
    def code_unit(self) -> cnodes.CCodeUnit:
        return self.compile_context.code_unit


class MethodCompileContext:
    def __init__(
        self,
        class_compile_context: ClassCompileContext,
        method: objects.Method,
        cfunc: cnodes.CFunc,
        method_type_visitor: MethodExprTypeVisitor,
        is_constructor: bool,
        node: ast.Method,
    ) -> None:
        self.class_compile_context = class_compile_context
        self.method = method
        self.cfunc = cfunc
        self.method_type_visitor = method_type_visitor
        self.is_constructor = is_constructor
        self.node = node

    @property
    def types(self) -> TypeContext:
        return self.class_compile_context.types

    @property
    def expr_types(self) -> t.Dict[ast.Node, typesys.Type]:
        return self.method_type_visitor.expr_types

    @property
    def global_ctx(self) -> GlobalContext:
        return self.class_compile_context.global_ctx

    @property
    def code_unit(self) -> cnodes.CCodeUnit:
        return self.class_compile_context.code_unit

    @property
    def class_info(self) -> objects.ClassInfo:
        return self.class_compile_context.class_info


class CompileVisitor(Visitor):
    def __init__(self, ctx: GlobalContext) -> None:
        self.ctx = CompileContext(ctx, cnodes.CCodeUnit())
        self.ctx.code_unit.includes.append("stdlib.h")

    def add_array_structs(self) -> None:
        for array_ty in self.ctx.types._used_array_types:
            assert is_array_type(array_ty)
            struct = make_array_struct(self.ctx.types, array_ty)
            self.ctx.code_unit.structs.append(struct)

    def visit_ClassDeclaration(self, node: ast.ClassDeclaration) -> None:
        class_ty = self.ctx.types.get_type_constructor(node.name.value)
        assert class_ty is not None
        class_info = self.ctx.types.get_class_info(class_ty)
        assert class_info is not None

        ctx = ClassCompileContext(self.ctx, class_info)
        ClassCompiler.compile(ctx, node)

    def compile_main_function(self, class_path: str):
        cls_ty = self.ctx.types.get_type_constructor(class_path)
        class_info = self.ctx.types.get_class_info(cls_ty) if cls_ty else None
        if cls_ty is None or class_info is None:
            # FIXME: Another exception type?
            raise Exception(f"Invalid class for main method: {class_path}")
        meth = class_info.get_attribute("main")
        if not isinstance(meth, objects.Method):
            raise Exception(f"No main method on class {class_path}")
        if (
            not isinstance(meth.return_type, typesys.BottomType)
            or not meth.static
            or meth.parameter_types
        ):
            raise Exception(
                f"Invalid signature for main method on class {class_path}"
            )
        assert isinstance(meth.type, typesys.Instance)
        main_name = get_class_method_impl_name(
            self.ctx.types, typesys.Instance(cls_ty, []), "main"
        )
        main_func = cnodes.CFunc(
            name="main",
            return_type=cnodes.CNamedType("int"),
            parameters=[],
            locals=[],
            body=[
                cnodes.CExprStmt(
                    cnodes.CCallExpr(
                        target=cnodes.CVariable(main_name),
                        arguments=[],
                    )
                ),
                cnodes.CReturnStmt(cnodes.CInteger(0)),
            ],
            static=False,
        )
        self.ctx.code_unit.functions.append(main_func)


class ClassCompiler(Visitor):
    def __init__(self, ctx: ClassCompileContext) -> None:
        self.ctx = ctx

    @classmethod
    def compile(
        cls, ctx: ClassCompileContext, node: ast.ClassDeclaration
    ) -> None:
        vis = cls(ctx)
        vis.visit(node)

    @property
    def type_ctx(self) -> TypeContext:
        return self.ctx.types

    def visit_ClassDeclaration(self, node: ast.ClassDeclaration) -> None:
        class_info = self.ctx.class_info
        class_ty = class_info.type
        obj_ty = typesys.Instance(class_ty, [])

        data_ctype = self._make_data_type(class_info)
        data_name = get_class_data_name(self.type_ctx, obj_ty)
        self.ctx.code_unit.typedefs.append(
            cnodes.CTypeDef(data_name, data_ctype)
        )
        data_ctype = cnodes.CNamedType(data_name)
        class_type_name = get_type_name(self.type_ctx, obj_ty)

        vtable_ctype = self._make_vtable_type(class_info)
        if class_info.superclass is not None:
            # Recursively populate `parent` structs for the chain of classes
            # (using `class_info.get_attribute(name)` to get the top-most
            # implementation)
            seen_methods = set()
            expr: t.Optional[cnodes.CArrayLiteral] = None
            for parent in reversed(list(class_info.hierarchy())):
                if expr is not None:
                    expr = cnodes.CArrayLiteral([expr])
                else:
                    expr = cnodes.CArrayLiteral([])

                for parent_method in parent.methods():
                    if (
                        parent_method.final
                        or parent_method.static
                        or parent_method.name in seen_methods
                    ):
                        continue
                    seen_methods.add(parent_method.name)

                    meth = class_info.get_attribute(parent_method.name)
                    assert isinstance(meth, objects.Method)

                    impl_name = get_class_method_impl_name(
                        self.type_ctx,
                        typesys.Instance(meth.class_info.type, []),
                        parent_method.name,
                    )

                    func_expr: cnodes.CExpr = cnodes.CVariable(impl_name)
                    if meth.overrides is not None:
                        # Cast override methods to the type of the method
                        # they're overriding
                        func_expr = cnodes.CCast(
                            value=func_expr,
                            new_type=cnodes.CFuncType(
                                return_type=get_ctype(
                                    self.type_ctx, meth.overrides.return_type
                                ),
                                parameter_types=[
                                    get_ctype(
                                        self.type_ctx,
                                        typesys.Instance(
                                            meth.overrides.class_info.type, []
                                        ),
                                    ),
                                    *map(
                                        partial(get_ctype, self.type_ctx),
                                        meth.overrides.parameter_types,
                                    ),
                                ],
                            ),
                        )
                    expr.elements.append(func_expr)

            vtable_expr = expr
        else:
            vtable_expr = cnodes.CArrayLiteral(
                elements=[
                    cnodes.CVariable(
                        get_class_method_impl_name(
                            self.type_ctx,
                            typesys.Instance(class_info.type, []),
                            meth.name,
                        )
                    )
                    for meth in class_info.methods()
                    if not meth.static
                ]
            )

        self.ctx.code_unit.variables.append(
            cnodes.CVarDecl(
                name=get_class_vtable_name(
                    self.type_ctx, typesys.Instance(class_info.type, [])
                ),
                type=vtable_ctype,
                value=vtable_expr,
            )
        )

        if class_info.final:
            # When a final class is used as a value of its own type, there is
            # no need to include a vtable, we can use static dispatch. The
            # vtable still exists in case the object is used as a value of a
            # superclass.
            class_ctype = cnodes.CTypeDef(class_type_name, data_ctype)
        else:
            class_struct = cnodes.CStruct(
                name=class_type_name,
                fields=[
                    cnodes.CStructField(
                        name="data",
                        type=data_ctype,
                    ),
                    cnodes.CStructField(
                        name="vtable",
                        type=vtable_ctype.as_pointer(),
                    ),
                ],
            )

            self.ctx.code_unit.structs.append(class_struct)

            class_ctype = cnodes.CTypeDef(class_type_name, class_struct.type)

        self.ctx.code_unit.typedefs.append(class_ctype)

        # Visit methods
        super().visit_ClassDeclaration(node)

    def _make_data_type(self, ci: objects.ClassInfo) -> cnodes.CType:
        if optimize_single_field(ci):
            field = next(ci.fields())
            field_type = get_ctype(self.type_ctx, field.type)
            # If a field is final it can't be reassigned, so there's no need to
            # put it behind a pointer.
            if field.final:
                # TODO: if the type of the field is larger than a pointer it should
                # probably still be a pointer to that type
                return field_type
            return field_type.as_pointer()

        data_ctype = cnodes.CStruct(
            name=get_class_data_name(
                self.type_ctx, typesys.Instance(ci.type, [])
            )
        )

        if ci.superclass is not None:
            # Add parent field to data struct
            data_ctype.fields.append(
                cnodes.CStructField(
                    name="parent",
                    # FIXME: need to make sure the data type is actually a
                    # struct
                    type=cnodes.CStructType(
                        get_class_data_name(
                            self.type_ctx,
                            typesys.Instance(ci.superclass.type, []),
                        )
                    ),
                )
            )

        for field in ci.fields():
            data_ctype.fields.append(
                cnodes.CStructField(
                    name=escape_attribute_name(field.name),
                    type=get_ctype(self.type_ctx, field.type),
                )
            )

        self.ctx.code_unit.structs.append(data_ctype)

        return data_ctype.type.as_pointer()

    def _make_vtable_type(self, ci: objects.ClassInfo) -> cnodes.CType:
        vtable_ctype = cnodes.CStruct(
            name=get_class_vtable_name(
                self.type_ctx, typesys.Instance(ci.type, [])
            )
        )

        if ci.superclass is not None:
            parent_vtable_name = get_class_vtable_name(
                self.type_ctx, typesys.Instance(ci.superclass.type, [])
            )
            vtable_ctype.fields.append(
                cnodes.CStructField(
                    name="parent",
                    type=cnodes.CStructType(parent_vtable_name),
                )
            )

        for method in ci.methods():
            if not method.static and not method.override:
                meth_cty = get_ctype(self.type_ctx, method.type)
                assert isinstance(meth_cty, cnodes.CFuncType)
                meth_cty.parameter_types.insert(
                    0,
                    get_ctype(self.type_ctx, typesys.Instance(ci.type, [])),
                )
                vtable_ctype.fields.append(
                    cnodes.CStructField(
                        name=escape_attribute_name(method.name), type=meth_cty
                    )
                )

        self.ctx.code_unit.structs.append(vtable_ctype)
        return vtable_ctype.type

    def visit_Method(self, node: ast.Method) -> None:
        class_ty = self.ctx.class_info
        meth = class_ty.attributes[node.name.value]
        assert isinstance(meth, objects.Method)

        method_type_visitor = MethodExprTypeVisitor(
            self.ctx.types, class_ty, meth
        )
        method_type_visitor.visit(node)

        cfunc = cnodes.CFunc(
            name=get_class_method_impl_name(
                self.ctx.types,
                typesys.Instance(class_ty.type, []),
                meth.name,
            ),
            return_type=get_ctype(self.ctx.types, meth.return_type),
            parameters=[
                cnodes.CParam(
                    name=get_local_name(escape_name(param.name.value)),
                    type=get_ctype(self.type_ctx, ty),
                )
                for param, ty in zip(node.parameters, meth.parameter_types)
            ],
            locals=[
                cnodes.CVarDecl(
                    name=get_local_name(l.actual_name),
                    type=get_ctype(self.type_ctx, l.type),
                )
                for l in method_type_visitor.locals.values()
            ],
            body=[],
        )

        is_constructor = meth.name == class_ty.id.basename
        if not node.static:
            self_type: cnodes.CType = get_ctype(
                self.type_ctx, typesys.Instance(class_ty.type, [])
            )
            if is_constructor and optimize_single_field(class_ty):
                self_type = self_type.as_pointer()
            cfunc.parameters.insert(
                0, cnodes.CParam(name="self", type=self_type)
            )

        ctx = MethodCompileContext(
            self.ctx,
            meth,
            cfunc,
            method_type_visitor=method_type_visitor,
            is_constructor=is_constructor,
            node=node,
        )
        MethodCompiler.compile(ctx, node)
        self.ctx.code_unit.functions.append(cfunc)


def new_variable(
    func: cnodes.CFunc, type_ctx: TypeContext, type_: typesys.Type
) -> cnodes.CAssignmentTarget:
    locs = func.locals
    new_loc_name = get_local_name(f"tmp_{len(locs)}")
    locs.append(
        cnodes.CVarDecl(name=new_loc_name, type=get_ctype(type_ctx, type_))
    )
    return cnodes.CVariable(new_loc_name)


def cache_in_local(
    func: cnodes.CFunc,
    type_ctx: TypeContext,
    expr: cnodes.CExpr,
    type_: typesys.Type,
) -> cnodes.CExpr:
    if isinstance(type_, typesys.BottomType):
        return expr
    var = new_variable(func, type_ctx, type_)
    func.body.append(make_assign_stmt(var, expr))
    return var


class MethodCompiler(ScopeVisitor):
    def __init__(
        self,
        ctx: MethodCompileContext,
    ) -> None:
        super().__init__()
        self.ctx = ctx

    @classmethod
    def compile(cls, ctx: MethodCompileContext, node: ast.Method) -> None:
        vis = cls(ctx)
        vis.visit(node)

    @property
    def type_ctx(self) -> TypeContext:
        return self.ctx.types

    def get_self(self) -> cnodes.CAssignmentTarget:
        return get_self(self.ctx.class_info, self.ctx.is_constructor)

    def get_node_type(self, node: ast.Node) -> typesys.Type:
        return self.ctx.expr_types[node]

    def new_variable(self, type_: typesys.Type) -> cnodes.CAssignmentTarget:
        return new_variable(self.ctx.cfunc, self.type_ctx, type_)

    def cache_in_local(
        self, expr: cnodes.CExpr, type_: typesys.Type
    ) -> cnodes.CExpr:
        return cache_in_local(self.ctx.cfunc, self.type_ctx, expr, type_)

    def compile_expr(self, expr: ast.Expr) -> cnodes.CExpr:
        return ExprCompiler.compile(self.ctx, expr)

    def visit_ExprStmt(self, node: ast.ExprStmt) -> None:
        super().visit_ExprStmt(node)

        self.ctx.cfunc.body.append(
            cnodes.CExprStmt(
                cnodes.CCast(
                    value=self.compile_expr(node.expr),
                    new_type=cnodes.CNamedType("void"),
                )
            )
        )

    def visit_DeleteStmt(self, node: ast.DeleteStmt) -> None:
        super().visit_DeleteStmt(node)

        # Free the data member of the object
        obj = self.compile_expr(node.expr)
        assert isinstance(obj, cnodes.CAssignmentTarget)
        ty = self.get_node_type(node.expr)
        assert isinstance(ty, typesys.Instance)

        if not is_array_type(ty):
            ci = get_obj_class_info(self.type_ctx, ty)
            if optimize_single_field(ci):
                # No extra allocation for the data
                return

        self.ctx.cfunc.body.append(
            make_free(get_object_data(self.type_ctx, obj, ty))
        )

    def visit_ReturnStmt(self, node: ast.ReturnStmt) -> None:
        super().visit_ReturnStmt(node)

        expr = None if node.expr is None else self.compile_expr(node.expr)
        self.ctx.cfunc.body.append(cnodes.CReturnStmt(expr))

    def visit_VarDeclaration(self, node: ast.VarDeclaration) -> None:
        super().visit_VarDeclaration(node)

        if node.initializer is None:
            return

        dest_name = self.resolve_name(node.name.value, location=node.location)
        dest_type = self.get_node_type(node)
        assert isinstance(dest_type, typesys.Instance)
        dest_ci = self.type_ctx.get_class_info(dest_type.type_constructor)

        src_type = self.get_node_type(node.initializer)
        assert isinstance(src_type, typesys.Instance)

        if is_null(src_type):
            assert dest_ci is not None
            value = make_null(self.type_ctx, dest_ci)
        else:
            value = self.compile_expr(node.initializer)
            src_ci = self.type_ctx.get_class_info(src_type.type_constructor)
            if src_ci is not None:
                assert dest_ci is not None
                value = obj_as_parent(
                    self.type_ctx,
                    src_ci,
                    dest_ci,
                    self.cache_in_local(value, src_type),
                )

        assign_stmt = make_assign_stmt(
            cnodes.CVariable(get_local_name(dest_name)), value
        )
        self.ctx.cfunc.body.append(assign_stmt)


class ExprCompiler(Visitor):
    def __init__(self, ctx: MethodCompileContext) -> None:
        self.ctx = ctx
        self.last_expr = None_[cnodes.CExpr]()
        self.receiver: Option[cnodes.CExpr] = None_()

    @classmethod
    def compile(
        cls,
        ctx: MethodCompileContext,
        node: ast.Expr,
    ) -> cnodes.CExpr:
        vis = cls(ctx)
        vis.visit(node)
        return vis.last_expr.take().unwrap()

    @property
    def type_ctx(self) -> TypeContext:
        return self.ctx.types

    def cache_in_local(
        self, expr: cnodes.CExpr, type_: typesys.Type
    ) -> cnodes.CExpr:
        return cache_in_local(self.ctx.cfunc, self.type_ctx, expr, type_)

    def try_get_node_type(self, node: ast.Node) -> t.Optional[typesys.Type]:
        return self.ctx.expr_types.get(node)

    def get_node_type(self, node: ast.Node) -> typesys.Type:
        return self.ctx.expr_types[node]

    def get_self(self) -> cnodes.CAssignmentTarget:
        return get_self(self.ctx.class_info, self.ctx.is_constructor)

    def new_variable(self, ty: typesys.Type) -> cnodes.CAssignmentTarget:
        return new_variable(self.ctx.cfunc, self.ctx.types, ty)

    def visit_IdentExpr(self, node: ast.IdentExpr) -> None:
        ty = self.try_get_node_type(node)
        if ty is None:
            # Static method call
            tycon = self.type_ctx.get_type_constructor(node.name)
            assert tycon is not None
            ci = self.type_ctx.get_class_info(tycon)
            assert ci is not None
            return
        expr: cnodes.CExpr
        assert isinstance(ty, typesys.Instance)
        if ty.type_constructor.is_function:
            func = self.ctx.class_info.get_attribute(node.name)
            assert isinstance(func, objects.Method)
            if func.static:
                assert isinstance(func.type, typesys.Instance)
                expr = cnodes.CVariable(
                    get_class_method_impl_name(
                        self.type_ctx,
                        typesys.Instance(self.ctx.class_info.type, []),
                        node.name,
                    )
                )
            else:
                expr = get_class_method(
                    self.type_ctx,
                    self.get_self(),
                    typesys.Instance(self.ctx.class_info.type, []),
                    node.name,
                )
                self.receiver.replace(self.get_self())
        else:
            local_name = self.ctx.method_type_visitor.try_resolve_name(
                node.name
            )
            if local_name is not None and get_local_name(local_name) in (
                l.name for l in self.ctx.cfunc.locals
            ):
                # It's a local variable. Scopes should already be flattened, so
                # variables are function-scoped.
                expr = cnodes.CVariable(get_local_name(local_name))
            elif node.name in (p.name.value for p in self.ctx.node.parameters):
                expr = cnodes.CVariable(
                    get_local_name(
                        self.ctx.method_type_visitor.resolve_name(
                            node.name,
                            location=node.location,
                        )
                    )
                )
            elif self.ctx.class_info.has_attribute(node.name):
                assert isinstance(
                    self.ctx.class_info.get_attribute(node.name), objects.Field
                )
                # It's accessing a field on self.
                expr = get_class_field(
                    self.type_ctx,
                    self.get_self(),
                    typesys.Instance(self.ctx.class_info.type, []),
                    node.name,
                )
            else:
                raise JoeUnreachable()
        self.last_expr.replace(expr)

    def visit_IntExpr(self, node: ast.IntExpr) -> None:
        self.last_expr.replace(cnodes.CInteger(node.value))

    def visit_CallExpr(self, node: ast.CallExpr) -> None:
        self.visit_Expr(node.target)
        target = self.last_expr.take().unwrap()
        func_ty = self.get_node_type(node.target)
        assert isinstance(func_ty, typesys.Instance)
        assert func_ty.type_constructor.is_function or isinstance(
            node.target, ast.SuperExpr
        )

        # FIXME: it should be easier to get the method info
        assert isinstance(
            node.target, (ast.IdentExpr, ast.DotExpr, ast.SuperExpr)
        )
        if isinstance(node.target, ast.IdentExpr):
            class_info = self.ctx.class_info
            meth_name = node.target.name
        elif isinstance(node.target, ast.DotExpr):
            recv_ty = self.try_get_node_type(node.target.left)
            if recv_ty is None:
                assert isinstance(node.target.left, ast.IdentExpr)
                tycon = self.type_ctx.get_type_constructor(
                    node.target.left.name
                )
                assert tycon is not None
                class_info2 = self.type_ctx.get_class_info(tycon)
            else:
                assert isinstance(recv_ty, typesys.Instance)
                class_info2 = self.type_ctx.get_class_info(
                    recv_ty.type_constructor
                )
            assert class_info2 is not None
            class_info = class_info2
            meth_name = node.target.name
        elif isinstance(node.target, ast.SuperExpr):
            assert self.ctx.is_constructor
            assert self.ctx.class_info.superclass is not None
            class_info = self.ctx.class_info.superclass
            meth_name = self.ctx.class_info.superclass.id.basename

        meth_info = class_info.get_attribute(meth_name)
        assert isinstance(meth_info, objects.Method)

        args: t.List[cnodes.CExpr] = []
        if not meth_info.static:
            receiver: cnodes.CExpr = self.receiver.take().unwrap()

            if meth_info.class_info.type != class_info.type:
                cfunc = self.ctx.cfunc

                recv_type = typesys.Instance(class_info.type, [])
                parent_type = typesys.Instance(meth_info.class_info.type, [])

                recv_obj = self.new_variable(recv_type)
                cfunc.body.append(make_assign_stmt(recv_obj, receiver))

                recv_data, recv_vtable = cast_as_parent(
                    self.type_ctx,
                    recv_obj,
                    recv_type,
                    meth_info.class_info,
                )

                receiver = self.new_variable(parent_type)
                cfunc.body.append(
                    make_assign_stmt(
                        receiver,
                        make_struct_literal(
                            get_ctype(
                                self.type_ctx,
                                typesys.Instance(meth_info.class_info.type, []),
                            ),
                            [recv_data, recv_vtable],
                        ),
                    ),
                )

            args.append(receiver)

        for param, arg in zip(meth_info.parameter_types, node.arguments):
            self.visit_Expr(arg)

            dest_type = param
            assert isinstance(dest_type, typesys.Instance)
            dest_ci = self.type_ctx.get_class_info(dest_type.type_constructor)

            src_type = self.get_node_type(arg)
            assert isinstance(src_type, typesys.Instance)
            src_ci = self.type_ctx.get_class_info(src_type.type_constructor)

            if is_null(dest_type):
                assert dest_ci is not None
                value = make_null(self.type_ctx, dest_ci)
            else:
                value = self.last_expr.take().unwrap()
                if src_ci is not None:
                    assert dest_ci is not None
                    value = obj_as_parent(
                        self.type_ctx,
                        src_ci,
                        dest_ci,
                        self.cache_in_local(value, src_type),
                    )

            args.append(value)

        result = self.cache_in_local(
            cnodes.CCallExpr(target, args), self.get_node_type(node)
        )
        self.last_expr.replace(result)

    def visit_AssignExpr(self, node: ast.AssignExpr) -> None:
        self.visit_Expr(node.target)
        dest = self.last_expr.take().unwrap()
        assert isinstance(dest, cnodes.CAssignmentTarget)
        self.visit_Expr(node.value)

        dest_type = self.get_node_type(node.target)
        assert isinstance(dest_type, typesys.Instance)
        dest_ci = self.type_ctx.get_class_info(dest_type.type_constructor)

        src_type = self.get_node_type(node.value)
        assert isinstance(src_type, typesys.Instance)

        if is_null(src_type):
            assert dest_ci is not None
            value = make_null(self.type_ctx, dest_ci)
        else:
            value = self.last_expr.take().unwrap()
            src_ci = self.type_ctx.get_class_info(src_type.type_constructor)
            if src_ci is not None:
                assert dest_ci is not None
                value = obj_as_parent(
                    self.type_ctx,
                    src_ci,
                    dest_ci,
                    self.cache_in_local(value, src_type),
                )

        self.last_expr.replace(cnodes.CAssignmentExpr(dest, value))

    def visit_PlusExpr(self, node: ast.PlusExpr) -> None:
        self.visit_Expr(node.left)
        left = self.last_expr.take().unwrap()
        self.visit_Expr(node.right)
        right = self.last_expr.take().unwrap()
        left_ty = self.get_node_type(node.left)
        right_ty = self.get_node_type(node.right)

        double_tycon = self.type_ctx.get_type_constructor("double")
        assert double_tycon
        double_ty = typesys.Instance(double_tycon, [])

        if (left_ty == double_ty) ^ (right_ty == double_ty):
            to_cast = left if right_ty == double_ty else right
            casted = cnodes.CCast(to_cast, get_ctype(self.type_ctx, double_ty))
            if left_ty == double_ty:
                right = casted
            else:
                left = casted
        self.last_expr.replace(
            cnodes.CBinExpr(left=left, right=right, op=cnodes.BinOp.Add)
        )

    def visit_DotExpr(self, node: ast.DotExpr) -> None:
        self.visit_Expr(node.left)

        left = self.last_expr.take().into_optional()

        if left is None:
            # Static method call
            assert isinstance(node.left, ast.IdentExpr)
            tycon = self.type_ctx.get_type_constructor(node.left.name)
            assert tycon is not None
            ci = self.type_ctx.get_class_info(tycon)
            assert ci is not None

            attr = ci.get_attribute(node.name)
            assert isinstance(attr, objects.Method) and attr.static

            self.last_expr.replace(
                cnodes.CVariable(
                    get_class_method_impl_name(
                        self.type_ctx,
                        typesys.Instance(tycon, []),
                        attr.name,
                    )
                )
            )
            return

        left_ty = self.get_node_type(node.left)
        assert isinstance(left_ty, typesys.Instance)

        force_static_dispatch = False
        if isinstance(node.left, ast.SuperExpr):
            left = self.new_variable(left_ty)
            self.ctx.cfunc.body.append(
                make_assign_stmt(left, self.receiver.take().unwrap())
            )
            # When calling a method like `super.someMethod()` we know which
            # method that is statically, since `super` can only represent the
            # current class's superclass, and the superclass is decided
            # statically.
            force_static_dispatch = True

        if is_array_type(left_ty):
            # The only field on an array (no data struct)
            assert node.name == "length"
            expr = get_array_length(left)
        else:
            class_info = self.type_ctx.get_class_info(left_ty.type_constructor)
            assert class_info is not None
            mem = class_info.get_attribute(node.name)
            if isinstance(mem, objects.Field):
                expr = get_class_field(self.type_ctx, left, left_ty, node.name)
            elif isinstance(mem, objects.Method):
                meth_ty = mem.type
                assert isinstance(meth_ty, typesys.Instance)
                expr = get_class_method(
                    self.type_ctx,
                    left,
                    left_ty,
                    node.name,
                    force_static_dispatch=force_static_dispatch,
                )
                self.receiver.replace(left)
            else:
                raise JoeUnreachable()

        self.last_expr.replace(expr)

    def visit_NewExpr(self, node: ast.NewExpr) -> None:
        obj_ty = self.get_node_type(node)
        obj_var = self.new_variable(obj_ty)
        assert isinstance(obj_ty, typesys.Instance)

        cfunc = self.ctx.cfunc

        if is_array_type(obj_ty):
            assert isinstance(node.type, ast.ArrayType)
            assert node.type.length is not None
            dest = get_object_data(self.type_ctx, obj_var, obj_ty)
            assert isinstance(dest, cnodes.CAssignmentTarget)
            self.visit_Expr(node.type.length)
            cfunc.body.extend(
                [
                    make_assign_stmt(
                        dest=get_struct_field(obj_var, "length"),
                        value=self.last_expr.take().unwrap(),
                    ),
                    make_assign_stmt(
                        dest=dest,
                        value=make_malloc(
                            get_ctype(self.type_ctx, obj_ty.arguments[0]),
                            get_struct_field(obj_var, "length"),
                        ),
                    ),
                ]
            )
            self.last_expr.replace(obj_var)
            return

        class_info = self.type_ctx.get_class_info(obj_ty.type_constructor)
        assert class_info is not None

        dest = get_object_data(self.type_ctx, obj_var, obj_ty)

        if not optimize_single_field(class_info):
            assert isinstance(dest, cnodes.CAssignmentTarget)
            create_data = make_assign_stmt(
                dest,
                make_malloc(
                    cnodes.CNamedType(
                        get_class_data_name(self.type_ctx, obj_ty)
                    )
                ),
            )

            cfunc.body.append(create_data)

        if not class_info.final:
            create_vtable = make_assign_stmt(
                get_struct_field(obj_var, "vtable"),
                cnodes.CRef(
                    cnodes.CVariable(
                        get_class_vtable_name(self.type_ctx, obj_ty)
                    )
                ),
            )
            cfunc.body.append(create_vtable)

        unqualified_name = ModulePath.from_class_path(class_info.id.name)[-1]
        constructor = class_info.attributes.get(unqualified_name)
        assert constructor is None or isinstance(constructor, objects.Method)
        if constructor is not None:
            constructor_args: t.List[cnodes.CExpr] = []
            if optimize_single_field(class_info):
                # Need a reference to actually change the value since it's not
                # heap-allocated.
                constructor_args.append(cnodes.CRef(obj_var))
            else:
                constructor_args.append(obj_var)
            for arg in node.arguments:
                self.visit_Expr(arg)
                constructor_args.append(self.last_expr.take().unwrap())

            target = get_class_method(
                self.type_ctx, obj_var, obj_ty, unqualified_name
            )

            # Call the constructor
            call_constructor = cnodes.CExprStmt(
                cnodes.CCallExpr(
                    target=target,
                    arguments=constructor_args,
                ),
            )
            cfunc.body.append(call_constructor)

        self.last_expr.replace(obj_var)

    def visit_SuperExpr(self, node: ast.SuperExpr) -> None:
        assert self.ctx.class_info.superclass is not None
        # FIXME: What if the method called is not on the direct superclass but a
        # class higher in the hierarchy?
        this_as_parent = obj_as_parent(
            self.type_ctx,
            self.ctx.class_info,
            self.ctx.class_info.superclass,
            get_self(self.ctx.class_info, self.ctx.is_constructor),
        )
        # For calling `super()`
        self.last_expr.replace(
            cnodes.CVariable(
                get_class_method_impl_name(
                    self.type_ctx,
                    typesys.Instance(self.ctx.class_info.superclass.type, []),
                    self.ctx.class_info.superclass.id.basename,
                )
            )
        )
        # For calling `super.method()`
        self.receiver.replace(this_as_parent)

    def visit_ThisExpr(self, node: ast.ThisExpr) -> None:
        self.last_expr.replace(
            get_self(self.ctx.class_info, self.ctx.is_constructor)
        )

    def visit_IndexExpr(self, node: ast.IndexExpr) -> None:
        self.visit_Expr(node.target)
        target = self.last_expr.take().unwrap()
        self.visit_Expr(node.index)
        index = self.last_expr.take().unwrap()
        self.last_expr.replace(get_array_element(target, index))

    def visit_BoolExpr(self, node: ast.BoolExpr) -> None:
        self.last_expr.replace(cnodes.CInteger(int(node.value)))
