import typing as t
from patina import Option, None_
from typing_extensions import TypeGuard

from joe import ast, cnodes, mangle, objects, typesys
from joe.context import GlobalContext, TypeContext
from joe.exc import JoeUnreachable
from joe.parse import ModulePath
from joe.scopevisitor import ScopeVisitor
from joe.typevisitor import Arrays, MethodExprTypeVisitor
from joe.visitor import Visitor


def prefix_ident(name: str) -> str:
    return f"__joe_{name}"


def ensure_instance(ty: typesys.Type) -> typesys.Instance:
    assert isinstance(ty, typesys.Instance)
    return ty


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
    meth_ty: typesys.Instance,
) -> str:
    assert meth_ty.type_constructor.is_function
    member_name = _get_type_name(ctx, obj_ty) + f"{len(meth_name)}{meth_name}"
    member_name += mangle.type_suffix(
        [_get_type_name(ctx, arg) for arg in meth_ty.arguments]
    )
    return prefix_ident(member_name + "E")


def get_class_data_name(ctx: TypeContext, obj: typesys.Instance) -> str:
    name = get_type_name(ctx, obj)
    return f"{name}_data"


def get_class_vtable_name(ctx: TypeContext, obj: typesys.Instance) -> str:
    name = get_type_name(ctx, obj)
    return f"{name}_vtable"


def get_ctype(ctx: TypeContext, typ: typesys.Type) -> cnodes.CType:
    if isinstance(
        typ, typesys.Instance
    ) and typ.type_constructor == ctx.get_type_constructor("int"):
        return cnodes.CNamedType("int")
    elif isinstance(
        typ, typesys.Instance
    ) and typ.type_constructor == ctx.get_type_constructor("double"):
        return cnodes.CNamedType("double")
    elif isinstance(typ, typesys.BottomType):
        return cnodes.CNamedType("void")
    elif (
        isinstance(typ, typesys.Instance)
        and not typ.type_constructor.is_function
    ):
        return cnodes.CNamedType(get_type_name(ctx, typ))
    elif isinstance(typ, typesys.Instance) and typ.type_constructor.is_function:
        return cnodes.CFuncType(
            return_type=get_ctype(ctx, typ.arguments[-1]),
            parameter_types=[get_ctype(ctx, p) for p in typ.arguments[:-1]],
        )
    else:
        raise NotImplementedError(typ)


def get_class_method(
    ctx: TypeContext, obj: cnodes.CExpr, obj_ty: typesys.Instance, name: str
) -> cnodes.CExpr:
    tycon = obj_ty.type_constructor
    ci = ctx.get_class_info(tycon)
    assert ci is not None

    meth = ci.attributes[name]
    assert isinstance(meth, objects.Method)

    if meth.final or ci.final:
        assert isinstance(meth.type, typesys.Instance)
        return cnodes.CVariable(
            get_class_method_impl_name(ctx, obj_ty, name, meth.type)
        )
    else:
        return get_vtable_member(obj, name)


def get_self() -> cnodes.CVariable:
    return cnodes.CVariable("self")


def get_member(
    struct: cnodes.CExpr, name: str, pointer: bool = False
) -> cnodes.CAssignmentTarget:
    return cnodes.CFieldAccess(
        struct_value=struct,
        field_name=name,
        pointer=pointer,
    )


def get_object_data(
    ctx: TypeContext, obj: cnodes.CAssignmentTarget, obj_ty: typesys.Instance
) -> cnodes.CAssignmentTarget:
    ci = ctx.get_class_info(obj_ty.type_constructor)
    assert ci is not None
    if ci.final:
        return obj
    return get_member(obj, "data")


def get_data_member(
    ctx: TypeContext,
    obj: cnodes.CAssignmentTarget,
    obj_ty: typesys.Instance,
    name: str,
) -> cnodes.CAssignmentTarget:
    # like: obj.data->field_name
    return get_member(get_object_data(ctx, obj, obj_ty), name, pointer=True)


def get_vtable_member(obj: cnodes.CExpr, name: str) -> cnodes.CAssignmentTarget:
    # like: obj.vtable->field_name
    return get_member(get_member(obj, "vtable"), name, pointer=True)


def get_self_data_member(
    ctx: TypeContext, obj_ty: typesys.Instance, name: str
) -> cnodes.CAssignmentTarget:
    return get_data_member(ctx, get_self(), obj_ty, name)


def get_self_vtable_member(name: str) -> cnodes.CAssignmentTarget:
    return get_vtable_member(get_self(), name)


def get_local_name(name: str) -> str:
    return f"__joe_{name}"


def is_array_type(ty: typesys.Type) -> TypeGuard[typesys.Instance]:
    return (
        isinstance(ty, typesys.Instance)
        and ty.type_constructor == Arrays.get_type_constructor()
    )


def get_array_length(expr: cnodes.CExpr) -> cnodes.CExpr:
    return get_member(expr, "length")


def get_array_element(
    expr: cnodes.CExpr, index: cnodes.CExpr
) -> cnodes.CAssignmentTarget:
    return cnodes.CArrayIndex(
        array_value=get_member(expr, "elements"),
        index_value=index,
    )


def make_assign_stmt(
    dest: cnodes.CAssignmentTarget, value: cnodes.CExpr
) -> cnodes.CStmt:
    return cnodes.CExprStmt(cnodes.CAssignmentExpr(dest, value))


def get_array_name(ctx: TypeContext, array_ty: typesys.Type) -> str:
    assert is_array_type(array_ty)
    return prefix_ident(
        mangle.mangle_name(
            "joe.0virtual.Array", [_get_type_name(ctx, array_ty.arguments[0])]
        )
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
                name="elements",
                type=get_ctype(ctx, array_ty.arguments[0]).as_pointer(),
            ),
            cnodes.CStructField(
                name="length",
                type=get_ctype(ctx, typesys.Instance(int_tycon, [])),
            ),
        ],
    )


def make_malloc(type_: cnodes.CType) -> cnodes.CExpr:
    return cnodes.CCallExpr(
        target=cnodes.CVariable("malloc"),
        arguments=[
            cnodes.CCallExpr(
                target=cnodes.CVariable("sizeof"),
                arguments=[cnodes.CTypeExpr(type_)],
            ),
        ],
    )


def make_free(expr: cnodes.CExpr) -> cnodes.CStmt:
    return cnodes.CExprStmt(
        expr=cnodes.CCallExpr(
            target=cnodes.CVariable("free"),
            arguments=[expr],
        )
    )


class CompileContext:
    def __init__(self, global_ctx: GlobalContext) -> None:
        self.global_ctx = global_ctx
        self.emitted_arrays: t.Set[typesys.Type] = set()
        self.code_unit = cnodes.CCodeUnit()


def emit_array(ctx: CompileContext, array_ty: typesys.Type) -> None:
    assert is_array_type(array_ty)
    if array_ty.arguments[0] in ctx.emitted_arrays:
        return
    array_struct = make_array_struct(ctx.global_ctx.type_ctx, array_ty)
    ctx.code_unit.structs.append(array_struct)
    ctx.emitted_arrays.add(array_ty.arguments[0])


class CompileVisitor(Visitor):
    def __init__(self, ctx: GlobalContext) -> None:
        self.ctx = CompileContext(ctx)
        self.ctx.code_unit.includes.append("stdlib.h")
        self.class_stack: t.List[objects.ClassInfo] = []

    @property
    def type_ctx(self) -> TypeContext:
        return self.ctx.global_ctx.type_ctx

    def visit_ClassDeclaration(self, node: ast.ClassDeclaration) -> None:
        class_ty = self.type_ctx.get_type_constructor(node.name.value)
        assert class_ty is not None
        class_info = self.type_ctx.get_class_info(class_ty)
        assert class_info is not None
        self.class_stack.append(class_info)

        data_ctype = cnodes.CStruct(
            name=get_class_data_name(
                self.type_ctx, typesys.Instance(class_ty, [])
            )
        )
        vtable_ctype = cnodes.CStruct(
            name=get_class_vtable_name(
                self.type_ctx, typesys.Instance(class_ty, [])
            )
        )

        for name, attr in class_info.attributes.items():
            if isinstance(attr, objects.Field):
                field = attr
                if is_array_type(field.type):
                    emit_array(self.ctx, field.type)
                data_ctype.fields.append(
                    cnodes.CStructField(
                        name=name,
                        type=get_ctype(self.type_ctx, field.type),
                    )
                )

            elif isinstance(attr, objects.Method) and not attr.final:
                method = attr
                if is_array_type(method.return_type):
                    emit_array(self.ctx, method.return_type)
                for param in method.parameter_types:
                    if is_array_type(param):
                        emit_array(self.ctx, param)

                if not method.static:
                    meth_cty = get_ctype(self.type_ctx, method.type)
                    assert isinstance(meth_cty, cnodes.CFuncType)
                    meth_cty.parameter_types.insert(
                        0,
                        get_ctype(
                            self.type_ctx, typesys.Instance(class_ty, [])
                        ),
                    )
                    vtable_ctype.fields.append(
                        cnodes.CStructField(name=name, type=meth_cty)
                    )

        class_type_name = get_type_name(
            self.type_ctx, typesys.Instance(class_ty, [])
        )
        if class_info.final:
            class_ctype: cnodes.CDecl = cnodes.CTypeDef(
                class_type_name, data_ctype.type.as_pointer()
            )
        else:
            class_ctype = cnodes.CTypeDef(
                class_type_name,
                cnodes.CStruct(
                    name=class_type_name,
                    fields=[
                        cnodes.CStructField(
                            name="data",
                            type=data_ctype.type.as_pointer(),
                        ),
                        cnodes.CStructField(
                            name="vtable",
                            type=vtable_ctype.type.as_pointer(),
                        ),
                    ],
                ),
            )

        self.ctx.code_unit.classes.append(
            cnodes.CClassDecl(
                data_type=data_ctype,
                vtable_type=None if class_info.final else vtable_ctype,
                class_type=class_ctype,
            )
        )

        # Visit methods
        super().visit_ClassDeclaration(node)

        if not class_info.final:
            self.ctx.code_unit.variables.append(
                cnodes.CVarDecl(
                    name=vtable_ctype.name,
                    type=vtable_ctype.type,
                    value=cnodes.CArrayLiteral(
                        [
                            cnodes.CVariable(
                                get_class_method_impl_name(
                                    self.type_ctx,
                                    typesys.Instance(class_ty, []),
                                    meth_name,
                                    ensure_instance(meth.type),
                                )
                            )
                            for meth_name, meth in class_info.attributes.items()
                            if isinstance(meth, objects.Method)
                            and not meth.static
                        ]
                    ),
                )
            )

        self.class_stack.pop()

    def visit_Method(self, node: ast.Method) -> None:
        class_ty = self.class_stack[-1]
        meth = class_ty.attributes[node.name.value]
        assert isinstance(meth, objects.Method)

        comp = MethodCompiler(self.ctx, class_ty, meth, node)
        comp.run()
        func = comp.cfunction.take().unwrap()
        self.ctx.code_unit.functions.append(func)

    def compile_main_function(self, name: str):
        class_path, meth_name = name.rsplit(".")
        cls_ty = self.type_ctx.get_type_constructor(class_path)
        class_info = self.type_ctx.get_class_info(cls_ty) if cls_ty else None
        if cls_ty is None or class_info is None:
            # FIXME: Another exception type?
            raise Exception(f"Invalid class for main method: {class_path}")
        meth = class_info.attributes.get(meth_name)
        if not isinstance(meth, objects.Method):
            raise Exception(f"No such method: {meth_name}")
        if (
            not isinstance(meth.return_type, typesys.BottomType)
            or not meth.static
            or meth.parameter_types
        ):
            raise Exception(f"Invalid signature for main method: {meth_name}")
        assert isinstance(meth.type, typesys.Instance)
        main_name = get_class_method_impl_name(
            self.type_ctx, typesys.Instance(cls_ty, []), meth_name, meth.type
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
        )
        self.ctx.code_unit.functions.append(main_func)


class MethodCompiler(ScopeVisitor):
    def __init__(
        self,
        ctx: CompileContext,
        class_ty: objects.ClassInfo,
        meth_ty: objects.Method,
        meth_node: ast.Method,
    ) -> None:
        super().__init__()
        self.ctx = ctx
        self.class_ty = class_ty
        self.meth_ty = meth_ty
        self.meth_node = meth_node
        self.cfunction: Option[cnodes.CFunc] = None_()
        self.method_type_visitor = MethodExprTypeVisitor(
            ctx.global_ctx.type_ctx, class_ty, meth_ty
        )
        self.method_type_visitor.visit(meth_node)
        self.expr_types = self.method_type_visitor.expr_types
        self.last_expr: Option[cnodes.CExpr] = None_()
        self.receiver: Option[cnodes.CExpr] = None_()

    @property
    def type_ctx(self) -> TypeContext:
        return self.ctx.global_ctx.type_ctx

    def get_node_type(self, node: ast.Node) -> typesys.Type:
        return self.expr_types[node]

    def new_variable(self, type_: typesys.Type) -> cnodes.CAssignmentTarget:
        locs = self.cfunction.unwrap().locals
        new_loc_name = get_local_name(f"tmp_{len(locs)}")
        locs.append(
            cnodes.CVarDecl(
                name=new_loc_name, type=get_ctype(self.type_ctx, type_)
            )
        )
        return cnodes.CVariable(new_loc_name)

    def cache_in_local(
        self, expr: cnodes.CExpr, type_: typesys.Type
    ) -> cnodes.CExpr:
        if isinstance(type_, typesys.BottomType):
            return expr
        var = self.new_variable(type_)
        self.cfunction.unwrap().body.append(make_assign_stmt(var, expr))
        return var

    def run(self) -> None:
        self.visit(self.meth_node)

    def visit_Method(self, node: ast.Method) -> None:
        func_attr = self.class_ty.attributes[node.name.value]
        assert isinstance(func_attr, objects.Method)
        func_ty = func_attr.type
        assert isinstance(func_ty, typesys.Instance)

        func = cnodes.CFunc(
            name=get_class_method_impl_name(
                self.type_ctx,
                typesys.Instance(self.class_ty.type, []),
                node.name.value,
                func_ty,
            ),
            return_type=get_ctype(self.type_ctx, self.meth_ty.return_type),
            parameters=[
                cnodes.CParam(
                    name=get_local_name(
                        # FIXME
                        self.method_type_visitor._names[0, param.name.value]
                    ),
                    type=get_ctype(self.type_ctx, ty),
                )
                for param, ty in zip(
                    node.parameters, self.meth_ty.parameter_types
                )
            ],
            locals=[
                cnodes.CVarDecl(
                    name=get_local_name(l.actual_name),
                    type=get_ctype(self.type_ctx, l.type),
                )
                for l in self.method_type_visitor.locals.values()
            ],
            body=[],
        )

        if not node.static:
            func.parameters.insert(
                0,
                cnodes.CParam(
                    name="self",
                    type=get_ctype(
                        self.type_ctx, typesys.Instance(self.class_ty.type, [])
                    ),
                ),
            )

        self.cfunction.replace(func)

        # Visit statements
        super().visit_Method(node)

    def visit_ExprStmt(self, node: ast.ExprStmt) -> None:
        super().visit_ExprStmt(node)
        self.cfunction.unwrap().body.append(
            cnodes.CExprStmt(self.last_expr.take().unwrap())
        )

    def visit_DeleteStmt(self, node: ast.DeleteStmt) -> None:
        super().visit_DeleteStmt(node)
        # Free the data member of the object
        obj = self.last_expr.take().unwrap()
        assert isinstance(obj, cnodes.CAssignmentTarget)
        ty = self.get_node_type(node.expr)
        assert isinstance(ty, typesys.Instance)
        self.cfunction.unwrap().body.append(
            make_free(get_object_data(self.type_ctx, obj, ty))
        )

    def visit_ReturnStmt(self, node: ast.ReturnStmt) -> None:
        super().visit_ReturnStmt(node)
        ret_expr = self.last_expr.take()
        expr = None if ret_expr.is_none() else ret_expr.unwrap()
        self.cfunction.unwrap().body.append(cnodes.CReturnStmt(expr))

    def visit_VarDeclaration(self, node: ast.VarDeclaration) -> None:
        super().visit_VarDeclaration(node)

        if node.initializer is None:
            return

        dest_name = self.resolve_name(node.name.value, location=node.location)
        value = self.last_expr.take().unwrap()

        assign_stmt = make_assign_stmt(
            cnodes.CVariable(get_local_name(dest_name)), value
        )
        self.cfunction.unwrap().body.append(assign_stmt)

    def visit_IdentExpr(self, node: ast.IdentExpr) -> None:
        ty = self.get_node_type(node)
        expr: cnodes.CExpr
        assert isinstance(ty, typesys.Instance)
        if ty.type_constructor.is_function:
            func = self.class_ty.attributes[node.name]
            assert isinstance(func, objects.Method)
            if func.static:
                assert isinstance(func.type, typesys.Instance)
                expr = cnodes.CVariable(
                    get_class_method_impl_name(
                        self.type_ctx,
                        typesys.Instance(self.class_ty.type, []),
                        node.name,
                        func.type,
                    )
                )
            else:
                expr = get_class_method(
                    self.type_ctx, get_self(), ty, node.name
                )
                self.receiver.replace(get_self())
        else:
            local_name = self.method_type_visitor.try_resolve_name(node.name)
            if local_name is not None and get_local_name(local_name) in (
                l.name for l in self.cfunction.unwrap().locals
            ):
                # It's a local variable. Scopes should already be flattened, so
                # variables are function-scoped.
                expr = cnodes.CVariable(get_local_name(local_name))
            elif node.name in (p.name.value for p in self.meth_node.parameters):
                expr = cnodes.CVariable(
                    get_local_name(
                        self.method_type_visitor.resolve_name(
                            node.name,
                            location=node.location,
                        )
                    )
                )
            elif node.name in self.class_ty.attributes:
                assert isinstance(
                    self.class_ty.attributes[node.name], objects.Field
                )
                # It's accessing a field on self.
                expr = get_self_data_member(
                    self.type_ctx,
                    typesys.Instance(self.class_ty.type, []),
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
        assert func_ty.type_constructor.is_function

        # FIXME: it should be easier to get the method info
        assert isinstance(node.target, (ast.IdentExpr, ast.DotExpr))
        if isinstance(node.target, ast.IdentExpr):
            class_info = self.class_ty
            meth_name = node.target.name
        elif isinstance(node.target, ast.DotExpr):
            recv_ty = self.get_node_type(node.target.left)
            assert isinstance(recv_ty, typesys.Instance)
            class_info2 = self.type_ctx.get_class_info(recv_ty.type_constructor)
            assert class_info2 is not None
            class_info = class_info2
            meth_name = node.target.name

        meth_info = class_info.attributes[meth_name]
        assert isinstance(meth_info, objects.Method)

        args = []
        if not meth_info.static:
            receiver = self.receiver.take().unwrap()
            args.append(receiver)

        for arg in node.arguments:
            self.visit_Expr(arg)
            args.append(self.last_expr.take().unwrap())

        result = self.cache_in_local(
            cnodes.CCallExpr(target, args), self.get_node_type(node)
        )
        self.last_expr.replace(result)

    def visit_AssignExpr(self, node: ast.AssignExpr) -> None:
        self.visit_Expr(node.target)
        dest = self.last_expr.take().unwrap()
        assert isinstance(dest, cnodes.CAssignmentTarget)
        self.visit_Expr(node.value)
        value = self.last_expr.take().unwrap()
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
        left = self.last_expr.take().unwrap()
        left_ty = self.get_node_type(node.left)
        assert isinstance(left_ty, typesys.Instance)

        if is_array_type(left_ty):
            # The only field on an array (no data struct)
            assert node.name == "length"
            expr = get_array_length(left)
        else:
            class_info = self.type_ctx.get_class_info(left_ty.type_constructor)
            assert class_info is not None
            mem = class_info.attributes[node.name]
            if isinstance(mem, objects.Field):
                assert isinstance(left, cnodes.CAssignmentTarget)
                expr = get_data_member(self.type_ctx, left, left_ty, node.name)
            elif isinstance(mem, objects.Method):
                meth_ty = mem.type
                assert isinstance(meth_ty, typesys.Instance)
                expr = get_class_method(self.type_ctx, left, left_ty, node.name)
                self.receiver.replace(left)
            else:
                raise JoeUnreachable()

        self.last_expr.replace(expr)

    def visit_NewExpr(self, node: ast.NewExpr) -> None:
        obj_ty = self.get_node_type(node)
        obj_var = self.new_variable(obj_ty)
        assert isinstance(obj_ty, typesys.Instance)
        class_info = self.type_ctx.get_class_info(obj_ty.type_constructor)
        assert class_info is not None

        if class_info.final:
            dest = obj_var
        else:
            dest = get_member(obj_var, "data")

        create_data = make_assign_stmt(
            dest,
            make_malloc(
                cnodes.CStructType(get_class_data_name(self.type_ctx, obj_ty))
            ),
        )

        cfunc = self.cfunction.unwrap()
        cfunc.body.append(create_data)

        if not class_info.final:
            create_vtable = make_assign_stmt(
                get_member(obj_var, "vtable"),
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
            constructor_args: t.List[cnodes.CExpr] = [obj_var]
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

    def visit_IndexExpr(self, node: ast.IndexExpr) -> None:
        self.visit_Expr(node.target)
        target = self.last_expr.take().unwrap()
        self.visit_Expr(node.index)
        index = self.last_expr.take().unwrap()
        self.last_expr.replace(get_array_element(target, index))
