import typing as t
from joe import ast, cnodes, typesys
from joe.context import GlobalContext
from joe.exc import JoeUnreachable
from joe.parse import ModulePath
from joe.typevisitor import MethodExprTypeVisitor
from joe.visitor import Visitor
from patina import Option, None_


def prefix_ident(name: str) -> str:
    return f"__joe_{name}"


def get_class_name(class_: typesys.Class) -> str:
    return prefix_ident(class_.mangled_name())


def get_class_member_name(class_: typesys.Class, member: str) -> str:
    name = prefix_ident(class_.id.mangle())
    # FIXME: name mangling logic is leaking
    return f"{name}{len(member)}{member}E"


def get_class_method_impl_name(function: typesys.FunctionInstance) -> str:
    return prefix_ident(function.mangled_name())


def get_class_data_name(class_: typesys.Class) -> str:
    name = get_class_name(class_)
    return f"{name}_data"


def get_class_vtable_name(class_: typesys.Class) -> str:
    name = get_class_name(class_)
    return f"{name}_vtable"


def get_ctype(typ: typesys.Type) -> cnodes.CType:
    if isinstance(typ, typesys.IntType):
        return cnodes.CNamedType("int")
    elif isinstance(typ, typesys.VoidType):
        return cnodes.CNamedType("void")
    elif isinstance(typ, typesys.ClassInstance):
        return cnodes.CStructType(get_class_name(typ.class_))
    elif isinstance(typ, typesys.FunctionInstance):
        return cnodes.CFuncType(
            return_type=get_ctype(typ.function.return_type),
            parameter_types=[
                get_ctype(p) for p in typ.function.formal_parameters
            ],
        )
    else:
        raise NotImplementedError(typ)


def get_self() -> cnodes.CExpr:
    return cnodes.CVariable("self")


def get_member(
    struct: cnodes.CExpr, name: str, pointer: bool = False
) -> cnodes.CAssignmentTarget:
    return cnodes.CFieldAccess(
        struct_value=struct,
        field_name=name,
        pointer=pointer,
    )


def get_data_member(obj: cnodes.CExpr, name: str) -> cnodes.CAssignmentTarget:
    # like: obj.data->field_name
    return get_member(get_member(obj, "data"), name, pointer=True)


def get_vtable_member(obj: cnodes.CExpr, name: str) -> cnodes.CAssignmentTarget:
    # like: obj.vtable->field_name
    return get_member(get_member(obj, "vtable"), name, pointer=True)


def get_self_data_member(name: str) -> cnodes.CAssignmentTarget:
    return get_data_member(get_self(), name)


def get_self_vtable_member(name: str) -> cnodes.CAssignmentTarget:
    return get_vtable_member(get_self(), name)


def get_local_name(name: str) -> cnodes.CName:
    return cnodes.CMangledName([name])


def make_assign_stmt(
    dest: cnodes.CAssignmentTarget, value: cnodes.CExpr
) -> cnodes.CStmt:
    return cnodes.CExprStmt(cnodes.CAssignmentExpr(dest, value))


def make_malloc(type_: cnodes.CType) -> cnodes.CExpr:
    return cnodes.CCallExpr(
        target=cnodes.CVariable(cnodes.CUnmangledName("malloc")),
        arguments=[
            cnodes.CCallExpr(
                target=cnodes.CVariable(cnodes.CUnmangledName("sizeof")),
                arguments=[cnodes.CTypeExpr(type_)],
            ),
        ],
    )


class CompileVisitor(Visitor):
    def __init__(self, ctx: GlobalContext) -> None:
        self.ctx = ctx
        self.code_unit = cnodes.CCodeUnit()
        self.code_unit.includes.append("stdlib.h")
        self.class_stack: t.List[typesys.Class] = []

    def visit_ClassDeclaration(self, node: ast.ClassDeclaration) -> None:
        class_ty = self.ctx.type_scope[node.name.value]
        assert isinstance(class_ty, typesys.Class)
        self.class_stack.append(class_ty)

        data_ctype = cnodes.CStruct(name=get_class_data_name(class_ty))
        for name, field_ty in class_ty.members.items():
            data_ctype.fields.append(
                cnodes.CStructField(
                    name=name,
                    type=get_ctype(field_ty),
                )
            )

        vtable_ctype = cnodes.CStruct(name=get_class_vtable_name(class_ty))
        for name, meth_ty in class_ty.methods.items():
            if not meth_ty.static:
                meth_cty = get_ctype(typesys.FunctionInstance(meth_ty, []))
                assert isinstance(meth_cty, cnodes.CFuncType)
                meth_cty.parameter_types.insert(
                    0, get_ctype(typesys.ClassInstance(class_ty, []))
                )
                vtable_ctype.fields.append(
                    cnodes.CStructField(name=name, type=meth_cty)
                )

        class_ctype = cnodes.CStruct(
            name=get_class_name(class_ty),
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
        )

        self.code_unit.classes.append(
            cnodes.CClassDecl(
                data_type=data_ctype,
                vtable_type=vtable_ctype,
                class_type=class_ctype,
            )
        )

        # Visit methods
        super().visit_ClassDeclaration(node)

        self.code_unit.variables.append(
            cnodes.CVarDecl(
                name=vtable_ctype.name,
                type=vtable_ctype.type,
                value=cnodes.CArrayLiteral(
                    [
                        cnodes.CVariable(
                            get_class_member_name(class_ty, meth_name)
                        )
                        for meth_name, meth_ty in class_ty.methods.items()
                        if not meth_ty.static
                    ]
                ),
            )
        )

        self.class_stack.pop()

    def visit_Method(self, node: ast.Method) -> None:
        class_ty = self.class_stack[-1]
        meth_ty = class_ty.get_method(node.name.value)
        assert meth_ty is not None

        comp = MethodCompiler(self.ctx, class_ty, meth_ty, node)
        comp.run()
        func = comp.cfunction.take().unwrap()
        self.code_unit.functions.append(func)

    def compile_main_function(self, name: str):
        class_path, meth_name = name.rsplit(".")
        cls_ty = self.ctx.type_scope[class_path]
        if not isinstance(cls_ty, typesys.Class):
            # FIXME: Another exception type?
            raise Exception(f"Invalid class for main method: {class_path}")
        meth_ty = cls_ty.get_method(meth_name)
        if meth_ty is None:
            raise Exception(f"No such method: {meth_name}")
        if (
            not isinstance(meth_ty.return_type, typesys.VoidType)
            or not meth_ty.static
            or meth_ty.formal_parameters
        ):
            raise Exception(f"Invalid signature for main method: {meth_name}")
        main_name = get_class_member_name(cls_ty, meth_name)
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
        self.code_unit.functions.append(main_func)


class MethodCompiler(Visitor):
    def __init__(
        self,
        ctx: GlobalContext,
        class_ty: typesys.Class,
        meth_ty: typesys.Function,
        meth_node: ast.Method,
    ) -> None:
        self.ctx = ctx
        self.class_ty = class_ty
        self.meth_ty = meth_ty
        self.meth_node = meth_node
        self.cfunction: Option[cnodes.CFunc] = None_()
        vis = MethodExprTypeVisitor(
            ctx.type_scope,
            class_ty,
            meth_ty,
            meth_node,
        )
        vis.visit(meth_node)
        self.expr_types: t.Dict[ast.Node, typesys.Type] = vis.expr_types
        self.last_expr: Option[cnodes.CExpr] = None_()
        self.receiver: Option[cnodes.CExpr] = None_()

    def new_variable(self, type_: typesys.Type) -> cnodes.CAssignmentTarget:
        locs = self.cfunction.unwrap().locals
        new_loc_name = get_local_name(str(len(locs)))
        locs.append(cnodes.CVarDecl(name=new_loc_name, type=get_ctype(type_)))
        return cnodes.CVariable(new_loc_name)

    def cache_in_local(
        self, expr: cnodes.CExpr, type_: typesys.Type
    ) -> cnodes.CExpr:
        if isinstance(type_, typesys.VoidType):
            return expr
        var = self.new_variable(type_)
        self.cfunction.unwrap().body.append(make_assign_stmt(var, expr))
        return var

    def run(self) -> None:
        self.visit(self.meth_node)

    def visit_Method(self, node: ast.Method) -> None:
        func = cnodes.CFunc(
            name=get_class_member_name(self.class_ty, node.name.value),
            return_type=get_ctype(self.meth_ty.return_type),
            parameters=[
                cnodes.CParam(
                    name=get_local_name(param.name.value),
                    type=get_ctype(ty),
                )
                for param, ty in zip(
                    node.parameters, self.meth_ty.formal_parameters
                )
            ],
            locals=[],
            body=[],
        )

        if not node.static:
            func.parameters.insert(
                0,
                cnodes.CParam(
                    name="self",
                    type=get_ctype(typesys.ClassInstance(self.class_ty, [])),
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

    def visit_ReturnStmt(self, node: ast.ReturnStmt) -> None:
        super().visit_ReturnStmt(node)
        ret_expr = self.last_expr.take()
        expr = None if ret_expr.is_none() else ret_expr.unwrap()
        self.cfunction.unwrap().body.append(cnodes.CReturnStmt(expr))

    def visit_IdentExpr(self, node: ast.IdentExpr) -> None:
        ty = self.expr_types[node]
        expr: cnodes.CExpr
        if isinstance(ty, typesys.FunctionInstance):
            if ty.function.static:
                expr = cnodes.CVariable(
                    get_class_member_name(self.class_ty, node.name)
                )
            else:
                expr = get_self_vtable_member(node.name)
                self.receiver.replace(get_self())
        elif node.name in (
            l.name for l in self.cfunction.unwrap().locals
        ) or node.name in (p.name.value for p in self.meth_node.parameters):
            # It's a local variable. Scopes should already be flattened, so
            # variables are function-scoped.
            expr = cnodes.CVariable(get_local_name(node.name))
        elif node.name in self.class_ty.members:
            # It's accessing a field on self.
            expr = get_self_data_member(node.name)
        else:
            raise JoeUnreachable()
        self.last_expr.replace(expr)

    def visit_IntExpr(self, node: ast.IntExpr) -> None:
        self.last_expr.replace(cnodes.CInteger(node.value))

    def visit_CallExpr(self, node: ast.CallExpr) -> None:
        self.visit_Expr(node.target)
        target = self.last_expr.take().unwrap()
        func_ty = self.expr_types[node.target]
        assert isinstance(func_ty, typesys.FunctionInstance)

        args = []
        if not func_ty.function.static:
            receiver = self.receiver.take().unwrap()
            args.append(receiver)

        for arg in node.arguments:
            self.visit_Expr(arg)
            args.append(self.last_expr.take().unwrap())

        result = self.cache_in_local(
            cnodes.CCallExpr(target, args), self.expr_types[node]
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
        left_ty = self.expr_types[node.left]
        right_ty = self.expr_types[node.right]
        if isinstance(left_ty, typesys.DoubleType) ^ isinstance(
            right_ty, typesys.DoubleType
        ):
            to_cast = left if isinstance(left_ty, typesys.DoubleType) else right
            casted = cnodes.CCast(to_cast, get_ctype(typesys.DoubleType()))
            if isinstance(left_ty, typesys.DoubleType):
                left = casted
            else:
                right = casted
        self.last_expr.replace(
            cnodes.CBinExpr(left=left, right=right, op=cnodes.BinOp.Add)
        )

    def visit_DotExpr(self, node: ast.DotExpr) -> None:
        self.visit_Expr(node.left)
        left = self.last_expr.take().unwrap()
        left_ty = self.expr_types[node.left]
        assert isinstance(left_ty, typesys.ClassInstance)

        mem = left_ty.get_member(node.name)
        if mem is not None:
            expr = get_data_member(left, node.name)
        else:
            meth = left_ty.get_method(node.name)
            assert meth is not None
            expr = get_vtable_member(left, node.name)
            self.receiver.replace(left)

        self.last_expr.replace(expr)

    def visit_NewExpr(self, node: ast.NewExpr) -> None:
        obj_var = self.new_variable(self.expr_types[node])
        obj_ty = self.expr_types[node]
        assert isinstance(obj_ty, typesys.ClassInstance)
        class_ty = obj_ty.class_

        create_data = make_assign_stmt(
            get_member(obj_var, "data"),
            make_malloc(cnodes.CStructType(get_class_data_name(class_ty))),
        )
        create_vtable = make_assign_stmt(
            get_member(obj_var, "vtable"),
            cnodes.CRef(cnodes.CVariable(get_class_vtable_name(class_ty))),
        )

        cfunc = self.cfunction.unwrap()
        cfunc.body.extend([create_data, create_vtable])

        unqualified_name = ModulePath.from_class_path(class_ty.id.name)[-1]
        constructor = obj_ty.get_method(unqualified_name)
        if constructor is not None:
            constructor_args: t.List[cnodes.CExpr] = [obj_var]
            for arg in node.arguments:
                self.visit_Expr(arg)
                constructor_args.append(self.last_expr.take().unwrap())

            # Call the constructor
            call_constructor = cnodes.CExprStmt(
                cnodes.CCallExpr(
                    target=get_vtable_member(obj_var, unqualified_name),
                    arguments=constructor_args,
                ),
            )
            cfunc.body.append(call_constructor)

        self.last_expr.replace(obj_var)
