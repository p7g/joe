import typing as t
from contextlib import contextmanager
from dataclasses import dataclass
from joe import ast, objects, typesys
from joe.context import TypeContext
from joe.source import JoeNameError, JoeSyntaxError, JoeTypeError, Location
from joe.visitor import Visitor


class TypeVisitor(Visitor):
    """Analyzes type expressions and results in typesys types."""

    def __init__(self, type_ctx: TypeContext):
        self.type_ctx = type_ctx
        self.result: t.Optional[typesys.Type] = None

    @classmethod
    def analyze(cls, type_ctx: TypeContext, node: ast.Type) -> typesys.Type:
        vis = cls(type_ctx)
        vis.visit(node)
        assert vis.result is not None
        return vis.result

    def visit_NamedType(self, node: ast.NamedType):
        ty = self.type_ctx.get_type_constructor(node.name.value)
        if ty is None:
            raise JoeNameError(
                node.location, f"Unknown type '{node.name.value}'"
            )
        # TODO: Generics
        self.result = typesys.Instance(ty, [])

    def visit_VoidType(self, node: ast.VoidType):
        self.result = typesys.BottomType()

    def visit_ArrayType(self, node: ast.ArrayType):
        element_ty = self.analyze(self.type_ctx, node.element_type)
        self.result = Arrays.get_type(element_ty)


# Class visitor:
# - Runs for each class and records the (analyzed) types of all fields and
#   methods.
# - The results are used to populate the global list of classes and types


class ClassDeclarationVisitor(Visitor):
    def __init__(self, type_ctx: TypeContext, name: str):
        self.ty = objects.ClassInfo(
            id_=objects.ClassID(name),
            type_=typesys.TypeConstructor(
                parameters=[], super_=typesys.TopType()
            ),
            attributes={},
        )
        self.type_ctx = type_ctx

    @classmethod
    def get_class_info(
        cls,
        type_ctx: TypeContext,
        class_decl: ast.ClassDeclaration,
    ) -> objects.ClassInfo:
        vis = cls(type_ctx, class_decl.name.value)
        vis.visit(class_decl)
        return vis.ty

    def analyze_type(self, node: ast.Type) -> typesys.Type:
        return TypeVisitor.analyze(self.type_ctx, node)

    def visit_ClassDeclaration(self, node: ast.ClassDeclaration):
        # TODO: inheritance
        # if node.extends:
        #     self.superclass = self.analyze_type(node.extends)
        # Add current class to scope after resolving superclass (avoid cycle)
        self.type_ctx.add_class(self.ty)
        super().visit_ClassDeclaration(node)

    def visit_Field(self, node: ast.Field):
        if node.name.value in self.ty.attributes:
            raise JoeSyntaxError(
                node.location, f"Duplicate attribute name '{node.name.value}'"
            )
        self.ty.attributes[node.name.value] = objects.Field(
            self.analyze_type(node.type)
        )

    def visit_Method(self, node: ast.Method):
        if node.name.value in self.ty.attributes:
            raise JoeSyntaxError(
                node.location, f"Duplicate attribute name '{node.name.value}'"
            )
        if node.name.value == self.ty.id.name and not isinstance(
            node.return_type, ast.VoidType
        ):
            raise JoeSyntaxError(
                node.location, "Constructor must have void return type"
            )
        meth_ty = typesys.Instance(
            function_type(len(node.parameters)),
            [self.analyze_type(p.type) for p in node.parameters]
            + [self.analyze_type(node.return_type)],
        )
        self.ty.attributes[node.name.value] = objects.Method(
            meth_ty,
            static=node.static,
        )


def function_type(arity: int) -> typesys.TypeConstructor:
    return typesys.TypeConstructor(
        parameters=[
            typesys.TypeParameter(typesys.Contravariant(), typesys.TopType())
            for _ in range(arity)
        ]
        + [typesys.TypeParameter(typesys.Covariant(), typesys.BottomType())],
        super_=typesys.TopType(),
        is_function=True,
    )


class Arrays:
    _array_type_constructor: t.Optional[typesys.TypeConstructor] = None

    @classmethod
    def get_type_constructor(cls) -> typesys.TypeConstructor:
        if cls._array_type_constructor is None:
            cls._array_type_constructor = typesys.TypeConstructor(
                parameters=[
                    typesys.TypeParameter(
                        typesys.Invariant(), typesys.BottomType()
                    )
                ],
                super_=typesys.TopType(),
            )
        return cls._array_type_constructor

    @classmethod
    def get_type(cls, element_type: typesys.Type) -> typesys.Type:
        return typesys.Instance(cls.get_type_constructor(), [element_type])


# Method visitors:
# - Semantic analysis
# - Type checking
# - A series of transformations on methods
#   - Flatten scopes (i.e. transform lexical scope to function scope)


@dataclass
class _Local:
    name: str
    type: typesys.Type


# TODO: Use separate pass to convert lexical scoping to function scoping (i.e.
# collect all the local variables used and their types in 1 place).

# The class's type should already exist in GlobalContext. This means that the
# types of all methods and members should already be analyzed.
class MethodExprTypeVisitor(Visitor):
    def __init__(
        self,
        type_ctx: TypeContext,
        class_: objects.ClassInfo,
        method: objects.Method,
        method_node: ast.Method,
    ):
        self.type_ctx = type_ctx
        self.class_ = class_
        self.method = method
        self.locals: t.List[_Local] = [
            _Local(name=param.name.value, type=ty)
            for param, ty in zip(method_node.parameters, method.parameter_types)
        ]
        self.scope_start: t.List[int] = [0]
        self.expr_types: t.Dict[ast.Node, typesys.Type] = {}

    @classmethod
    def get_expr_types(
        cls,
        type_ctx: TypeContext,
        class_: objects.ClassInfo,
        method: objects.Method,
        method_node: ast.Method,
    ) -> t.Dict[ast.Node, typesys.Type]:
        vis = cls(type_ctx, class_, method, method_node)
        vis.visit(method_node)
        return vis.expr_types

    def declare_local(
        self, name: str, type_: typesys.Type, *, location: Location
    ) -> _Local:
        for i, loc in reversed(list(enumerate(self.locals))):
            if i < self.scope_start[-1]:
                break
            if loc.name == name:
                raise JoeSyntaxError(
                    location, f"Duplicate variable declaration {name}"
                )
        loc = _Local(name=name, type=type_)
        self.locals.append(loc)
        return loc

    def set_type(self, node: ast.Node, type_: typesys.Type) -> None:
        self.expr_types[node] = type_

    def get_type(self, node: ast.Node) -> typesys.Type:
        ty = self.expr_types[node]
        return ty

    def resolve_local(self, name: str) -> t.Optional[_Local]:
        for loc in reversed(self.locals):
            if loc.name == name:
                return loc
        return None

    @contextmanager  # type: ignore
    def scope(self) -> t.Iterator[None]:
        self.scope_start.append(len(self.locals))
        yield
        self.locals = self.locals[: self.scope_start.pop()]

    def lookup_class(self, name: str) -> t.Optional[objects.ClassInfo]:
        ty = self.type_ctx.get_type_constructor(name)
        if ty is None:
            return None
        ci = self.type_ctx.get_class_info(ty)
        if ci is None:
            return None
        return ci

    def analyze_type(self, node: ast.Type) -> typesys.Type:
        return TypeVisitor.analyze(self.type_ctx, node)

    def visit_AssignExpr(self, node: ast.AssignExpr):
        super().visit_AssignExpr(node)
        lhs_ty = self.get_type(node.target)
        rhs_ty = self.get_type(node.value)
        if lhs_ty != rhs_ty and not lhs_ty.is_supertype_of(rhs_ty):
            raise JoeTypeError(
                node.location, "Incompatible types in assignment"
            )
        if not isinstance(
            node.target, (ast.IdentExpr, ast.DotExpr, ast.IndexExpr)
        ):
            raise JoeSyntaxError(node.location, "Invalid lhs in assignment")
        if isinstance(node.target, ast.DotExpr):
            left_ty = self.get_type(node.target.left)
            assert isinstance(left_ty, typesys.Instance)
            class_info = self.type_ctx.get_class_info(left_ty.type_constructor)
            assert class_info is not None, "Accessing property of primitive"
            if node.target.name not in class_info.attributes:
                raise JoeTypeError(node.target.location, "No such attribute")
            if isinstance(
                class_info.attributes[node.target.name], objects.Method
            ):
                raise JoeTypeError(node.target.location, "Assignment to method")
        elif isinstance(node.target, ast.IdentExpr):
            local = self.resolve_local(node.target.name)
            if local is None:
                if node.target.name not in self.class_.attributes:
                    raise JoeTypeError(
                        node.target.location, "No such attribute"
                    )
                if isinstance(
                    self.class_.attributes[node.target.name], objects.Method
                ):
                    raise JoeTypeError(
                        node.target.location, "Assignment to method"
                    )
        elif isinstance(node.target, ast.IndexExpr):
            target_ty = self.get_type(node.target.target)
            if (
                not isinstance(target_ty, typesys.Instance)
                or target_ty.type_constructor != Arrays.get_type_constructor()
            ):
                raise JoeTypeError(
                    node.target.location, "Can't index non-array"
                )
        self.set_type(node, lhs_ty)

    def visit_NewExpr(self, node: ast.NewExpr):
        super().visit_NewExpr(node)
        created_type = self.analyze_type(node.type)
        # FIXME: use some resolve_type method to handle type variables
        assert isinstance(created_type, typesys.Instance)
        class_info = self.type_ctx.get_class_info(created_type.type_constructor)
        if class_info is None:
            raise JoeTypeError(
                node.type.location, "Can't create primitive object"
            )
        class_basename = class_info.id.name.rsplit(".")[-1]
        constructor = class_info.attributes.get(class_basename)
        if constructor is None:
            if node.arguments:
                raise JoeTypeError(
                    node.location, "Too many arguments to constructor"
                )
        else:
            assert isinstance(constructor, objects.Method)
            if len(node.arguments) != len(constructor.parameter_types):
                raise JoeTypeError(
                    node.location,
                    "Incorrect number of arguments to constructor",
                )
            for arg, param in zip(node.arguments, constructor.parameter_types):
                arg_ty = self.get_type(arg)
                if arg_ty != param and not param.is_supertype_of(arg_ty):
                    raise JoeTypeError(
                        arg.location, "Incorrect type for constructor argument"
                    )
        self.set_type(node, created_type)

    def visit_IdentExpr(self, node: ast.IdentExpr):
        name_str = node.name
        loc = self.resolve_local(name_str)
        ty: t.Optional[typesys.Type] = None
        if loc is not None:
            ty = loc.type
        else:
            attr = self.class_.attributes.get(name_str)
            if attr is not None:
                ty = attr.type
        if ty is None:
            raise JoeNameError(
                node.location, f"Failed to resolve variable {name_str}"
            )
        self.set_type(node, ty)

    def visit_DotExpr(self, node: ast.DotExpr):
        # TODO: Support static methods
        super().visit_DotExpr(node)
        lhs_ty = self.get_type(node.left)
        assert isinstance(lhs_ty, typesys.Instance)
        class_info = self.type_ctx.get_class_info(lhs_ty.type_constructor)
        if class_info is None:
            raise JoeTypeError(
                node.location, f"Can't access property on {lhs_ty!r}"
            )
        attr = class_info.attributes.get(node.name)
        if attr is None:
            raise JoeTypeError(node.location, f"No such property {node.name}")
        self.set_type(node, attr.type)

    def visit_IntExpr(self, node: ast.IntExpr):
        int_tycon = self.type_ctx.get_type_constructor("int")
        assert int_tycon is not None
        self.set_type(node, typesys.Instance(int_tycon, []))

    def visit_CallExpr(self, node: ast.CallExpr):
        super().visit_CallExpr(node)
        fn_ty = self.get_type(node.target)
        assert isinstance(fn_ty, typesys.Instance)
        if not fn_ty.type_constructor.is_function:
            raise JoeTypeError(node.location, "Target is not callable")
        # Get target type constructor
        # Get class info
        # Get attribute
        # Ensure method
        assert isinstance(node.target, (ast.IdentExpr, ast.DotExpr))
        if isinstance(node.target, ast.IdentExpr):
            # Method on current class instance
            target_tycon = self.class_.type
            method_name = node.target.name
            this_is_receiver = True
        elif isinstance(node.target, ast.DotExpr):
            # Method on some other class instance
            recv_ty = self.get_type(node.target.left)
            assert isinstance(recv_ty, typesys.Instance)
            target_tycon = recv_ty.type_constructor
            method_name = node.target.name
            this_is_receiver = False

        class_info = self.type_ctx.get_class_info(target_tycon)
        if class_info is None:
            raise JoeTypeError(
                node.location, "Cannot call method on primitive type"
            )

        called_method = class_info.attributes.get(method_name)
        if called_method is None:
            raise JoeTypeError(node.location, "No such method")

        if not isinstance(called_method, objects.Method):
            raise JoeTypeError(node.location, "Cannot call field")

        # Can't call non-static methods using implicit "this" from a static
        # method.
        if self.method.static and not called_method.static and this_is_receiver:
            raise JoeTypeError(
                node.location, "Cannot call instance method from static method"
            )
        if len(node.arguments) != len(called_method.parameter_types):
            raise JoeTypeError(
                node.location,
                f"Incorrect number of arguments to '{method_name}'",
            )
        if not all(
            self.get_type(arg) == param
            or param.is_supertype_of(self.get_type(arg))
            for arg, param in zip(node.arguments, called_method.parameter_types)
        ):
            raise JoeTypeError(
                node.location,
                f"Incorrect argument types to '{method_name}'",
            )
        self.set_type(node, called_method.return_type)
        # TODO: Maintain visit "path" and ensure path[-2] is ExprStmt if void
        # (can't use VoidType as an expression)

    def visit_PlusExpr(self, node: ast.PlusExpr):
        super().visit_PlusExpr(node)
        lhs_ty = self.get_type(node.left)
        rhs_ty = self.get_type(node.right)
        int_tycon = self.type_ctx.get_type_constructor("int")
        double_tycon = self.type_ctx.get_type_constructor("int")
        assert int_tycon and double_tycon
        int_ty = typesys.Instance(int_tycon, [])
        double_ty = typesys.Instance(double_tycon, [])
        if lhs_ty not in (int_ty, double_ty) or rhs_ty not in (
            int_ty,
            double_ty,
        ):
            raise JoeTypeError(node.location, "Can only add numeric values")
        if lhs_ty == double_ty or rhs_ty == double_ty:
            self.set_type(node, double_ty)
        else:
            self.set_type(node, int_ty)

    def visit_IndexExpr(self, node: ast.IndexExpr):
        super().visit_IndexExpr(node)
        target_ty = self.get_type(node.target)
        index_ty = self.get_type(node.index)
        # make sure target_ty is an array
        assert isinstance(target_ty, typesys.Instance) and isinstance(
            index_ty, typesys.Instance
        )
        if target_ty.type_constructor != Arrays.get_type_constructor():
            raise JoeTypeError(node.target.location, "Can only index arrays")
        if index_ty.type_constructor != self.type_ctx.get_type_constructor("int"):
            raise JoeTypeError(
                node.index.location, "Can only index arrays by integers"
            )
        self.set_type(node, target_ty.arguments[0])

    def visit_ReturnStmt(self, node: ast.ReturnStmt):
        super().visit_ReturnStmt(node)
        if node.expr is None:
            ret_expr_ty: typesys.Type = typesys.BottomType()
        else:
            ret_expr_ty = self.get_type(node.expr)
        if (
            ret_expr_ty != self.method.return_type
            and not self.method.return_type.is_supertype_of(ret_expr_ty)
        ):
            raise JoeTypeError(node.location, "Incorrect return type")
