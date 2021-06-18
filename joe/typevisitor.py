import typing as t
from collections import ChainMap
from contextlib import contextmanager
from dataclasses import dataclass
from joe import ast, typesys
from joe.source import JoeNameError, JoeSyntaxError, JoeTypeError, Location
from joe.visitor import Visitor

TypeOrClass = t.Union[typesys.Type, typesys.Class]


class TypeVisitor(Visitor):
    """Analyzes type expressions and results in typesys types."""

    def __init__(self, type_scope: t.Mapping[str, TypeOrClass]):
        self._type_scope = type_scope
        self.result: t.Optional[typesys.Type] = None

    @classmethod
    def analyze(
        cls, type_scope: t.Mapping[str, TypeOrClass], node: ast.Type
    ) -> typesys.Type:
        vis = cls(type_scope)
        vis.visit(node)
        assert vis.result is not None
        return vis.result

    def visit_NamedType(self, node: ast.NamedType):
        try:
            ty = self._type_scope[node.name.value]
        except KeyError:
            raise JoeNameError(
                node.location, f"Unknown type '{node.name.value}'"
            )
        if isinstance(ty, typesys.Class):
            ty = typesys.ClassInstance(ty, [])
        self.result = ty

    def visit_VoidType(self, node: ast.VoidType):
        self.result = typesys.VoidType()

    # TODO: array type (i.e. self.result = typesys.ArrayType(self.result))


# Class visitor:
# - Runs for each class and records the (analyzed) types of all fields and
#   methods.
# - The results are used to populate the global list of classes and types


class ClassDeclarationVisitor(Visitor):
    def __init__(self, type_scope: t.Mapping[str, TypeOrClass]):
        self.methods: t.Dict[str, typesys.Function] = {}
        self.fields: t.Dict[str, typesys.Type] = {}
        self.class_id: t.Optional[typesys.ClassID] = None
        self.superclass: t.Optional[typesys.Type] = None
        self._type_scope = type_scope

    @classmethod
    def get_class_type(
        cls,
        type_scope: t.Mapping[str, TypeOrClass],
        class_decl: ast.ClassDeclaration,
    ) -> typesys.Class:
        vis = cls(type_scope)
        vis.visit(class_decl)
        assert vis.class_id is not None
        assert vis.superclass is None or isinstance(
            vis.superclass, typesys.ClassInstance
        )
        return typesys.Class(
            id_=vis.class_id,
            type_parameters=[],
            members=vis.fields,
            methods=vis.methods,
            superclass=vis.superclass,
        )

    def analyze_type(self, node: ast.Type) -> typesys.Type:
        return TypeVisitor.analyze(self._type_scope, node)

    def visit_ClassDeclaration(self, node: ast.ClassDeclaration):
        self.class_id = typesys.ClassID(node.name.value)
        # TODO: inheritance
        # if node.extends:
        #     self.superclass = self.analyze_type(node.extends)
        super().visit_ClassDeclaration(node)

    def visit_Field(self, node: ast.Field):
        if node.name.value in self.fields:
            raise JoeSyntaxError(
                node.location, f"Duplicate field name '{node.name.value}'"
            )
        self.fields[node.name.value] = self.analyze_type(node.type)

    def visit_Method(self, node: ast.Method):
        if node.name.value in self.methods:
            raise JoeSyntaxError(
                node.location, f"Duplicate method name '{node.name.value}'"
            )
        if node.name.value == self.class_id.name and not isinstance(
            node.return_type, ast.VoidType
        ):
            raise JoeSyntaxError(
                node.location, "Constructor must have void return type"
            )
        assert self.class_id is not None
        self.methods[node.name.value] = typesys.Function(
            id_=typesys.FunctionID(self.class_id, node.name.value),
            type_parameters=[],
            formal_parameters=[
                self.analyze_type(p.type) for p in node.parameters
            ],
            return_type=self.analyze_type(node.return_type),
            static=node.static,
        )


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
        type_scope: t.Mapping[str, TypeOrClass],
        class_: typesys.Class,
        method: typesys.Function,
        method_node: ast.Method,
    ):
        self.type_scope = type_scope
        self.class_ = class_
        self.method = method
        self.locals: t.List[_Local] = [
            _Local(name=param.name.value, type=ty)
            for param, ty in zip(
                method_node.parameters, method.formal_parameters
            )
        ]
        self.scope_start: t.List[int] = [0]
        self.expr_types: t.Dict[ast.Node, typesys.Type] = {}

    @classmethod
    def get_expr_types(
        cls,
        type_scope: t.Mapping[str, TypeOrClass],
        class_: typesys.Class,
        method: typesys.Function,
        method_node: ast.Method,
    ) -> t.Dict[ast.Node, typesys.Type]:
        vis = cls(type_scope, class_, method, method_node)
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
        ty.check({})
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

    def lookup_class(self, name: str) -> t.Optional[typesys.Class]:
        ty = self.type_scope.get(name)
        if ty is None or not isinstance(ty, typesys.Class):
            return None
        return ty

    def analyze_type(self, node: ast.Type) -> typesys.Type:
        return TypeVisitor.analyze(self.type_scope, node)

    def visit_AssignExpr(self, node: ast.AssignExpr):
        super().visit_AssignExpr(node)
        lhs_ty = self.get_type(node.target)
        rhs_ty = self.get_type(node.value)
        if lhs_ty != rhs_ty and not rhs_ty.is_subtype_of(lhs_ty):
            raise JoeTypeError(
                node.location, "Incompatible types in assignment"
            )
        if not isinstance(node.target, (ast.IdentExpr, ast.DotExpr)):
            raise JoeSyntaxError(node.location, "Invalid lhs in assignment")
        if isinstance(node.target, ast.DotExpr):
            left_ty = self.get_type(node.target.left)
            assert isinstance(left_ty, typesys.ClassInstance)
            if left_ty.get_member(node.target.name) is None:
                raise JoeTypeError(node.target.location, "No such attribute")
        elif isinstance(node.target, ast.IdentExpr):
            if self.class_.get_member(node.target.name) is None:
                raise JoeTypeError(node.target.location, "No such attribute")
        self.set_type(node, lhs_ty)

    def visit_NewExpr(self, node: ast.NewExpr):
        super().visit_NewExpr(node)
        class_ = self.lookup_class(node.path)
        if class_ is None:
            raise JoeNameError(node.location, f"Unknown class {node.path}")
        constructor = class_.get_method(class_.id.name.rsplit(".")[-1])
        if constructor is None:
            if node.arguments:
                raise JoeTypeError(
                    node.location, "Too many arguments to constructor"
                )
        else:
            if len(node.arguments) != len(constructor.formal_parameters):
                raise JoeTypeError(
                    node.location,
                    "Incorrect number of arguments to constructor",
                )
            for arg, param in zip(
                node.arguments, constructor.formal_parameters
            ):
                arg_ty = self.get_type(arg)
                if arg_ty != param and not arg_ty.is_subtype_of(param):
                    raise JoeTypeError(
                        arg.location, "Incorrect type for constructor argument"
                    )
        self.set_type(node, typesys.ClassInstance(class_, []))

    def visit_IdentExpr(self, node: ast.IdentExpr):
        name_str = node.name
        loc = self.resolve_local(name_str)
        ty: t.Optional[typesys.Type]
        if loc is not None:
            ty = loc.type
        else:
            ty = self.class_.get_member(name_str)
            if ty is None:
                meth = self.class_.get_method(name_str)
                if meth is not None:
                    ty = typesys.FunctionInstance(meth, [])
        if ty is None:
            raise JoeNameError(
                node.location, f"Failed to resolve variable {name_str}"
            )
        self.set_type(node, ty)

    def visit_DotExpr(self, node: ast.DotExpr):
        # TODO: Support static methods
        super().visit_DotExpr(node)
        lhs_ty = self.get_type(node.left)
        if not isinstance(lhs_ty, typesys.ClassInstance):
            raise JoeTypeError(
                node.location, f"Can't access property on {lhs_ty!r}"
            )
        ty = lhs_ty.get_member(node.name)
        if ty is None:
            meth = lhs_ty.get_method(node.name)
            if meth is not None:
                ty = typesys.FunctionInstance(meth, [])
        if ty is None:
            raise JoeTypeError(node.location, f"No such property {node.name}")
        self.set_type(node, ty)

    def visit_IntExpr(self, node: ast.IntExpr):
        self.set_type(node, typesys.IntType())

    def visit_CallExpr(self, node: ast.CallExpr):
        super().visit_CallExpr(node)
        fn_ty = self.get_type(node.target)
        if not isinstance(fn_ty, typesys.FunctionInstance):
            raise JoeTypeError(node.location, "Target is not callable")
        if (
            self.method.static
            and not fn_ty.function.static
            and fn_ty.function in self.class_.methods.values()
            and isinstance(node.target, ast.IdentExpr)
        ):
            raise JoeTypeError(
                node.location, "Cannot call instance method from static method"
            )
        if len(node.arguments) != len(fn_ty.function.formal_parameters):
            raise JoeTypeError(
                node.location,
                f"Incorrect number of arguments to '{fn_ty.function.id.name}'",
            )
        if not all(
            self.get_type(arg) == param
            or self.get_type(arg).is_subtype_of(param)
            for arg, param in zip(
                node.arguments, fn_ty.function.formal_parameters
            )
        ):
            raise JoeTypeError(
                node.location,
                f"Incorrect argument types to '{fn_ty.function.id.name}'",
            )
        self.set_type(node, fn_ty.function.return_type)
        # TODO: Maintain visit "path" and ensure path[-2] is ExprStmt if void
        # (can't use VoidType as an expression)

    def visit_PlusExpr(self, node: ast.PlusExpr):
        super().visit_PlusExpr(node)
        lhs_ty = self.get_type(node.left)
        rhs_ty = self.get_type(node.right)
        if not isinstance(
            lhs_ty, (typesys.IntType, typesys.DoubleType)
        ) or not isinstance(rhs_ty, (typesys.IntType, typesys.DoubleType)):
            raise JoeTypeError(node.location, "Can only add numeric values")
        if isinstance(lhs_ty, typesys.DoubleType) or isinstance(
            rhs_ty, typesys.DoubleType
        ):
            self.set_type(node, typesys.DoubleType())
        else:
            self.set_type(node, typesys.IntType())

    def visit_ReturnStmt(self, node: ast.ReturnStmt):
        super().visit_ReturnStmt(node)
        if node.expr is None:
            ret_expr_ty: typesys.Type = typesys.VoidType()
        else:
            ret_expr_ty = self.get_type(node.expr)
        if (
            ret_expr_ty != self.method.return_type
            and not ret_expr_ty.is_subtype_of(self.method.return_type)
        ):
            raise JoeTypeError(node.location, "Incorrect return type")
