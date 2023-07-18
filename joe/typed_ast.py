from abc import ABC, abstractmethod
from collections.abc import Iterable
from typing import TYPE_CHECKING, TypeAlias, Union

from joe import ast

if TYPE_CHECKING:
    from .eval import BoundMethod, BoundType


class Node(ABC):
    __slots__ = ("type", "original_node")

    def __init__(self, type_: "BoundType", original_node: ast.Node) -> None:
        self.type = type_
        self.original_node = original_node

    @abstractmethod
    def accept(self, visitor: "TypedAstVisitor") -> None:
        ...


class TypedAstVisitor:
    def visit_expression(self, expression: "Expr") -> None:
        expression.accept(self)

    def visit_literal_int(self, literal_int: "LiteralInt") -> None:
        pass

    def visit_literal_float(self, literal_float: "LiteralFloat") -> None:
        pass

    def visit_literal_string(self, literal_string: "LiteralString") -> None:
        pass

    def visit_literal_bool(self, literal_bool: "LiteralBool") -> None:
        pass

    def visit_identifier_expr(self, identifier_expr: "IdentifierExpr") -> None:
        pass

    def visit_this_expr(self, this_expr: "ThisExpr") -> None:
        pass

    def visit_dot_expr(self, dot_expr: "DotExpr") -> None:
        self.visit_expression(dot_expr.expr)

    def visit_call_expr(self, call_expr: "CallExpr") -> None:
        self.visit_expression(call_expr.expr)
        for arg in call_expr.arguments:
            self.visit_expression(arg)

    def visit_new_expr(self, new_expr: "NewExpr") -> None:
        for arg in new_expr.arguments:
            self.visit_expression(arg)

    def visit_unary_expr(self, unary_expr: "UnaryExpr") -> None:
        self.visit_expression(unary_expr.expr)

    def visit_binary_expr(self, binary_expr: "BinaryExpr") -> None:
        self.visit_expression(binary_expr.left)
        self.visit_expression(binary_expr.right)

    def visit_new_array_expr(self, new_array_expr: "NewArrayExpr") -> None:
        self.visit_expression(new_array_expr.size)

    def visit_index_expr(self, array_access_expr: "IndexExpr") -> None:
        self.visit_expression(array_access_expr.expr)
        self.visit_expression(array_access_expr.index)

    def visit_integer_cast_expr(self, integer_cast_expr: "IntegerCastExpr") -> None:
        self.visit_expression(integer_cast_expr.expr)


Expr: TypeAlias = Union[
    "LiteralInt",
    "LiteralFloat",
    "LiteralString",
    "LiteralBool",
    "IdentifierExpr",
    "ThisExpr",
    "DotExpr",
    "CallExpr",
    "NewExpr",
    "UnaryExpr",
    "BinaryExpr",
    "NewArrayExpr",
    "IndexExpr",
    "IntegerCastExpr",
]


class LiteralInt(Node):
    __slots__ = ("value",)

    def __init__(self, type_: "BoundType", original_node: ast.Node, value: int) -> None:
        super().__init__(type_, original_node)
        self.value = value

    def accept(self, visitor: TypedAstVisitor) -> None:
        visitor.visit_literal_int(self)


class LiteralFloat(Node):
    __slots__ = ("value",)

    def __init__(
        self, type_: "BoundType", original_node: ast.Node, value: float
    ) -> None:
        super().__init__(type_, original_node)
        self.value = value

    def accept(self, visitor: TypedAstVisitor) -> None:
        visitor.visit_literal_float(self)


class LiteralString(Node):
    __slots__ = ("value",)

    def __init__(self, type_: "BoundType", original_node: ast.Node, value: str) -> None:
        super().__init__(type_, original_node)
        self.value = value

    def accept(self, visitor: TypedAstVisitor) -> None:
        visitor.visit_literal_string(self)


class LiteralBool(Node):
    __slots__ = ("value",)

    def __init__(
        self, type_: "BoundType", original_node: ast.Node, value: bool
    ) -> None:
        super().__init__(type_, original_node)
        self.value = value

    def accept(self, visitor: TypedAstVisitor) -> None:
        visitor.visit_literal_bool(self)


class IdentifierExpr(Node):
    __slots__ = ("name",)

    def __init__(
        self, type_: "BoundType", original_node: ast.Node, name: ast.Identifier
    ) -> None:
        super().__init__(type_, original_node)
        self.name = name

    def accept(self, visitor: TypedAstVisitor) -> None:
        visitor.visit_identifier_expr(self)


class ThisExpr(Node):
    __slots__ = ()

    def accept(self, visitor: TypedAstVisitor) -> None:
        visitor.visit_this_expr(self)


class DotExpr(Node):
    __slots__ = ("expr", "name")

    def __init__(
        self,
        type_: "BoundType",
        original_node: ast.Node,
        expr: Expr,
        name: ast.Identifier,
    ) -> None:
        super().__init__(type_, original_node)
        self.expr = expr
        self.name = name

    def accept(self, visitor: TypedAstVisitor) -> None:
        visitor.visit_dot_expr(self)


class CallExpr(Node):
    __slots__ = ("method", "expr", "name", "arguments")

    def __init__(
        self,
        method: "BoundMethod",
        original_node: ast.Node,
        expr: Expr | None,
        name: ast.Identifier,
        arguments: Iterable[Expr],
    ) -> None:
        super().__init__(method.get_return_type(), original_node)
        self.method = method
        self.expr = expr
        self.name = name
        self.arguments = tuple(arguments)

    def accept(self, visitor: TypedAstVisitor) -> None:
        visitor.visit_call_expr(self)


class NewExpr(Node):
    __slots__ = ("type", "arguments")

    def __init__(
        self, type_: "BoundType", original_node: ast.Node, arguments: Iterable[Expr]
    ) -> None:
        super().__init__(type_, original_node)
        self.arguments = tuple(arguments)

    def accept(self, visitor: TypedAstVisitor) -> None:
        visitor.visit_new_expr(self)


class UnaryExpr(Node):
    __slots__ = ("operator", "expr")

    def __init__(
        self,
        type_: "BoundType",
        original_node: ast.Node,
        operator: ast.UnaryOperator,
        expr: Expr,
    ) -> None:
        super().__init__(type_, original_node)
        self.operator = operator
        self.expr = expr

    def accept(self, visitor: TypedAstVisitor) -> None:
        visitor.visit_unary_expr(self)


class BinaryExpr(Node):
    __slots__ = ("operator", "left", "right")

    def __init__(
        self,
        type_: "BoundType",
        original_node: ast.Node,
        operator: ast.BinaryOperator,
        left: Expr,
        right: Expr,
    ) -> None:
        super().__init__(type_, original_node)
        self.operator = operator
        self.left = left
        self.right = right

    def accept(self, visitor: TypedAstVisitor) -> None:
        visitor.visit_binary_expr(self)


class NewArrayExpr(Node):
    __slots__ = ("type", "size")

    def __init__(self, type_: "BoundType", original_node: ast.Node, size: Expr) -> None:
        super().__init__(type_, original_node)
        self.size = size

    def accept(self, visitor: TypedAstVisitor) -> None:
        visitor.visit_new_array_expr(self)


class IndexExpr(Node):
    __slots__ = ("expr", "index")

    def __init__(
        self, type_: "BoundType", original_node: ast.Node, expr: Expr, index: Expr
    ) -> None:
        super().__init__(type_, original_node)
        self.expr = expr
        self.index = index

    def accept(self, visitor: TypedAstVisitor) -> None:
        visitor.visit_index_expr(self)


class IntegerCastExpr(Node):
    def __init__(self, type_: "BoundType", original_node: ast.Node, expr: Expr) -> None:
        super().__init__(type_, original_node)
        self.expr = expr

    def accept(self, visitor: TypedAstVisitor) -> None:
        visitor.visit_integer_cast_expr(self)
