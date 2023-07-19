from abc import ABC, abstractmethod
from collections.abc import Iterable
from enum import Enum, auto
from typing import Final, Literal, TypeAlias, Union


class Location:
    __slots__ = ("filename", "line", "column")

    filename: str
    line: int
    column: int

    def __init__(self, filename: str, line: int, column: int) -> None:
        self.filename = filename
        self.line = line
        self.column = column

    def __str__(self) -> str:
        return f"{self.filename}:{self.line}:{self.column}:"


class AstVisitor:
    def visit_identifier(self, identifier: "Identifier") -> None:
        pass

    def visit_type(self, type_: "Type") -> None:
        self.visit_identifier(type_.name)
        for arg in type_.type_arguments:
            self.visit_type(arg)

    def visit_type_parameter(self, type_parameter: "TypeParameter") -> None:
        self.visit_identifier(type_parameter.name)
        if type_parameter.constraint:
            self.visit_type(type_parameter.constraint)

    def visit_parameter(self, parameter: "Parameter") -> None:
        self.visit_identifier(parameter.name)
        self.visit_type(parameter.type)

    def visit_method_sig(self, method_sig: "MethodSig") -> None:
        self.visit_type(method_sig.return_type)
        self.visit_identifier(method_sig.name)
        for type_parameter in method_sig.type_parameters:
            self.visit_type_parameter(type_parameter)
        for parameter in method_sig.parameters:
            self.visit_parameter(parameter)

    def visit_import_decl(self, import_decl: "ImportDecl") -> None:
        if import_decl.imports != "*":
            assert isinstance(import_decl.imports, tuple)
            for import_ in import_decl.imports:
                self.visit_identifier(import_)

    def visit_method_decl(self, method_decl: "MethodDecl") -> None:
        self.visit_type(method_decl.return_type)
        self.visit_identifier(method_decl.name)
        for type_parameter in method_decl.type_parameters:
            self.visit_type_parameter(type_parameter)
        for parameter in method_decl.parameters:
            self.visit_parameter(parameter)
        for statement in method_decl.body:
            self.visit_statement(statement)

    def visit_constructor_decl(self, constructor_decl: "ConstructorDecl") -> None:
        self.visit_identifier(constructor_decl.name)
        for parameter in constructor_decl.parameters:
            self.visit_parameter(parameter)
        for statement in constructor_decl.body:
            self.visit_statement(statement)

    def visit_field_decl(self, field_decl: "FieldDecl") -> None:
        self.visit_type(field_decl.type)
        self.visit_identifier(field_decl.name)

    def visit_class_decl(self, class_decl: "ClassDecl") -> None:
        self.visit_identifier(class_decl.name)
        for type_parameter in class_decl.type_parameters:
            self.visit_type_parameter(type_parameter)
        for interface in class_decl.implements:
            self.visit_type(interface)
        for member in class_decl.members:
            self.visit_class_member(member)

    def visit_class_member(self, class_member: "ClassMember") -> None:
        class_member.accept(self)

    def visit_interface_member(self, interface_member: "InterfaceMember") -> None:
        interface_member.accept(self)

    def visit_interface_decl(self, interface_decl: "InterfaceDecl") -> None:
        self.visit_identifier(interface_decl.name)
        for type_parameter in interface_decl.type_parameters:
            self.visit_type_parameter(type_parameter)
        for interface in interface_decl.extends:
            self.visit_type(interface)
        for member in interface_decl.members:
            self.visit_interface_member(member)

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
        self.visit_identifier(identifier_expr.name)

    def visit_this_expr(self, this_expr: "ThisExpr") -> None:
        pass

    def visit_dot_expr(self, dot_expr: "DotExpr") -> None:
        self.visit_expression(dot_expr.expr)
        self.visit_identifier(dot_expr.name)

    def visit_call_expr(self, call_expr: "CallExpr") -> None:
        if call_expr.expr:
            self.visit_expression(call_expr.expr)
        self.visit_identifier(call_expr.name)
        for type_argument in call_expr.type_arguments:
            self.visit_type(type_argument)
        for arg in call_expr.arguments:
            self.visit_expression(arg)

    def visit_new_expr(self, new_expr: "NewExpr") -> None:
        self.visit_type(new_expr.type)
        for arg in new_expr.arguments:
            self.visit_expression(arg)

    def visit_unary_expr(self, unary_expr: "UnaryExpr") -> None:
        self.visit_expression(unary_expr.expr)

    def visit_binary_expr(self, binary_expr: "BinaryExpr") -> None:
        self.visit_expression(binary_expr.left)
        self.visit_expression(binary_expr.right)

    def visit_new_array_expr(self, new_array_expr: "NewArrayExpr") -> None:
        self.visit_type(new_array_expr.type)
        self.visit_expression(new_array_expr.size)

    def visit_index_expr(self, array_access_expr: "IndexExpr") -> None:
        self.visit_expression(array_access_expr.expr)
        self.visit_expression(array_access_expr.index)

    def visit_statement(self, statement: "Statement") -> None:
        statement.accept(self)

    def visit_expr_statement(self, expr_statement: "ExprStatement") -> None:
        self.visit_expression(expr_statement.expr)

    def visit_return_statement(self, return_statement: "ReturnStatement") -> None:
        if return_statement.expr is not None:
            self.visit_expression(return_statement.expr)

    def visit_if_statement(self, if_statement: "IfStatement") -> None:
        self.visit_expression(if_statement.condition)
        for statement in if_statement.then:
            self.visit_statement(statement)
        for statement in if_statement.else_:
            self.visit_statement(statement)

    def visit_while_statement(self, while_statement: "WhileStatement") -> None:
        self.visit_expression(while_statement.condition)
        for statement in while_statement.body:
            self.visit_statement(statement)

    def visit_for_statement(self, for_statement: "ForStatement") -> None:
        if for_statement.init:
            self.visit_statement(for_statement.init)
        if for_statement.condition:
            self.visit_expression(for_statement.condition)
        if for_statement.update:
            self.visit_expression(for_statement.update)
        for statement in for_statement.body:
            self.visit_statement(statement)

    def visit_variable_decl_statement(
        self, variable_decl_statement: "VariableDeclStatement"
    ) -> None:
        if variable_decl_statement.type:
            self.visit_type(variable_decl_statement.type)
        self.visit_identifier(variable_decl_statement.name)
        if variable_decl_statement.expr:
            self.visit_expression(variable_decl_statement.expr)

    def visit_delete_statement(self, delete_statement: "DeleteStatement") -> None:
        self.visit_expression(delete_statement.expr)


class Node(ABC):
    __slots__ = ("location",)

    def __init__(self, location: Location) -> None:
        self.location = location

    @abstractmethod
    def accept(self, visitor: AstVisitor) -> None:
        ...


class Identifier(Node):
    __slots__ = ("name",)

    def __init__(self, location: Location, name: str) -> None:
        super().__init__(location)
        self.name = name

    def accept(self, visitor: AstVisitor) -> None:
        visitor.visit_identifier(self)


class Type(Node):
    __slots__ = ("name", "type_arguments")

    def __init__(
        self, location: Location, name: Identifier, type_arguments: Iterable["Type"]
    ) -> None:
        super().__init__(location)
        self.name = name
        self.type_arguments = tuple(type_arguments)

    def accept(self, visitor: AstVisitor) -> None:
        visitor.visit_type(self)


class TypeParameter(Node):
    __slots__ = ("name", "constraint")

    def __init__(
        self, location: Location, name: Identifier, constraint: Type | None
    ) -> None:
        super().__init__(location)
        self.name = name
        self.constraint = constraint

    def accept(self, visitor: AstVisitor) -> None:
        visitor.visit_type_parameter(self)


class Parameter(Node):
    __slots__ = ("type", "name")

    def __init__(self, location: Location, type_: Type, name: Identifier) -> None:
        super().__init__(location)
        self.type = type_
        self.name = name

    def accept(self, visitor: AstVisitor) -> None:
        visitor.visit_parameter(self)


class MethodSig(Node):
    __slots__ = ("return_type", "name", "type_parameters", "parameters")

    def __init__(
        self,
        location: Location,
        return_type: Type,
        name: Identifier,
        type_parameters: Iterable[TypeParameter],
        parameters: Iterable[Parameter],
    ) -> None:
        super().__init__(location)
        self.return_type = return_type
        self.name = name
        self.type_parameters = tuple(type_parameters)
        self.parameters = tuple(parameters)

    def accept(self, visitor: AstVisitor) -> None:
        visitor.visit_method_sig(self)


# TODO: break, continue, throw
Statement: TypeAlias = Union[
    "ExprStatement",
    "ReturnStatement",
    "IfStatement",
    "WhileStatement",
    "ForStatement",
    "VariableDeclStatement",
    "DeleteStatement",
]


class ImportDecl(Node):
    __slots__ = ("module_path", "imports")

    def __init__(
        self,
        location: Location,
        module_path: tuple[str, ...],
        imports: tuple[Identifier, ...] | Literal["*"],
    ) -> None:
        super().__init__(location)
        self.module_path = module_path
        self.imports = imports

    def accept(self, visitor: AstVisitor) -> None:
        visitor.visit_import_decl(self)


class ConstructorDecl(Node):
    __slots__ = ("name", "parameters", "body")

    def __init__(
        self,
        location: Location,
        name: Identifier,
        parameters: Iterable[Parameter],
        body: Iterable[Statement],
    ) -> None:
        super().__init__(location)
        self.name = name
        self.parameters = tuple(parameters)
        self.body = tuple(body)

    def accept(self, visitor: AstVisitor) -> None:
        visitor.visit_constructor_decl(self)


class MethodDecl(Node):
    __slots__ = (
        "return_type",
        "name",
        "type_parameters",
        "parameters",
        "body",
        "static",
    )

    def __init__(
        self,
        location: Location,
        return_type: Type,
        name: Identifier,
        type_parameters: Iterable[TypeParameter],
        parameters: Iterable[Parameter],
        body: Iterable[Statement],
        static: bool,
    ) -> None:
        super().__init__(location)
        self.return_type = return_type
        self.name = name
        self.type_parameters = tuple(type_parameters)
        self.parameters = tuple(parameters)
        self.body = tuple(body)
        self.static = static

    def accept(self, visitor: AstVisitor) -> None:
        visitor.visit_method_decl(self)


class FieldDecl(Node):
    __slots__ = ("type", "name")

    def __init__(self, location: Location, type_: Type, name: Identifier) -> None:
        super().__init__(location)
        self.type = type_
        self.name = name

    def accept(self, visitor: AstVisitor) -> None:
        visitor.visit_field_decl(self)


ClassMember: TypeAlias = MethodDecl | ConstructorDecl | FieldDecl
valid_class_members: Final = (MethodDecl, ConstructorDecl, FieldDecl)
InterfaceMember: TypeAlias = MethodSig | MethodDecl
valid_interface_members: Final = (MethodSig, MethodDecl)


class ClassDecl(Node):
    __slots__ = ("name", "type_parameters", "implements", "members")

    def __init__(
        self,
        location: Location,
        name: Identifier,
        type_parameters: Iterable[TypeParameter],
        implements: Iterable[Type],
        members: Iterable[ClassMember],
    ) -> None:
        super().__init__(location)
        self.name = name
        self.type_parameters = tuple(type_parameters)
        self.implements = tuple(implements)
        self.members = tuple(members)

    def accept(self, visitor: AstVisitor) -> None:
        visitor.visit_class_decl(self)


class InterfaceDecl(Node):
    __slots__ = ("name", "type_parameters", "extends", "members")

    def __init__(
        self,
        location: Location,
        name: Identifier,
        type_parameters: Iterable[TypeParameter],
        extends: Iterable[Type],
        members: list[InterfaceMember],
    ) -> None:
        super().__init__(location)
        self.name = name
        self.type_parameters = tuple(type_parameters)
        self.extends = tuple(extends)
        self.members = tuple(members)

    def accept(self, visitor: AstVisitor) -> None:
        visitor.visit_interface_decl(self)


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
]


class LiteralInt(Node):
    __slots__ = ("value",)

    def __init__(self, location: Location, value: int) -> None:
        super().__init__(location)
        self.value = value

    def accept(self, visitor: AstVisitor) -> None:
        visitor.visit_literal_int(self)


class LiteralFloat(Node):
    __slots__ = ("value",)

    def __init__(self, location: Location, value: float) -> None:
        super().__init__(location)
        self.value = value

    def accept(self, visitor: AstVisitor) -> None:
        visitor.visit_literal_float(self)


class LiteralString(Node):
    __slots__ = ("value",)

    def __init__(self, location: Location, value: str) -> None:
        super().__init__(location)
        self.value = value

    def accept(self, visitor: AstVisitor) -> None:
        visitor.visit_literal_string(self)


class LiteralBool(Node):
    __slots__ = ("value",)

    def __init__(self, location: Location, value: bool) -> None:
        super().__init__(location)
        self.value = value

    def accept(self, visitor: AstVisitor) -> None:
        visitor.visit_literal_bool(self)


class IdentifierExpr(Node):
    __slots__ = ("name",)

    def __init__(self, location: Location, name: Identifier) -> None:
        super().__init__(location)
        self.name = name

    def accept(self, visitor: AstVisitor) -> None:
        visitor.visit_identifier_expr(self)


class ThisExpr(Node):
    __slots__ = ()

    def __init__(self, location: Location) -> None:
        super().__init__(location)

    def accept(self, visitor: AstVisitor) -> None:
        visitor.visit_this_expr(self)


class DotExpr(Node):
    __slots__ = ("expr", "name")

    def __init__(self, location: Location, expr: Expr, name: Identifier) -> None:
        super().__init__(location)
        self.expr = expr
        self.name = name

    def accept(self, visitor: AstVisitor) -> None:
        visitor.visit_dot_expr(self)


class CallExpr(Node):
    __slots__ = ("expr", "name", "type_arguments", "arguments")

    def __init__(
        self,
        location: Location,
        expr: Expr | None,
        name: Identifier,
        type_arguments: Iterable[Type],
        arguments: Iterable[Expr],
    ) -> None:
        super().__init__(location)
        self.expr = expr
        self.name = name
        self.type_arguments = tuple(type_arguments)
        self.arguments = tuple(arguments)

    def accept(self, visitor: AstVisitor) -> None:
        visitor.visit_call_expr(self)


class NewExpr(Node):
    __slots__ = ("type", "arguments")

    def __init__(
        self, location: Location, type_: Type, arguments: Iterable[Expr]
    ) -> None:
        super().__init__(location)
        self.type = type_
        self.arguments = tuple(arguments)

    def accept(self, visitor: AstVisitor) -> None:
        visitor.visit_new_expr(self)


class UnaryOperator(Enum):
    MINUS = auto()
    NOT = auto()


class UnaryExpr(Node):
    __slots__ = ("operator", "expr")

    def __init__(self, location: Location, operator: UnaryOperator, expr: Expr) -> None:
        super().__init__(location)
        self.operator = operator
        self.expr = expr

    def accept(self, visitor: AstVisitor) -> None:
        visitor.visit_unary_expr(self)


class BinaryOperator(Enum):
    PLUS = auto()
    MINUS = auto()
    TIMES = auto()
    DIVIDE = auto()
    MODULO = auto()
    EQUALS = auto()
    NOT_EQUALS = auto()
    LESS_THAN = auto()
    LESS_THAN_OR_EQUAL = auto()
    GREATER_THAN = auto()
    GREATER_THAN_OR_EQUAL = auto()
    AND = auto()
    OR = auto()
    ASSIGN = auto()


class BinaryExpr(Node):
    __slots__ = ("operator", "left", "right")

    def __init__(
        self, location: Location, operator: BinaryOperator, left: Expr, right: Expr
    ) -> None:
        super().__init__(location)
        self.operator = operator
        self.left = left
        self.right = right

    def accept(self, visitor: AstVisitor) -> None:
        visitor.visit_binary_expr(self)


class NewArrayExpr(Node):
    __slots__ = ("type", "size")

    def __init__(self, location: Location, type_: Type, size: Expr) -> None:
        super().__init__(location)
        self.type = type_
        self.size = size

    def accept(self, visitor: AstVisitor) -> None:
        visitor.visit_new_array_expr(self)


class IndexExpr(Node):
    __slots__ = ("expr", "index")

    def __init__(self, location: Location, expr: Expr, index: Expr) -> None:
        super().__init__(location)
        self.expr = expr
        self.index = index

    def accept(self, visitor: AstVisitor) -> None:
        visitor.visit_index_expr(self)


class ExprStatement(Node):
    __slots__ = ("expr",)

    def __init__(self, location: Location, expr: Expr) -> None:
        super().__init__(location)
        self.expr = expr

    def accept(self, visitor: AstVisitor) -> None:
        visitor.visit_expr_statement(self)


class ReturnStatement(Node):
    __slots__ = ("expr",)

    def __init__(self, location: Location, expr: Expr | None) -> None:
        super().__init__(location)
        self.expr = expr

    def accept(self, visitor: AstVisitor) -> None:
        visitor.visit_return_statement(self)


class IfStatement(Node):
    __slots__ = ("condition", "then", "else_")

    def __init__(
        self,
        location: Location,
        condition: Expr,
        then: tuple[Statement, ...],
        else_: tuple[Statement, ...],
    ) -> None:
        super().__init__(location)
        self.condition = condition
        self.then = then
        self.else_ = else_

    def accept(self, visitor: AstVisitor) -> None:
        visitor.visit_if_statement(self)


class WhileStatement(Node):
    __slots__ = ("condition", "body")

    def __init__(
        self, location: Location, condition: Expr, body: tuple[Statement, ...]
    ) -> None:
        super().__init__(location)
        self.condition = condition
        self.body = body

    def accept(self, visitor: AstVisitor) -> None:
        visitor.visit_while_statement(self)


class ForStatement(Node):
    __slots__ = ("init", "condition", "update", "body")

    def __init__(
        self,
        location: Location,
        init: "VariableDeclStatement | ExprStatement | None",
        condition: Expr | None,
        update: Expr | None,
        body: tuple[Statement, ...],
    ) -> None:
        super().__init__(location)
        self.init = init
        self.condition = condition
        self.update = update
        self.body = body

    def accept(self, visitor: AstVisitor) -> None:
        visitor.visit_for_statement(self)


class VariableDeclStatement(Node):
    __slots__ = ("type", "name", "expr")

    def __init__(
        self,
        location: Location,
        type_: Type | None,
        name: Identifier,
        expr: Expr | None,
    ) -> None:
        super().__init__(location)
        self.type = type_
        self.name = name
        self.expr = expr

    def accept(self, visitor: AstVisitor) -> None:
        visitor.visit_variable_decl_statement(self)


class DeleteStatement(Node):
    __slots__ = ("expr",)

    def __init__(self, location: Location, expr: Expr) -> None:
        super().__init__(location)
        self.expr = expr

    def accept(self, visitor: AstVisitor) -> None:
        visitor.visit_delete_statement(self)
