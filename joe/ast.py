from abc import ABC, abstractmethod
from collections.abc import Iterable
from typing import Final, Generic, TypeAlias, TypeVar

from joe._internal.exceptions import unreachable

T = TypeVar("T")


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


class AstVisitor(Generic[T]):
    def visit_identifier(self, identifier: "Identifier") -> T:
        unreachable()

    def visit_type(self, type_: "Type") -> T:
        unreachable()

    def visit_type_parameter(self, type_parameter: "TypeParameter") -> T:
        unreachable()

    def visit_parameter(self, parameter: "Parameter") -> T:
        unreachable()

    def visit_method_sig(self, method_sig: "MethodSig") -> T:
        unreachable()

    def visit_method_decl(self, method_decl: "MethodDecl") -> T:
        unreachable()

    def visit_constructor_decl(self, constructor_decl: "ConstructorDecl") -> T:
        unreachable()

    def visit_field_decl(self, field_decl: "FieldDecl") -> T:
        unreachable()

    def visit_class_decl(self, class_decl: "ClassDecl") -> T:
        unreachable()

    def visit_interface_decl(self, interface_decl: "InterfaceDecl") -> T:
        unreachable()


class Node(ABC):
    __slots__ = ("location",)

    def __init__(self, location: Location) -> None:
        self.location = location

    @abstractmethod
    def accept(self, visitor: AstVisitor[T]) -> T:
        ...


class Identifier(Node):
    __slots__ = ("name",)

    def __init__(self, location: Location, name: str) -> None:
        super().__init__(location)
        self.name = name

    def accept(self, visitor: AstVisitor[T]) -> T:
        return visitor.visit_identifier(self)


class Type(Node):
    __slots__ = ("name", "type_arguments")

    def __init__(
        self, location: Location, name: Identifier, type_arguments: Iterable["Type"]
    ) -> None:
        super().__init__(location)
        self.name = name
        self.type_arguments = tuple(type_arguments)

    def accept(self, visitor: AstVisitor[T]) -> T:
        return visitor.visit_type(self)


class TypeParameter(Node):
    __slots__ = ("name", "constraint")

    def __init__(
        self, location: Location, name: Identifier, constraint: Type | None
    ) -> None:
        super().__init__(location)
        self.name = name
        self.constraint = constraint

    def accept(self, visitor: AstVisitor[T]) -> T:
        return visitor.visit_type_parameter(self)


class Parameter(Node):
    __slots__ = ("type", "name")

    def __init__(self, location: Location, type_: Type, name: Identifier) -> None:
        super().__init__(location)
        self.type = type_
        self.name = name

    def accept(self, visitor: AstVisitor[T]) -> T:
        return visitor.visit_parameter(self)


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

    def accept(self, visitor: AstVisitor[T]) -> T:
        return visitor.visit_method_sig(self)


Statement: TypeAlias = None


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

    def accept(self, visitor: AstVisitor[T]) -> T:
        return visitor.visit_constructor_decl(self)


class MethodDecl(Node):
    __slots__ = ("return_type", "name", "type_parameters", "parameters", "body")

    def __init__(
        self,
        location: Location,
        return_type: Type,
        name: Identifier,
        type_parameters: Iterable[TypeParameter],
        parameters: Iterable[Parameter],
        body: Iterable[Statement],
    ) -> None:
        super().__init__(location)
        self.return_type = return_type
        self.name = name
        self.type_parameters = tuple(type_parameters)
        self.parameters = tuple(parameters)
        self.body = tuple(body)

    def accept(self, visitor: AstVisitor[T]) -> T:
        return visitor.visit_method_decl(self)


class FieldDecl(Node):
    __slots__ = ("type", "name")

    def __init__(self, location: Location, type_: Type, name: Identifier) -> None:
        super().__init__(location)
        self.type = type_
        self.name = name

    def accept(self, visitor: AstVisitor[T]) -> T:
        return visitor.visit_field_decl(self)


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

    def accept(self, visitor: AstVisitor[T]) -> T:
        return visitor.visit_class_decl(self)


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

    def accept(self, visitor: AstVisitor[T]) -> T:
        return visitor.visit_interface_decl(self)
