import typing as t
from dataclasses import dataclass, field

from joe.source import Location


@dataclass
class Node:
    location: Location


@dataclass
class Name(Node):
    value: str


@dataclass
class Type(Node):
    pass


@dataclass
class NamedType(Type):
    name: Name


@dataclass
class VoidType(Type):
    pass


@dataclass
class ArrayType(Type):
    element_type: Type


@dataclass
class Field(Node):
    name: Name
    type: Type


@dataclass
class Parameter(Node):
    name: Name
    type: Type


@dataclass
class Method(Node):
    name: Name
    return_type: Type
    parameters: t.List[Parameter] = field(default_factory=list)
    static: bool = False


@dataclass
class ClassDeclaration(Node):
    name: Name
    fields: t.List[Field] = field(default_factory=list)
    methods: t.List[Method] = field(default_factory=list)


@dataclass
class Import(Node):
    path: Name


@dataclass
class Module:
    name: str
    class_decl: ClassDeclaration
    imports: t.List[Import] = field(default_factory=list)
