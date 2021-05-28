import typing as t
from dataclasses import dataclass, field
from patina import Option, Some, None_

from joe import ast


def builtin_type(node: ast.Type) -> Option["Type"]:
    if isinstance(node, ast.NamedType):
        if node.name.value == "int":
            return Some(IntType())
        else:
            return None_()
    elif isinstance(node, ast.VoidType):
        return Some(VoidType())
    else:
        return None_()


@dataclass(frozen=True)
class Type:
    pass


@dataclass(frozen=True)
class VoidType(Type):
    pass


@dataclass(frozen=True)
class IntType(Type):
    pass


@dataclass(frozen=True)
class ArrayType(Type):
    element_type: Type


@dataclass(frozen=True)
class Parameter:
    name: str
    type: Type


@dataclass
class MethodType(Type):
    class_type: "ClassType"
    static: bool
    name: str
    return_type: Type
    parameters: t.List[Parameter]


@dataclass
class ClassType(Type):
    name: str
    instance_type: "ObjectType"
    static_methods: t.Dict[str, MethodType] = field(default_factory=dict)


@dataclass
class ObjectType(Type):
    methods: t.Dict[str, MethodType] = field(default_factory=dict)
    fields: t.Dict[str, Type] = field(default_factory=dict)
