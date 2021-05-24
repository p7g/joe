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


@dataclass
class Type:
    pass


@dataclass
class VoidType(Type):
    pass


@dataclass
class IntType(Type):
    pass


@dataclass
class ArrayType(Type):
    element_type: Type


@dataclass
class Parameter:
    name: str
    type: Type


@dataclass
class MethodType:
    name: str
    return_type: Type
    parameters: t.List[Parameter] = field(default_factory=list)


@dataclass
class ClassType(Type):
    name: str
    instance_type: "ObjectType"
    static_methods: t.Dict[str, MethodType] = field(default_factory=dict)


@dataclass
class ObjectType(Type):
    methods: t.Dict[str, MethodType] = field(default_factory=dict)
    fields: t.Dict[str, Type] = field(default_factory=dict)
