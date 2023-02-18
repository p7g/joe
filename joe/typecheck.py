from abc import ABC, abstractmethod
from collections import ChainMap
from collections.abc import Iterable
from typing import Generic, Mapping, TypeAlias, TypeVar

from joe import ast
from joe._internal.exceptions import unreachable

T = TypeVar("T")
Environment: TypeAlias = ChainMap[str, "Type"]


class JoeTypeError(Exception):
    pass


class TypeParameter:
    __slots__ = ("name", "constraint")

    def __init__(self, name: str, constraint: "Type | None") -> None:
        self.name = name
        self.constraint = constraint

    def __str__(self) -> str:
        return f"{self.name}: {self.constraint}" if self.constraint else self.name

    def map(self, visitor: "TypeVisitor[Type]") -> "TypeParameter":
        return TypeParameter(
            self.name, self.constraint.accept(visitor) if self.constraint else None
        )

    def run_visitor(self, visitor: "TypeVisitor[None]") -> None:
        if self.constraint:
            self.constraint.accept(visitor)

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, TypeParameter):
            return NotImplemented
        return self.name == other.name and self.constraint == other.constraint


class Method:
    __slots__ = ("name", "return_type", "parameter_types", "type_parameters")

    def __init__(
        self,
        name: str,
        type_parameters: Iterable[TypeParameter],
        return_type: "Type",
        parameter_types: Iterable["Type"],
    ) -> None:
        self.name = name
        self.type_parameters = tuple(type_parameters)
        self.return_type = return_type
        self.parameter_types = tuple(parameter_types)

    def __str__(self) -> str:
        type_params = ", ".join(map(str, self.type_parameters))
        type_params = f"<{type_params}>" if type_params else ""
        params = ", ".join(map(str, self.parameter_types))
        return f"{self.return_type} {self.name}{type_params}({params})"

    def map(self, visitor: "TypeVisitor[Type]") -> "Method":
        return Method(
            self.name,
            [type_param.map(visitor) for type_param in self.type_parameters],
            self.return_type.accept(visitor),
            [ty.accept(visitor) for ty in self.parameter_types],
        )

    def run_visitor(self, visitor: "TypeVisitor[None]") -> None:
        for type_param in self.type_parameters:
            type_param.run_visitor(visitor)
        self.return_type.accept(visitor)
        for ty in self.parameter_types:
            ty.accept(visitor)

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, Method):
            return NotImplemented
        return (
            self.name == other.name
            and self.type_parameters == other.type_parameters
            and self.return_type == other.return_type
            and self.parameter_types == other.parameter_types
        )


class Field:
    __slots__ = ("name", "type")

    def __init__(self, name: str, type_: "Type") -> None:
        self.name = name
        self.type = type_

    def map(self, visitor: "TypeVisitor[Type]") -> "Field":
        return Field(self.name, self.type.accept(visitor))

    def run_visitor(self, visitor: "TypeVisitor[None]") -> None:
        self.type.accept(visitor)

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, Field):
            return NotImplemented
        return self.name == other.name and self.type == other.type


class TypeInfo:
    __slots__ = (
        "is_concrete",
        "name",
        "type_parameters",
        "methods",
        "static_methods",
        "fields",
        "implements",
        "is_resolved",
    )

    def __init__(
        self,
        name: str,
        is_concrete: bool,
        type_parameters: Iterable[TypeParameter],
        fields: Iterable[Field],
        methods: Iterable[Method],
        static_methods: Iterable[Method],
        implements: Iterable["Type"],
        is_resolved: bool = True,
    ) -> None:
        self.name = name
        self.is_concrete = is_concrete
        self.type_parameters = tuple(type_parameters)
        self.methods = {method.name: method for method in methods}
        self.static_methods = {method.name: method for method in static_methods}
        self.fields = {field.name: field for field in fields}
        self.implements = tuple(implements)
        self.is_resolved = is_resolved

    def __str__(self) -> str:
        if not self.is_resolved:
            return f"{self.name}?"
        return self.name

    def __repr__(self) -> str:
        if not self.is_resolved:
            return str(self)
        kind = "class" if self.is_concrete else "interface"
        type_params = ", ".join(map(str, self.type_parameters))
        fields = "\n".join(
            f"\t{field.type} {name};" for name, field in self.fields.items()
        )
        static_methods = "\n".join(
            f"\tstatic {meth};" for meth in self.static_methods.values()
        )
        methods = "\n".join(f"\t{meth};" for meth in self.methods.values())
        implements = ""
        if self.implements:
            implements += "implements " if self.is_concrete else "extends "
            implements += ", ".join(map(str, self.implements))
            implements += " "
        body = "\n\n".join(filter(bool, [fields, static_methods, methods]))
        body = f"\n{body}\n" if body else ""
        return f"{kind} {self}<{type_params}> {implements}{{{body}}}"

    def map(self, visitor: "TypeVisitor[Type]") -> "TypeInfo":
        return TypeInfo(
            self.name,
            self.is_concrete,
            [param.map(visitor) for param in self.type_parameters],
            [field.map(visitor) for field in self.fields.values()],
            [method.map(visitor) for method in self.methods.values()],
            [method.map(visitor) for method in self.static_methods.values()],
            [ty.accept(visitor) for ty in self.implements],
            self.is_resolved,
        )

    def run_visitor(self, visitor: "TypeVisitor[None]") -> None:
        for param in self.type_parameters:
            param.run_visitor(visitor)
        for field in self.fields.values():
            field.run_visitor(visitor)
        for method in self.methods.values():
            method.run_visitor(visitor)
        for method in self.static_methods.values():
            method.run_visitor(visitor)
        for ty in self.implements:
            ty.accept(visitor)

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, TypeInfo):
            return NotImplemented
        return (
            self.name == other.name
            and self.is_concrete == other.is_concrete
            and self.type_parameters == other.type_parameters
            and self.fields == other.fields
            and self.methods == other.methods
            and self.static_methods == other.static_methods
            and self.implements == other.implements
            and self.is_resolved == other.is_resolved
        )


class Type(ABC):
    @abstractmethod
    def accept(self, visitor: "TypeVisitor[T]") -> T:
        ...

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, Type):
            return NotImplemented
        return is_same_type(self, other)


class TopType(Type):
    def accept(self, visitor: "TypeVisitor[T]") -> T:
        return visitor.visit_top_type(self)

    def __str__(self) -> str:
        return "⊤"


class VoidType(Type):
    def accept(self, visitor: "TypeVisitor[T]") -> T:
        return visitor.visit_void_type(self)

    def __str__(self) -> str:
        return "void"


class Instance(Type):
    __slots__ = ("type_info", "arguments")

    def __init__(self, type_info: TypeInfo, arguments: Iterable[Type]) -> None:
        super().__init__()
        self.type_info = type_info
        self.arguments = tuple(arguments)

    def copy_modified(
        self, *, type_info: TypeInfo = None, arguments: Iterable[Type] = ()
    ) -> "Instance":
        return Instance(
            type_info or self.type_info,
            tuple(arguments) or self.arguments,
        )

    def accept(self, visitor: "TypeVisitor[T]") -> T:
        return visitor.visit_instance(self)

    def __str__(self) -> str:
        args = ", ".join(map(str, self.arguments))
        args = f"<{args}>" if args else ""
        return f"{self.type_info}{args}"

    def __repr__(self) -> str:
        return f"Instance({self.type_info}, {self.arguments})"

    def _env(self) -> dict[str, Type]:
        return dict(
            zip(
                (p.name for p in self.type_info.type_parameters),
                self.arguments,
                strict=True,
            )
        )

    def supertypes(self) -> Iterable["Type"]:
        visitor = SubstituteTypeVariables({}, self._env())
        for ty in self.type_info.implements:
            concrete_ty = ty.accept(visitor)
            assert isinstance(concrete_ty, Instance)
            yield concrete_ty
            yield from concrete_ty.supertypes()


class TypeVisitor(Generic[T]):
    def visit_instance(self, instance: Instance) -> T:
        unreachable()

    def visit_void_type(self, void_type: VoidType) -> T:
        unreachable()

    def visit_top_type(self, top_type: TopType) -> T:
        unreachable()


class SubstituteTypeVariables(TypeVisitor[Type]):
    __slots__ = ("environment", "locals")

    def __init__(
        self, environment: Mapping[str, TypeInfo], locals_: Mapping[str, Type | None]
    ) -> None:
        self.environment = environment
        self.locals = locals_

    def visit_instance(self, instance: Instance) -> Type:
        type_name = instance.type_info.name
        if type_name in self.locals:
            if instance.arguments:
                raise JoeTypeError("no higher order types")
            return self.locals[type_name] or instance

        new_arguments = [ty.accept(self) for ty in instance.arguments]
        if instance.type_info.is_resolved:
            return instance.copy_modified(arguments=new_arguments)
        env_value = self.environment.get(type_name)
        if not env_value:
            raise JoeTypeError("unknown type", type_name)
        return instance.copy_modified(type_info=env_value, arguments=new_arguments)

    def visit_void_type(self, void_type: VoidType) -> Type:
        return void_type

    def visit_top_type(self, top_type: TopType) -> Type:
        return top_type


def _unresolved_typeinfo(name: str) -> TypeInfo:
    return TypeInfo(name, False, [], [], [], [], [], False)


def _compile_type(type_ast: ast.Type) -> Type:
    if type_ast.name.name == "void":
        return VoidType()
    return Instance(
        _unresolved_typeinfo(type_ast.name.name),
        [_compile_type(arg) for arg in type_ast.type_arguments],
    )


def _compile_members(
    decl: ast.ClassDecl | ast.InterfaceDecl,
) -> tuple[list[Field], list[Method], list[Method]]:
    fields = []
    methods = []
    static_methods: list[Method] = []

    for member in decl.members:
        if isinstance(member, ast.FieldDecl):
            fields.append(Field(member.name.name, _compile_type(member.type)))
        elif isinstance(member, (ast.MethodSig, ast.MethodDecl)):
            name = member.name.name
            return_type = _compile_type(member.return_type)
            type_parameters = [
                TypeParameter(
                    param.name.name,
                    _compile_type(param.constraint) if param.constraint else None,
                )
                for param in decl.type_parameters
            ]
            param_types = [_compile_type(param.type) for param in member.parameters]
            methods.append(Method(name, type_parameters, return_type, param_types))
        elif isinstance(member, ast.ConstructorDecl):
            methods.append(
                Method(
                    decl.name.name,
                    [],
                    VoidType(),
                    [_compile_type(param.type) for param in member.parameters],
                )
            )
        else:
            raise NotImplementedError(type(member).__name__)

    return fields, methods, static_methods


def _initialize_typeinfo(decl: ast.ClassDecl | ast.InterfaceDecl) -> TypeInfo:
    is_concrete = isinstance(decl, ast.ClassDecl)
    name = decl.name.name
    fields, methods, static_methods = _compile_members(decl)
    type_info = TypeInfo(
        name,
        is_concrete,
        [
            TypeParameter(
                param.name.name,
                _compile_type(param.constraint) if param.constraint else None,
            )
            for param in decl.type_parameters
        ],
        fields,
        methods,
        static_methods,
        [
            _compile_type(ty)
            for ty in (
                decl.implements if isinstance(decl, ast.ClassDecl) else decl.extends
            )
        ],
    )
    return type_info


def is_same_type(a: Type, b: Type) -> bool:
    # True if:
    # - a and b have the same type info
    # - all type arguments are the same
    if isinstance(a, VoidType) or isinstance(b, VoidType):
        return isinstance(a, VoidType) and isinstance(b, VoidType)
    assert isinstance(a, Instance) and isinstance(b, Instance)
    return a.type_info == b.type_info and all(
        aa == bb for aa, bb in zip(a.arguments, b.arguments, strict=True)
    )


def is_subtype(a: Type, b: Type) -> bool:
    # True if:
    # - b is an interface type
    # - a is a type that implements or extends that interface
    # - all type arguments are the same (TODO covariance and contravariance)
    if isinstance(a, VoidType) or isinstance(b, VoidType):
        return isinstance(a, VoidType) and isinstance(b, VoidType)
    elif isinstance(b, TopType):
        return True

    assert isinstance(a, Instance) and isinstance(b, Instance)

    # No subclassing
    if b.type_info.is_concrete:
        return False

    # DFS hierarchy of "a", if any is_same_type as "b" then return true
    return any(is_same_type(parent, b) for parent in a.supertypes())
