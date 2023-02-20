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
        self.constraint = constraint or TopType()

    def __str__(self) -> str:
        return f"{self.name}: {self.constraint}"

    def update(self, visitor: "TypeVisitor[Type]") -> None:
        self.constraint = self.constraint.accept(visitor)

    def run_visitor(self, visitor: "TypeVisitor[None]") -> None:
        self.constraint.accept(visitor)


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

    def update(self, visitor: "TypeVisitor[Type]") -> None:
        for param in self.type_parameters:
            param.update(visitor)
        self.return_type = self.return_type.accept(visitor)
        self.parameter_types = tuple(ty.accept(visitor) for ty in self.parameter_types)

    def run_visitor(self, visitor: "TypeVisitor[None]") -> None:
        for type_param in self.type_parameters:
            type_param.run_visitor(visitor)
        self.return_type.accept(visitor)
        for ty in self.parameter_types:
            ty.accept(visitor)


class Field:
    __slots__ = ("name", "type")

    def __init__(self, name: str, type_: "Type") -> None:
        self.name = name
        self.type = type_

    def update(self, visitor: "TypeVisitor[Type]") -> None:
        self.type = self.type.accept(visitor)

    def run_visitor(self, visitor: "TypeVisitor[None]") -> None:
        self.type.accept(visitor)


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

    def update(self, visitor: "TypeVisitor[Type]") -> None:
        for param in self.type_parameters:
            param.update(visitor)
        for field in self.fields.values():
            field.update(visitor)
        # for method in self.methods.values():
        #     method.update(visitor)
        # for method in self.static_methods.values():
        #     method.update(visitor)
        self.implements = tuple(ty.accept(visitor) for ty in self.implements)

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


class Type(ABC):
    @abstractmethod
    def accept(self, visitor: "TypeVisitor[T]") -> T:
        ...

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, Type):
            return NotImplemented
        return is_same_type(self, other)


class TypeVariable(Type):
    __slots__ = ("name", "constraint")

    def __init__(self, name: str, constraint: Type) -> None:
        super().__init__()
        self.name = name
        self.constraint = constraint

    def accept(self, visitor: "TypeVisitor[T]") -> T:
        return visitor.visit_type_variable(self)

    def __str__(self) -> str:
        return f"{self.name}*"

    def __repr__(self) -> str:
        return f"{self.name}:{self.constraint}"


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

    def get_method(
        self, name: str, type_arguments: Iterable[Type], argument_types: Iterable[Type]
    ) -> "MethodInstance":
        method = self.type_info.methods[name]

        type_arguments = tuple(type_arguments)
        if not type_arguments:
            bound_type_variables = bind_type_variables_from_arguments(
                self, method, argument_types
            )
            missing = {param.name for param in method.type_parameters} - set(
                bound_type_variables
            )
            if missing:
                raise JoeTypeError(
                    f"Cannot infer type(s) of {','.join(missing)}, must be specified explicitly"  # noqa: E501
                )
            type_arguments = tuple(
                bound_type_variables[param.name] for param in method.type_parameters
            )

        return MethodInstance(self, method, type_arguments)


# left to right, parallel depth-first traversal of types
# if at any point the passed type is incompatible, abort
# upon encountering a type variable:
# - if bound in instance, get the real type from the instance
# - otherwise declare it equal to the type at same position in other type
# Afterward, if any types are unbound, require explicit type arguments


def bind_type_variables_from_arguments(
    self_type: Instance, method: Method, arguments: Iterable[Type]
) -> dict[str, Type]:
    bound = {}
    method_type_variables = {param.name for param in method.type_parameters}
    # for name, value in self_type._env().items():
    #     if name in method_arg_names:
    #         continue
    #     bound[name] = value

    def search(a: Type, b: Type) -> None:
        if isinstance(a, TypeVariable):
            if a.name not in bound and a.name in method_type_variables:
                bound[a.name] = b
            return
        elif isinstance(a, Instance):
            # TODO: it may still be possible to infer the types when the type
            # infos are not the same for example Collection<T> and
            # HashMap<String, Integer> could maybe infer T=Integer
            if not isinstance(b, Instance) or a.type_info != b.type_info:
                return
            for a2, b2 in zip(a.arguments, b.arguments, strict=True):
                search(a2, b2)
        else:
            raise NotImplementedError(a)

    for param, arg in zip(method.parameter_types, arguments, strict=True):
        search(param, arg)

    return bound


class MethodInstance:
    __slots__ = ("self_type", "method", "arguments")

    def __init__(
        self, self_type: Instance, method: Method, arguments: Iterable[Type]
    ) -> None:
        super().__init__()
        self.self_type = self_type
        self.method = method
        self.arguments = tuple(arguments)

    def __str__(self) -> str:
        arguments = f"<{', '.join(map(str, self.arguments))}>" if self.arguments else ""
        return f"{self.self_type}.{self.method.name}{arguments}"

    def _real_type(self, type_: Type) -> Type:
        vis = SubstituteTypeVariables({}, ChainMap(self._env(), self.self_type._env()))
        return type_.accept(vis)

    def parameter_types(self) -> list[Type]:
        return [self._real_type(ty) for ty in self.method.parameter_types]

    def return_type(self) -> Type:
        return self._real_type(self.method.return_type)

    def _env(self) -> dict[str, Type]:
        return {
            param.name: ty
            for param, ty in zip(
                self.method.type_parameters, self.arguments, strict=True
            )
        }

    # FIXME: Return reason why if false
    def implements(self, other: "MethodInstance") -> bool:
        if len(self.method.type_parameters) != len(other.method.type_parameters):
            return False
        for self_param, other_param in zip(
            self.method.type_parameters, other.method.type_parameters
        ):
            if not is_subtype(other_param.constraint, self_param.constraint):
                return False
        self_parameter_types = self.parameter_types()
        other_parameter_types = other.parameter_types()
        if len(self_parameter_types) != len(other_parameter_types):
            return False
        for self_param, other_param in zip(self_parameter_types, other_parameter_types):
            if not is_subtype(other_param, self_param):
                return False
        return is_subtype(self.return_type(), other.return_type())


class TypeVisitor(Generic[T]):
    def visit_instance(self, instance: Instance) -> T:
        unreachable()

    def visit_void_type(self, void_type: VoidType) -> T:
        unreachable()

    def visit_top_type(self, top_type: TopType) -> T:
        unreachable()

    def visit_type_variable(self, type_variable: TypeVariable) -> T:
        unreachable()


class SubstituteTypeVariables(TypeVisitor[Type]):
    __slots__ = ("environment", "locals")

    def __init__(
        self, environment: Mapping[str, TypeInfo], locals_: Mapping[str, Type]
    ) -> None:
        self.environment = environment
        self.locals = locals_

    def visit_instance(self, instance: Instance) -> Type:
        type_name = instance.type_info.name
        if type_name in self.locals:
            if instance.arguments:
                raise JoeTypeError("no higher order types")
            return self.locals[type_name].accept(self)

        new_arguments = [ty.accept(self) for ty in instance.arguments]
        if instance.type_info.is_resolved:
            return instance.copy_modified(arguments=new_arguments)
        env_value = self.environment.get(type_name)
        if not env_value:
            raise JoeTypeError("unknown type", type_name)
        assert env_value.is_resolved
        return instance.copy_modified(type_info=env_value, arguments=new_arguments)

    def visit_void_type(self, void_type: VoidType) -> Type:
        return void_type

    def visit_top_type(self, top_type: TopType) -> Type:
        return top_type

    def visit_type_variable(self, type_variable: TypeVariable) -> Type:
        local = self.locals.get(type_variable.name)
        if local and not isinstance(local, TypeVariable):
            return local.accept(self)
        return TypeVariable(type_variable.name, type_variable.constraint.accept(self))


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
                for param in member.type_parameters
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
    elif isinstance(a, TopType) or isinstance(b, TopType):
        return isinstance(a, TopType) and isinstance(b, TopType)
    elif isinstance(a, TypeVariable) or isinstance(b, TypeVariable):
        if isinstance(a, TypeVariable):
            a = a.constraint
        if isinstance(b, TypeVariable):
            b = b.constraint
        return is_same_type(a, b)
    assert isinstance(a, Instance) and isinstance(b, Instance)
    return a.type_info == b.type_info and all(
        aa == bb for aa, bb in zip(a.arguments, b.arguments, strict=True)
    )


def is_subtype(a: Type, b: Type) -> bool:
    if is_same_type(a, b):
        return True

    # True if:
    # - b is an interface type
    # - a is a type that implements or extends that interface
    # - all type arguments are the same (TODO covariance and contravariance)
    if isinstance(a, VoidType) or isinstance(b, VoidType):
        return isinstance(a, VoidType) and isinstance(b, VoidType)
    elif isinstance(a, TopType) or isinstance(b, TopType):
        return isinstance(b, TopType)
    elif isinstance(a, TypeVariable) or isinstance(b, TypeVariable):
        if isinstance(a, TypeVariable):
            a = a.constraint
        if isinstance(b, TypeVariable):
            b = b.constraint
        return is_subtype(a, b)

    assert isinstance(a, Instance) and isinstance(b, Instance), f"{a!r} {b!r}"

    # No subclassing
    if b.type_info.is_concrete:
        return False

    # DFS hierarchy of "a", if any is_same_type as "b" then return true
    return any(is_same_type(parent, b) for parent in a.supertypes())
