# Types and type constructors

from __future__ import annotations


class TypeConstructor:
    def __init__(
        self,
        parameters: list[TypeParameter],
        super_: Instance | TopType,
        is_function: bool = False,
    ) -> None:
        self.parameters = parameters
        self.super = super_
        self.is_function = is_function

    def __eq__(self, other) -> bool:
        if not isinstance(other, TypeConstructor):  # type: ignore
            return NotImplemented
        return self is other

    def __hash__(self) -> int:
        return id(self)

    def parents(self) -> list[Type]:
        result: list[Type] = []
        current = self.super
        while True:
            if isinstance(current, TopType):
                break
            result.append(current)
            current = current.type_constructor.super
        return result


class TypeParameter:
    def __init__(self, variance: Variance, bound: Type) -> None:
        self.variance = variance
        self.bound = bound

    def as_variable(self) -> TypeVariable:
        return TypeVariable(self)


class Variance:
    def is_satisfied(self, left: Type, right: Type) -> bool:
        raise NotImplementedError()


class Invariant(Variance):
    def is_satisfied(self, left: Type, right: Type) -> bool:
        return left == right

    def __eq__(self, other) -> bool:
        if not isinstance(other, Variance):  # type: ignore
            return NotImplemented
        return isinstance(other, Invariant)


class Contravariant(Variance):
    def is_satisfied(self, left: Type, right: Type) -> bool:
        return left == right or right.is_supertype_of(left)

    def __eq__(self, other) -> bool:
        if not isinstance(other, Variance):  # type: ignore
            return NotImplemented
        return isinstance(other, Contravariant)


class Covariant(Variance):
    def is_satisfied(self, left: Type, right: Type) -> bool:
        return left == right or left.is_supertype_of(right)

    def __eq__(self, other) -> bool:
        if not isinstance(other, Variance):  # type: ignore
            return NotImplemented
        return isinstance(other, Covariant)


class Type:
    def is_supertype_of(self, other: Type) -> bool:
        raise NotImplementedError()


class TopType(Type):
    def is_supertype_of(self, other: Type) -> bool:
        return True

    def __eq__(self, other) -> bool:
        if not isinstance(other, Type):  # type: ignore
            return NotImplemented
        return isinstance(other, TopType)


class BottomType(Type):
    def is_supertype_of(self, other: Type) -> bool:
        return False

    def __eq__(self, other) -> bool:
        if not isinstance(other, Type):  # type: ignore
            return NotImplemented
        return isinstance(other, BottomType)


class TypeVariable(Type):
    def __init__(self, parameter: TypeParameter) -> None:
        self.parameter = parameter

    def is_supertype_of(self, other: Type) -> bool:
        # If this variable is a covariant type parameter, it could be its bound
        # or any subtype of its bound.
        # If it's a contravariant type parameter, it could be its bound or any
        # supertype of its bound.
        # Therefore, this variable "is a supertype" (i.e. `other` is compatible
        # with it) if `other` is related to the bounding type according to the
        # variance declared by the type parameter. (I think)
        return self.parameter.variance.is_satisfied(self.parameter.bound, other)

    def __eq__(self, other) -> bool:
        if not isinstance(other, Type):  # type: ignore
            return NotImplemented
        elif not isinstance(other, TypeVariable):
            return False
        return self.parameter is other.parameter


class Instance(Type):
    def __init__(
        self, type_constructor: TypeConstructor, arguments: list[Type]
    ) -> None:
        self.type_constructor = type_constructor
        self.arguments = arguments

    def is_supertype_of(self, other: Type) -> bool:
        if not isinstance(other, Instance) or self == other:
            return False
        if self.type_constructor == other.type_constructor:
            return all(
                p.variance.is_satisfied(l, r)
                for p, l, r in zip(
                    self.type_constructor.parameters,
                    self.arguments,
                    other.arguments,
                )
            )
        for parent in other.type_constructor.parents():
            if self == parent or self.is_supertype_of(parent):
                return True
        return False

    def __eq__(self, other) -> bool:
        if not isinstance(other, Type):  # type: ignore
            return NotImplemented
        elif not isinstance(other, Instance):
            return False
        return (
            self.type_constructor == other.type_constructor
            and self.arguments == other.arguments
        )


# Tests

bottom = BottomType()
top = TopType()
object_instance = Instance(TypeConstructor([], top), [])

A = TypeConstructor([], object_instance)
B = TypeConstructor([], Instance(A, []))

# Invariant type parameter
C = TypeConstructor([TypeParameter(Invariant(), top)], object_instance)
assert not Instance(C, [Instance(A, [])]).is_supertype_of(
    Instance(C, [Instance(B, [])])
)
assert not Instance(C, [Instance(B, [])]).is_supertype_of(
    Instance(C, [Instance(A, [])])
)
assert Instance(C, [Instance(A, [])]) == Instance(C, [Instance(A, [])])

# Covariant type parameter
D = TypeConstructor([TypeParameter(Covariant(), top)], object_instance)
assert Instance(D, [Instance(A, [])]).is_supertype_of(
    Instance(D, [Instance(B, [])])
)
assert not Instance(D, [Instance(B, [])]).is_supertype_of(
    Instance(D, [Instance(A, [])])
)

# Contravariant type parameter
E = TypeConstructor([TypeParameter(Contravariant(), top)], object_instance)
assert Instance(E, [Instance(B, [])]).is_supertype_of(
    Instance(E, [Instance(A, [])])
)
assert not Instance(E, [Instance(A, [])]).is_supertype_of(
    Instance(E, [Instance(B, [])])
)

# Type constructor for all binary functions
function2 = TypeConstructor(
    [
        TypeParameter(Contravariant(), top),
        TypeParameter(Contravariant(), top),
        TypeParameter(Covariant(), bottom),
    ],
    top,
)

function_type = Instance(
    function2,
    [
        Instance(A, []),
        Instance(B, []),
        object_instance,
    ],
)

assert function_type.is_supertype_of(
    Instance(
        function2,
        [
            object_instance,
            Instance(A, []),
            Instance(B, []),
        ],
    )
)

# When visiting class body
# Context of type parameters in scope
# Any occurrence of a type variable gets associated with the parameter it's
# referring to
# The representation of a class will be composed of:
# - Type constructor
# - Method and field types by name (could be `TypeVariable`s)
# - References to each monomorphisation of the class
# Monomorphisation involves:
# - Check for an existing monomorphised instance
# - Create a new type constructor which has no type parameters
# - For each method or field
#   - Replace any occurrence of a type variable with the monomorphised type of
#     the corresponding parameter
#   - Monomorphise the type of the field
# - Store a reference to the monomorphised class instance alongside the class
