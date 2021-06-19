import abc
import typing as t


class TypeError(Exception):
    pass


class TypeParam:
    def __init__(self, name: str, var: "TypeVar") -> None:
        self.name = name
        self.var = var

    def __repr__(self) -> str:
        return f"<TypeParam name={self.name} var={self.var}>"


Scope = t.Mapping["TypeVar", "Type"]


class ID(abc.ABC):
    name: str

    @abc.abstractmethod
    def mangle(self) -> str:
        ...


class ClassID(ID):
    def __init__(
        self, name: str, concrete_arguments: t.Iterable["Type"] = None
    ) -> None:
        self.name = name
        self.concrete_arguments = tuple(concrete_arguments or [])

    def _key(self) -> t.Hashable:
        return (self.name, self.concrete_arguments)

    def __eq__(self, other: t.Any) -> bool:
        if isinstance(other, ClassID):  # type: ignore
            return self._key() == other._key()
        return NotImplemented

    def __hash__(self) -> int:
        return hash(self._key())

    def __repr__(self) -> str:
        if self.concrete_arguments:
            args = "[%s]" % ", ".join(map(repr, self.concrete_arguments))
        else:
            args = ""
        return f"{self.name}{args}"

    def mangle(self) -> str:
        name_parts = self.name.split(".")
        buf = "N"
        for part in name_parts:
            buf += f"{len(part)}{part}"
        if self.concrete_arguments:
            buf += "I"
            for arg in self.concrete_arguments:
                buf += arg.mangled_name()
            buf += "E"
        return buf


def test():
    T = TypeVar()
    class_id = ClassID("this.is.my.test.Class")
    class_ = ClassInstance(
        Class(
            id_=class_id,
            type_parameters=[TypeParam(n, TypeVar()) for n in "ABC"],
            members={},
            methods={
                "myfavfunction": Function(
                    id_=FunctionID(
                        class_id, "myfavfunction", concrete_arguments=[]
                    ),
                    type_parameters=[TypeParam("T", T)],
                    formal_parameters=[T, DoubleType()],
                    return_type=VoidType(),
                    static=False,
                ),
            },
        ),
        [
            IntType(),
            DoubleType(),
            ClassInstance(
                Class(
                    id_=ClassID("some.other.class.Here"),
                    type_parameters=[],
                    members={},
                    methods={},
                ),
                [],
            ),
        ],
    ).concretize({})
    print(class_.mangled_name())
    # assert (
    #     class_.mangled_name()
    #     == "N4this2is2my4test5ClassIidN4some5other5class4HereEE"
    # )
    f = FunctionInstance(
        class_.get_method("myfavfunction"),
        [IntType()],
    ).concretize({})
    print(f.mangled_name())

    print(
        FunctionInstance(
            Function(
                id_=FunctionID(ClassID("Greeter"), "greet"),
                type_parameters=[],
                formal_parameters=[
                    ClassInstance(
                        Class(
                            id_=ClassID("unsafe.Pointer"),
                            type_parameters=[TypeParam("T", TypeVar())],
                            members={},
                            methods={},
                        ),
                        [IntType()],
                    ).concretize({})
                ],
                return_type=VoidType(),
                static=False,
            ),
            [],
        ).mangled_name()
    )


class FunctionID(ID):
    def __init__(
        self,
        class_id: ClassID,
        name: str,
        concrete_arguments: t.Iterable["Type"] = None,
    ) -> None:
        self.class_id = class_id
        self.name = name
        self.concrete_arguments = tuple(concrete_arguments or [])

    def _key(self) -> t.Hashable:
        return (self.class_id, self.name, self.concrete_arguments)

    def __eq__(self, other: t.Any) -> bool:
        if isinstance(other, FunctionID):  # type: ignore
            return self._key() == other._key()
        return NotImplemented

    def __hash__(self) -> int:
        return hash(self._key())

    def __repr__(self) -> str:
        if self.concrete_arguments:
            args = "[%s]" % ", ".join(map(repr, self.concrete_arguments))
        else:
            args = ""
        return f"{self.class_id!r}.{self.name}{args}"

    def mangle(self) -> str:
        self_mangled = f"{len(self.name)}{self.name}"
        if self.concrete_arguments:
            self_mangled += "I"
            for arg in self.concrete_arguments:
                self_mangled += arg.mangled_name()
        self_mangled += "E"
        return self.class_id.mangle() + self_mangled


_TyCon = t.TypeVar("_TyCon", bound="TypeConstructor")


class TypeConstructor(abc.ABC):
    id: ID
    type_parameters: t.List[TypeParam]

    @abc.abstractmethod
    def concretize(self: _TyCon, scope: Scope) -> _TyCon:
        ...

    def __hash__(self) -> int:
        return hash(self.id)

    def mangled_name(self) -> str:
        return self.id.mangle() + "E"


class Class(TypeConstructor):
    id: ClassID

    def __init__(
        self,
        id_: ClassID,
        type_parameters: t.List[TypeParam],
        members: t.Dict[str, "Type"],
        methods: t.Dict[str, "Function"],
        superclass: t.Optional["ClassInstance"] = None,
    ) -> None:
        self.id = id_
        self.type_parameters = type_parameters
        self.members = members
        self.methods = methods
        self.superclass = superclass

    def get_member(self, name: str) -> t.Optional["Type"]:
        if name in self.members:
            return self.members[name]
        elif self.superclass is not None:
            return self.superclass.get_member(name)
        else:
            return None

    def get_method(self, name: str) -> t.Optional["Function"]:
        if name in self.methods:
            return self.methods[name]
        elif self.superclass is not None:
            return self.superclass.get_method(name)
        else:
            return None

    def concretize(self, scope: Scope) -> "Class":
        if not self.type_parameters:
            return self

        concrete_members = {
            name: mem.concretize(scope) for name, mem in self.members.items()
        }
        new_id = ClassID(
            name=self.id.name,
            concrete_arguments=[
                scope.get(p.var, p.var) for p in self.type_parameters
            ],
        )
        concrete_methods: t.Dict[str, Function] = {}
        for name, meth in self.methods.items():
            concrete_methods[name] = meth.concretize(scope)
            concrete_methods[name].id.class_id = new_id
        concrete_superclass: t.Optional[ClassInstance] = None
        if self.superclass is not None:
            _sup = self.superclass.concretize(scope)
            assert isinstance(_sup, ClassInstance)
            concrete_superclass = _sup
        return Class(
            id_=new_id,
            type_parameters=[],
            members=concrete_members,
            methods=concrete_methods,
            superclass=concrete_superclass,
        )

    def __eq__(self, other):
        if isinstance(other, Class):  # type: ignore
            return self.id == other.id
        return NotImplemented

    def __repr__(self) -> str:
        if self.type_parameters:
            params = "[%s]" % ", ".join(map(repr, self.type_parameters))
        else:
            params = ""
        return f"<Class {self.id}{params}>"


class Function(TypeConstructor):
    id: FunctionID

    def __init__(
        self,
        id_: FunctionID,
        type_parameters: t.List[TypeParam],
        formal_parameters: t.List["Type"],
        return_type: "Type",
        static: bool,
    ):
        self.id = id_
        self.type_parameters = type_parameters
        self.formal_parameters = formal_parameters
        self.return_type = return_type
        self.static = static

    def concretize(self, scope: Scope) -> "Function":
        if not self.type_parameters:
            return self

        return Function(
            id_=FunctionID(
                class_id=self.id.class_id,
                name=self.id.name,
                concrete_arguments=[
                    scope[p.var] for p in self.type_parameters if p.var in scope
                ],
            ),
            type_parameters=[
                p for p in self.type_parameters if p.var not in scope
            ],
            formal_parameters=[
                p.concretize(scope) for p in self.formal_parameters
            ],
            return_type=self.return_type.concretize(scope),
            static=self.static,
        )

    def __eq__(self, other):
        if isinstance(other, Function):  # type: ignore
            return self.id == other.id
        return NotImplemented

    def __repr__(self) -> str:
        if self.type_parameters:
            type_params = "[%s]" % ", ".join(map(repr, self.type_parameters))
        else:
            type_params = ""
        params = ", ".join(map(repr, self.formal_parameters))
        ret_ty = repr(self.return_type)
        return f"<Function {self.id}{type_params}({params}): {ret_ty}>"

    def mangled_name(self) -> str:
        mangled = super().mangled_name()

        for param in self.formal_parameters:
            mangled += param.mangled_name()

        return mangled


class Type(abc.ABC):
    @abc.abstractmethod
    def is_subtype_of(self, other: "Type") -> bool:
        ...

    @abc.abstractmethod
    def check(self, scope: Scope) -> None:
        ...

    @abc.abstractmethod
    def concretize(self, scope: Scope) -> "Type":
        ...

    @abc.abstractmethod
    def mangled_name(self) -> str:
        ...


class PlaceholderType(Type):
    def is_subtype_of(self, other: "Type") -> bool:
        return False

    def check(self, scope: Scope) -> None:
        raise TypeError("Placeholder type remains during type check")

    def concretize(self, scope: Scope) -> "Type":
        return self

    def mangled_name(self) -> str:
        raise JoeUnreachable()


class VoidType(Type):
    def is_subtype_of(self, other: Type) -> bool:
        return False

    def check(self, scope: Scope) -> None:
        pass

    def concretize(self, scope: Scope) -> Type:
        return self

    def __eq__(self, other):
        if isinstance(other, VoidType):  # type: ignore
            return True
        return NotImplemented

    def __hash__(self) -> int:
        return hash("void")

    def __repr__(self) -> str:
        return "void"

    def mangled_name(self) -> str:
        return "v"


class TypeVar(Type):
    def is_subtype_of(self, other: Type) -> bool:
        return self is other

    def check(self, scope: Scope) -> None:
        if self not in scope:
            raise TypeError(f"Unbound type variable {self!r}")

    def concretize(self, scope: Scope) -> Type:
        return scope.get(self, self)

    def mangled_name(self) -> str:
        raise JoeUnreachable()


class IntType(Type):
    def is_subtype_of(self, other: Type) -> bool:
        return self is other

    def check(self, scope: Scope) -> None:
        pass

    def concretize(self, scope: Scope) -> Type:
        return self

    def __eq__(self, other):
        if isinstance(other, IntType):  # type: ignore
            return True
        return NotImplemented

    def __hash__(self) -> int:
        return hash("int")

    def __repr__(self) -> str:
        return "int"

    def mangled_name(self) -> str:
        return "i"


class DoubleType(Type):
    def is_subtype_of(self, other: Type) -> bool:
        return self is other

    def check(self, scope: Scope) -> None:
        pass

    def concretize(self, scope: Scope) -> Type:
        return self

    def __eq__(self, other):
        if isinstance(other, DoubleType):  # type: ignore
            return True
        return NotImplemented

    def __hash__(self) -> int:
        return hash("double")

    def __repr__(self) -> str:
        return "double"

    def mangled_name(self) -> str:
        return "d"


class Instance(Type, abc.ABC):
    def __init__(self, tycon: TypeConstructor, arguments: t.List[Type]) -> None:
        self.tycon = tycon
        self.arguments = arguments

    @abc.abstractmethod
    def is_subtype_of(self, other: Type) -> bool:
        ...

    def _create_scope(self) -> t.Mapping[TypeVar, Type]:
        return dict(
            zip((p.var for p in self.tycon.type_parameters), self.arguments)
        )

    def check(self, scope: Scope) -> None:
        if len(self.arguments) != len(self.tycon.type_parameters):
            raise TypeError(
                f"Incorrect number of type arguments for {self.tycon.id}"
            )

    def concretize(self, scope: Scope) -> Type:
        concrete_args = [arg.concretize(scope) for arg in self.arguments]

        if any(isinstance(arg, TypeVar) for arg in concrete_args):
            return self.__class__(self.tycon, concrete_args)

        concrete_scope = {
            var: ty.concretize(scope)
            for var, ty in self._create_scope().items()
        }
        concrete_tycon = self.tycon.concretize(concrete_scope)
        return self.__class__(concrete_tycon, [])

    def __eq__(self, other):
        if isinstance(other, Instance):  # type: ignore
            if not self.arguments and not other.arguments:
                return self.tycon == other.tycon
            return self.concretize({}) == other.concretize({})
        return NotImplemented

    def __hash__(self) -> int:
        return hash((self.tycon.id, self.arguments))

    def __repr__(self) -> str:
        if self.arguments:
            args = "[%s]" % ", ".join(map(repr, self.arguments))
        else:
            args = ""
        return f"<{self.__class__.__name__} of {self.tycon}{args}>"

    def mangled_name(self) -> str:
        return self.tycon.mangled_name()


class ClassInstance(Instance):
    def __init__(self, class_: Class, arguments: t.List[Type]) -> None:
        super().__init__(class_, arguments)
        self.class_ = class_

    def is_subtype_of(self, other: Type) -> bool:
        if isinstance(other, ClassInstance):
            if self.class_.id == other.class_.id:
                return True
            elif self.class_.superclass is not None:
                return self.class_.superclass.is_subtype_of(other)
            else:
                return False
        else:
            return False

    def check(self, scope: Scope) -> None:
        super().check(scope)
        inner_scope = self._create_scope()
        for type_ in self.class_.members.values():
            type_.check(inner_scope)

    def get_member(self, name: str) -> t.Optional[Type]:
        return self.class_.concretize(self._create_scope()).get_member(name)

    def get_method(self, name: str) -> t.Optional[Function]:
        return self.class_.concretize(self._create_scope()).get_method(name)


class FunctionInstance(Instance):
    def __init__(self, function: Function, arguments: t.List[Type]) -> None:
        super().__init__(function, arguments)
        self.function = function

    def is_subtype_of(self, other: Type) -> bool:
        if isinstance(other, FunctionInstance):
            # Function parameters are contravariant and return types are
            # covariant.
            if self.function.id == other.function.id:
                return True
            else:
                params_all_supertype = all(
                    b.is_subtype_of(a) or a == b
                    for a, b in zip(
                        self.function.formal_parameters,
                        other.function.formal_parameters,
                    )
                )
                return_type_subtype = (
                    self.function.return_type == other.function.return_type
                    or self.function.return_type.is_subtype_of(
                        other.function.return_type
                    )
                )
                return params_all_supertype and return_type_subtype
        else:
            return False

    def check(self, scope: Scope) -> None:
        super().check(scope)
        for param in self.function.formal_parameters:
            param.check(scope)
        self.function.return_type.check(scope)


# int_ty = IntType()
# double_ty = DoubleType()
#
# T = TypeVar()
# U = TypeVar()
# V = TypeVar()
# W = TypeVar()
# X = TypeVar()
#
# """
# class super<W>                     { hello: W }
# class inner<T, V> extends super<V> { test: T }
# class outer<U>                     { inner: inner<U, int> }
# """
# container = Class(
#     id_=ClassID("container"),
#     type_parameters=[TypeParam("T", T)],
#     members={"thing": T},
#     methods={
#         "get": Function(
#             id_=FunctionID(ClassID("container"), "get"),
#             type_parameters=[],
#             formal_parameters=[],
#             return_type=T,
#         ),
#         "set": Function(
#             id_=FunctionID(ClassID("container"), "set"),
#             type_parameters=[],
#             formal_parameters=[T],
#             return_type=None,
#         ),
#     },
# )
# superclass = Class(
#     id_=ClassID("super"),
#     type_parameters=[TypeParam("W", W)],
#     members={"hello": W},
#     methods={
#         "some_func": Function(
#             id_=FunctionID(ClassID("super"), "some_func"),
#             type_parameters=[TypeParam("X", X)],
#             formal_parameters=[W, ClassInstance(container, [X])],
#             return_type=None,
#         ),
#     },
# )
# inner = Class(
#     id_=ClassID("inner"),
#     type_parameters=[TypeParam("T", T), TypeParam("V", V)],
#     members={"test": T},
#     methods={},
#     superclass=ClassInstance(superclass, [V]),
# )
# outer = Class(
#     id_=ClassID("outer"),
#     type_parameters=[TypeParam("U", U)],
#     members={"inner": ClassInstance(inner, [U, int_ty])},
#     methods={},
# )
#
# inst = ClassInstance(outer, [int_ty])
# inst.check({})
# c = inst.concretize({})
# c.check({})
# assert isinstance(c, ClassInstance)
#
# # typeof((new outer<int>()).inner.test)
# print(inst.get_member("inner").get_member("test"))  # type: ignore
# print(c.get_member("inner").get_member("test"))  # type: ignore
# # typeof((new outer<int>()).inner.hello)
# print(c.get_member("inner").get_member("hello"))  # type: ignore
#
# print(c.get_member("inner"), ClassInstance(superclass, [int_ty]).concretize({}))
#
# # (new outer<int>()).inner instanceof super<int>
# print(
#     c.get_member("inner").is_subtype_of(ClassInstance(superclass, [int_ty]).concretize({}))  # type: ignore
# )
# # (new outer<int>()).inner instanceof super<double>
# print(
#     c.get_member("inner").is_subtype_of(ClassInstance(superclass, [double_ty]).concretize({}))  # type: ignore
# )
#
# print(
#     c.get_member("inner").get_method("some_func"),
# )
# print(
#     FunctionInstance(
#         c.get_member("inner").get_method("some_func"), [double_ty]
#     ).concretize({})
# )
#
# # function A(Animal): Cat
# # function B(Cat): Animal
# # assert: A <: B
#
#
# Animal = Class(
#     id_=ClassID("Animal"),
#     type_parameters=[],
#     members={},
#     methods={},
# )
# Cat = Class(
#     id_=ClassID("Cat"),
#     type_parameters=[],
#     members={},
#     methods={},
#     superclass=ClassInstance(Animal, []),
# )
#
# A = FunctionInstance(
#     Function(
#         id_=FunctionID(ClassID("whatever"), "A"),
#         type_parameters=[],
#         formal_parameters=[ClassInstance(Animal, [])],
#         return_type=ClassInstance(Cat, []),
#     ),
#     [],
# )
# B = FunctionInstance(
#     Function(
#         id_=FunctionID(ClassID("whatever"), "B"),
#         type_parameters=[],
#         formal_parameters=[ClassInstance(Cat, [])],
#         return_type=ClassInstance(Animal, []),
#     ),
#     [],
# )
#
# assert A.is_subtype_of(B)
# assert not B.is_subtype_of(A)
