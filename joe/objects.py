from __future__ import annotations

import typing as t

from joe import typesys


class ClassID:
    def __init__(self, name: str) -> None:
        self.name = name


class ClassInfo:
    def __init__(
        self,
        id_: ClassID,
        type_: typesys.TypeConstructor,
        attributes: dict[str, Attribute],
        final: bool,
        superclass: t.Optional["ClassInfo"],
    ) -> None:
        self.id = id_
        self.type = type_
        self.attributes = attributes
        self.final = final
        self.superclass = superclass

    @property
    def field_count(self) -> int:
        return len(tuple(self.fields()))

    def methods(self) -> t.Generator[tuple[str, Method], None, None]:
        for name, attr in self.attributes.items():
            if isinstance(attr, Method):
                yield name, attr

    def fields(self) -> t.Generator[tuple[str, Field], None, None]:
        for name, attr in self.attributes.items():
            if isinstance(attr, Field):
                yield name, attr

    def all_attributes(
        self,
    ) -> t.Generator[tuple[str, "Attribute"], None, None]:
        yield from self.attributes.items()
        if not self.superclass:
            return
        seen = set(self.attributes)
        for name, attr in self.superclass.all_attributes():
            if name in seen:
                continue
            seen.add(name)
            yield name, attr

    def has_attribute(self, name: str) -> bool:
        return name in [name for name, _attr in self.all_attributes()]

    def get_attribute(self, name: str) -> t.Optional["Attribute"]:
        return next(
            (attr for name2, attr in self.all_attributes() if name == name2),
            None,
        )

    def hierarchy(self) -> t.Generator[ClassInfo, None, None]:
        yield self
        parent = self.superclass
        while parent is not None:
            yield parent
            parent = parent.superclass


class Attribute:
    """Method or field"""

    def __init__(self, type_: typesys.Type, ci: ClassInfo) -> None:
        self.type = type_
        self.class_info = ci


class Method(Attribute):
    def __init__(
        self,
        type_: typesys.Type,
        ci: ClassInfo,
        static: bool,
        final: bool,
        overrides: Method | None,
    ) -> None:
        super().__init__(type_, ci)
        self.static = static
        self.final = final
        self.overrides = overrides

    def _ensure_valid_function_type(self) -> typesys.Instance:
        assert (
            isinstance(self.type, typesys.Instance)
            and self.type.type_constructor.is_function
        )
        assert self.type.arguments
        return self.type

    @property
    def return_type(self) -> typesys.Type:
        return self._ensure_valid_function_type().arguments[-1]

    @property
    def parameter_types(self) -> list[typesys.Type]:
        return self._ensure_valid_function_type().arguments[:-1]

    @property
    def override(self) -> bool:
        return self.overrides is not None


class Field(Attribute):
    def __init__(self, type_: typesys.Type, ci: ClassInfo, final: bool) -> None:
        super().__init__(type_, ci)
        self.final = final
