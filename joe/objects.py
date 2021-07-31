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
    ) -> None:
        self.id = id_
        self.type = type_
        self.attributes = attributes
        self.final = final

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


class Attribute:
    """Method or field"""

    def __init__(self, type_: typesys.Type) -> None:
        self.type = type_


class Method(Attribute):
    def __init__(self, type_: typesys.Type, static: bool, final: bool) -> None:
        super().__init__(type_)
        self.static = static
        self.final = final

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


class Field(Attribute):
    def __init__(self, type_: typesys.Type, final: bool) -> None:
        super().__init__(type_)
        self.final = final
