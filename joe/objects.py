from __future__ import annotations

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
    ) -> None:
        self.id = id_
        self.type = type_
        self.attributes = attributes


class Attribute:
    """Method or field"""

    def __init__(self, type_: typesys.Type) -> None:
        self.type = type_


class Method(Attribute):
    def __init__(self, type_: typesys.Type, static: bool) -> None:
        super().__init__(type_)
        self.static = static

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
    pass
