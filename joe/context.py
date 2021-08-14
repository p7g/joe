from __future__ import annotations

import typing as t

from joe import ast, objects, typesys
from joe.source import JoeNameError

primitive_types = {
    "int": typesys.TypeConstructor(parameters=[], super_=typesys.TopType()),
    "double": typesys.TypeConstructor(parameters=[], super_=typesys.TopType()),
    "boolean": typesys.TypeConstructor(parameters=[], super_=typesys.TopType()),
}

_primitive_names: t.Mapping[typesys.TypeConstructor, str] = {}

for name, tycon in primitive_types.items():
    t.cast(t.MutableMapping[typesys.TypeConstructor, str], _primitive_names)[
        tycon
    ] = name


class NullType(typesys.Instance):
    type_constructor = typesys.TypeConstructor([], typesys.TopType())

    def __init__(self, type_ctx: TypeContext) -> None:
        super().__init__(self.type_constructor, [])
        self.type_ctx = type_ctx

    def is_subtype_of(self, other: typesys.Type) -> bool:
        if not isinstance(other, typesys.Instance):
            return False
        # null is a subtype of every object
        return self.type_ctx.get_class_info(other.type_constructor) is not None


class GlobalContext:
    def __init__(self):
        self.type_ctx = TypeContext(
            type_scope=primitive_types.copy(), class_infos={}
        )

    def populate_from_modules(self, modules: t.List[ast.Module]):
        for module in modules:
            child_type_ctx = self.type_ctx.new_child()
            classinfos = self._class_type(child_type_ctx, module)
            for classinfo in classinfos:
                self.type_ctx.add_class(classinfo.id.name, classinfo)

    def _class_type(
        self, child_type_ctx: "TypeContext", mod: ast.Module
    ) -> t.List[objects.ClassInfo]:
        from joe.typevisitor import ClassDeclarationVisitor

        for import_ in mod.imports:
            path = import_.path.value
            tycon = self.type_ctx.get_type_constructor(path)
            if tycon is None:
                raise JoeNameError(import_.location, f"Unknown class {path}")
            classinfo = self.type_ctx.get_class_info(tycon)
            if classinfo is None:
                raise JoeNameError(import_.location, f"Unknown class {path}")
            child_type_ctx.add_class(classinfo.id.basename, classinfo)

        decls = []
        for class_decl in mod.class_decls:
            ci = ClassDeclarationVisitor.get_class_info(
                child_type_ctx, class_decl
            )
            decls.append(ci)
            child_type_ctx.add_class(ci.id.basename, ci)
        return decls


class TypeContext:
    def __init__(
        self,
        type_scope: t.Mapping[str, typesys.TypeConstructor],
        class_infos: t.Mapping[typesys.TypeConstructor, objects.ClassInfo],
    ):
        self._type_scope: t.MutableMapping[
            str, typesys.TypeConstructor
        ] = t.ChainMap(type_scope)
        self._classes_by_type: t.MutableMapping[
            typesys.TypeConstructor, objects.ClassInfo
        ] = t.ChainMap(class_infos)
        self._used_array_types: t.List[typesys.Type] = []
        self._null_type = NullType(self)

    def new_child(self) -> "TypeContext":
        return TypeContext(self._type_scope, self._classes_by_type)

    def add_class(self, name: str, info: objects.ClassInfo) -> None:
        self._type_scope[name] = info.type
        self._classes_by_type[info.type] = info

    def add_array_type(self, ty: typesys.Type) -> None:
        # FIXME: __hash__ for types
        if any(t == ty for t in self._used_array_types):
            return
        self._used_array_types.append(ty)

    def get_null_type(self) -> NullType:
        return self._null_type

    def get_type_constructor(
        self, name: str
    ) -> t.Optional[typesys.TypeConstructor]:
        return self._type_scope.get(name)

    def get_class_info(
        self, tycon: typesys.TypeConstructor
    ) -> t.Optional[objects.ClassInfo]:
        # None means it's a primitive type
        return self._classes_by_type.get(tycon)

    def get_primitive_name(self, tycon: typesys.TypeConstructor) -> str:
        return _primitive_names[tycon]
