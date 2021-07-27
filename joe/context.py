import typing as t

from joe import ast, objects, typesys
from joe.source import JoeNameError

primitive_types = {
    "int": typesys.TypeConstructor(parameters=[], super_=typesys.TopType()),
    "double": typesys.TypeConstructor(parameters=[], super_=typesys.TopType()),
}

_primitive_names: t.Mapping[typesys.TypeConstructor, str] = {}

for name, tycon in primitive_types.items():
    t.cast(t.MutableMapping[typesys.TypeConstructor, str], _primitive_names)[
        tycon
    ] = name


class GlobalContext:
    def __init__(self):
        self.type_ctx = TypeContext(
            type_scope=primitive_types.copy(), class_infos={}
        )

    def populate_from_modules(self, modules: t.List[ast.Module]):
        for module in modules:
            classinfo = self._class_type(module)
            self.type_ctx.add_class(classinfo)

    def _class_type(self, mod: ast.Module) -> objects.ClassInfo:
        from joe.typevisitor import ClassDeclarationVisitor

        child_type_ctx = self.type_ctx.new_child()
        for import_ in mod.imports:
            path = import_.path.value
            tycon = self.type_ctx.get_type_constructor(path)
            if tycon is None:
                raise JoeNameError(import_.location, f"Unknown module {path}")
            classinfo = self.type_ctx.get_class_info(tycon)
            if classinfo is None:
                raise JoeNameError(import_.location, f"Unknown class {path}")
            child_type_ctx.add_class(classinfo)

        return ClassDeclarationVisitor.get_class_info(
            child_type_ctx, mod.class_decl
        )


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

    def new_child(self) -> "TypeContext":
        return TypeContext(self._type_scope, self._classes_by_type)

    def add_class(self, info: objects.ClassInfo) -> None:
        self._type_scope[info.id.name] = info.type
        self._classes_by_type[info.type] = info

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
