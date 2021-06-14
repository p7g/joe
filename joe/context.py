import typing as t
from patina import HashMap, Option, None_

from joe import ast, typesys
from joe.source import JoeNameError
from joe.typevisitor import ClassDeclarationVisitor

intrinsic_types = {
    "int": typesys.IntType(),
    "double": typesys.DoubleType(),
}

ClassOrType = t.Union[typesys.Class, typesys.Type]


class GlobalContext:
    def __init__(self):
        self.type_scope: t.Dict[str, ClassOrType] = {**intrinsic_types}
        # self.array_types: t.Set[ty.ArrayType] = set()

    def populate_from_modules(self, modules: t.List[ast.Module]):
        for module in modules:
            self.type_scope[module.name] = self._class_type(module)

    def _class_type(self, mod: ast.Module) -> typesys.Class:
        types_in_scope: t.Dict[str, ClassOrType] = self.type_scope.copy()
        for import_ in mod.imports:
            path = import_.path.value
            if path not in self.type_scope:
                raise JoeNameError(import_.location, f"Unknown module {path}")
            _, cls_name = path.rsplit(".")
            types_in_scope[cls_name] = self.type_scope[path]

        return ClassDeclarationVisitor.get_class_type(
            types_in_scope, mod.class_decl
        )
