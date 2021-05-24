import typing as t
from patina import HashMap, Option, None_

from joe import ast, types_ as ty
from joe.source import JoeNameError


class GlobalContext:
    def __init__(self):
        self.classes: t.Dict[str, ty.ClassType] = {}

    def populate_from_modules(self, modules: t.List[ast.Module]):
        for module in modules:
            self.classes[module.name] = self._class_type(module)

    def _class_type(self, mod: ast.Module) -> ty.ClassType:
        types_in_scope: HashMap[str, ty.ClassType] = HashMap()
        for import_ in mod.imports:
            path = import_.path.value
            if path not in self.classes:
                raise JoeNameError(import_.location, f"Unknown module {path}")
            types_in_scope.insert(path, self.classes[path])

        instance_ty = ty.ObjectType()
        class_ty = ty.ClassType(name=mod.name, instance_type=instance_ty)

        for field in mod.class_decl.fields:
            instance_ty.fields[field.name.value] = self._type_expr(
                types_in_scope, field.type
            )

        for meth in mod.class_decl.methods:
            ret_ty = self._type_expr(types_in_scope, meth.return_type)
            params = [
                ty.Parameter(
                    name=p.name.value,
                    type=self._type_expr(types_in_scope, p.type),
                )
                for p in meth.parameters
            ]
            meth_ty = ty.MethodType(
                name=meth.name.value, return_type=ret_ty, parameters=params
            )
            if meth.static:
                class_ty.static_methods[meth.name.value] = meth_ty
            else:
                instance_ty.methods[meth.name.value] = meth_ty

        return class_ty

    def _type_expr(
        self, scope_types: HashMap[str, ty.ClassType], node: ast.Type
    ) -> ty.Type:
        if isinstance(node, ast.ArrayType):
            el_type_node = node.element_type
            return ty.ArrayType(self._type_expr(scope_types, el_type_node))
        else:
            type_: Option[ty.Type]
            if isinstance(node, ast.NamedType):
                type_ = scope_types.get(node.name.value).map(
                    # Casting to a superclass because Option[ClassType] isn't
                    # assignable to Option[Type] for whatever reason.
                    lambda typ: t.cast(ty.Type, typ)
                )
            else:
                type_ = None_()
            type_ = type_.or_else(lambda: ty.builtin_type(node))
            if type_.is_none():
                raise JoeNameError(node.location, f"Unknown type {node}")
            return type_.unwrap()
