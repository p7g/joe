import typing as t
from patina import HashMap, Option, None_

from joe import ast, types_ as ty
from joe.source import JoeNameError


class GlobalContext:
    def __init__(self):
        self.classes: t.Dict[str, ty.ClassType] = {}
        self.types: t.Dict[ast.Node, ty.Type] = {}
        self.array_types: t.Set[ty.ArrayType] = set()

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
        self.types[mod.class_decl] = class_ty

        for field in mod.class_decl.fields:
            field_ty = self._type_expr(types_in_scope, field.type)
            instance_ty.fields[field.name.value] = field_ty
            self.types[field] = field_ty

        for meth in mod.class_decl.methods:
            ret_ty = self._type_expr(types_in_scope, meth.return_type)
            self.types[meth.return_type] = ret_ty
            params = []
            for param in meth.parameters:
                param_ty = self._type_expr(types_in_scope, param.type)
                self.types[param] = param_ty
                params.append(
                    ty.Parameter(name=param.name.value, type=param_ty)
                )
            meth_ty = ty.MethodType(
                class_type=class_ty,
                name=meth.name.value,
                return_type=ret_ty,
                parameters=params,
                static=meth.static,
            )
            if meth.static:
                class_ty.static_methods[meth.name.value] = meth_ty
            else:
                instance_ty.methods[meth.name.value] = meth_ty
            self.types[meth] = meth_ty

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
