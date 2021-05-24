import os
import typing as t
from contextlib import contextmanager

from joe import ast, codegen, types_ as ty
from joe.context import GlobalContext
from joe.parse import ModulePath


# FIXME: name mangling for int[] is not great
def _mangle_ident(*parts: str) -> str:
    ident = "__joe"
    for part in parts:
        ident += f"{len(part)}{part}"
    return f"{ident}E"


def _mangle_class_name(class_ty: ty.ClassType) -> str:
    return _mangle_ident(*ModulePath.from_class_path(class_ty.name))


_type_to_ident_table = str.maketrans(
    {
        "*": "_ptr",
        " ": "_",
        "[": "_array",
        "]": "_",
    }
)


# Global types include all classes, named types (fully-qualified)
#   -> possible to distinguish a.b.C from property access by checking for a
#      value called `a` in scope
# Module-level types extend global types including imported ones

# Separate scope for variables
# Method scope is top-level, includes class fields, then parameters, then
# locals.


class CodeGenerator:
    def __init__(self, indent_str: str = "    "):
        self._indent_level = 0
        self._buf: t.List[str] = []
        self._indent_str = indent_str

    def __indent(self) -> t.Generator[None, None, None]:
        self._indent_level += 1
        yield
        self._indent_level -= 1

    indent = t.cast(
        t.Callable[["CodeGenerator"], t.ContextManager[None]],
        contextmanager(__indent),
    )

    def emit(self, *lines: str) -> None:
        if not lines:
            self._buf.append("")
        else:
            self._buf.extend(
                self._indent_level * self._indent_str + line for line in lines
            )

    def get(self) -> str:
        return os.linesep.join(self._buf)


class ModuleCodeGenerator(CodeGenerator):
    def __init__(self, modules: t.List[ast.Module], indent_str: str = "    "):
        super().__init__(indent_str=indent_str)

        self.ctx = GlobalContext()
        self.ctx.populate_from_modules(modules)
        self._modules = modules
        self._generated_array_types: t.Dict[ty.ArrayType, codegen.CStruct] = {}

    def generate(self):
        for mod in self._modules:
            self.emit(f"/* start module {mod.name} */")
            self._generate_class(mod)
            self.emit(f"/* end module {mod.name} */")
            self.emit()

    def _resolve_class(self, mod: ast.Module, path: str) -> ty.ClassType:
        if "." not in path:
            for import_ in mod.imports:
                if import_.path.value.endswith(f".{path}"):
                    path = import_.path.value
                    break
        return self.ctx.classes[path]

    def _generate_class(self, mod: ast.Module) -> None:
        # FIXME: generate static methods
        class_path = mod.name
        class_path_parts = ModulePath.from_class_path(class_path)
        class_type = self._resolve_class(mod, class_path)

        # Generate data struct type
        data_type = codegen.CStruct(
            name=_mangle_ident(*class_path_parts, "data")
        )
        for name, field_ty in class_type.instance_type.fields.items():
            if isinstance(field_ty, ty.ArrayType):
                self._get_or_create_array_type(field_ty)
                self.emit()

        for name, field_ty in class_type.instance_type.fields.items():
            data_type.fields.append(
                codegen.CStructField(name=name, type=self._type(field_ty))
            )

        data_type.emit(self)

        # Forward declaration for object struct type
        object_type = codegen.CStruct(name=_mangle_ident(*class_path_parts))
        object_type.emit_forward_decl(self)

        # Generate vtable struct type
        vtable_type = codegen.CStruct(
            name=_mangle_ident(*class_path_parts, "vtable")
        )
        for name in class_type.instance_type.methods:
            vtable_type.fields.append(self._method_field(class_type, name))
        vtable_type.emit(self)

        # Generate object type struct
        object_type.fields.extend(
            [
                codegen.CStructField(
                    name="data", type=data_type.type.as_pointer()
                ),
                codegen.CStructField(
                    name="vtable", type=vtable_type.type.as_pointer()
                ),
            ]
        )
        object_type.emit(self)

        # Generate method implementations
        for name in class_type.instance_type.methods:
            self.emit()
            self._method(class_type, name)

        # Generate vtable variable

    def _type(self, typ: ty.Type) -> codegen.CType:
        if isinstance(typ, ty.IntType):
            return codegen.CNamedType("int")
        elif isinstance(typ, ty.VoidType):
            return codegen.CNamedType("void")
        elif isinstance(typ, ty.ArrayType):
            return self._get_or_create_array_type(typ)
        elif isinstance(typ, ty.ClassType):
            return codegen.CStructType(_mangle_class_name(typ)).as_pointer()
        elif isinstance(typ, ty.MethodType):
            return codegen.CFuncType(
                return_type=self._type(typ.return_type),
                parameter_types=[self._type(p.type) for p in typ.parameters],
            )
        else:
            raise NotImplementedError(typ)

    def _type_str(self, typ: ty.Type) -> str:
        return self._type(typ).render().translate(_type_to_ident_table)

    def _method_field(
        self, class_type: ty.ClassType, meth_name: str
    ) -> codegen.CStructField:
        meth_ty = class_type.instance_type.methods[meth_name]
        c_type = self._type(meth_ty)

        assert isinstance(c_type, codegen.CFuncType)
        c_type.parameter_types.insert(0, self._type(class_type))

        return codegen.CStructField(name=meth_name, type=c_type)

    def _method(
        self, class_type: ty.ClassType, meth_name: str, static: bool = False
    ) -> None:
        meth_ty = class_type.instance_type.methods[meth_name]
        meth_name = _mangle_ident(
            *ModulePath.from_class_path(class_type.name), meth_name
        )
        c_type = codegen.CFunc(
            name=meth_name,
            return_type=self._type(meth_ty.return_type),
            parameters=[
                codegen.CParam(name=p.name, type=self._type(p.type))
                for p in meth_ty.parameters
            ],
        )

        if not static:
            c_type.parameters.insert(
                0,
                codegen.CParam(name="self", type=self._type(class_type)),
            )

        c_type.emit(self)

    def _get_or_create_array_type(
        self, typ: ty.ArrayType
    ) -> codegen.CStructType:
        if typ in self._generated_array_types:
            return self._generated_array_types[typ].type

        element_type = typ.element_type
        struct = codegen.CStruct(name=self._type_str(element_type) + "_array")
        self._generated_array_types[typ] = struct
        struct.fields.extend(
            [
                codegen.CStructField(
                    name="elements", type=self._type(element_type).as_pointer()
                ),
                codegen.CStructField(
                    name="length", type=codegen.CNamedType("int")
                ),
            ]
        )
        struct.emit(self)
        return struct.type


class MethodCodeGenerator(CodeGenerator):
    def __init__(
        self,
        ctx: GlobalContext,
        meth_ty: ty.MethodType,
        method: ast.Method,
        indent_str: str = "    ",
    ):
        super().__init__(indent_str)
        self._method = method
        self._scope = t.ChainMap[str, ty.Type](
            ctx.classes, dict((p.name, p.type) for p in meth_ty.parameters), {}
        )

    def generate(self) -> str:
        pass
