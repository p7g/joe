import os
import typing as t
from contextlib import contextmanager
from patina import Option, None_

from joe import ast, types_ as ty
from joe.context import GlobalContext
from joe.parse import ModulePath


# FIXME: name mangling for int[] is not great
def _mangle_ident(*parts: str) -> str:
    ident = "__joe"
    for part in parts:
        ident += f"{len(part)}{part}"
    return f"{ident}E"


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
        self._generated_array_types: t.Set[str] = set()

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
        data_type_name = _mangle_ident(*class_path_parts, "data")
        for name, field_ty in class_type.instance_type.fields.items():
            if isinstance(field_ty, ty.ArrayType):
                self._get_or_create_array_type(field_ty)
                self.emit()
        self.emit(f"struct {data_type_name} " + "{")
        with self.indent():
            for name, field_ty in class_type.instance_type.fields.items():
                self.emit(f"{self._type(field_ty)} {name};")
        self.emit("};")

        # Forward declaration for object struct type
        object_type_name = _mangle_ident(*class_path_parts)
        self.emit()
        self.emit(f"struct {object_type_name};")

        # Generate vtable struct type
        self.emit()
        vtable_type_name = _mangle_ident(*class_path_parts, "vtable")
        self.emit(f"struct {vtable_type_name} " + "{")
        with self.indent():
            for name in class_type.instance_type.methods:
                self.emit(f"{self._method_field(class_type, name)};")
        self.emit("};")

        # Generate object type struct
        self.emit()
        self.emit(f"struct {object_type_name} " + "{")
        with self.indent():
            self.emit(f"struct {data_type_name} *data;")
            self.emit(f"struct {vtable_type_name} *vtable;")
        self.emit("};")

        # Generate method implementations
        for name in class_type.instance_type.methods:
            self.emit()
            self._method(class_type, name)

        # Generate vtable variable

    def _type(self, typ: ty.Type) -> str:
        if isinstance(typ, ty.IntType):
            return "int"
        elif isinstance(typ, ty.VoidType):
            return "void"
        elif isinstance(typ, ty.ArrayType):
            return self._get_or_create_array_type(typ)
        elif isinstance(typ, ty.ClassType):
            return f"struct {_mangle_ident(typ.name)}"
        else:
            raise NotImplementedError(typ)

    def _type_str(self, typ: ty.Type) -> str:
        if isinstance(typ, ty.ArrayType):
            return f"{self._type_str(typ.element_type)}__array"
        else:
            return self._type(typ)

    def _method_field(self, class_type: ty.ClassType, meth_name: str) -> str:
        meth_ty = class_type.instance_type.methods[meth_name]

        buf = ""
        buf += self._type(meth_ty.return_type)
        self_type_name = f"struct {_mangle_ident(class_type.name)}"
        buf += f" (*{meth_ty.name})({self_type_name} *"
        for param in meth_ty.parameters:
            buf += ", "
            buf += self._type(param.type)
        buf += ")"

        return buf

    def _method(
        self, class_type: ty.ClassType, meth_name: str, static: bool = False
    ) -> str:
        meth_ty = class_type.instance_type.methods[meth_name]
        buf = ""

        meth_name = _mangle_ident(class_type.name, meth_name)
        buf += f"{self._type(meth_ty.return_type)} {meth_name}("

        if not static:
            self_type_name = _mangle_ident(class_type.name)
            buf += f"struct {self_type_name} *self"
            if meth_ty.parameters:
                buf += ", "

        is_first_param = True
        for param in meth_ty.parameters:
            if is_first_param:
                is_first_param = False
            else:
                buf += ", "
            buf += f"{self._type(param.type)} {param.name}"
        buf += ") {"
        self.emit(buf)
        # FIXME: method body
        self.emit("}")

        return buf

    def _get_or_create_array_type(self, typ: ty.ArrayType) -> str:
        element_type = typ.element_type
        array_type_name = _mangle_ident(self._type_str(typ))
        array_type_name = f"struct {array_type_name}"
        if array_type_name in self._generated_array_types:
            return array_type_name
        self._generated_array_types.add(array_type_name)
        self.emit("%s {" % array_type_name)
        with self.indent():
            self.emit("%s *elements;" % self._type(element_type))
            self.emit("int length;")
        self.emit("};")
        return array_type_name


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
