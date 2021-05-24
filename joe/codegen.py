import abc
import typing as t
from dataclasses import dataclass, field

if t.TYPE_CHECKING:
    from joe.compile import CodeGenerator


class CType(abc.ABC):
    @abc.abstractmethod
    def render(self) -> str:
        ...

    def render_named(self, name: str) -> str:
        return f"{self.render()} {name}"

    def as_pointer(self) -> "CPointerType":
        return CPointerType(inner_type=self)


@dataclass
class CPointerType(CType):
    inner_type: CType

    def render(self) -> str:
        return f"{self.inner_type.render()}*"


@dataclass
class CStructType(CType):
    name: str

    def render(self) -> str:
        return f"struct {self.name}"


@dataclass
class CFuncType(CType):
    return_type: CType
    parameter_types: t.List[CType]

    def _render(self, name: str = "") -> str:
        params = ", ".join([ty.render() for ty in self.parameter_types])
        return f"{self.return_type.render()} (*{name})({params})"

    def render(self) -> str:
        return self._render()

    def render_named(self, name: str) -> str:
        return self._render(name=name)


@dataclass
class CNamedType(CType):
    name: str

    def render(self) -> str:
        return self.name


@dataclass
class CStructField:
    name: str
    type: CType

    def render(self) -> str:
        return f"{self.type.render_named(self.name)};"


@dataclass  # type: ignore
class CDecl(abc.ABC):
    name: str

    @abc.abstractmethod
    def emit(self, gen: "CodeGenerator") -> None:
        ...


@dataclass
class CStruct(CDecl):
    fields: t.List[CStructField] = field(default_factory=list)

    def emit(self, gen: "CodeGenerator") -> None:
        gen.emit(f"struct {self.name} " + "{")
        with gen.indent():
            for f in self.fields:
                gen.emit(f.render())
        gen.emit("};")

    def emit_forward_decl(self, gen: "CodeGenerator") -> None:
        gen.emit(f"struct {self.name};")

    @property
    def type(self) -> CStructType:
        return CStructType(name=self.name)


@dataclass
class CParam:
    name: str
    type: CType


@dataclass
class CFunc(CDecl):
    return_type: CType
    parameters: t.List[CParam]
    # FIXME: body
    # body: t.List[CStatement]

    def emit(self, gen: "CodeGenerator") -> None:
        params = ", ".join(
            [p.type.render_named(p.name) for p in self.parameters]
        )
        proto = f"{self.return_type.render()} {self.name}({params})"
        gen.emit("%s {" % proto)
        with gen.indent():
            pass
        gen.emit("}")


@dataclass
class CCodeUnit:
    decls: t.List[CDecl]

    def emit(self, gen: "CodeGenerator") -> None:
        for decl in self.decls:
            decl.emit(gen)
