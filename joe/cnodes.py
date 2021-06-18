import abc
import enum
import typing as t
from dataclasses import dataclass, field
from joe.emit import Emitter


class CName(abc.ABC):
    @abc.abstractmethod
    def __str__(self) -> str:
        ...

    @abc.abstractmethod
    def unmangled(self) -> str:
        ...


@dataclass
class CUnmangledName(CName):
    name: str = ""

    def __str__(self) -> str:
        return self.name

    def unmangled(self) -> str:
        return self.name


@dataclass
class CMangledName(CName):
    parts: t.List[str]

    def __str__(self) -> str:
        # "Inspired by" mypyc
        # https://github.com/mypy/mypyc/blob/master/mypyc/namegen.py
        return "__joe_" + "___".join(
            [part.replace("___", "___4_") for part in self.parts]
        )

    def unmangled(self) -> str:
        return ".".join(self.parts)


class CType(abc.ABC):
    @abc.abstractmethod
    def render(self) -> str:
        ...

    def render_named(self, name: CName) -> str:
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
    name: CName

    def render(self) -> str:
        return f"struct {self.name}"


@dataclass
class CFuncType(CType):
    return_type: CType
    parameter_types: t.List[CType]

    def _render(self, name: CName = None) -> str:
        params = ", ".join([ty.render() for ty in self.parameter_types])
        return f"{self.return_type.render()} (*{name or CUnmangledName()})({params})"

    def render(self) -> str:
        return self._render()

    def render_named(self, name: CName) -> str:
        return self._render(name=name)


@dataclass
class CNamedType(CType):
    name: CName

    def render(self) -> str:
        return str(self.name)


@dataclass
class CStructField:
    name: CName
    type: CType

    def render(self) -> str:
        return f"{self.type.render_named(self.name)};"


@dataclass  # type: ignore
class CDecl(abc.ABC):
    name: CName

    @abc.abstractmethod
    def emit_forward_decl(self, gen: Emitter) -> None:
        ...

    @abc.abstractmethod
    def emit(self, gen: Emitter) -> None:
        ...


@dataclass
class CStruct(CDecl):
    fields: t.List[CStructField] = field(default_factory=list)

    def emit(self, gen: Emitter) -> None:
        gen.emit(f"struct {self.name} " + "{")
        with gen.indent():
            for f in self.fields:
                gen.emit(f.render())
        gen.emit("};")

    def emit_forward_decl(self, gen: Emitter) -> None:
        gen.emit(f"struct {self.name};")

    @property
    def type(self) -> CStructType:
        return CStructType(name=self.name)


@dataclass
class CParam:
    name: CName
    type: CType


@dataclass
class CFunc(CDecl):
    return_type: CType
    parameters: t.List[CParam]
    locals: t.List["CVarDecl"]
    body: t.List["CStmt"]

    def _proto(self) -> str:
        params = ", ".join(
            [p.type.render_named(p.name) for p in self.parameters]
        )
        return f"{self.return_type.render()} {self.name}({params})"

    def emit_forward_decl(self, gen: Emitter) -> None:
        proto = self._proto()
        gen.emit(f"{proto};")

    def emit(self, gen: Emitter) -> None:
        proto = self._proto()
        gen.emit("%s {" % proto)
        with gen.indent():
            for decl in self.locals:
                decl.emit(gen)
            for stmt in self.body:
                stmt.emit(gen)
        gen.emit("}")


@dataclass
class CClassDecl:
    data_type: CStruct
    vtable_type: CStruct
    class_type: CStruct


@dataclass
class CCodeUnit:
    includes: t.List[str] = field(default_factory=list)
    classes: t.List[CClassDecl] = field(default_factory=list)
    functions: t.List[CFunc] = field(default_factory=list)
    variables: t.List["CVarDecl"] = field(default_factory=list)

    def emit(self, gen: Emitter) -> None:
        for include in self.includes:
            gen.emit(f"#include <{include}>")
        for class_ in self.classes:
            class_.class_type.emit(gen)
            class_.data_type.emit(gen)
            class_.vtable_type.emit(gen)
        for func in self.functions:
            func.emit_forward_decl(gen)
        for var in self.variables:
            var.emit(gen)
        for func in self.functions:
            func.emit(gen)


@dataclass
class CExpr:
    pass


@dataclass
class CAssignmentTarget(CExpr):
    pass


@dataclass
class CParens(CExpr):
    inner: CExpr

    def __str__(self):
        return f"({self.inner})"


@dataclass
class CVariable(CAssignmentTarget):
    name: CName

    def __str__(self):
        return str(self.name)


@dataclass
class CArrayLiteral(CExpr):
    elements: t.List[CExpr]

    def __str__(self):
        elements = ", ".join([str(el) for el in self.elements])
        return "{%s}" % elements


@dataclass
class CStringLiteral(CExpr):
    value: str

    def __str__(self):
        return '"%s"' % repr(self.value)[1:-1]


@dataclass
class CArrayIndex(CAssignmentTarget):
    array_value: CExpr
    index_value: CExpr

    def __str__(self):
        return f"({self.array_value})[{self.index_value}]"


@dataclass
class CFieldAccess(CAssignmentTarget):
    struct_value: CExpr
    field_name: CName
    pointer: bool = False

    def __str__(self):
        op = "->" if self.pointer else "."
        return f"({self.struct_value}){op}{self.field_name}"


class BinOp(enum.Enum):
    Add = "+"
    Subtract = "-"
    Multiply = "*"
    Divide = "/"


@dataclass
class CBinExpr(CExpr):
    left: CExpr
    right: CExpr
    op: BinOp

    def __str__(self):
        return f"({self.left}) {self.op.value} ({self.right})"


@dataclass
class CAssignmentExpr(CExpr):
    target: CAssignmentTarget
    value: CExpr
    op: t.Optional[BinOp] = None

    def __str__(self):
        if self.op is None:
            op = "="
        else:
            op = f"{self.op.value}="
        return f"({self.target}) {op} ({self.value})"


@dataclass
class CInteger(CExpr):
    value: int

    def __str__(self):
        return str(self.value)


@dataclass
class CRef(CExpr):
    inner: CExpr

    def __str__(self):
        return f"&({self.inner})"


@dataclass
class CCast(CExpr):
    value: CExpr
    new_type: CType

    def __str__(self):
        return f"(({self.new_type.render()}) {self.value})"


@dataclass
class CTypeExpr(CExpr):
    type: CType

    def __str__(self):
        return self.type.render()


@dataclass
class CCallExpr(CExpr):
    target: CExpr
    arguments: t.List[CExpr]

    def __str__(self):
        fn = str(self.target)
        args = ", ".join([f"{arg}" for arg in self.arguments])
        return f"{fn}({args})"


class CStmt(abc.ABC):
    @abc.abstractmethod
    def emit(self, gen: Emitter) -> None:
        ...


@dataclass
class CExprStmt(CStmt):
    expr: CExpr

    def emit(self, gen: Emitter) -> None:
        gen.emit(f"{self.expr};")


@dataclass
class CVarDecl(CStmt, CDecl):
    name: CName
    type: CType
    value: t.Optional[CExpr] = None

    def emit_forward_decl(self, gen: Emitter) -> None:
        gen.emit(f"{self.type.render_named(self.name)};")

    def emit(self, gen: Emitter) -> None:
        if self.value:
            value = f" = {self.value}"
        else:
            value = ""
        gen.emit(f"{self.type.render_named(self.name)}{value};")


@dataclass
class CReturnStmt(CStmt):
    value: t.Optional[CExpr]

    def emit(self, gen: Emitter) -> None:
        if self.value:
            gen.emit("return %s;" % str(self.value))
        else:
            gen.emit("return;")


@dataclass
class CIfStmt(CStmt):
    condition: CExpr
    then: t.List[CStmt]
    else_: t.Optional[t.List[CStmt]] = None

    def emit(self, gen: Emitter) -> None:
        gen.emit("if (%s) {" % str(self.condition))
        with gen.indent():
            for stmt in self.then:
                stmt.emit(gen)
        if self.else_:
            gen.emit("} else {")
            with gen.indent():
                for stmt in self.else_:
                    stmt.emit(gen)
        gen.emit("}")


@dataclass
class CForStmt(CStmt):
    init: t.Optional[CExpr]
    cond: t.Optional[CExpr]
    inc: t.Optional[CExpr]
    body: t.List[CStmt]

    def emit(self, gen: Emitter) -> None:
        init = str(self.init) if self.init else ""
        cond = str(self.cond) if self.cond else ""
        inc = str(self.inc) if self.inc else ""
        gen.emit("for (%s; %s; %s) {" % (init, cond, inc))
        with gen.indent():
            for stmt in self.body:
                stmt.emit(gen)
        gen.emit("}")


@dataclass
class CWhileStmt(CStmt):
    cond: CExpr
    body: t.List[CStmt]

    def emit(self, gen: Emitter) -> None:
        gen.emit("while (%s) {" % str(self.cond))
        with gen.indent():
            for stmt in self.body:
                stmt.emit(gen)
        gen.emit("}")


@dataclass
class CDoWhileStmt(CStmt):
    cond: CExpr
    body: t.List[CStmt]

    def emit(self, gen: Emitter) -> None:
        gen.emit("do {")
        with gen.indent():
            for stmt in self.body:
                stmt.emit(gen)
        gen.emit("} while (%s);" % str(self.cond))
