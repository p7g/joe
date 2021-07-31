import abc
import enum
import typing as t
from dataclasses import dataclass, field
from joe.emit import Emitter


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
        return str(self.name)


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
    def emit_forward_decl(self, gen: Emitter) -> None:
        ...

    @abc.abstractmethod
    def emit(self, gen: Emitter) -> None:
        ...


@dataclass
class CStruct(CDecl, CType):
    fields: t.List[CStructField] = field(default_factory=list)

    def emit(self, gen: Emitter) -> None:
        gen.emit(f"struct {self.name} " + "{")
        with gen.indent():
            for f in self.fields:
                gen.emit(f.render())
        gen.emit("};")

    def emit_forward_decl(self, gen: Emitter) -> None:
        gen.emit(f"struct {self.name};")

    def render(self) -> str:
        e = Emitter()
        self.emit(e)
        return e.get()

    @property
    def type(self) -> CStructType:
        return CStructType(name=self.name)


@dataclass
class CTypeDef(CDecl):
    aliased_type: CType

    def emit(self, gen: Emitter) -> None:
        pass

    def emit_forward_decl(self, gen: Emitter) -> None:
        ty = self.aliased_type.render_named(self.name)
        gen.emit(f"typedef {ty};")


@dataclass
class CParam:
    name: str
    type: CType


@dataclass
class CFunc(CDecl):
    return_type: CType
    parameters: t.List[CParam]
    locals: t.List["CVarDecl"]
    body: t.List["CStmt"]
    static: bool = True  # TODO: C interop

    def _static(self) -> str:
        return "static " if self.static else ""

    def _proto(self) -> str:
        params = ", ".join(
            [p.type.render_named(p.name) for p in self.parameters]
        )
        return (
            f"{self._static()}{self.return_type.render()} {self.name}({params})"
        )

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
    vtable_type: t.Optional[CStruct]
    class_type: CDecl


@dataclass
class CCodeUnit:
    includes: t.List[str] = field(default_factory=list)
    typedefs: t.List[CTypeDef] = field(default_factory=list)
    structs: t.List[CStruct] = field(default_factory=list)
    functions: t.List[CFunc] = field(default_factory=list)
    variables: t.List["CVarDecl"] = field(default_factory=list)

    def emit(self, gen: Emitter) -> None:
        for include in self.includes:
            gen.emit(f"#include <{include}>")
        for typedef in self.typedefs:
            typedef.emit_forward_decl(gen)
        for struct in self.structs:
            struct.emit(gen)
        for typedef in self.typedefs:
            typedef.emit(gen)
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
    name: str

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
    field_name: str
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
    name: str
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
