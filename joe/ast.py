import typing as t
from dataclasses import dataclass, field

from joe.source import Location


@dataclass(eq=False)
class Node:
    location: Location


@dataclass(eq=False)
class Name(Node):
    value: str


@dataclass(eq=False)
class Type(Node):
    pass


@dataclass(eq=False)
class NamedType(Type):
    name: Name


@dataclass(eq=False)
class VoidType(Type):
    pass


@dataclass(eq=False)
class ArrayType(Type):
    element_type: Type


@dataclass(eq=False)
class Field(Node):
    name: Name
    type: Type
    final: bool


@dataclass(eq=False)
class Parameter(Node):
    name: Name
    type: Type


@dataclass(eq=False)
class Method(Node):
    name: Name
    return_type: Type
    parameters: t.List[Parameter] = field(default_factory=list)
    body: t.List["Stmt"] = field(default_factory=list)
    static: bool = False
    final: bool = False


@dataclass(eq=False)
class ClassDeclaration(Node):
    name: Name
    superclass: t.Optional[Type]
    fields: t.List[Field] = field(default_factory=list)
    methods: t.List[Method] = field(default_factory=list)
    final: bool = False


@dataclass(eq=False)
class Import(Node):
    path: Name


@dataclass
class Module:
    name: str
    class_decls: t.List[ClassDeclaration]
    imports: t.List[Import] = field(default_factory=list)


@dataclass(eq=False)
class Expr(Node):
    pass


@dataclass(eq=False)
class AssignmentTarget(Expr):
    pass


@dataclass(eq=False)
class AssignExpr(Expr):
    target: AssignmentTarget
    value: Expr


@dataclass(eq=False)
class NewExpr(Expr):
    type: Type
    arguments: t.List[Expr]


@dataclass(eq=False)
class SuperExpr(Expr):
    pass


@dataclass(eq=False)
class ThisExpr(Expr):
    pass


@dataclass(eq=False)
class IdentExpr(AssignmentTarget):
    name: str


@dataclass(eq=False)
class DotExpr(AssignmentTarget):
    left: Expr
    name: str


@dataclass(eq=False)
class IntExpr(Expr):
    value: int


@dataclass(eq=False)
class CallExpr(Expr):
    target: Expr
    arguments: t.List[Expr]


@dataclass(eq=False)
class IndexExpr(AssignmentTarget):
    target: Expr
    index: Expr


@dataclass(eq=False)
class PlusExpr(Expr):
    left: Expr
    right: Expr


@dataclass(eq=False)
class Stmt(Node):
    pass


@dataclass(eq=False)
class ReturnStmt(Stmt):
    expr: t.Optional[Expr]


@dataclass(eq=False)
class DeleteStmt(Stmt):
    expr: Expr


@dataclass(eq=False)
class ExprStmt(Stmt):
    expr: Expr


@dataclass(eq=False)
class VarDeclaration(Stmt):
    name: Name
    type: Type
    initializer: t.Optional["Expr"];
