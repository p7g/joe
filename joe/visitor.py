import dataclasses
import typing as t

from joe import ast


class Visitor:
    def visit(self, node: ast.Node):
        if isinstance(node, ast.Name):
            self.visit_Name(node)
        elif isinstance(node, ast.Type):
            self.visit_Type(node)
        elif isinstance(node, ast.Field):
            self.visit_Field(node)
        elif isinstance(node, ast.Parameter):
            self.visit_Parameter(node)
        elif isinstance(node, ast.Method):
            self.visit_Method(node)
        elif isinstance(node, ast.ClassDeclaration):
            self.visit_ClassDeclaration(node)
        elif isinstance(node, ast.Import):
            self.visit_Import(node)
        elif isinstance(node, ast.Expr):
            self.visit_Expr(node)
        elif isinstance(node, ast.Stmt):
            self.visit_Stmt(node)
        else:
            raise NotImplementedError(node)

    def visit_Name(self, node: ast.Name):
        pass

    def visit_Type(self, node: ast.Type):
        if isinstance(node, ast.NamedType):
            self.visit_NamedType(node)
        elif isinstance(node, ast.VoidType):
            self.visit_VoidType(node)
        elif isinstance(node, ast.ArrayType):
            self.visit_ArrayType(node)
        else:
            raise NotImplementedError(node)

    def visit_NamedType(self, node: ast.NamedType):
        self.visit_Name(node.name)

    def visit_VoidType(self, node: ast.VoidType):
        pass

    def visit_ArrayType(self, node: ast.ArrayType):
        self.visit_Type(node.element_type)

    def visit_Field(self, node: ast.Field):
        self.visit_Name(node.name)
        self.visit_Type(node.type)

    def visit_Parameter(self, node: ast.Parameter):
        self.visit_Name(node.name)
        self.visit_Type(node.type)

    def visit_Method(self, node: ast.Method):
        self.visit_Name(node.name)
        self.visit_Type(node.return_type)
        for param in node.parameters:
            self.visit_Parameter(param)
        for stmt in node.body:
            self.visit_Stmt(stmt)

    def visit_ClassDeclaration(self, node: ast.ClassDeclaration):
        self.visit_Name(node.name)
        for field in node.fields:
            self.visit_Field(field)
        for method in node.methods:
            self.visit_Method(method)

    def visit_Import(self, node: ast.Import):
        self.visit_Name(node.path)

    def visit_Expr(self, node: ast.Expr):
        if isinstance(node, ast.AssignmentTarget):
            self.visit_AssignmentTarget(node)
        elif isinstance(node, ast.AssignExpr):
            self.visit_AssignExpr(node)
        elif isinstance(node, ast.NewExpr):
            self.visit_NewExpr(node)
        elif isinstance(node, ast.IntExpr):
            self.visit_IntExpr(node)
        elif isinstance(node, ast.CallExpr):
            self.visit_CallExpr(node)
        elif isinstance(node, ast.PlusExpr):
            self.visit_PlusExpr(node)
        elif isinstance(node, ast.SuperExpr):
            self.visit_SuperExpr(node)
        elif isinstance(node, ast.ThisExpr):
            self.visit_ThisExpr(node)
        else:
            raise NotImplementedError(node)

    def visit_AssignmentTarget(self, node: ast.AssignmentTarget):
        if isinstance(node, ast.IdentExpr):
            self.visit_IdentExpr(node)
        elif isinstance(node, ast.DotExpr):
            self.visit_DotExpr(node)
        elif isinstance(node, ast.IndexExpr):
            self.visit_IndexExpr(node)
        else:
            raise NotImplementedError(node)

    def visit_AssignExpr(self, node: ast.AssignExpr):
        self.visit_AssignmentTarget(node.target)
        self.visit_Expr(node.value)

    def visit_NewExpr(self, node: ast.NewExpr):
        for arg in node.arguments:
            self.visit_Expr(arg)

    def visit_SuperExpr(self, node: ast.SuperExpr):
        pass

    def visit_ThisExpr(self, node: ast.ThisExpr):
        pass

    def visit_IdentExpr(self, node: ast.IdentExpr):
        pass

    def visit_DotExpr(self, node: ast.DotExpr):
        self.visit_Expr(node.left)

    def visit_IntExpr(self, node: ast.IntExpr):
        pass

    def visit_CallExpr(self, node: ast.CallExpr):
        self.visit_Expr(node.target)
        for arg in node.arguments:
            self.visit_Expr(arg)

    def visit_PlusExpr(self, node: ast.PlusExpr):
        self.visit_Expr(node.left)
        self.visit_Expr(node.right)

    def visit_IndexExpr(self, node: ast.IndexExpr):
        self.visit_Expr(node.target)
        self.visit_Expr(node.index)

    def visit_Stmt(self, node: ast.Stmt):
        if isinstance(node, ast.ReturnStmt):
            self.visit_ReturnStmt(node)
        elif isinstance(node, ast.DeleteStmt):
            self.visit_DeleteStmt(node)
        elif isinstance(node, ast.ExprStmt):
            self.visit_ExprStmt(node)
        elif isinstance(node, ast.VarDeclaration):
            self.visit_VarDeclaration(node)
        else:
            raise NotImplementedError(node)

    def visit_ReturnStmt(self, node: ast.ReturnStmt):
        if node.expr is not None:
            self.visit_Expr(node.expr)

    def visit_DeleteStmt(self, node: ast.DeleteStmt):
        self.visit_Expr(node.expr)

    def visit_ExprStmt(self, node: ast.ExprStmt):
        self.visit_Expr(node.expr)

    def visit_VarDeclaration(self, node: ast.VarDeclaration):
        self.visit_Type(node.type)
        self.visit_Name(node.name)
        if node.initializer:
            self.visit_Expr(node.initializer)


def _validate_visitor_methods():
    missing = []
    for export in dir(ast):
        node_cls = getattr(ast, export)  # type: ignore
        if (
            node_cls is ast.Node  # type: ignore
            or not isinstance(node_cls, type)  # type: ignore
            or not issubclass(node_cls, ast.Node)
        ):
            continue
        if not hasattr(Visitor, f"visit_{export}"):
            missing.append(export)
    assert not missing, f"Missing visitor methods for: {', '.join(missing)}"


if __debug__:
    _validate_visitor_methods()
