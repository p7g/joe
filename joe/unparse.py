from contextlib import contextmanager
from collections.abc import Iterable, Iterator, Sequence

from joe import ast


def unparse(node: ast.Node):
    visitor = _Unparser()
    node.accept(visitor)
    return visitor.get_result()


class _Unparser(ast.AstVisitor):
    def __init__(self, indent_chars: str = "    ") -> None:
        self._strings = []
        self._indent = 0
        self._indent_chars = indent_chars

    @contextmanager
    def indent(self) -> Iterator[None]:
        self._indent += 1
        yield
        self._indent -= 1

    def get_result(self) -> str:
        return "".join(self._strings)

    def _append(self, string: str) -> None:
        self._strings.append(string)

    def _indentation(self) -> str:
        return "    " * self._indent

    def _list(
        self, nodes: Iterable[ast.Node], *, sep: str = ", ", delims: Sequence[str] = ""
    ) -> None:
        if delims:
            self._append(delims[0])
        first = True
        for node in nodes:
            if first:
                first = False
            else:
                self._append(sep)
            node.accept(self)
        if delims:
            self._append(delims[1])

    def visit_identifier(self, identifier: ast.Identifier) -> None:
        self._append(identifier.name)

    def visit_type_parameter(self, type_parameter: ast.TypeParameter) -> None:
        type_parameter.name.accept(self)
        if type_parameter.constraint:
            self._append(": ")
            type_parameter.constraint.accept(self)

    def visit_parameter(self, parameter: ast.Parameter) -> None:
        if parameter.type:
            parameter.type.accept(self)
            self._append(" ")
        parameter.name.accept(self)

    def visit_method_sig(self, method_sig: ast.MethodSig) -> None:
        self._append(self._indentation())
        method_sig.return_type.accept(self)
        self._append(" ")
        method_sig.name.accept(self)
        if method_sig.type_parameters:
            self._list(method_sig.type_parameters, delims="<>")
        self._list(method_sig.parameters, delims="()")

    def visit_import_decl(self, import_decl: ast.ImportDecl) -> None:
        self._append("from ")
        self._append(".".join(import_decl.module_path))
        self._append(" ")
        self._append("import ")
        if import_decl.imports == "*":
            self._append("*")
        else:
            assert isinstance(import_decl.imports, tuple)
            self._list(import_decl.imports)

    def visit_method_decl(self, method_decl: ast.MethodDecl) -> None:
        self._append(self._indentation())
        method_decl.return_type.accept(self)
        self._append(" ")
        method_decl.name.accept(self)
        if method_decl.type_parameters:
            self._list(method_decl.type_parameters, delims="<>")
        self._list(method_decl.parameters, delims="()")
        self._append(" {")
        if method_decl.body:
            self._append("\n")
            with self.indent():
                self._list(method_decl.body, sep="\n\n")
            self._append("\n")
            self._append(self._indentation())
        self._append("}")

    def visit_constructor_decl(self, constructor_decl: ast.ConstructorDecl) -> None:
        self._append(self._indentation())
        constructor_decl.name.accept(self)
        self._list(constructor_decl.parameters, delims="()")
        self._append(" {")
        if constructor_decl.body:
            self._append("\n")
            with self.indent():
                self._list(constructor_decl.body, sep="\n\n")
            self._append("\n")
            self._append(self._indentation())
        self._append("}")

    def visit_field_decl(self, field_decl: ast.FieldDecl) -> None:
        self._append(self._indentation())
        field_decl.type.accept(self)
        self._append(" ")
        field_decl.name.accept(self)
        self._append(";")

    def visit_class_decl(self, class_decl: ast.ClassDecl) -> None:
        self._append(self._indentation())
        self._append("class ")
        class_decl.name.accept(self)
        if class_decl.type_parameters:
            self._list(class_decl.type_parameters, delims="<>")
        if class_decl.implements:
            self._append(" implements ")
            self._list(class_decl.implements)
        self._append(" {")
        if class_decl.members:
            self._append("\n")
            with self.indent():
                self._list(class_decl.members, sep="\n\n")
            self._append("\n")
            self._append(self._indentation())
        self._append("}")

    def visit_class_member(self, class_member: ast.ClassMember) -> None:
        class_member.accept(self)

    def visit_interface_member(self, interface_member: ast.InterfaceMember) -> None:
        interface_member.accept(self)

    def visit_interface_decl(self, interface_decl: ast.InterfaceDecl) -> None:
        self._append(self._indentation())
        self._append("interface ")
        interface_decl.name.accept(self)
        if interface_decl.type_parameters:
            self._list(interface_decl.type_parameters, delims="<>")
        if interface_decl.extends:
            self._append(" extends ")
            self._list(interface_decl.extends)
        self._append(" {")
        if interface_decl.members:
            self._append("\n")
            with self.indent():
                self._list(interface_decl.members, sep="\n\n")
            self._append("\n")
            self._append(self._indentation())
        self._append("}")

    def visit_expression(self, expression: ast.Expr) -> None:
        expression.accept(self)

    def visit_literal_int(self, literal_int: ast.LiteralInt) -> None:
        self._append(str(literal_int.value))

    def visit_literal_float(self, literal_float: ast.LiteralFloat) -> None:
        self._append(str(literal_float.value))

    def visit_literal_string(self, literal_string: ast.LiteralString) -> None:
        # Close enough
        self._append(repr(literal_string.value))

    def visit_literal_bool(self, literal_bool: ast.LiteralBool) -> None:
        self._append("true" if literal_bool.value else "false")

    def visit_identifier_expr(self, identifier_expr: ast.IdentifierExpr) -> None:
        identifier_expr.name.accept(self)

    def visit_this_expr(self, this_expr: ast.ThisExpr) -> None:
        self._append("this")

    def visit_dot_expr(self, dot_expr: ast.DotExpr) -> None:
        dot_expr.expr.accept(self)
        self._append(".")
        dot_expr.name.accept(self)

    def visit_call_expr(self, call_expr: ast.CallExpr) -> None:
        if call_expr.expr:
            call_expr.expr.accept(self)
            self._append(".")
        call_expr.name.accept(self)
        if call_expr.type_arguments:
            self._list(call_expr.type_arguments, delims="<>")
        self._list(call_expr.arguments, delims="()")

    def visit_new_expr(self, new_expr: ast.NewExpr) -> None:
        self._append("new ")
        new_expr.type.accept(self)
        self._list(new_expr.arguments, delims="()")

    def visit_unary_expr(self, unary_expr: ast.UnaryExpr) -> None:
        self._append(unary_expr.operator.value)
        unary_expr.expr.accept(self)

    def visit_binary_expr(self, binary_expr: ast.BinaryExpr) -> None:
        binary_expr.left.accept(self)
        self._append(" ")
        self._append(binary_expr.operator.value)
        self._append(" ")
        binary_expr.right.accept(self)

    def visit_new_array_expr(self, new_array_expr: ast.NewArrayExpr) -> None:
        self._append("new ")
        new_array_expr.type.accept(self)
        self._append("[")
        new_array_expr.size.accept(self)
        self._append("]")

    def visit_index_expr(self, array_access_expr: ast.IndexExpr) -> None:
        array_access_expr.expr.accept(self)
        self._append("[")
        array_access_expr.index.accept(self)
        self._append("]")

    def visit_statement(self, statement: ast.Statement) -> None:
        self._append(self._indentation())
        statement.accept(self)

    def visit_expr_statement(self, expr_statement: ast.ExprStatement) -> None:
        expr_statement.expr.accept(self)
        self._append(";")

    def visit_return_statement(self, return_statement: ast.ReturnStatement) -> None:
        self._append("return")
        if return_statement.expr is not None:
            self._append(" ")
            return_statement.expr.accept(self)
        self._append(";")

    def _unparse_block(self, block: Sequence[ast.Statement]) -> None:
        if not block:
            self._append("{}")
        elif len(block) == 1:
            block[0].accept(self)
        else:
            self._append("{\n")
            with self.indent():
                self._list(block, sep="\n")
            self._append("\n")
            self._append(self._indentation())
            self._append("}")

    def visit_if_statement(self, if_statement: ast.IfStatement) -> None:
        self._append("if (")
        if_statement.condition.accept(self)
        self._append(") ")
        self._unparse_block(if_statement.then)
        if if_statement.else_:
            if len(if_statement.then) > 1:
                self._append(" else ")
            else:
                self._append("\n")
                self._append(self._indentation())
                self._append("else ")
                self._unparse_block(if_statement.else_)

    def visit_while_statement(self, while_statement: ast.WhileStatement) -> None:
        self._append("while (")
        while_statement.condition.accept(self)
        self._append(") ")
        self._unparse_block(while_statement.body)

    def visit_for_statement(self, for_statement: ast.ForStatement) -> None:
        self._append("for (")
        if for_statement.init:
            for_statement.init.accept(self)
        else:
            self._append(";")
        if for_statement.condition:
            self._append(" ")
            for_statement.condition.accept(self)
        self._append(";")
        if for_statement.update:
            self._append(" ")
            for_statement.update.accept(self)
        self._append(") ")
        self._unparse_block(for_statement.body)

    def visit_variable_decl_statement(
        self, variable_decl_statement: ast.VariableDeclStatement
    ) -> None:
        if variable_decl_statement.type:
            variable_decl_statement.type.accept(self)
            self._append(" ")
        else:
            self._append("var ")
        variable_decl_statement.name.accept(self)
        if variable_decl_statement.expr:
            self._append(" = ")
            variable_decl_statement.expr.accept(self)
        self._append(";")

    def visit_delete_statement(self, delete_statement: ast.DeleteStatement) -> None:
        self._append("delete ")
        delete_statement.expr.accept(self)
        self._append(";")
