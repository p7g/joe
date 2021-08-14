import os
import os.path
import typing as t
from patina import Option, None_

from joe import ast
from joe.lexer import Token, TokenType, lex
from joe.objects import path_modname
from joe.source import Location, JoeSyntaxError
from joe._utils import Peekable


class ModulePath(t.List[str]):
    EXT = ".java"

    def __init__(self, parts: t.List[str]):
        super().__init__(parts)

    @classmethod
    def from_file_path(cls, path: str) -> "ModulePath":
        *path_parts, filename = os.path.normpath(path).split(os.sep)
        filename, ext = os.path.splitext(filename)
        if ext != cls.EXT:
            raise ValueError(path)
        return cls([*path_parts, filename])

    @classmethod
    def from_class_path(cls, path: str) -> "ModulePath":
        return cls(path.split("."))

    def file_path(self) -> str:
        *path, filename, cls_name = self
        return os.path.join(*path, f"{filename}{self.EXT}")

    def dotted_path(self) -> str:
        return ".".join(self)


class _ParserSnapshot:
    def __init__(self, tokens: Peekable[Token]) -> None:
        self.tokens = tokens


class Parser:
    def __init__(
        self, filename: str, contents: str, seen_files: t.Set[str] = None
    ):
        self.filename: t.Final = filename
        self.tokens = Peekable(lex(filename, contents))
        self.seen_files = seen_files or set()
        self.modules: t.List[ast.Module] = []

    def _take_snapshot(self) -> _ParserSnapshot:
        # Don't mutate modules or its contents in between taking and applying a
        # snapshot
        return _ParserSnapshot(self.tokens.copy())

    def _apply_snapshot(self, snapshot: _ParserSnapshot) -> None:
        self.tokens = snapshot.tokens

    def parse_file(self) -> t.List[ast.Module]:
        imports, modules = self._parse_imports()

        class_decls = []
        while True:
            try:
                self.tokens.peek()
            except StopIteration:
                break

            final = False
            if self.tokens.peek().type == TokenType.Final:
                self.tokens.next()
                final = True
            class_decls.append(self._parse_class_decl(final=final))

        modules.append(
            ast.Module(
                name=path_modname(
                    ModulePath.from_file_path(self.filename).dotted_path()
                ) or "",
                class_decls=class_decls,
                imports=imports,
            )
        )
        return modules

    def _parse_imports(self) -> t.Tuple[t.List[ast.Import], t.List[ast.Module]]:
        imports = []
        modules = []

        # FIXME: raise error on self-import instead of stack overflow
        while self.tokens.peek().type == TokenType.Import:
            import_tok = self.tokens.next().expect(TokenType.Import)
            path_start: Option[Location] = None_()
            path = ""
            while True:
                name = self.tokens.next().expect(TokenType.Ident)
                path += name.value
                if path_start.is_none():
                    path_start.replace(name.location)
                if self.tokens.peek().type == TokenType.SemiColon:
                    break
                path += self.tokens.next().expect(TokenType.Dot).value
            self.tokens.next().expect(TokenType.SemiColon)
            imports.append(
                ast.Import(
                    location=import_tok.location,
                    path=ast.Name(location=path_start.unwrap(), value=path),
                )
            )
            import_filename = ModulePath.from_class_path(path).file_path()
            if import_filename in self.seen_files:
                continue
            self.seen_files.add(import_filename)
            with open(import_filename, "r") as f:
                import_src = f.read()
            modules.extend(
                Parser(
                    import_filename, import_src, self.seen_files
                ).parse_file()
            )

        return imports, modules

    def _parse_class_decl(self, *, final: bool) -> ast.ClassDeclaration:
        class_tok = self.tokens.next().expect(TokenType.Class)
        name_tok = self.tokens.next().expect(TokenType.Ident)

        superclass = None
        if self.tokens.peek().type == TokenType.Extends:
            self.tokens.next()
            superclass = self._parse_type()

        # FIXME: implements
        self.tokens.next().expect(TokenType.LBrace)

        class_name = name_tok.value
        fields, methods = self._parse_methods_and_fields(class_name)
        self.tokens.next().expect(TokenType.RBrace)

        fully_qualified_name = ModulePath.from_file_path(self.filename)
        fully_qualified_name.append(class_name)

        return ast.ClassDeclaration(
            name=ast.Name(
                value=fully_qualified_name.dotted_path(),
                location=name_tok.location,
            ),
            superclass=superclass,
            fields=fields,
            methods=methods,
            location=class_tok.location,
            final=final,
        )

    def _parse_methods_and_fields(
        self, class_name: str
    ) -> t.Tuple[t.List[ast.Field], t.List[ast.Method]]:
        fields = []
        methods = []

        while self.tokens.peek().type != TokenType.RBrace:
            tok = self.tokens.peek()
            loc = tok.location

            static = False
            final = False
            if tok.type == TokenType.Static:
                self.tokens.next()
                tok = self.tokens.peek()
                static = True
            elif tok.type == TokenType.Final:
                self.tokens.next()
                tok = self.tokens.peek()
                final = True

            return_type = self._parse_type()
            name_tok = self.tokens.next().expect(TokenType.Ident)
            name = ast.Name(value=name_tok.value, location=name_tok.location)

            tok = self.tokens.next()
            if tok.type == TokenType.SemiColon:
                fields.append(
                    ast.Field(
                        name=name, type=return_type, location=loc, final=final
                    )
                )
                continue

            meth = ast.Method(
                name=name,
                return_type=return_type,
                location=loc,
                static=static,
                final=final,
            )
            methods.append(meth)

            tok.expect(TokenType.LParen)
            while self.tokens.peek().type != TokenType.RParen:
                meth.parameters.append(self._parse_parameter())
                if self.tokens.peek().type != TokenType.RParen:
                    self.tokens.next().expect(TokenType.Comma)
            self.tokens.next().expect(TokenType.RParen)

            self.tokens.next().expect(TokenType.LBrace)
            while self.tokens.peek().type != TokenType.RBrace:
                meth.body.append(self._parse_statement())
            self.tokens.next().expect(TokenType.RBrace)

        return fields, methods

    def _parse_type(self) -> ast.Type:
        tok = self.tokens.next()
        if tok.type == TokenType.Void:
            return ast.VoidType(location=tok.location)

        tok.expect(TokenType.Ident)
        ty: ast.Type = ast.NamedType(
            location=tok.location,
            name=ast.Name(value=tok.value, location=tok.location),
        )
        while self.tokens.peek().type == TokenType.LBracket:
            self.tokens.next()
            length = None
            if self.tokens.peek().type != TokenType.RBracket:
                length = self._parse_expr()
            self.tokens.next().expect(TokenType.RBracket)
            ty = ast.ArrayType(
                location=ty.location, element_type=ty, length=length
            )
        return ty

    def _parse_parameter(self) -> ast.Parameter:
        ty = self._parse_type()
        name = self.tokens.next().expect(TokenType.Ident)
        return ast.Parameter(
            location=ty.location,
            type=ty,
            name=ast.Name(value=name.value, location=name.location),
        )

    def _parse_statement(self) -> ast.Stmt:
        tok = self.tokens.peek()
        if tok.type == TokenType.Return:
            tok = self.tokens.next()
            expr = None
            if self.tokens.peek().type != TokenType.SemiColon:
                expr = self._parse_expr()
            self.tokens.next().expect(TokenType.SemiColon)
            return ast.ReturnStmt(location=tok.location, expr=expr)
        elif tok.type == TokenType.Delete:
            tok = self.tokens.next()
            expr = self._parse_expr()
            self.tokens.next().expect(TokenType.SemiColon)
            return ast.DeleteStmt(location=tok.location, expr=expr)
        else:
            return self._parse_var_decl_or_expr_stmt()

    def _parse_var_decl_or_expr_stmt(self) -> ast.Stmt:
        snapshot = self._take_snapshot()
        try:
            expr = self._parse_expr()
            self.tokens.next().expect(TokenType.SemiColon)
            return ast.ExprStmt(location=expr.location, expr=expr)
        except JoeSyntaxError:
            self._apply_snapshot(snapshot)
            ty = self._parse_type()
            name_tok = self.tokens.next().expect(TokenType.Ident)
            init = None
            if self.tokens.peek().type == TokenType.Eq:
                self.tokens.next()
                init = self._parse_expr()
            self.tokens.next().expect(TokenType.SemiColon)
            return ast.VarDeclaration(
                location=ty.location,
                name=ast.Name(name_tok.location, name_tok.value),
                type=ty,
                initializer=init,
            )

    def _parse_expr(self) -> ast.Expr:
        left = self._parse_atom()
        while self.tokens.peek().type != TokenType.SemiColon:
            tok = self.tokens.peek()
            if tok.type == TokenType.Eq:
                self.tokens.next()
                # assignment
                if not isinstance(left, ast.AssignmentTarget):
                    raise JoeSyntaxError(tok.location, "Unexpected equal")
                left = ast.AssignExpr(
                    location=left.location,
                    target=left,
                    value=self._parse_expr(),
                )
            elif tok.type == TokenType.LParen:
                # Call
                self.tokens.next()
                args = []
                while self.tokens.peek().type != TokenType.RParen:
                    args.append(self._parse_expr())
                    if self.tokens.peek().type != TokenType.RParen:
                        self.tokens.next().expect(TokenType.Comma)
                self.tokens.next().expect(TokenType.RParen)
                left = ast.CallExpr(
                    location=left.location, target=left, arguments=args
                )
            elif tok.type == TokenType.Plus:
                self.tokens.next()
                left = ast.PlusExpr(left.location, left, self._parse_expr())
            elif tok.type == TokenType.Dot:
                self.tokens.next()
                left = ast.DotExpr(
                    location=left.location,
                    left=left,
                    name=self.tokens.next().expect(TokenType.Ident).value,
                )
            elif tok.type == TokenType.LBracket:
                self.tokens.next()
                index = self._parse_expr()
                self.tokens.next().expect(TokenType.RBracket)
                left = ast.IndexExpr(left.location, left, index)
            else:
                break
        return left

    def _parse_atom(self) -> ast.Expr:
        tok = self.tokens.next()
        if tok.type == TokenType.Int:
            return ast.IntExpr(location=tok.location, value=int(tok.value))
        elif tok.type == TokenType.Ident:
            return ast.IdentExpr(location=tok.location, name=tok.value)
        elif tok.type == TokenType.New:
            new_tok = tok
            ty = self._parse_type()
            args = []
            if not isinstance(ty, ast.ArrayType):
                self.tokens.next().expect(TokenType.LParen)
                while self.tokens.peek().type != TokenType.RParen:
                    args.append(self._parse_expr())
                    if self.tokens.peek().type != TokenType.RParen:
                        self.tokens.next().expect(TokenType.Comma)
                self.tokens.next().expect(TokenType.RParen)
            return ast.NewExpr(
                location=new_tok.location, type=ty, arguments=args
            )
        elif tok.type == TokenType.Super:
            return ast.SuperExpr(location=tok.location)
        elif tok.type == TokenType.This:
            return ast.ThisExpr(location=tok.location)
        elif tok.type in (TokenType.True_, TokenType.False_):
            return ast.BoolExpr(tok.location, tok.type == TokenType.True_)
        elif tok.type == TokenType.Null:
            return ast.NullExpr(tok.location)
        elif tok.type == TokenType.LParen:
            expr = self._parse_expr()
            self.tokens.next().expect(TokenType.RParen)
            return expr
        elif tok.type == TokenType.Char:
            return ast.CharExpr(tok.location, tok.value[1])
        else:
            raise JoeSyntaxError(
                tok.location, f"Unexpected token {tok.type.value}"
            )
