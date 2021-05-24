import os
import os.path
import typing as t
from patina import Option, None_

from joe import ast
from joe.lexer import TokenType, lex
from joe.source import Location
from joe._utils import Peekable

# worry about circular imports later

# Start from Main class
# Read every import statement and parse each file
# Calculate ClassType and ObjectType for the class (inheritance?)


class ModulePath:
    EXT = ".java"

    def __init__(self, parts: t.List[str]):
        self._parts = parts

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
        *path, filename = self._parts
        return os.path.join(*path, f"{filename}{self.EXT}")

    def class_path(self) -> str:
        return ".".join(self._parts)

    def __iter__(self):
        return iter(self._parts)


class Parser:
    def __init__(self, filename: str, contents: str):
        self.filename = filename
        self.tokens = Peekable(lex(filename, contents))
        self.modules: t.List[ast.Module] = []

    def parse_file(self) -> t.List[ast.Module]:
        imports, modules = self._parse_imports()
        class_decl = self._parse_class_decl()

        modules.append(
            ast.Module(
                name=ModulePath.from_file_path(self.filename).class_path(),
                class_decl=class_decl,
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
            with open(import_filename, "r") as f:
                import_src = f.read()
            modules.extend(Parser(import_filename, import_src).parse_file())

        return imports, modules

    def _parse_class_decl(self) -> ast.ClassDeclaration:
        class_tok = self.tokens.next().expect(TokenType.Class)
        name_tok = self.tokens.next().expect(TokenType.Ident)

        # FIXME: extends, implements
        self.tokens.next().expect(TokenType.LBrace)

        class_name = name_tok.value
        fields, methods = self._parse_methods_and_fields(class_name)
        self.tokens.next().expect(TokenType.RBrace)

        return ast.ClassDeclaration(
            name=ast.Name(value=class_name, location=name_tok.location),
            fields=fields,
            methods=methods,
            location=class_tok.location,
        )

    def _parse_methods_and_fields(
        self, class_name: str
    ) -> t.Tuple[t.List[ast.Field], t.List[ast.Method]]:
        fields = []
        methods = []

        while self.tokens.peek().type != TokenType.RBrace:
            tok = self.tokens.peek()
            loc = tok.location
            if tok.type == TokenType.Static:
                self.tokens.next()
                tok = self.tokens.peek()
                static = True
            else:
                static = False
            if (
                not static
                and tok.type == TokenType.Ident
                and tok.value == class_name
            ):
                name = ast.Name(value=tok.value, location=tok.location)
                return_type: ast.Type = ast.VoidType(loc)
                self.tokens.next()
            else:
                return_type = self._parse_type()
                name_tok = self.tokens.next().expect(TokenType.Ident)
                name = ast.Name(
                    value=name_tok.value, location=name_tok.location
                )

            tok = self.tokens.next()
            if tok.type == TokenType.SemiColon:
                fields.append(
                    ast.Field(name=name, type=return_type, location=loc)
                )
                continue

            meth = ast.Method(
                name=name, return_type=return_type, location=loc, static=static
            )
            methods.append(meth)

            tok.expect(TokenType.LParen)
            is_first_param = True
            while self.tokens.peek().type != TokenType.RParen:
                if not is_first_param:
                    self.tokens.next().expect(TokenType.Comma)
                else:
                    is_first_param = False
                meth.parameters.append(self._parse_parameter())
            self.tokens.next().expect(TokenType.RParen)

            self.tokens.next().expect(TokenType.LBrace)
            # FIXME: method body
            while self.tokens.peek().type != TokenType.RBrace:
                self.tokens.next()
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
            self.tokens.next().expect(TokenType.RBracket)
            ty = ast.ArrayType(location=ty.location, element_type=ty)
        return ty

    def _parse_parameter(self) -> ast.Parameter:
        ty = self._parse_type()
        name = self.tokens.next().expect(TokenType.Ident)
        return ast.Parameter(
            location=ty.location,
            type=ty,
            name=ast.Name(value=name.value, location=name.location),
        )
