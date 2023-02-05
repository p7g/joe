from collections.abc import Iterable, Iterator
from enum import Enum, auto
from string import ascii_letters, digits
from typing import Final, cast

from joe._internal.itertools import Peekable


class JoeParseError(Exception):
    pass


class Location:
    __slots__ = ("filename", "line", "column")

    filename: str
    line: int
    column: int

    def __init__(self, filename: str, line: int, column: int) -> None:
        self.filename = filename
        self.line = line
        self.column = column

    def __str__(self) -> str:
        return f"{self.filename}:{self.line}:{self.column}:"


class TokenType(Enum):
    CLASS = auto()
    COLON = auto()
    COMMA = auto()
    EXTENDS = auto()
    IDENTIFIER = auto()
    IMPLEMENTS = auto()
    INTERFACE = auto()
    LEFT_ANGLE_BRACKET = auto()
    LEFT_BRACE = auto()
    LEFT_PAREN = auto()
    RIGHT_ANGLE_BRACKET = auto()
    RIGHT_BRACE = auto()
    RIGHT_PAREN = auto()
    SEMICOLON = auto()


class Token:
    __slots__ = ("type", "location", "text")

    type: TokenType
    location: Location
    text: str

    def __init__(self, type_: TokenType, location: Location, text: str) -> None:
        self.type = type_
        self.location = location
        self.text = text


_one_char_tokens: Final = {
    "(": TokenType.LEFT_PAREN,
    ")": TokenType.RIGHT_PAREN,
    ",": TokenType.COMMA,
    ":": TokenType.COLON,
    ";": TokenType.SEMICOLON,
    "<": TokenType.LEFT_ANGLE_BRACKET,
    ">": TokenType.RIGHT_ANGLE_BRACKET,
    "{": TokenType.LEFT_BRACE,
    "}": TokenType.RIGHT_BRACE,
}

_keywords: Final = {
    "class": TokenType.CLASS,
    "extends": TokenType.EXTENDS,
    "implements": TokenType.IMPLEMENTS,
    "interface": TokenType.INTERFACE,
}


def _is_identifier_start(c: str) -> bool:
    return c == "_" or c in ascii_letters


def _is_identifier(c: str) -> bool:
    return _is_identifier_start(c) or c in digits


def scan(filename: str, chars: Iterable[str]) -> Iterator[Token]:
    chars = Peekable(chars)
    line = 1
    column = 0

    while True:
        try:
            c = next(chars)
        except StopIteration:
            return

        if c.isspace():
            continue

        if c == "\n":
            column = 0
            line += 1
        else:
            column += 1

        location = Location(filename, line, column)

        if c in _one_char_tokens:
            yield Token(_one_char_tokens[c], location, c)
        elif _is_identifier_start(c):
            ident = c
            while True:
                c2 = chars.peek()
                if c2 is None or not _is_identifier(c2):
                    break
                ident += next(chars)
            yield Token(_keywords.get(ident, TokenType.IDENTIFIER), location, ident)
        else:
            raise JoeParseError(f"Unexpected token {c!r} at {location}")


class NodeType(Enum):
    CLASS = auto()
    CONSTRUCTOR_DECL = auto()
    FIELD_DECL = auto()
    GENERIC_PARAM = auto()
    IDENTIFIER = auto()
    IMPLEMENTS_LIST = auto()
    INTERFACE = auto()
    METHOD_BODY = auto()
    METHOD_DECL = auto()
    METHOD_SIG = auto()
    PARAM = auto()
    PARAM_LIST = auto()
    TYPE = auto()
    TYPE_DECL = auto()


class Node:
    __slots__ = ("type", "location", "children", "text")

    type: NodeType
    location: Location
    children: tuple["Node | None", ...]
    text: str

    def __init__(
        self,
        type_: NodeType,
        location: Location,
        children: Iterable["Node | None"],
        text: str = "",
    ) -> None:
        self.type = type_
        self.location = location
        self.children = tuple(children)
        self.text = text

    def __repr__(self) -> str:
        strs = [cast(str, self.type.name), "("]
        if self.text:
            strs.append(repr(self.text))
        else:
            first = True
            for child in self.children:
                if first:
                    first = False
                else:
                    strs.append(", ")
                strs.append(repr(child))
        strs.append(")")
        return "".join(strs)


class _Tokens(Iterator[Token]):
    __slots__ = ("tokens",)

    tokens: Peekable[Token]

    def __init__(self, tokens: Iterable[Token]) -> None:
        self.tokens = Peekable(tokens)

    def __next__(self) -> Token:
        return next(self.tokens)

    def expect(self, type_: TokenType) -> Token:
        try:
            tok = next(self)
        except StopIteration as exc:
            raise JoeParseError(
                f"Unexpected end of input; expected {cast(str, type_.name)}"
            ) from exc

        if tok.type is not type_:
            raise JoeParseError(
                f"Unexpected token {tok.text!r}; expected {cast(str, type_.name)} at {tok.location}"
            )
        return tok

    def match(self, type_: TokenType, *, consume: bool = True) -> Token | None:
        tok = self.tokens.peek()
        if tok is None or tok.type is not type_:
            return None
        return next(self) if consume else tok


def _parse_list(tokens: _Tokens, end: TokenType) -> Iterator[None]:
    first = True
    while not tokens.match(end):
        if first:
            first = False
        else:
            tokens.expect(TokenType.COMMA)
        yield


def _parse_identifier(tokens: _Tokens) -> Node:
    tok = tokens.expect(TokenType.IDENTIFIER)
    return Node(NodeType.IDENTIFIER, tok.location, (), tok.text)


def parse(tokens: Iterable[Token]) -> Iterator[Node]:
    tokens = _Tokens(tokens)

    while True:
        try:
            tok = next(tokens)
        except StopIteration:
            break

        if tok.type is TokenType.INTERFACE:
            type_decl = _parse_type_decl(tokens)
            parent = _parse_type(tokens) if tokens.match(TokenType.EXTENDS) else None
            tokens.expect(TokenType.LEFT_BRACE)
            members = []
            while not tokens.match(TokenType.RIGHT_BRACE):
                members.append(_parse_interface_member(tokens))
            yield Node(NodeType.INTERFACE, tok.location, [type_decl, parent, *members])
        elif tok.type is TokenType.CLASS:
            type_decl = _parse_type_decl(tokens)
            implements = None
            if implements_tok := tokens.match(TokenType.IMPLEMENTS):
                interfaces = []
                for _ in _parse_list(tokens, TokenType.LEFT_BRACE):
                    interfaces.append(_parse_type(tokens))
                if not interfaces:
                    raise JoeParseError(
                        f"Unexpected empty interface list at {implements_tok.location}"
                    )
                implements = Node(
                    NodeType.IMPLEMENTS_LIST, interfaces[0].location, interfaces
                )
            else:
                tokens.expect(TokenType.LEFT_BRACE)
            members = []
            while not tokens.match(TokenType.RIGHT_BRACE):
                members.append(_parse_class_member(tokens))
            yield Node(NodeType.CLASS, tok.location, [type_decl, implements, *members])
        else:
            raise NotImplementedError()


def _parse_type_decl(tokens: _Tokens) -> Node:
    name = _parse_identifier(tokens)
    args = []
    if tokens.match(TokenType.LEFT_ANGLE_BRACKET):
        for _ in _parse_list(tokens, TokenType.RIGHT_ANGLE_BRACKET):
            args.append(_parse_generic_param(tokens))
    return Node(NodeType.TYPE_DECL, name.location, [name, *args])


def _parse_generic_param(tokens: _Tokens) -> Node:
    arg_name = _parse_identifier(tokens)
    if tokens.match(TokenType.COLON):
        constraint = _parse_type(tokens)
    else:
        constraint = None
    return Node(NodeType.GENERIC_PARAM, arg_name.location, [arg_name, constraint])


def _parse_type(tokens: _Tokens) -> Node:
    name = _parse_identifier(tokens)
    args = []
    if tokens.match(TokenType.LEFT_ANGLE_BRACKET):
        for _ in _parse_list(tokens, TokenType.RIGHT_ANGLE_BRACKET):
            args.append(_parse_type(tokens))
    return Node(NodeType.TYPE, name.location, [name, *args])


def _parse_interface_member(tokens: _Tokens) -> Node:
    member = _parse_class_member(tokens)
    if member.type not in (NodeType.METHOD_DECL, NodeType.METHOD_SIG):
        raise JoeParseError(
            f"Unexpected {cast(str, member.type.name)} in interface at {member.location}"
        )
    return member


def _parse_class_member(tokens: _Tokens) -> Node:
    type_: Node | None = _parse_type(tokens)
    if tokens.match(TokenType.LEFT_PAREN, consume=False):
        assert type_ is not None
        if len(type_.children) > 1:
            raise JoeParseError(f"Constructor cannot be generic at {type_.location}")
        name = type_.children[0]
        assert name is not None
        type_ = None
        is_constructor = True
    else:
        name = _parse_identifier(tokens)
        is_constructor = False

    if is_constructor or tokens.match(TokenType.LEFT_PAREN, consume=False):
        params = _parse_param_list(tokens)
        if tokens.match(TokenType.SEMICOLON):
            if is_constructor:
                raise JoeParseError(f"Constructor must have a body at {name.location}")
            assert type_ is not None
            return Node(NodeType.METHOD_SIG, type_.location, [type_, name, params])
        body = _parse_method_body(tokens)
        if is_constructor:
            return Node(NodeType.CONSTRUCTOR_DECL, name.location, [name, params, body])
        else:
            assert type_ is not None
            return Node(
                NodeType.METHOD_DECL, type_.location, [type_, name, params, body]
            )
    else:
        assert type_ is not None
        tokens.expect(TokenType.SEMICOLON)
        return Node(NodeType.FIELD_DECL, type_.location, [type_, name])


def _parse_param_list(tokens: _Tokens) -> Node:
    left_paren = tokens.expect(TokenType.LEFT_PAREN)
    args = []
    for _ in _parse_list(tokens, TokenType.RIGHT_PAREN):
        args.append(_parse_param(tokens))
    return Node(NodeType.PARAM_LIST, left_paren.location, args)


def _parse_param(tokens: _Tokens) -> Node:
    type_ = _parse_type(tokens)
    name = _parse_identifier(tokens)
    return Node(NodeType.PARAM, type_.location, [type_, name])


def _parse_method_body(tokens: _Tokens) -> Node:
    open_brace = tokens.expect(TokenType.LEFT_BRACE)
    tokens.expect(TokenType.RIGHT_BRACE)
    return Node(NodeType.METHOD_BODY, open_brace.location, [])
