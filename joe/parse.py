from collections.abc import Iterable, Iterator
from enum import Enum, auto
from string import ascii_letters, digits
from typing import Final, cast

from joe._internal.itertools import Peekable
from joe.ast import (ClassDecl, ClassMember, ConstructorDecl, FieldDecl,
                     Identifier, InterfaceDecl, InterfaceMember, Location,
                     MethodDecl, MethodSig, Node, Parameter, Statement, Type,
                     TypeParameter, valid_class_members,
                     valid_interface_members)


class JoeParseError(Exception):
    pass


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
                f"Unexpected token {tok.text!r}; expected {cast(str, type_.name)} at {tok.location}"  # noqa E501
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


def _parse_identifier(tokens: _Tokens) -> Identifier:
    tok = tokens.expect(TokenType.IDENTIFIER)
    return Identifier(tok.location, tok.text)


def parse(tokens: Iterable[Token]) -> Iterator[Node]:
    tokens = _Tokens(tokens)

    while True:
        try:
            tok = next(tokens)
        except StopIteration:
            break

        if tok.type is TokenType.INTERFACE:
            yield _parse_interface_decl(tok.location, tokens)
        elif tok.type is TokenType.CLASS:
            yield _parse_class_decl(tok.location, tokens)
        else:
            raise NotImplementedError()


def _parse_interface_decl(location: Location, tokens: _Tokens) -> InterfaceDecl:
    name, type_parameters = _parse_type_decl(tokens)
    parents = [_parse_type(tokens)] if tokens.match(TokenType.EXTENDS) else []
    tokens.expect(TokenType.LEFT_BRACE)
    members = []
    while not tokens.match(TokenType.RIGHT_BRACE):
        members.append(_parse_interface_member(tokens))
    return InterfaceDecl(location, name, type_parameters, parents, members)


def _parse_class_decl(location: Location, tokens: _Tokens) -> ClassDecl:
    name, type_parameters = _parse_type_decl(tokens)
    implements = []
    if implements_tok := tokens.match(TokenType.IMPLEMENTS):
        for _ in _parse_list(tokens, TokenType.LEFT_BRACE):
            implements.append(_parse_type(tokens))
        if not implements:
            raise JoeParseError(
                f"Unexpected empty interface list at {implements_tok.location}"
            )
    else:
        tokens.expect(TokenType.LEFT_BRACE)
    members = []
    while not tokens.match(TokenType.RIGHT_BRACE):
        members.append(_parse_class_member(tokens))
    return ClassDecl(location, name, type_parameters, implements, members)


def _parse_type_decl(tokens: _Tokens) -> tuple[Identifier, list[TypeParameter]]:
    name = _parse_identifier(tokens)
    if tokens.match(TokenType.LEFT_ANGLE_BRACKET, consume=False):
        type_param_list = _parse_type_param_list(tokens)
    else:
        type_param_list = []
    return name, type_param_list


def _parse_type_param_list(tokens: _Tokens) -> list[TypeParameter]:
    open_tok = tokens.expect(TokenType.LEFT_ANGLE_BRACKET)
    params = []
    for _ in _parse_list(tokens, TokenType.RIGHT_ANGLE_BRACKET):
        params.append(_parse_generic_param(tokens))
    if not params:
        raise JoeParseError(
            f"Type parameter list must not be empty at {open_tok.location}"
        )
    return params


def _parse_generic_param(tokens: _Tokens) -> TypeParameter:
    arg_name = _parse_identifier(tokens)
    if tokens.match(TokenType.COLON):
        constraint = _parse_type(tokens)
    else:
        constraint = None
    return TypeParameter(arg_name.location, arg_name, constraint)


def _parse_type(tokens: _Tokens) -> Type:
    name = _parse_identifier(tokens)
    args = []
    if tokens.match(TokenType.LEFT_ANGLE_BRACKET):
        for _ in _parse_list(tokens, TokenType.RIGHT_ANGLE_BRACKET):
            args.append(_parse_type(tokens))
    return Type(name.location, name, args)


def _parse_interface_member(tokens: _Tokens) -> InterfaceMember:
    member = _parse_member(tokens)
    if not isinstance(member, valid_interface_members):
        raise JoeParseError(
            f"Unexpected {type(member).__name__} in interface at {member.location}"
        )
    return member


def _parse_class_member(tokens: _Tokens) -> ClassMember:
    member = _parse_member(tokens)
    if not isinstance(member, valid_class_members):
        raise JoeParseError(
            f"Unexpected {type(member).__name__} in class at {member.location}"
        )
    return member


def _parse_member(
    tokens: _Tokens,
) -> MethodSig | MethodDecl | ConstructorDecl | FieldDecl:
    type_ = _parse_type(tokens)
    if tokens.match(TokenType.LEFT_PAREN, consume=False):
        if type_.type_arguments:
            raise JoeParseError(f"Constructor cannot be generic at {type_.location}")
        name = type_.name
        type_ = Type(name.location, Identifier(name.location, "void"), [])
        is_constructor = True
    else:
        name = _parse_identifier(tokens)
        is_constructor = False

    if (
        is_constructor
        or tokens.match(TokenType.LEFT_PAREN, consume=False)
        or tokens.match(TokenType.LEFT_ANGLE_BRACKET, consume=False)
    ):
        if tokens.match(TokenType.LEFT_ANGLE_BRACKET, consume=False):
            assert not is_constructor
            type_param_list = _parse_type_param_list(tokens)
        else:
            type_param_list = []
        params = _parse_param_list(tokens)
        if tokens.match(TokenType.SEMICOLON):
            if is_constructor:
                raise JoeParseError(f"Constructor must have a body at {name.location}")
            return MethodSig(type_.location, type_, name, type_param_list, params)
        body = _parse_method_body(tokens)
        if is_constructor:
            return ConstructorDecl(name.location, name, params, body)
        else:
            return MethodDecl(
                type_.location, type_, name, type_param_list, params, body
            )
    else:
        tokens.expect(TokenType.SEMICOLON)
        return FieldDecl(type_.location, type_, name)


def _parse_param_list(tokens: _Tokens) -> list[Parameter]:
    tokens.expect(TokenType.LEFT_PAREN)
    params = []
    for _ in _parse_list(tokens, TokenType.RIGHT_PAREN):
        params.append(_parse_param(tokens))
    return params


def _parse_param(tokens: _Tokens) -> Parameter:
    type_ = _parse_type(tokens)
    name = _parse_identifier(tokens)
    return Parameter(type_.location, type_, name)


def _parse_method_body(tokens: _Tokens) -> list[Statement]:
    tokens.expect(TokenType.LEFT_BRACE)
    tokens.expect(TokenType.RIGHT_BRACE)
    return []
