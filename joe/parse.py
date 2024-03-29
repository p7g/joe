from collections.abc import Iterable, Iterator
from contextlib import contextmanager
from enum import Enum, auto
from itertools import tee
from string import ascii_letters, digits
from typing import Final

from joe._internal.exceptions import unreachable
from joe._internal.itertools import Peekable
from joe.ast import (
    BinaryExpr,
    BinaryOperator,
    CallExpr,
    ClassDecl,
    ClassMember,
    ConstructorDecl,
    DeleteStatement,
    DestructorDecl,
    DotExpr,
    Expr,
    ExprStatement,
    FieldDecl,
    ForStatement,
    Identifier,
    IdentifierExpr,
    IfStatement,
    IndexExpr,
    InterfaceDecl,
    InterfaceMember,
    LiteralBool,
    LiteralChar,
    LiteralFloat,
    LiteralInt,
    LiteralString,
    Location,
    MethodDecl,
    MethodSig,
    NewArrayExpr,
    NewExpr,
    Node,
    Parameter,
    ReturnStatement,
    Statement,
    ThisExpr,
    Type,
    TypeParameter,
    UnaryExpr,
    UnaryOperator,
    VariableDeclStatement,
    WhileStatement,
    valid_class_members,
    valid_interface_members,
)


class JoeParseError(Exception):
    pass


class TokenType(Enum):
    AMP_AMP = auto()
    ASTERISK = auto()
    BANG = auto()
    BANG_EQUAL = auto()
    CHAR = auto()
    CLASS = auto()
    COLON = auto()
    COMMA = auto()
    DELETE = auto()
    DOT = auto()
    ELSE = auto()
    EQUAL = auto()
    EQUAL_EQUAL = auto()
    EXTENDS = auto()
    FALSE = auto()
    FLOAT = auto()
    FOR = auto()
    GREATER_THAN_EQUAL = auto()
    IDENTIFIER = auto()
    IF = auto()
    IMPLEMENTS = auto()
    INT = auto()
    INTERFACE = auto()
    LEFT_ANGLE_BRACKET = auto()
    LEFT_BRACE = auto()
    LEFT_BRACKET = auto()
    LEFT_PAREN = auto()
    LESS_THAN_EQUAL = auto()
    MINUS = auto()
    NEW = auto()
    PERCENT = auto()
    PIPE_PIPE = auto()
    PLUS = auto()
    RETURN = auto()
    RIGHT_ANGLE_BRACKET = auto()
    RIGHT_BRACE = auto()
    RIGHT_BRACKET = auto()
    RIGHT_PAREN = auto()
    SEMICOLON = auto()
    SLASH = auto()
    STATIC = auto()
    STRING = auto()
    THIS = auto()
    TILDE = auto()
    TRUE = auto()
    VAR = auto()
    WHILE = auto()


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
    "%": TokenType.PERCENT,
    "(": TokenType.LEFT_PAREN,
    ")": TokenType.RIGHT_PAREN,
    "*": TokenType.ASTERISK,
    ",": TokenType.COMMA,
    ".": TokenType.DOT,
    ":": TokenType.COLON,
    ";": TokenType.SEMICOLON,
    "[": TokenType.LEFT_BRACKET,
    "]": TokenType.RIGHT_BRACKET,
    "{": TokenType.LEFT_BRACE,
    "}": TokenType.RIGHT_BRACE,
    # TODO: ++ and --
    "-": TokenType.MINUS,
    "+": TokenType.PLUS,
    "~": TokenType.TILDE,
}

_one_or_two_char_tokens: Final = {
    "=": {
        "": TokenType.EQUAL,
        "=": TokenType.EQUAL_EQUAL,
    },
    "&": {
        "&": TokenType.AMP_AMP,
    },
    "|": {
        "|": TokenType.PIPE_PIPE,
    },
    "!": {
        "": TokenType.BANG,
        "=": TokenType.BANG_EQUAL,
    },
    ">": {
        "": TokenType.RIGHT_ANGLE_BRACKET,
        "=": TokenType.GREATER_THAN_EQUAL,
    },
    "<": {
        "": TokenType.LEFT_ANGLE_BRACKET,
        "=": TokenType.LESS_THAN_EQUAL,
    },
}

_keywords: Final = {
    "class": TokenType.CLASS,
    "delete": TokenType.DELETE,
    "else": TokenType.ELSE,
    "extends": TokenType.EXTENDS,
    "false": TokenType.FALSE,
    "for": TokenType.FOR,
    "if": TokenType.IF,
    "implements": TokenType.IMPLEMENTS,
    "interface": TokenType.INTERFACE,
    "new": TokenType.NEW,
    "return": TokenType.RETURN,
    "static": TokenType.STATIC,
    "this": TokenType.THIS,
    "true": TokenType.TRUE,
    "var": TokenType.VAR,
    "while": TokenType.WHILE,
}


def _is_identifier_start(c: str) -> bool:
    return c == "_" or c in ascii_letters


def _is_identifier(c: str) -> bool:
    return _is_identifier_start(c) or c in digits


class _Chars(Peekable[str]):
    __slots__ = ("line", "column")

    def __init__(self, iterable: Iterable[str]) -> None:
        super().__init__(iterable)
        self.line = 1
        self.column = 0

    def __next__(self) -> str:
        c = super().__next__()
        if c == "\n":
            self.line += 1
            self.column = 0
        else:
            self.column += 1
        return c


def _read_string_char(chars: _Chars, location: Location) -> str:
    c = next(chars)
    if c == "\\":
        c = next(chars)
        if c == "n":
            return "\n"
        elif c == "t":
            return "\t"
        elif c == '"':
            return '"'
        elif c == "\\":
            return "\\"
        else:
            raise JoeParseError(f"Unexpected escape sequence {c!r} at {location}")
    else:
        return c


def scan(filename: str, chars: Iterable[str]) -> Iterator[Token]:
    chars = _Chars(chars)

    def get_location() -> Location:
        return Location(filename, chars.line, chars.column)

    while True:
        try:
            c = next(chars)
        except StopIteration:
            return

        if c.isspace():
            continue

        location = get_location()

        if c in _one_char_tokens:
            yield Token(_one_char_tokens[c], location, c)
        elif c in _one_or_two_char_tokens:
            next_chars = _one_or_two_char_tokens[c]
            c2 = chars.peek()

            if c2 in next_chars:
                next(chars)
                ty = next_chars[c2]
            elif "" in next_chars:
                ty = next_chars[""]
                c2 = None
            else:
                unexpected = repr(c2) if c2 is not None else "end of input"
                raise JoeParseError(
                    f"Unexpected token {unexpected} at {get_location()}"
                )
            yield Token(ty, location, c + (c2 or ""))
        elif c == "/":
            if chars.peek() == "/":
                while True:
                    try:
                        c2 = next(chars)
                    except StopIteration:
                        break
                    if c2 == "\n":
                        break
            elif chars.peek() == "*":
                next(chars)
                while True:
                    try:
                        a = next(chars)
                    except StopIteration:
                        break
                    b = chars.peek()
                    if a == "*" and b == "/":
                        next(chars)
                        break
            else:
                yield Token(TokenType.SLASH, location, c)
        elif _is_identifier_start(c):
            ident = c
            while True:
                c2 = chars.peek()
                if c2 is None or not _is_identifier(c2):
                    break
                ident += next(chars)
            yield Token(_keywords.get(ident, TokenType.IDENTIFIER), location, ident)
        elif c.isdigit():
            s = c
            while True:
                c2 = chars.peek()
                if c2 is None or not c2.isdigit():
                    break
                s += next(chars)
            yield Token(TokenType.INT, location, s)
        elif c == "'":
            s = _read_string_char(chars, get_location())
            c2 = chars.peek()
            if c2 != "'":
                unexpected = repr(c2) if c2 is not None else "end of input"
                raise JoeParseError(
                    f"Unexpected token {unexpected} at {get_location()}"
                )
            next(chars)
            yield Token(TokenType.CHAR, location, s)
        elif c == '"':
            s = ""
            while True:
                c2 = next(chars)
                if c2 == '"':
                    break
                else:
                    s += _read_string_char(chars, get_location())
            yield Token(TokenType.STRING, location, s)
        else:
            raise JoeParseError(f"Unexpected token {c!r} at {location}")


class _Tokens(Iterator[Token]):
    __slots__ = ("tokens", "last_location")

    tokens: Peekable[Token]
    last_location: Location | None

    def __init__(self, tokens: Iterable[Token]) -> None:
        self.tokens = Peekable(tokens)
        self.last_location = None

    def __next__(self) -> Token:
        tok = next(self.tokens)
        self.last_location = tok.location
        return tok

    @contextmanager
    def backtrack_on_error(self) -> Iterator[None]:
        tokens1, tokens2 = tee(self.tokens._iterable)
        self.tokens._iterable = tokens1
        tokens_before = Peekable(tokens2)
        tokens_before._peeked = self.tokens._peeked
        last_location = self.last_location
        try:
            yield
        except JoeParseError:
            self.tokens = tokens_before
            self.last_location = last_location
            raise

    def peek(self) -> Token | None:
        return self.tokens.peek()

    def expect(self, type_: TokenType | Iterable[TokenType]) -> Token:
        try:
            tok = next(self)
        except StopIteration as exc:
            msg = (
                f"one of {', '.join(tok.name for tok in type_)}"
                if isinstance(type_, Iterable)
                else type_.name
            )
            loc_message = f" at {self.last_location}" if self.last_location else ""
            raise JoeParseError(
                f"Unexpected end of input; expected {msg}{loc_message}"
            ) from exc

        ok = tok.type in type_ if isinstance(type_, Iterable) else tok.type is type_
        if not ok:
            msg = (
                f"one of {', '.join(tok.name for tok in type_)}"
                if isinstance(type_, Iterable)
                else type_.name
            )
            raise JoeParseError(
                f"Unexpected token {tok.text!r}; expected {msg} at {tok.location}"
            )
        return tok

    def match(
        self, type_: TokenType | Iterable[TokenType], *, consume: bool = True
    ) -> Token | None:
        tok = self.tokens.peek()
        if tok is None or (
            (tok.type not in type_)
            if isinstance(type_, Iterable)
            else (tok.type is not type_)
        ):
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
) -> MethodSig | MethodDecl | ConstructorDecl | DestructorDecl | FieldDecl:
    static = bool(tokens.match(TokenType.STATIC))
    tilde = tokens.match(TokenType.TILDE)
    is_destructor = tilde is not None
    type_ = _parse_type(tokens)
    if tokens.match(TokenType.LEFT_PAREN, consume=False):
        if type_.type_arguments:
            raise JoeParseError(f"Constructor cannot be generic at {type_.location}")
        name = type_.name
        type_ = Type(name.location, Identifier(name.location, "void"), [])
        is_constructor = not is_destructor
    elif is_destructor:
        raise JoeParseError(f"Invalid destructor signature at {tilde.location}")
    else:
        name = _parse_identifier(tokens)
        is_constructor = False

    if (
        is_constructor
        or is_destructor
        or tokens.match(TokenType.LEFT_PAREN, consume=False)
        or tokens.match(TokenType.LEFT_ANGLE_BRACKET, consume=False)
    ):
        if tokens.match(TokenType.LEFT_ANGLE_BRACKET, consume=False):
            assert not is_constructor and not is_destructor
            type_param_list = _parse_type_param_list(tokens)
        else:
            type_param_list = []
        params = _parse_param_list(tokens)
        if is_destructor and params:
            raise JoeParseError(
                f"Destructor cannot have any parameters at {name.location}"
            )
        if tokens.match(TokenType.SEMICOLON):
            if is_constructor:
                raise JoeParseError(f"Constructor must have a body at {name.location}")
            if is_destructor:
                raise JoeParseError(f"Destructor must have a body at {name.location}")
            elif static:
                raise JoeParseError(
                    f"Interface method cannot be static at {name.location}"
                )
            return MethodSig(type_.location, type_, name, type_param_list, params)
        body = _parse_method_body(tokens)
        if is_constructor:
            return ConstructorDecl(name.location, name, params, body)
        elif is_destructor:
            return DestructorDecl(name.location, name, body)
        else:
            return MethodDecl(
                type_.location, type_, name, type_param_list, params, body, static
            )
    else:
        tokens.expect(TokenType.SEMICOLON)
        return FieldDecl(type_.location, type_, name, static)


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
    statements = []
    while not tokens.match(TokenType.RIGHT_BRACE):
        statements.append(_parse_statement(tokens))
    return statements


def _parse_block_or_statement(tokens: _Tokens) -> list[Statement]:
    if not tokens.match(TokenType.LEFT_BRACE, consume=False):
        return [_parse_statement(tokens)]
    tokens.expect(TokenType.LEFT_BRACE)
    statements = []
    while not tokens.match(TokenType.RIGHT_BRACE):
        statements.append(_parse_statement(tokens))
    return statements


def _parse_statement(tokens: _Tokens) -> Statement:
    if tokens.match(TokenType.RETURN, consume=False):
        return _parse_return_statement(tokens)
    elif tokens.match(TokenType.IF, consume=False):
        return _parse_if_statement(tokens)
    elif tokens.match(TokenType.WHILE, consume=False):
        return _parse_while_statement(tokens)
    elif tokens.match(TokenType.FOR, consume=False):
        return _parse_for_statement(tokens)
    elif tokens.match(TokenType.DELETE, consume=False):
        return _parse_delete_statement(tokens)
    elif tokens.match(TokenType.VAR, consume=False):
        return _parse_variable_decl_statement(tokens)
    else:
        return _parse_variable_decl_or_expression_statement(tokens)


def _parse_return_statement(tokens: _Tokens) -> ReturnStatement:
    location = tokens.expect(TokenType.RETURN).location
    if tokens.match(TokenType.SEMICOLON):
        return ReturnStatement(location, None)
    else:
        expr = _parse_expression(tokens)
        tokens.expect(TokenType.SEMICOLON)
        return ReturnStatement(location, expr)


def _parse_if_statement(tokens: _Tokens) -> IfStatement:
    location = tokens.expect(TokenType.IF).location
    tokens.expect(TokenType.LEFT_PAREN)
    condition = _parse_expression(tokens)
    tokens.expect(TokenType.RIGHT_PAREN)
    then_body = tuple(_parse_block_or_statement(tokens))
    if tokens.match(TokenType.ELSE):
        else_body = tuple(_parse_block_or_statement(tokens))
    else:
        else_body = ()
    return IfStatement(location, condition, then_body, else_body)


def _parse_while_statement(tokens: _Tokens) -> WhileStatement:
    location = tokens.expect(TokenType.WHILE).location
    tokens.expect(TokenType.LEFT_PAREN)
    condition = _parse_expression(tokens)
    tokens.expect(TokenType.RIGHT_PAREN)
    body = tuple(_parse_block_or_statement(tokens))
    return WhileStatement(location, condition, body)


def _parse_for_statement(tokens: _Tokens) -> ForStatement:
    location = tokens.expect(TokenType.FOR).location
    tokens.expect(TokenType.LEFT_PAREN)
    init: Statement | None
    if tokens.match(TokenType.VAR, consume=False):
        init = _parse_variable_decl_statement(tokens)
    elif tokens.match(TokenType.SEMICOLON, consume=False):
        init = None
    else:
        init = _parse_variable_decl_or_expression_statement(tokens)
    if not tokens.match(TokenType.SEMICOLON, consume=False):
        condition = _parse_expression(tokens)
    else:
        condition = None
    tokens.expect(TokenType.SEMICOLON)
    if not tokens.match(TokenType.RIGHT_PAREN, consume=False):
        update = _parse_expression(tokens)
    else:
        update = None
    tokens.expect(TokenType.RIGHT_PAREN)
    body = tuple(_parse_block_or_statement(tokens))
    return ForStatement(location, init, condition, update, body)


def _parse_variable_decl_statement(tokens: _Tokens) -> VariableDeclStatement:
    if var := tokens.match(TokenType.VAR):
        location = var.location
        type_ = None
    else:
        type_ = _parse_type(tokens)
        location = type_.location
    name = _parse_identifier(tokens)
    if not tokens.match(TokenType.SEMICOLON, consume=False):
        tokens.expect(TokenType.EQUAL)
        expr = _parse_expression(tokens)
    else:
        expr = None
    tokens.expect(TokenType.SEMICOLON)
    return VariableDeclStatement(location, type_, name, expr)


def _parse_delete_statement(tokens: _Tokens) -> DeleteStatement:
    location = tokens.expect(TokenType.DELETE).location
    expr = _parse_expression(tokens)
    tokens.expect(TokenType.SEMICOLON)
    return DeleteStatement(location, expr)


def _parse_variable_decl_or_expression_statement(
    tokens: _Tokens,
) -> VariableDeclStatement | ExprStatement:
    assert not tokens.match(TokenType.VAR, consume=False)
    try:
        with tokens.backtrack_on_error():
            return _parse_variable_decl_statement(tokens)
    except JoeParseError:
        return _parse_expr_statement(tokens)


def _parse_expr_statement(tokens: _Tokens) -> ExprStatement:
    expr = _parse_expression(tokens)
    tokens.expect(TokenType.SEMICOLON)
    return ExprStatement(expr.location, expr)


def _parse_expression(tokens: _Tokens) -> Expr:
    return _parse_expression_inner(tokens, 0)


def _parse_expression_inner(tokens: _Tokens, rbp: int) -> Expr:
    lhs = _nud(tokens)

    while (
        (next_tok := tokens.peek())
        and next_tok.type in _lbp
        and _lbp[next_tok.type] >= rbp
    ):
        lhs = _led(tokens, lhs)

    return lhs


_lbp: Final = {
    TokenType.EQUAL: 1,
    TokenType.PIPE_PIPE: 2,
    TokenType.AMP_AMP: 3,
    TokenType.EQUAL_EQUAL: 4,
    TokenType.BANG_EQUAL: 4,
    TokenType.LEFT_ANGLE_BRACKET: 5,
    TokenType.LESS_THAN_EQUAL: 5,
    TokenType.RIGHT_ANGLE_BRACKET: 5,
    TokenType.GREATER_THAN_EQUAL: 5,
    TokenType.PLUS: 6,
    TokenType.MINUS: 6,
    TokenType.ASTERISK: 7,
    TokenType.SLASH: 7,
    TokenType.PERCENT: 7,
    TokenType.LEFT_PAREN: 9,
    TokenType.LEFT_BRACKET: 9,
    TokenType.DOT: 10,
}

_rbp: Final = {
    TokenType.LEFT_PAREN: 0,
    TokenType.LEFT_BRACKET: 0,
    TokenType.BANG: 7,
    TokenType.MINUS: 7,
}

_UNARY_OPERATOR_TOKEN_TYPES: Final = frozenset([TokenType.BANG, TokenType.MINUS])
_LEFT_ASSOC_BINARY_OPERATOR_TOKEN_TYPES: Final = frozenset(
    [
        TokenType.PLUS,
        TokenType.MINUS,
        TokenType.ASTERISK,
        TokenType.SLASH,
        TokenType.PERCENT,
        TokenType.LEFT_ANGLE_BRACKET,
        TokenType.LESS_THAN_EQUAL,
        TokenType.RIGHT_ANGLE_BRACKET,
        TokenType.GREATER_THAN_EQUAL,
        TokenType.EQUAL_EQUAL,
        TokenType.BANG_EQUAL,
        TokenType.AMP_AMP,
        TokenType.PIPE_PIPE,
    ]
)
_RIGHT_ASSOC_BINARY_OPERATOR_TOKEN_TYPES: Final = frozenset([TokenType.EQUAL])


def _nud(tokens: _Tokens) -> Expr:
    if tok := tokens.match(TokenType.INT):
        return LiteralInt(tok.location, int(tok.text))
    elif tok := tokens.match(TokenType.FLOAT):
        return LiteralFloat(tok.location, float(tok.text))
    elif tok := tokens.match(TokenType.STRING):
        return LiteralString(tok.location, tok.text)
    elif (tok := tokens.match(TokenType.TRUE)) or (
        tok := tokens.match(TokenType.FALSE)
    ):
        return LiteralBool(tok.location, tok.type == TokenType.TRUE)
    elif tok := tokens.match(TokenType.CHAR):
        return LiteralChar(tok.location, tok.text)
    elif tok := tokens.match(TokenType.THIS):
        return ThisExpr(tok.location)
    elif tok := tokens.match(TokenType.IDENTIFIER):
        return IdentifierExpr(tok.location, Identifier(tok.location, tok.text))
    elif tok := tokens.match(TokenType.LEFT_PAREN):
        expr = _parse_expression(tokens)
        tokens.expect(TokenType.RIGHT_PAREN)
        return expr
    elif tokens.match(_UNARY_OPERATOR_TOKEN_TYPES, consume=False):
        return _parse_unary_expression(tokens)
    elif tokens.match(TokenType.NEW, consume=False):
        return _parse_new_expression(tokens)
    else:
        raise JoeParseError(
            f"Expected expression at {tokens.last_location or 'end of file'}"
        )


def _led(tokens: _Tokens, lhs: Expr) -> Expr:
    if tokens.match(TokenType.LEFT_ANGLE_BRACKET, consume=False):
        try:
            with tokens.backtrack_on_error():
                return _parse_call_expression(tokens, lhs)
        except JoeParseError:
            return _parse_left_assoc_binary_expression(tokens, lhs)
    elif tokens.match(_LEFT_ASSOC_BINARY_OPERATOR_TOKEN_TYPES, consume=False):
        return _parse_left_assoc_binary_expression(tokens, lhs)
    elif tokens.match(_RIGHT_ASSOC_BINARY_OPERATOR_TOKEN_TYPES, consume=False):
        return _parse_right_assoc_binary_expression(tokens, lhs)
    elif tokens.match(TokenType.LEFT_PAREN, consume=False):
        return _parse_call_expression(tokens, lhs)
    elif tokens.match(TokenType.LEFT_BRACKET, consume=False):
        return _parse_index_expression(tokens, lhs)
    elif tokens.match(TokenType.DOT, consume=False):
        try:
            with tokens.backtrack_on_error():
                return _parse_call_expression_from_dot(tokens, lhs)
        except JoeParseError:
            return _parse_dot_expression(tokens, lhs)
    else:
        raise JoeParseError(
            f"Expected expression at {tokens.last_location or 'end of file'}"
        )


def _token_type_to_unary_operator(token_type: TokenType) -> UnaryOperator:
    if token_type is TokenType.BANG:
        return UnaryOperator.NOT
    elif token_type is TokenType.MINUS:
        return UnaryOperator.MINUS
    else:
        unreachable()


def _parse_unary_expression(tokens: _Tokens) -> Expr:
    op = tokens.expect(_UNARY_OPERATOR_TOKEN_TYPES)
    operand = _parse_expression_inner(tokens, _rbp[op.type])
    return UnaryExpr(op.location, _token_type_to_unary_operator(op.type), operand)


def _token_type_to_binary_operator(token_type: TokenType) -> BinaryOperator:
    if token_type is TokenType.PLUS:
        return BinaryOperator.PLUS
    elif token_type is TokenType.MINUS:
        return BinaryOperator.MINUS
    elif token_type is TokenType.ASTERISK:
        return BinaryOperator.TIMES
    elif token_type is TokenType.SLASH:
        return BinaryOperator.DIVIDE
    elif token_type is TokenType.PERCENT:
        return BinaryOperator.MODULO
    elif token_type is TokenType.LEFT_ANGLE_BRACKET:
        return BinaryOperator.LESS_THAN
    elif token_type is TokenType.LESS_THAN_EQUAL:
        return BinaryOperator.LESS_THAN_OR_EQUAL
    elif token_type is TokenType.RIGHT_ANGLE_BRACKET:
        return BinaryOperator.GREATER_THAN
    elif token_type is TokenType.GREATER_THAN_EQUAL:
        return BinaryOperator.GREATER_THAN_OR_EQUAL
    elif token_type is TokenType.EQUAL_EQUAL:
        return BinaryOperator.EQUALS
    elif token_type is TokenType.BANG_EQUAL:
        return BinaryOperator.NOT_EQUALS
    elif token_type is TokenType.AMP_AMP:
        return BinaryOperator.AND
    elif token_type is TokenType.PIPE_PIPE:
        return BinaryOperator.OR
    elif token_type is TokenType.EQUAL:
        return BinaryOperator.ASSIGN
    else:
        unreachable()


def _parse_left_assoc_binary_expression(tokens: _Tokens, lhs: Expr) -> Expr:
    op = tokens.expect(_LEFT_ASSOC_BINARY_OPERATOR_TOKEN_TYPES)
    rhs = _parse_expression_inner(tokens, _lbp[op.type])
    return BinaryExpr(lhs.location, _token_type_to_binary_operator(op.type), lhs, rhs)


def _parse_right_assoc_binary_expression(tokens: _Tokens, lhs: Expr) -> Expr:
    op = tokens.expect(_RIGHT_ASSOC_BINARY_OPERATOR_TOKEN_TYPES)
    rhs = _parse_expression_inner(tokens, _lbp[op.type] - 1)
    return BinaryExpr(lhs.location, _token_type_to_binary_operator(op.type), lhs, rhs)


def _parse_dot_expression(tokens: _Tokens, lhs: Expr) -> Expr:
    tokens.expect(TokenType.DOT)
    name = _parse_identifier(tokens)
    return DotExpr(lhs.location, lhs, name)


def _parse_index_expression(tokens: _Tokens, lhs: Expr) -> Expr:
    tokens.expect(TokenType.LEFT_BRACKET)
    index = _parse_expression(tokens)
    tokens.expect(TokenType.RIGHT_BRACKET)
    return IndexExpr(lhs.location, lhs, index)


def _parse_call_expression_from_dot(tokens: _Tokens, receiver: Expr) -> Expr:
    dot_expr = _parse_dot_expression(tokens, receiver)
    return _parse_call_expression(tokens, dot_expr)


def _parse_call_expression(tokens: _Tokens, callee: Expr) -> Expr:
    if isinstance(callee, IdentifierExpr):
        name = callee.name
        receiver = None
    elif isinstance(callee, DotExpr):
        name = callee.name
        receiver = callee.expr
    else:
        raise JoeParseError(
            f"Expected identifier or dot expression at {callee.location}"
        )
    type_arguments = []
    # FIXME: Somehow handle a<b>(c) (i.e. id(a),lt,id(b),gt,lpar,id(c),rpar)
    if tokens.match(TokenType.LEFT_ANGLE_BRACKET):
        while not tokens.match(TokenType.RIGHT_ANGLE_BRACKET, consume=False):
            type_arguments.append(_parse_type(tokens))
            if not tokens.match(TokenType.COMMA):
                break
        tokens.expect(TokenType.RIGHT_ANGLE_BRACKET)
    tokens.expect(TokenType.LEFT_PAREN)
    args = []
    while not tokens.match(TokenType.RIGHT_PAREN, consume=False):
        args.append(_parse_expression(tokens))
        if not tokens.match(TokenType.COMMA):
            break
    tokens.expect(TokenType.RIGHT_PAREN)
    return CallExpr(callee.location, receiver, name, type_arguments, args)


def _parse_new_expression(tokens: _Tokens) -> Expr:
    new = tokens.expect(TokenType.NEW)
    type = _parse_type(tokens)
    if tokens.match(TokenType.LEFT_PAREN):
        args = []
        while not tokens.match(TokenType.RIGHT_PAREN, consume=False):
            args.append(_parse_expression(tokens))
            if not tokens.match(TokenType.COMMA):
                break
        tokens.expect(TokenType.RIGHT_PAREN)
        return NewExpr(new.location, type, args)
    elif tokens.match(TokenType.LEFT_BRACKET):
        size = _parse_expression(tokens)
        tokens.expect(TokenType.RIGHT_BRACKET)
        return NewArrayExpr(new.location, type, size)
    else:
        raise JoeParseError(
            f"Expected '(' or '[' at {tokens.last_location or 'end of file'}"
        )
