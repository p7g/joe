import enum
import typing as t
from dataclasses import dataclass
from patina import Option, None_

from joe.source import Location, JoeSyntaxError


@enum.unique
class TokenType(enum.IntEnum):
    Class = enum.auto()
    Delete = enum.auto()
    Do = enum.auto()
    Extends = enum.auto()
    False_ = enum.auto()
    Final = enum.auto()
    For = enum.auto()
    Free = enum.auto()
    If = enum.auto()
    Implements = enum.auto()
    Import = enum.auto()
    Interface = enum.auto()
    New = enum.auto()
    Null = enum.auto()
    Return = enum.auto()
    Static = enum.auto()
    Super = enum.auto()
    This = enum.auto()
    True_ = enum.auto()
    Void = enum.auto()
    While = enum.auto()
    Ident = enum.auto()
    Int = enum.auto()
    LBrace = enum.auto()
    RBrace = enum.auto()
    LBracket = enum.auto()
    RBracket = enum.auto()
    LParen = enum.auto()
    RParen = enum.auto()
    Plus = enum.auto()
    Minus = enum.auto()
    Star = enum.auto()
    Slash = enum.auto()
    PlusEq = enum.auto()
    MinusEq = enum.auto()
    StarEq = enum.auto()
    SlashEq = enum.auto()
    Lt = enum.auto()
    LtLt = enum.auto()
    Gt = enum.auto()
    GtGt = enum.auto()
    Lte = enum.auto()
    Gte = enum.auto()
    Eq = enum.auto()
    EqEq = enum.auto()
    SemiColon = enum.auto()
    Colon = enum.auto()
    Dot = enum.auto()
    Comma = enum.auto()

    def __str__(self):
        return self.name


@dataclass(frozen=True)
class Token:
    type: TokenType
    value: str
    location: Location

    def expect(self, type: TokenType) -> "Token":
        if self.type != type:
            raise JoeSyntaxError(
                self.location, f"Expected {type.name}, got {self.type.name}"
            )
        return self


_keywords = {
    "class": TokenType.Class,
    "delete": TokenType.Delete,
    "do": TokenType.Do,
    "extends": TokenType.Extends,
    "false": TokenType.False_,
    "final": TokenType.Final,
    "for": TokenType.For,
    "free": TokenType.Free,
    "if": TokenType.If,
    "implements": TokenType.Implements,
    "import": TokenType.Import,
    "interface": TokenType.Interface,
    "new": TokenType.New,
    "null": TokenType.Null,
    "return": TokenType.Return,
    "static": TokenType.Static,
    "super": TokenType.Super,
    "this": TokenType.This,
    "true": TokenType.True_,
    "void": TokenType.Void,
    "while": TokenType.While,
}

_one_char_tokens = {
    "{": TokenType.LBrace,
    "}": TokenType.RBrace,
    "[": TokenType.LBracket,
    "]": TokenType.RBracket,
    "(": TokenType.LParen,
    ")": TokenType.RParen,
    ";": TokenType.SemiColon,
    ":": TokenType.Colon,
    ".": TokenType.Dot,
    ",": TokenType.Comma,
}

_sequences = {
    "+": (TokenType.Plus, {"=": TokenType.PlusEq}),
    "-": (TokenType.Minus, {"=": TokenType.MinusEq}),
    "*": (TokenType.Star, {"=": TokenType.StarEq}),
    "<": (TokenType.Lt, {"<": TokenType.LtLt, "=": TokenType.Lte}),
    ">": (TokenType.Gt, {">": TokenType.GtGt, "=": TokenType.Gte}),
    "=": (TokenType.Eq, {"=": TokenType.EqEq}),
}


def lex(filename: str, contents: str) -> t.Generator[Token, None, None]:
    it = iter(contents)
    peeked: Option[str] = None_()
    line = 1
    column = 1

    def current_location() -> Location:
        return Location(filename, line, column)

    def peek() -> str:
        if peeked.is_some():
            return peeked.unwrap()
        c = next(it)
        peeked.replace(c)
        return c

    def next_() -> str:
        nonlocal line, column
        c = peeked.take().unwrap_or_else(lambda: next(it))
        if c == "\n":
            line += 1
            column = 0
            return next_()
        else:
            column += 1
            return c

    while True:
        try:
            c = next_()
        except StopIteration:
            break

        if c in [" ", "\t"]:
            continue

        loc = current_location()

        if c.isdigit():
            acc = c
            while True:
                try:
                    c = peek()
                    if not c.isdigit():
                        break
                except StopIteration:
                    break
                acc += next_()
            yield Token(TokenType.Int, acc, loc)
        elif c.isalpha() or c == "_":
            acc = c
            while True:
                try:
                    c = peek()
                    if not (c.isalpha() or c.isdigit() or c == "_"):
                        break
                except StopIteration:
                    break
                acc += next_()
            yield Token(_keywords.get(acc, TokenType.Ident), acc, loc)
        elif c in _one_char_tokens:
            yield Token(_one_char_tokens[c], c, loc)
        elif c == "/":
            if peek() == "*":
                next_()
                while True:
                    try:
                        c = next_()
                    except StopIteration:
                        raise JoeSyntaxError(loc, "Unterminated block comment")
                    if c == "*" and peek() == "/":
                        next_()
                        break
            elif peek() == "=":
                next_()
                yield Token(TokenType.SlashEq, "/=", loc)
            elif peek() == "/":
                try:
                    while peek() != "\n":
                        next_()
                except StopIteration:
                    break
            else:
                yield Token(TokenType.Slash, "/", loc)
        elif c == "+":
            if peek() == "=":
                next_()
                yield Token(TokenType.PlusEq, "+=", loc)
            else:
                yield Token(TokenType.Plus, "+", loc)
        elif c in _sequences:
            first_char = c
            ty, next_chars = _sequences[c]
            if peek() in next_chars:
                c = next_()
                yield Token(next_chars[c], first_char + c, loc)
            else:
                yield Token(ty, c, loc)
        else:
            raise JoeSyntaxError(loc, f"Invalid token '{c}'")
