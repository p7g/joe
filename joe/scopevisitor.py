from __future__ import annotations

import typing as t
from collections import Counter
from contextlib import contextmanager

from joe import ast
from joe.source import JoeNameError, JoeSyntaxError, Location
from joe.visitor import Visitor


def escape_name(name: str, *, discriminator: int = 0) -> str:
    n_str = "" if discriminator == 0 else str(discriminator)
    return f"L{len(name)}{name}{n_str}"


class ScopeVisitor(Visitor):
    def __init__(self):
        super().__init__()
        self._scope_level = 0
        self._name_occurrences: Counter[str] = Counter()
        self._names: dict[tuple[int, str], str] = {}

    def try_resolve_name(self, base_name: str) -> str | None:
        for l in range(self._scope_level, -1, -1):
            if (l, base_name) in self._names:
                return self._names[l, base_name]
        return None

    def resolve_name(self, base_name: str, *, location: Location) -> str:
        name = self.try_resolve_name(base_name)
        if name is None:
            raise JoeNameError(location, f"Use of undeclared name {base_name}")
        return name

    def declare_name(self, name: str, *, location: Location) -> str:
        if (self._scope_level, name) in self._names:
            raise JoeSyntaxError(location, f"{name} has already been declared")
        n = self._name_occurrences[name]
        self._name_occurrences[name] += 1

        escaped_name = escape_name(name, discriminator=n)
        self._names[self._scope_level, name] = escaped_name
        return escaped_name

    def clear_names(self) -> None:
        self._scope_level = 0
        self._names.clear()
        self._name_occurrences.clear()

    @contextmanager  # type: ignore
    def scope(self) -> t.Iterator[None]:
        self._scope_level += 1
        yield
        self._scope_level -= 1

    def visit_Method(self, node: ast.Method):
        super().visit_Method(node)

    def visit_VarDeclaration(self, node: ast.VarDeclaration):
        # Call super method first so that a shadowed name can be referred to in
        # the initializer.
        super().visit_VarDeclaration(node)

        self.declare_name(node.name.value, location=node.location)

    # TODO: etc
    # def visit_IfStmt(self, node: ast.IfStmt):
    #     with self.scope():
    #         super().visit_IfStmt(node)
