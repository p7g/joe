import typing as t
from patina import Option, None_

Peekable_T = t.TypeVar("Peekable_T")


class Peekable(t.Generic[Peekable_T]):
    def __init__(self, iterable: t.Iterable[Peekable_T]):
        self._it = iter(iterable)
        self._peeked: Option[Peekable_T] = None_()

    def next(self) -> Peekable_T:
        return self._peeked.take().unwrap_or_else(lambda: next(self._it))

    __next__ = next

    def __iter__(self):
        return self

    def peek(self) -> Peekable_T:
        return self._peeked.get_or_insert_with(lambda: next(self._it)).get()
