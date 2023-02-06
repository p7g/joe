from collections.abc import Iterable, Iterator
from enum import Enum, auto
from typing import Generic, Literal, TypeVar

T = TypeVar("T")


class _Nothing(Enum):
    NOTHING = auto()


class Peekable(Generic[T], Iterator[T]):
    __slots__ = ("_iterable", "_peeked")

    _iterable: Iterator[T]
    _peeked: T | Literal[_Nothing.NOTHING]

    def __init__(self, iterable: Iterable[T]) -> None:
        self._iterable = iter(iterable)
        self._peeked = _Nothing.NOTHING

    def __next__(self) -> T:
        if self._peeked is not _Nothing.NOTHING:
            peeked = self._peeked
            self._peeked = _Nothing.NOTHING
            return peeked
        return next(self._iterable)

    def peek(self) -> T | None:
        if self._peeked is _Nothing.NOTHING:
            try:
                self._peeked = peeked = next(self._iterable)
            except StopIteration:
                return None
        else:
            peeked = self._peeked
        return peeked
