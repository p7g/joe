from typing import NoReturn


class Unreachable(Exception):
    pass


def unreachable() -> NoReturn:
    raise Unreachable
