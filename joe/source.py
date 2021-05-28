from dataclasses import dataclass


class JoeException(Exception):
    def __init__(self, location: "Location", message: str):
        super().__init__(f"{message} in {location}")


class JoeSyntaxError(JoeException):
    pass


class JoeNameError(JoeException):
    pass


class JoeTypeError(JoeException):
    pass


@dataclass(frozen=True)
class Location:
    file: str
    line: int
    column: int

    def __str__(self):
        return f"{self.file} at {self.line}:{self.column}"
