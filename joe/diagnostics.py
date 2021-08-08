import enum
import sys

from joe.source import Location


class Diagnostic(enum.Enum):
    hidden_field = "hidden-field"
    ignored_array_type_length = "ignore-array-type-length"

    @property
    def message(self) -> str:
        return _diagnostic_messages[self]

    def __call__(self, *args, location: Location, **kwargs) -> None:
        warn(self, *args, location=location, **kwargs)  # type: ignore


def warn(type: Diagnostic, *args, location: Location, **kwargs) -> None:
    if type not in enabled_diagnostics:
        return

    if args or kwargs:  # type: ignore
        assert (bool(args) ^ bool(kwargs))  # type: ignore

    diagnostic_message = type.message % (args or kwargs)  # type: ignore
    print(
        f"WARN({type.value}): {diagnostic_message} in {location}",
        file=sys.stderr,
    )


enabled_diagnostics = {
    Diagnostic.hidden_field,
    Diagnostic.ignored_array_type_length,
}


_diagnostic_messages = {
    Diagnostic.hidden_field: "Field '%s' on class '%s' hides field from parent class",
    Diagnostic.ignored_array_type_length: "Specifying the length in an array type has no effect",
}
