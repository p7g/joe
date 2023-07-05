import inspect
import reprlib


class AutoRepr(reprlib.Repr):
    fillvalue: str

    def __init__(self) -> None:
        super().__init__()
        self.maxlevel = 50

    def repr_instance(self, obj: object, level: int):
        if level <= 0:
            return self.fillvalue
        typename = type(obj).__name__
        members_str = ", ".join(
            f"{name}: {self.repr1(value, level - 1)}"
            for name, value in inspect.getmembers(obj)
            if not callable(value)
            and not hasattr(value, "__get__")
            and not name.startswith("_")
        )
        return f"{typename}({members_str})"


autorepr = AutoRepr().repr
