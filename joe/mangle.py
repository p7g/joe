import typing as t

_primitive_names = {
    "int": "i",
    "double": "d",
    "void": "v",
}


def mangle_name(path: str, type_arguments: t.List[str]) -> str:
    if path in _primitive_names:
        return _primitive_names[path]
    path_parts = path.split(".")
    buf = "N"
    for part in path_parts:
        buf += f"{len(part)}{part}"
    if type_arguments:
        buf += type_suffix(type_arguments)
    return buf


def type_suffix(mangled_types: t.List[str]) -> str:
    buf = "I"
    for arg in mangled_types:
        buf += arg
    return buf + "E"
