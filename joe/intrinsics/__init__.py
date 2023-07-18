from collections.abc import Callable, Mapping
from typing import Final, TypeAlias

from llvmlite import ir

from joe.codegen import CompileContext
from joe.eval import BoundMethod, BoundTypeConstructor
from joe.intrinsics import file, pointer

_Factories: TypeAlias = Mapping[
    str, Mapping[str, Callable[[CompileContext, BoundMethod], ir.Function]]
]

_factories: Final[_Factories] = {
    "joe.prelude.File": {
        "open<>": file.make_file_open,
        "read<>": file.make_file_read,
        "write<>": file.make_file_write,
        "close<>": file.make_file_close,
        "lseek<>": file.make_file_lseek,
    },
    "joe.prelude.Pointer": {
        "deref<>": pointer.make_pointer_deref,
    },
}

# Monomorphic
_cache: Mapping[str, ir.Function] = {}


def get_intrinsic(ctx: CompileContext, method: BoundMethod) -> ir.Function | None:
    type_constructor = method.self_type
    if not isinstance(type_constructor, BoundTypeConstructor):
        type_constructor = type_constructor.type_constructor

    method_fqname = method.name()
    method_name = method_fqname[method_fqname.rfind(".") + 1 :]

    if method_fqname in _cache:
        return _cache[method_fqname]

    type_constructor_name = type_constructor.name()
    if type_constructor_name not in _factories:
        return None

    intrinsics_for_type = _factories[type_constructor_name]
    if method_name not in intrinsics_for_type:
        return None

    intrinsic = intrinsics_for_type[method_name](ctx, method)
    _cache[method_fqname] = intrinsic
    return intrinsic
