from llvmlite import ir

from joe.codegen import CompileContext, _method_to_llvm_type
from joe.eval import BoundMethod, BoundType


def make_string_as_pointer(
    ctx: CompileContext, as_pointer_method: BoundMethod
) -> ir.Function:
    assert isinstance(as_pointer_method.self_type, BoundType)

    string_as_pointer_ty = _method_to_llvm_type(
        ctx.ir_module,
        ir.IntType(8).as_pointer(),
        as_pointer_method,
        as_pointer_method.static,
    )
    string_as_pointer = ir.Function(
        ctx.ir_module, string_as_pointer_ty, name=as_pointer_method.name()
    )
    (this_arg,) = string_as_pointer.args

    ir_builder = ir.IRBuilder(string_as_pointer.append_basic_block(name="entry"))
    ir_builder.ret(this_arg)

    return string_as_pointer


def make_string_length(ctx: CompileContext, length_method: BoundMethod) -> ir.Function:
    assert isinstance(length_method.self_type, BoundType)

    strlen = ir.Function(
        ctx.ir_module,
        ir.FunctionType(
            ir.IntType(64),
            [ir.IntType(8).as_pointer()],
        ),
        "strlen",
    )

    string_length_ty = _method_to_llvm_type(
        ctx.ir_module, ir.IntType(8).as_pointer(), length_method, length_method.static
    )
    string_length = ir.Function(
        ctx.ir_module, string_length_ty, name=length_method.name()
    )
    (this_arg,) = string_length.args

    ir_builder = ir.IRBuilder(string_length.append_basic_block(name="entry"))
    ir_builder.ret(ir_builder.call(strlen, [this_arg]))

    return string_length
