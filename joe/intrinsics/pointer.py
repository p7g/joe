from llvmlite import ir

from joe.codegen import CompileContext, _method_to_llvm_type, _type_to_llvm
from joe.eval import BoundMethod, BoundType


def make_pointer_deref(ctx: CompileContext, deref_method: BoundMethod) -> ir.Function:
    assert isinstance(deref_method.self_type, BoundType)
    pointer_llvm_type = _type_to_llvm(ctx.ir_module, deref_method.self_type)

    deref_ty = _method_to_llvm_type(
        ctx.ir_module, pointer_llvm_type, deref_method, False
    )
    deref = ir.Function(ctx.ir_module, deref_ty, name=deref_method.name())
    this_arg, offset_arg = deref.args

    ir_builder = ir.IRBuilder(deref.append_basic_block(name="entry"))
    ptr = ir_builder.gep(this_arg, [offset_arg])
    ir_builder.ret(ir_builder.load(ptr))

    return deref


def make_pointer_as_array(
    ctx: CompileContext, as_array_method: BoundMethod
) -> ir.Function:
    assert isinstance(as_array_method.self_type, BoundType)
    pointer_llvm_type = _type_to_llvm(ctx.ir_module, as_array_method.self_type)

    as_array_ty = _method_to_llvm_type(
        ctx.ir_module, pointer_llvm_type, as_array_method, False
    )
    as_array = ir.Function(ctx.ir_module, as_array_ty, name=as_array_method.name())
    this_arg, length_arg = as_array.args

    ir_builder = ir.IRBuilder(as_array.append_basic_block(name="entry"))
    array_ty = _type_to_llvm(ctx.ir_module, as_array_method.get_return_type())
    malloc = ctx.get_malloc()
    array = ir_builder.bitcast(
        ir_builder.call(
            malloc, [ir.IntType(64)(array_ty.get_abi_size(ctx.get_target_data()))]
        ),
        array_ty,
    )
    data_ptr = ir_builder.gep(array, [ir.IntType(32)(0), ir.IntType(32)(0)])
    ir_builder.store(this_arg, data_ptr)
    length_ptr = ir_builder.gep(array, [ir.IntType(32)(0), ir.IntType(32)(1)])
    ir_builder.store(length_arg, length_ptr)
    ir_builder.ret(array)

    return as_array
