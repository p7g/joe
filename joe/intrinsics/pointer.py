from llvmlite import ir

from joe.codegen import CompileContext, _method_to_llvm_type, _type_to_llvm
from joe.eval import BoundMethod, BoundType


def make_pointer_deref(ctx: CompileContext, deref_method: BoundMethod) -> ir.Function:
    assert isinstance(deref_method.self_type, BoundType)
    file_llvm_type = _type_to_llvm(ctx.ir_module, deref_method.self_type)

    deref_ty = _method_to_llvm_type(ctx.ir_module, file_llvm_type, deref_method, True)
    deref = ir.Function(ctx.ir_module, deref_ty, name=deref_method.name())
    this_arg, offset_arg = deref.args

    ir_builder = ir.IRBuilder(deref.append_basic_block(name="entry"))
    ptr = ir_builder.gep(this_arg, [offset_arg])
    ir_builder.ret(ir_builder.load(ptr))

    return deref
