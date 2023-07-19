from llvmlite import ir

from joe.codegen import CompileContext, _method_to_llvm_type, _type_to_llvm
from joe.eval import BoundMethod, BoundType, BoundTypeConstructor


def make_file_open(ctx: CompileContext, open_method: BoundMethod) -> ir.Function:
    assert isinstance(open_method.self_type, BoundTypeConstructor)
    file_type = open_method.self_type.instantiate([])
    file_llvm_type = _type_to_llvm(ctx.ir_module, file_type)
    assert isinstance(file_llvm_type, ir.PointerType)

    real_open = ir.Function(
        ctx.ir_module,
        ir.FunctionType(ir.IntType(32), [ir.IntType(8).as_pointer(), ir.IntType(32)]),
        "open",
    )

    file_open_ty = _method_to_llvm_type(
        ctx.ir_module, None, open_method, open_method.static
    )
    file_open = ir.Function(ctx.ir_module, file_open_ty, name=open_method.name())
    pathname_arg, flags_arg = file_open.args

    ir_builder = ir.IRBuilder(file_open.append_basic_block(name="entry"))
    fd = ir_builder.call(real_open, [pathname_arg, flags_arg])
    # FIXME: Handle error and raise exception, not invalid File
    new_file_obj = ir_builder.bitcast(
        ir_builder.call(
            ctx.get_malloc(),
            [
                ir.IntType(64)(
                    file_llvm_type.pointee.get_abi_size(ctx.get_target_data())
                )
            ],
        ),
        file_llvm_type,
    )
    fd_ptr = ir_builder.gep(new_file_obj, [ir.IntType(32)(0), ir.IntType(32)(0)])
    ir_builder.store(fd, fd_ptr)
    ir_builder.ret(new_file_obj)

    return file_open


def make_file_read(ctx: CompileContext, read_method: BoundMethod) -> ir.Function:
    assert isinstance(read_method.self_type, BoundType)
    file_llvm_type = _type_to_llvm(ctx.ir_module, read_method.self_type)

    real_read = ir.Function(
        ctx.ir_module,
        ir.FunctionType(
            ir.IntType(64),
            [ir.IntType(32), ir.IntType(8).as_pointer(), ir.IntType(64)],
        ),
        "read",
    )

    file_read_ty = _method_to_llvm_type(
        ctx.ir_module, file_llvm_type, read_method, read_method.static
    )
    file_read = ir.Function(ctx.ir_module, file_read_ty, name=read_method.name())
    this_arg, buf_arg, count_arg = file_read.args

    ir_builder = ir.IRBuilder(file_read.append_basic_block(name="entry"))
    fd = ir_builder.gep(
        this_arg,
        [
            ir.IntType(32)(0),
            ir.IntType(32)(read_method.self_type.get_field_index("_fd")),
        ],
    )
    buf_arg = ir_builder.load(buf_arg)
    buf_ptr = ir_builder.extract_value(buf_arg, 0)
    # FIXME: check the size
    # buf_size = ir_builder.extract_value(buf_arg, 1)
    ir_builder.ret(
        ir_builder.call(real_read, [ir_builder.load(fd), buf_ptr, count_arg])
    )

    return file_read


def make_file_write(ctx: CompileContext, write_method: BoundMethod) -> ir.Function:
    assert isinstance(write_method.self_type, BoundType)
    file_llvm_type = _type_to_llvm(ctx.ir_module, write_method.self_type)

    real_write = ir.Function(
        ctx.ir_module,
        ir.FunctionType(
            ir.IntType(64),
            [ir.IntType(32), ir.IntType(8).as_pointer(), ir.IntType(64)],
        ),
        "write",
    )

    file_write_ty = _method_to_llvm_type(
        ctx.ir_module, file_llvm_type, write_method, write_method.static
    )
    file_write = ir.Function(ctx.ir_module, file_write_ty, name=write_method.name())
    this_arg, buf_arg, count_arg = file_write.args

    ir_builder = ir.IRBuilder(file_write.append_basic_block(name="entry"))
    fd = ir_builder.gep(
        this_arg,
        [
            ir.IntType(32)(0),
            ir.IntType(32)(write_method.self_type.get_field_index("_fd")),
        ],
    )
    buf_arg = ir_builder.load(buf_arg)
    buf_ptr = ir_builder.extract_value(buf_arg, 0)
    # FIXME: check size
    # buf_size = ir_builder.extract_value(buf_arg, 1)
    ir_builder.ret(
        ir_builder.call(real_write, [ir_builder.load(fd), buf_ptr, count_arg])
    )

    return file_write


def make_file_close(ctx: CompileContext, close_method: BoundMethod) -> ir.Function:
    assert isinstance(close_method.self_type, BoundType)
    file_llvm_type = _type_to_llvm(ctx.ir_module, close_method.self_type)

    real_close = ir.Function(
        ctx.ir_module,
        ir.FunctionType(ir.IntType(32), [ir.IntType(32)]),
        "close",
    )

    file_close_ty = _method_to_llvm_type(
        ctx.ir_module, file_llvm_type, close_method, close_method.static
    )
    file_close = ir.Function(ctx.ir_module, file_close_ty, name=close_method.name())
    this_arg = file_close.args[0]

    ir_builder = ir.IRBuilder(file_close.append_basic_block(name="entry"))
    fd = ir_builder.gep(
        this_arg,
        [
            ir.IntType(32)(0),
            ir.IntType(32)(close_method.self_type.get_field_index("_fd")),
        ],
    )
    ir_builder.ret(ir_builder.call(real_close, [ir_builder.load(fd)]))

    return file_close


def make_file_lseek(ctx: CompileContext, lseek_method: BoundMethod) -> ir.Function:
    assert isinstance(lseek_method.self_type, BoundType)
    file_llvm_type = _type_to_llvm(ctx.ir_module, lseek_method.self_type)

    real_lseek = ir.Function(
        ctx.ir_module,
        ir.FunctionType(
            ir.IntType(64),
            [ir.IntType(32), ir.IntType(64), ir.IntType(32)],
        ),
        "lseek",
    )

    file_lseek_ty = _method_to_llvm_type(
        ctx.ir_module, file_llvm_type, lseek_method, lseek_method.static
    )
    file_lseek = ir.Function(ctx.ir_module, file_lseek_ty, name=lseek_method.name())
    this_arg, offset_arg, whence_arg = file_lseek.args

    ir_builder = ir.IRBuilder(file_lseek.append_basic_block(name="entry"))
    fd = ir_builder.gep(
        this_arg,
        [
            ir.IntType(32)(0),
            ir.IntType(32)(lseek_method.self_type.get_field_index("_fd")),
        ],
    )
    ir_builder.ret(
        ir_builder.call(real_lseek, [ir_builder.load(fd), offset_arg, whence_arg])
    )

    return file_lseek
