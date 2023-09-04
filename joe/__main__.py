from argparse import ArgumentParser
from ctypes import CFUNCTYPE, POINTER, c_char_p, c_int

from llvmlite import binding as llvm_binding
from llvmlite import ir

from joe import eval as joeeval
from joe.codegen import CompileContext, MethodCompiler, _type_to_llvm
from joe.parse import parse, scan

arg_parser = ArgumentParser()
arg_parser.add_argument("FILE", type=str, help="File to run")
arg_parser.add_argument(
    "--main", type=str, help="Main function to run", default="Main.main"
)
arg_parser.add_argument("--dump-llvm", action="store_true", help="Dump LLVM IR")
arg_parser.add_argument(
    "--run", action="store_true", help="Run the program after compilation"
)
args, remaining_args = arg_parser.parse_known_args()

with open(args.FILE, "r") as f:
    source = f.read()

nodes = list(parse(scan(args.FILE, source)))
module = joeeval.evaluate_module(joeeval.ModuleAST("script", "<script>", nodes))

llvm_binding.initialize()
llvm_binding.initialize_native_target()
llvm_binding.initialize_native_asmprinter()

main_class, user_main_name = args.main.split(".", 1)
user_main = module.environment.get_type_constructor(main_class).get_method(
    user_main_name, []
)

ctx = CompileContext()
ctx.ir_module.data_layout = (
    "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
)
compiler = MethodCompiler(ctx, user_main)
user_main.decl_ast.accept(compiler)

main_fn = ir.Function(
    compiler.ctx.ir_module,
    ir.FunctionType(
        ir.IntType(32), [ir.IntType(32), ir.IntType(8).as_pointer().as_pointer()]
    ),
    "main",
)

args_array_type = _type_to_llvm(
    compiler.ctx.ir_module,
    module.environment.get_type_constructor("Array").instantiate(
        [module.environment.get_type_constructor("String").instantiate([])]
    ),
)
assert isinstance(args_array_type, ir.PointerType)
ir_builder = ir.IRBuilder(main_fn.append_basic_block())
args_array = ir_builder.alloca(args_array_type.pointee)
ir_builder.store(
    main_fn.args[1],
    ir_builder.gep(
        args_array,
        [
            ir.Constant(ir.IntType(32), 0),
            ir.Constant(ir.IntType(32), 0),
        ],
        inbounds=True,
    ),
)
ir_builder.store(
    ir_builder.zext(main_fn.args[0], ir.IntType(64)),
    ir_builder.gep(
        args_array,
        [
            ir.Constant(ir.IntType(32), 0),
            ir.Constant(ir.IntType(32), 1),
        ],
        inbounds=True,
    ),
)
ir_builder.ret(ir_builder.call(compiler.llvm_function, [args_array]))

compiler.ctx.ir_module.triple = llvm_binding.get_default_triple()
llvm_ir = str(compiler.ctx.ir_module)
if args.dump_llvm:
    print(llvm_ir)

target = llvm_binding.Target.from_default_triple()
target_machine = target.create_target_machine()
backing_mod = llvm_binding.parse_assembly("")
engine = llvm_binding.create_mcjit_compiler(backing_mod, target_machine)
mod = llvm_binding.parse_assembly(llvm_ir)
mod.verify()

if args.run:
    engine.add_module(mod)
    engine.finalize_object()
    engine.run_static_constructors()

    func_ptr = engine.get_function_address("main")
    cfunc = CFUNCTYPE(c_int, c_int, POINTER(c_char_p))(func_ptr)
    args_array = (c_char_p * len(remaining_args))()
    args_array[:] = [arg.encode("ascii") for arg in remaining_args]
    cfunc(len(remaining_args), args_array)
