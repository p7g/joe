from llvmlite import binding as llvm_binding

from joe import eval as joeeval
from joe.codegen import CompileContext, MethodCompiler
from joe.parse import parse, scan

tokens = list(
    scan(
        "<script>",
        """
        class A {
            Integer _i;

            A(Integer i) {
                _i = i * 2;
            }

            Integer getI() {
                return _i;
            }
        }

        class Main {
            void test() {
                var x = new A(123);
                x.getI().print();
                delete x;
            }
        }
        """,
    )
)
nodes = list(parse(tokens))
module = joeeval.evaluate_module(joeeval.ModuleAST("script", "<script>", nodes))

llvm_binding.initialize()
llvm_binding.initialize_native_target()
llvm_binding.initialize_native_asmprinter()

meth = (
    module.environment.get_type_constructor("Main")
    .instantiate([])
    .get_method("test", [])
)
ctx = CompileContext()
compiler = MethodCompiler(ctx, meth)
meth.decl_ast.accept(compiler)
llvm_ir = str(compiler.ctx.ir_module)
print(llvm_ir)

target = llvm_binding.Target.from_default_triple()
target_machine = target.create_target_machine()
backing_mod = llvm_binding.parse_assembly("")
engine = llvm_binding.create_mcjit_compiler(backing_mod, target_machine)
mod = llvm_binding.parse_assembly(llvm_ir)
mod.verify()
engine.add_module(mod)
engine.finalize_object()
engine.run_static_constructors()

from ctypes import CFUNCTYPE

func_ptr = engine.get_function_address(meth.name())
cfunc = CFUNCTYPE(None)(func_ptr)
cfunc()
