from collections import ChainMap
from itertools import chain

from joe import ast, eval as joeeval
from joe.parse import parse, scan
from joe.typecheck import (
    Instance,
    JoeTypeError,
    SubstituteTypeVariables,
    TopType,
    Type,
    TypeInfo,
    TypeVariable,
    TypeVisitor,
    VoidType,
    _initialize_typeinfo,
    is_subtype,
)

# text = """
# interface Xyz<T, U> extends Zyx<T> {
#     U somemeth(T arg);

#     T hello() {

#     }
# }

# interface Zyx<T> {}

# class Abc<T: String> implements Xyz<T, Integer> {
#     String yo;

#     Abc() {
#     }

#     Integer somemeth(T arg) {}

#     void method(Integer a) {
#     }
# }
# """

# text = """\
# interface X<T> {}

# class A {
#     Integer field;
#     void method<T>(X<T> arg) {
#         var x = new X<Integer>();
#         x = x.nonexistentMethod(x, this, false);
#     }
# }
# """

# environment: dict[str, TypeInfo] = {}
# for node in parse(scan("<script>", text)):
#     assert isinstance(node, (ast.ClassDecl, ast.InterfaceDecl))
#     type_info = _initialize_typeinfo(node)
#     environment[type_info.name] = type_info


# def vars_but_slots(obj):
#     if isinstance(obj, dict):
#         return obj
#     elif hasattr(obj, "__dict__"):
#         return vars(obj)
#     elif hasattr(type(obj), "__slots__"):
#         return {slot: getattr(obj, slot) for slot in type(obj).__slots__}
#     else:
#         return None


# def recursive_vars(obj):
#     if isinstance(obj, (str, int, float, bool, type(None))):
#         return obj
#     varses = vars_but_slots(obj)
#     if not varses:
#         try:
#             return [recursive_vars(el) for el in obj]
#         except TypeError:
#             return obj
#     return {k: recursive_vars(v) for k, v in varses.items()}


# TODO: prelude
# environment["Integer"] = TypeInfo("Integer", True, [], [], [], [], [])
# environment["String"] = TypeInfo("String", False, [], [], [], [], [])

# for decl in environment.values():
#     class_env: dict[str, Type] = {
#         p.name: TypeVariable(p.name, p.constraint) for p in decl.type_parameters
#     }
#     visitor = SubstituteTypeVariables(environment, class_env)
#     decl.update(visitor)

#     for method in chain(decl.methods.values(), decl.static_methods.values()):
#         method_env: dict[str, Type] = {
#             p.name: TypeVariable(p.name, p.constraint) for p in method.type_parameters
#         }
#         visitor = SubstituteTypeVariables(environment, ChainMap(method_env, class_env))
#         method.update(visitor)

# print(repr(environment[name]))
# from pprint import pprint
# pprint(recursive_vars(type_info))


# checks WE CAN DO:
# [x] Instance arity
# [x] type parameter constraints
# [ ] void in weird places
# [ ] leftover type variables
# [ ] interface implementation (make sure has all methods with right types)
# [ ] Method instance arity
#
# CONSIDERATIONS
# - for implementations of interface methods:
#   - return type covariance
#   - parameter type contravariance
# - self-referential types


# class CheckAllResolved(TypeVisitor[None]):
#     def visit_instance(self, instance: Instance) -> None:
#         for argument in instance.arguments:
#             argument.accept(self)
#         if not instance.type_info.is_resolved:
#             raise JoeTypeError("still unresolved", instance.type_info)

#     def visit_void_type(self, void_type: VoidType) -> None:
#         pass

#     def visit_top_type(self, top_type: TopType) -> None:
#         pass

#     def visit_type_variable(self, type_variable: TypeVariable) -> None:
#         type_variable.constraint.accept(self)


# check_resolved = CheckAllResolved()
# for decl in environment.values():
#     decl.run_visitor(check_resolved)


# class CheckInstanceArity(TypeVisitor[None]):
#     def visit_instance(self, instance: Instance) -> None:
#         if len(instance.arguments) != len(instance.type_info.type_parameters):
#             raise JoeTypeError("Incorrect number of arguments")

#     def visit_void_type(self, void_type: VoidType) -> None:
#         pass

#     def visit_top_type(self, top_type: TopType) -> None:
#         pass

#     def visit_type_variable(self, type_variable: TypeVariable) -> None:
#         type_variable.constraint.accept(self)


# check_arity = CheckInstanceArity()
# for decl in environment.values():
#     decl.run_visitor(check_arity)


# class CheckTypeParameterConstraints(TypeVisitor[None]):
#     def visit_instance(self, instance: Instance) -> None:
#         for argument in instance.arguments:
#             argument.accept(self)
#         for type_param, argument in zip(
#             instance.type_info.type_parameters, instance.arguments, strict=True
#         ):
#             if type_param.constraint and not is_subtype(
#                 argument, type_param.constraint
#             ):
#                 raise JoeTypeError(
#                     "doesn't match constraint", argument, type_param.constraint
#                 )

#     def visit_void_type(self, void_type: VoidType) -> None:
#         pass

#     def visit_top_type(self, top_type: TopType) -> None:
#         pass

#     def visit_type_variable(self, type_variable: TypeVariable) -> None:
#         type_variable.constraint.accept(self)


# check_constraints = CheckTypeParameterConstraints()
# for decl in environment.values():
#     decl.run_visitor(check_constraints)
#     for method in chain(decl.methods.values(), decl.static_methods.values()):
#         method.run_visitor(check_constraints)


# abc = Instance(environment["Abc"], [Instance(environment["String"], [])])
# zyx = Instance(environment["Zyx"], [Instance(environment["String"], [])])

# print("Abc < Xyz", is_subtype(abc, zyx))

# somemeth_abc = abc.get_method("somemeth", [], [Instance(environment["Integer"], [])])
# somemeth_xyz = cast(Instance, next(iter(abc.supertypes()))).get_method(
#     "somemeth", [], [Instance(environment["Integer"], [])]
# )

# print("Abc.somemeth < Xyz.somemeth", somemeth_abc.implements(somemeth_xyz))
# print(abc.get_method("somemeth", [], [Instance(environment["Integer"], [])]))

# a = Instance(environment["A"], [])
# arg = Instance(environment["X"], [Instance(environment["Integer"], [])])
# meth = a.get_method("method", [], [arg])
# print(meth)

# print("===")

from joe.codegen import CompileContext, MethodCompiler

tokens = list(
    scan(
        "<script>",
        """
        class Main {
            Integer abc(Integer n) { n = 234; return n; }
            void test() {
                abc(123).print();
            }
        }
        """,
    )
)
nodes = list(parse(tokens))
module = joeeval.evaluate_module(joeeval.ModuleAST("script", "<script>", nodes))
# A = module.environment.get_type_constructor("A")
# X = module.environment.get_type_constructor("X")
# a = A.instantiate([])
# arg = X.instantiate(
#     [joeeval.BoundType(module.environment.get_type_constructor("Integer"), ())]
# )
# meth = a.get_method("method", [arg])
# print(meth.environment.type_scope["T"].name())
# print(meth.get_return_type().name())
# print([p.name() for p in meth.get_parameter_types()])


from llvmlite import binding as llvm_binding

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


# from joe._internal.repr import autorepr

# print(autorepr(meth.decl_ast))
