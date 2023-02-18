from joe import ast
from joe.parse import parse, scan
from joe.typecheck import (
    Instance,
    JoeTypeError,
    SubstituteTypeVariables,
    TopType,
    TypeInfo,
    TypeVisitor,
    VoidType,
    _initialize_typeinfo,
    is_subtype,
)

text = """
interface Xyz<T, U> extends Zyx<T> {
    U somemeth(T arg);

    T hello() {

    }
}

interface Zyx<T> {}

class Abc<T: String> implements Xyz<T, Integer> {
    String yo;

    Abc() {
    }

    void method(Integer a) {
    }
}
"""

environment: dict[str, TypeInfo] = {}
for node in parse(scan("<script>", text)):
    assert isinstance(node, (ast.ClassDecl, ast.InterfaceDecl))
    type_info = _initialize_typeinfo(node)
    environment[type_info.name] = type_info


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
environment["Integer"] = TypeInfo("Integer", True, [], [], [], [], [])
environment["String"] = TypeInfo("String", False, [], [], [], [], [])

for name, type_ in environment.items():
    visitor = SubstituteTypeVariables(
        environment, {p.name: None for p in type_.type_parameters}
    )
    environment[name] = type_.map(visitor)

    # print(repr(environment[name]))
    # from pprint import pprint
    # pprint(recursive_vars(type_info))


# checks WE CAN DO:
# [x] Instance arity
# [ ] type parameter constraints
# [ ] void in weird places
# [ ] leftover type variables
# [ ] interface implementation (make sure has all methods with right types)
#
# CONSIDERATIONS
# - for implementations of interface methods:
#   - return type covariance
#   - parameter type contravariance
# - self-referential types


class CheckInstanceArity(TypeVisitor[None]):
    def visit_instance(self, instance: Instance) -> None:
        if len(instance.arguments) != len(instance.type_info.type_parameters):
            raise JoeTypeError("Incorrect number of arguments")

    def visit_void_type(self, void_type: VoidType) -> None:
        pass

    def visit_top_type(self, top_type: TopType) -> None:
        pass


check_arity = CheckInstanceArity()
for decl in environment.values():
    decl.run_visitor(check_arity)

abc = Instance(environment["Abc"], [Instance(environment["String"], [])])
zyx = Instance(environment["Zyx"], [Instance(environment["String"], [])])

print(is_subtype(abc, zyx))
