from joe import ast
from joe.parse import parse, scan
from joe.typecheck import (
    Instance,
    SubstituteTypeVariables,
    Type,
    TypeInfo,
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

environment: dict[str, TypeInfo | Type] = {}
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


integer_type = TypeInfo("Integer", True, [], [], [], [], [])
string_type = TypeInfo("String", False, [], [], [], [], [])

# TODO: prelude
environment["Integer"] = Instance(integer_type, [])
environment["String"] = Instance(string_type, [])

visitor = SubstituteTypeVariables(environment)
for name, type_ in environment.items():
    if isinstance(type_, TypeInfo):
        environment[name] = type_.map(visitor)
    else:
        environment[name] = type_.accept(visitor)

    # print(repr(type_info))
    # from pprint import pprint
    # pprint(recursive_vars(type_info))


Abc = environment["Abc"]
Zyx = environment["Zyx"]
assert isinstance(Abc, TypeInfo) and isinstance(Zyx, TypeInfo)
abc = Instance(Abc, [Instance(string_type, [])])
zyx = Instance(Zyx, [Instance(string_type, [])])

print(is_subtype(abc, zyx))


# checks WE CAN DO:
# - Instance arity
# - type parameter constraints
# - void in weird places
# - leftover type variables
# - interface implementation (make sure has all methods with right types)
#
# CONSIDERATIONS
# - for implementations of interface methods:
#   - return type covariance
#   - parameter type contravariance
# - self-referential types
