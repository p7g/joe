from collections import ChainMap
from collections.abc import Iterable
from pathlib import Path
from typing import Mapping

from joe import ast, parse, typed_ast


class Environment:
    def __init__(
        self,
        type_scope: ChainMap[str, "BoundType | BoundTypeConstructor"] | None = None,
        value_scope: ChainMap[str, "BoundType"] | None = None,
    ):
        self.type_scope = type_scope or ChainMap()
        self.value_scope = value_scope or ChainMap()

    def new_child(self) -> "Environment":
        return Environment(self.type_scope.new_child(), self.value_scope.new_child())

    def get_type(self, name: str) -> "BoundType":
        maybe_ty = self.type_scope[name]
        assert isinstance(maybe_ty, BoundType)
        return maybe_ty

    def get_type_constructor(self, name: str) -> "BoundTypeConstructor":
        maybe_ty = self.type_scope[name]
        assert isinstance(maybe_ty, BoundTypeConstructor)
        return maybe_ty

    def get_value(self, name: str) -> "BoundType":
        return self.value_scope[name]


class BoundTypeConstructor:
    def __init__(self, module: "Module", decl_ast: ast.ClassDecl | ast.InterfaceDecl):
        self.module = module
        self.environment = module.environment.new_child()
        self.environment.type_scope["Self"] = self
        self.decl_ast = decl_ast

    def instantiate(self, arguments: Iterable["BoundType"]) -> "BoundType":
        return BoundType(self, arguments)

    def name(self) -> str:
        return f"{self.module.name()}.{self.decl_ast.name.name}"

    def get_method(
        self, name: str, type_arguments: Iterable["BoundType"]
    ) -> "BoundMethod":
        """Can only find static methods"""
        method = next(
            member
            for member in self.decl_ast.members
            if isinstance(member, ast.MethodDecl)
            and member.name.name == name
            and member.static
        )
        return BoundMethod(self, method, type_arguments, method.static)

    def get_field(self, name: str) -> "BoundType":
        """Can only find static fields"""
        assert isinstance(self.decl_ast, ast.ClassDecl)
        field = next(
            member
            for member in self.decl_ast.members
            if isinstance(member, ast.FieldDecl)
            and member.name.name == name
            and member.static
        )
        return evaluate_type(self.environment, field.type)


class BoundType:
    def __init__(
        self,
        bound_type_constructor: BoundTypeConstructor,
        arguments: Iterable["BoundType"],
    ):
        self.type_constructor = bound_type_constructor
        self.environment = bound_type_constructor.environment.new_child()
        self.environment.type_scope.update(
            {
                param.name.name: arg
                for param, arg in zip(
                    bound_type_constructor.decl_ast.type_parameters,
                    arguments,
                    strict=True,
                )
            }
        )
        self.decl_ast = bound_type_constructor.decl_ast

    def name(self) -> str:
        arg_names = ",".join(
            self.environment.get_type(arg.name.name).name()
            for arg in self.decl_ast.type_parameters
        )
        return f"{self.type_constructor.name()}<{arg_names}>"

    def get_field(self, name: str) -> "BoundType":
        assert isinstance(self.decl_ast, ast.ClassDecl)
        field = next(
            member
            for member in self.decl_ast.members
            if isinstance(member, ast.FieldDecl)
            and member.name.name == name
            and not member.static
        )
        assert isinstance(field, ast.FieldDecl)
        return evaluate_type(self.environment, field.type)

    def get_field_index(self, name: str) -> int:
        assert isinstance(self.decl_ast, ast.ClassDecl)
        idx, field = next(
            (idx, member)
            for idx, member in enumerate(self.decl_ast.members)
            if isinstance(member, ast.FieldDecl)
            and member.name.name == name
            and not member.static
        )
        assert isinstance(field, ast.FieldDecl)
        return idx

    def get_method(
        self, name: str, type_arguments: Iterable["BoundType"]
    ) -> "BoundMethod":
        method = next(
            member
            for member in self.decl_ast.members
            if isinstance(member, ast.MethodDecl)
            and member.name.name == name
            and not member.static
        )
        return BoundMethod(self, method, type_arguments, method.static)

    def get_constructor(self) -> "BoundConstructor | None":
        assert isinstance(self.decl_ast, ast.ClassDecl)
        constructor = next(
            (
                member
                for member in self.decl_ast.members
                if isinstance(member, ast.ConstructorDecl)
            ),
            None,
        )
        if constructor:
            return BoundConstructor(self, constructor)
        return None

    def get_destructor(self) -> "BoundDestructor | None":
        assert isinstance(self.decl_ast, ast.ClassDecl)
        destructor = next(
            (
                member
                for member in self.decl_ast.members
                if isinstance(member, ast.DestructorDecl)
            ),
            None,
        )
        if destructor:
            return BoundDestructor(self, destructor)
        return None


class BoundMethod:
    def __init__(
        self,
        bound_type: BoundType | BoundTypeConstructor,
        decl_ast: ast.MethodDecl,
        arguments: Iterable["BoundType"],
        static: bool,
    ):
        self.self_type = bound_type
        self.environment = bound_type.environment.new_child()
        for param, arg in zip(decl_ast.type_parameters, arguments, strict=True):
            self.environment.type_scope[param.name.name] = arg
        for param in decl_ast.parameters:
            self.environment.value_scope[param.name.name] = evaluate_type(
                self.environment, param.type
            )
        self.decl_ast = decl_ast
        self.static = static

    def name(self) -> str:
        arg_names = ",".join(
            self.environment.get_type(arg.name.name).name()
            for arg in self.decl_ast.type_parameters
        )
        return f"{self.self_type.name()}.{self.decl_ast.name.name}<{arg_names}>"

    def get_return_type(self) -> BoundType:
        return evaluate_type(self.environment, self.decl_ast.return_type)

    def get_parameter_types(self) -> list[BoundType]:
        return [
            evaluate_type(self.environment, param.type)
            for param in self.decl_ast.parameters
        ]


class BoundConstructor:
    def __init__(
        self,
        bound_type: BoundType,
        decl_ast: ast.ConstructorDecl,
    ):
        self.self_type = bound_type
        self.environment = bound_type.environment.new_child()
        for param in decl_ast.parameters:
            self.environment.value_scope[param.name.name] = evaluate_type(
                bound_type.environment, param.type
            )
        self.decl_ast = decl_ast
        self.static = False

    def name(self) -> str:
        return f"{self.self_type.name()}.{self.decl_ast.name.name}"

    def get_parameter_types(self) -> list[BoundType]:
        return [
            evaluate_type(self.environment, param.type)
            for param in self.decl_ast.parameters
        ]


class BoundDestructor:
    def __init__(self, bound_type: BoundType, decl_ast: ast.DestructorDecl) -> None:
        self.self_type = bound_type
        self.environment = bound_type.environment
        self.decl_ast = decl_ast
        self.static = False

    def name(self) -> str:
        return f"{self.self_type.name()}.~{self.decl_ast.name.name}"


class ModuleAST:
    def __init__(self, name: str, filename: str, nodes: Iterable[ast.Node]):
        self.name = name
        self.filename = filename
        self.nodes = tuple(nodes)


class Module:
    def __init__(self, environment: Environment, module_ast: ModuleAST):
        self.module_ast = module_ast
        self.environment = environment

    def name(self) -> str:
        return self.module_ast.name


def evaluate_module(module_ast: ModuleAST) -> Module:
    # Create an environment with a populated global scope
    # For each class/interface in the file, create a wrapper around the AST
    # node bound to the environment.
    # The wrapper can then be evaluated with some arguments to get a concrete
    # instance of the class/interface.
    module = Module(Environment(), module_ast)

    import_decls = [
        node for node in module_ast.nodes if isinstance(node, ast.ImportDecl)
    ]
    if module_ast.name != "joe.prelude":
        import_decls.insert(
            0,
            ast.ImportDecl(
                ast.Location(module_ast.filename, 0, 0),
                ("joe", "prelude"),
                "*",
            ),
        )

    for import_decl in import_decls:
        # FIXME: Only import a module once
        relative_path = Path(*import_decl.module_path).with_suffix(".joe")
        # FIXME: configurable paths, at least include the project path
        for import_path in [Path(__file__).parent.parent / "lib"]:
            file_path = import_path / relative_path
            if not file_path.exists():
                continue
            with open(file_path) as f:
                imported_module_text = f.read()
            imported_module_ast = ModuleAST(
                ".".join(import_decl.module_path),
                str(file_path),
                parse.parse(parse.scan(str(file_path), imported_module_text)),
            )
            imported_module = evaluate_module(imported_module_ast)
            if import_decl.imports == "*":
                imports = {**imported_module.environment.type_scope}
            else:
                assert isinstance(import_decl.imports, tuple)
                imports = {
                    name.name: imported_module.environment.type_scope[name.name]
                    for name in import_decl.imports
                }
            conflicting_names = module.environment.type_scope.keys() & imports.keys()
            if conflicting_names:
                raise Exception(
                    "Conflict in imported names: " + ", ".join(conflicting_names)
                )
            module.environment.type_scope |= imports
            break
        else:
            raise Exception(
                f"Could not find module {'.'.join(import_decl.module_path)}"
            )

    for node in module_ast.nodes:
        if isinstance(node, (ast.ClassDecl, ast.InterfaceDecl)):
            module.environment.type_scope[node.name.name] = BoundTypeConstructor(
                module, node
            )
        else:
            raise NotImplementedError(type(node))
    return module


def evaluate_type(env: Environment, type: ast.Type) -> BoundType:
    constructor = env.type_scope[type.name.name]
    # FIXME: This doesn't feel right, maybe everything in the environment
    # should be a type constructor? But then how would type arguments work?
    if isinstance(constructor, BoundType):
        assert not type.type_arguments
        return constructor
    return constructor.instantiate(
        evaluate_type(env, arg) for arg in type.type_arguments
    )


def evaluate_expr(
    env: Environment, scope: Mapping[str, BoundType], expr_ast: ast.Expr
) -> typed_ast.Expr:
    if isinstance(expr_ast, ast.IdentifierExpr):
        return typed_ast.IdentifierExpr(
            scope[expr_ast.name.name], expr_ast, expr_ast.name
        )
    elif isinstance(expr_ast, ast.ThisExpr):
        return typed_ast.ThisExpr(scope["this"], expr_ast)
    elif isinstance(expr_ast, ast.LiteralInt):
        return typed_ast.LiteralInt(
            env.get_type_constructor("int").instantiate([]),
            expr_ast,
            expr_ast.value,
        )
    elif isinstance(expr_ast, ast.LiteralBool):
        return typed_ast.LiteralBool(
            env.get_type_constructor("boolean").instantiate([]),
            expr_ast,
            expr_ast.value,
        )
    elif isinstance(expr_ast, ast.LiteralChar):
        return typed_ast.LiteralChar(
            env.get_type_constructor("char").instantiate([]),
            expr_ast,
            expr_ast.value,
        )
    elif isinstance(expr_ast, ast.LiteralString):
        return typed_ast.LiteralString(
            env.get_type_constructor("String").instantiate([]), expr_ast, expr_ast.value
        )
    elif isinstance(expr_ast, ast.LiteralFloat):
        return typed_ast.LiteralFloat(
            env.get_type_constructor("double").instantiate([]), expr_ast, expr_ast.value
        )
    elif isinstance(expr_ast, ast.DotExpr):
        receiver_type: BoundType | BoundTypeConstructor
        if isinstance(expr_ast.expr, ast.IdentifierExpr):
            if expr_ast.expr.name.name in scope:
                receiver_type = evaluate_expr(env, scope, expr_ast.expr).type
            else:
                receiver_type = env.get_type_constructor(expr_ast.expr.name.name)
        else:
            receiver_type = evaluate_expr(env, scope, expr_ast.expr).type

        field_type = receiver_type.get_field(expr_ast.name.name)
        if isinstance(receiver_type, BoundTypeConstructor):
            assert isinstance(receiver_type, BoundTypeConstructor)
            return typed_ast.StaticDotExpr(
                field_type, expr_ast, receiver_type, expr_ast.name
            )
        else:
            obj_expr = evaluate_expr(env, scope, expr_ast.expr)
            field_type = obj_expr.type.get_field(expr_ast.name.name)
            return typed_ast.DotExpr(field_type, expr_ast, obj_expr, expr_ast.name)
    elif isinstance(expr_ast, ast.CallExpr):
        # NOTE: receiver could actually be an identifier referring to a type,
        # not an object
        # FIXME: when referring to static methods, it should not be possible to
        # pass type arguments to the class.
        receiver_type: BoundType | BoundTypeConstructor
        if isinstance(expr_ast.expr, ast.IdentifierExpr):
            if expr_ast.expr.name.name in scope:
                receiver_type = evaluate_expr(env, scope, expr_ast.expr).type
            else:
                receiver_type = env.get_type_constructor(expr_ast.expr.name.name)
        elif expr_ast.expr is None:
            this_type = scope["this"] if "this" in scope else env.type_scope["Self"]
            try:
                this_type.get_method(expr_ast.name.name, [])
                receiver_type = this_type
            except StopIteration:
                # Can call static methods from instance methods
                assert isinstance(this_type, BoundType)
                receiver_type = this_type.type_constructor
        else:
            receiver_type = evaluate_expr(env, scope, expr_ast.expr).type
        method = receiver_type.get_method(
            expr_ast.name.name,
            [evaluate_type(env, arg) for arg in expr_ast.type_arguments],
        )
        receiver = (
            evaluate_expr(env, scope, expr_ast.expr)
            if expr_ast.expr and not method.static
            else None
        )
        args = [
            _maybe_resize_integer(param, evaluate_expr(env, scope, arg))
            for param, arg in zip(
                method.get_parameter_types(), expr_ast.arguments, strict=True
            )
        ]
        return typed_ast.CallExpr(method, expr_ast, receiver, expr_ast.name, args)
    elif isinstance(expr_ast, ast.NewExpr):
        ty = evaluate_type(env, expr_ast.type)
        constructor = ty.get_constructor()
        constructor_params = constructor.get_parameter_types() if constructor else None
        args = []
        for i, arg in enumerate(expr_ast.arguments):
            arg_expr = evaluate_expr(env, scope, arg)
            if constructor_params is not None:
                arg_expr = _maybe_resize_integer(constructor_params[i], arg_expr)
            args.append(arg_expr)
        return typed_ast.NewExpr(ty, expr_ast, args)
    elif isinstance(expr_ast, ast.IndexExpr):
        array = evaluate_expr(env, scope, expr_ast.expr)
        index = _maybe_resize_integer(
            # FIXME: should be able to look up types by fully-qualified names
            env.get_type_constructor("long").instantiate([]),
            evaluate_expr(env, scope, expr_ast.index),
        )
        expr_ty = array.type.get_method("get", []).get_return_type()
        return typed_ast.IndexExpr(expr_ty, expr_ast, array, index)
    elif isinstance(expr_ast, ast.NewArrayExpr):
        ty = evaluate_type(env, expr_ast.type)
        size = _maybe_resize_integer(
            env.get_type_constructor("long").instantiate([]),
            evaluate_expr(env, scope, expr_ast.size),
        )
        array_ty = env.get_type_constructor("Array").instantiate([ty])
        return typed_ast.NewArrayExpr(array_ty, expr_ast, size)
    elif isinstance(expr_ast, ast.UnaryExpr):
        expr = evaluate_expr(env, scope, expr_ast.expr)
        return typed_ast.UnaryExpr(expr.type, expr_ast, expr_ast.operator, expr)
    elif isinstance(expr_ast, ast.BinaryExpr):
        lhs = evaluate_expr(env, scope, expr_ast.left)
        rhs = evaluate_expr(env, scope, expr_ast.right)

        if expr_ast.operator in [ast.BinaryOperator.AND, ast.BinaryOperator.OR]:
            ty = env.get_type_constructor("boolean").instantiate([])
        else:
            ty = lhs.type
            rhs = _maybe_resize_integer(lhs.type, rhs)
        return typed_ast.BinaryExpr(ty, expr_ast, expr_ast.operator, lhs, rhs)


_integer_type_sizes = {
    "joe.prelude.byte<>": 1,
    # "joe.prelude.Short<>": 2,
    "joe.prelude.int<>": 4,
    "joe.prelude.uint<>": 4,
    "joe.prelude.long<>": 8,
    "joe.prelude.ulong<>": 8,
}


def _maybe_resize_integer(dest_type: BoundType, expr: typed_ast.Expr) -> typed_ast.Expr:
    lhs_size = _integer_type_sizes.get(dest_type.name(), None)
    rhs_size = _integer_type_sizes.get(expr.type.name(), None)
    if lhs_size is None or rhs_size is None:
        return expr
    if lhs_size != rhs_size:
        return typed_ast.IntegerCastExpr(dest_type, expr.original_node, expr)
    return expr
