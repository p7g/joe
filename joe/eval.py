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
        self.environment = module.environment
        self.decl_ast = decl_ast

    def instantiate(self, arguments: Iterable["BoundType"]) -> "BoundType":
        return BoundType(self, arguments)

    def name(self) -> str:
        return f"{self.module.name()}.{self.decl_ast.name.name}"


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
            member for member in self.decl_ast.members if member.name.name == name
        )
        assert isinstance(field, ast.FieldDecl)
        return evaluate_type(self.environment, field.type)

    def get_field_index(self, name: str) -> int:
        assert isinstance(self.decl_ast, ast.ClassDecl)
        idx, field = next(
            (idx, member)
            for idx, member in enumerate(self.decl_ast.members)
            if member.name.name == name
        )
        assert isinstance(field, ast.FieldDecl)
        return idx

    def get_method(
        self, name: str, type_arguments: Iterable["BoundType"]
    ) -> "BoundMethod":
        method = next(
            member for member in self.decl_ast.members if member.name.name == name
        )
        assert isinstance(method, ast.MethodDecl)
        return BoundMethod(self, method, type_arguments)

    def get_constructor(self) -> "BoundConstructor":
        assert isinstance(self.decl_ast, ast.ClassDecl)
        constructor = next(
            member
            for member in self.decl_ast.members
            if isinstance(member, ast.ConstructorDecl)
        )
        return BoundConstructor(self, constructor)


class BoundMethod:
    def __init__(
        self,
        bound_type: BoundType,
        decl_ast: ast.MethodDecl,
        arguments: Iterable["BoundType"],
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

    def name(self) -> str:
        return f"{self.self_type.name()}.{self.decl_ast.name.name}"

    def get_parameter_types(self) -> list[BoundType]:
        return [
            evaluate_type(self.environment, param.type)
            for param in self.decl_ast.parameters
        ]


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
            env.get_type_constructor("Integer").instantiate([]),
            expr_ast,
            expr_ast.value,
        )
    elif isinstance(expr_ast, ast.LiteralBool):
        return typed_ast.LiteralBool(
            env.get_type_constructor("Boolean").instantiate([]),
            expr_ast,
            expr_ast.value,
        )
    elif isinstance(expr_ast, ast.LiteralString):
        return typed_ast.LiteralString(
            env.get_type_constructor("String").instantiate([]), expr_ast, expr_ast.value
        )
    elif isinstance(expr_ast, ast.LiteralFloat):
        return typed_ast.LiteralFloat(
            env.get_type_constructor("Double").instantiate([]), expr_ast, expr_ast.value
        )
    elif isinstance(expr_ast, ast.DotExpr):
        obj_expr = evaluate_expr(env, scope, expr_ast.expr)
        field_type = obj_expr.type.get_field(expr_ast.name.name)
        return typed_ast.DotExpr(field_type, expr_ast, obj_expr, expr_ast.name)
    elif isinstance(expr_ast, ast.CallExpr):
        receiver = evaluate_expr(env, scope, expr_ast.expr)
        method = receiver.type.get_method(
            expr_ast.name.name,
            [evaluate_type(env, arg) for arg in expr_ast.type_arguments],
        )
        args = [evaluate_expr(env, scope, arg) for arg in expr_ast.arguments]
        return typed_ast.CallExpr(
            method, expr_ast, receiver, expr_ast.name, args
        )
    elif isinstance(expr_ast, ast.NewExpr):
        ty = evaluate_type(env, expr_ast.type)
        return typed_ast.NewExpr(
            ty, expr_ast, [evaluate_expr(env, scope, arg) for arg in expr_ast.arguments]
        )
    elif isinstance(expr_ast, ast.IndexExpr):
        array = evaluate_expr(env, scope, expr_ast.expr)
        index = evaluate_expr(env, scope, expr_ast.index)
        expr_ty = array.type.get_method("get", [index.type]).get_return_type()
        return typed_ast.IndexExpr(expr_ty, expr_ast, array, index)
    elif isinstance(expr_ast, ast.NewArrayExpr):
        ty = evaluate_type(env, expr_ast.type)
        size = evaluate_expr(env, scope, expr_ast.size)
        array_ty = env.get_type_constructor("Array").instantiate([ty])
        return typed_ast.NewArrayExpr(array_ty, expr_ast, size)
    elif isinstance(expr_ast, ast.UnaryExpr):
        expr = evaluate_expr(env, scope, expr_ast.expr)
        return typed_ast.UnaryExpr(expr.type, expr_ast, expr_ast.operator, expr)
    elif isinstance(expr_ast, ast.BinaryExpr):
        lhs = evaluate_expr(env, scope, expr_ast.left)
        rhs = evaluate_expr(env, scope, expr_ast.right)
        return typed_ast.BinaryExpr(lhs.type, expr_ast, expr_ast.operator, lhs, rhs)