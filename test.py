from joe.compile import CompileVisitor
from joe.emit import Emitter
from joe.parse import Parser
from joe.context import GlobalContext

filename = "simple.java"

with open(filename, "r") as f:
	src = f.read()

p = Parser(filename, src)
mods = p.parse_file()

ctx = GlobalContext()
ctx.populate_from_modules(mods)

vis = CompileVisitor(ctx)
vis.visit(mods[0].class_decl)
vis.compile_main_function("simple.main")

e = Emitter()
vis.code_unit.emit(e)

print(e.get())
