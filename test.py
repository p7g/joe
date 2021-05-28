from joe.compile import ModuleCodeGenerator
from joe.parse import Parser
from joe.context import GlobalContext

filename = "simple.java"

with open(filename, "r") as f:
	src = f.read()

p = Parser(filename, src)
mods = p.parse_file()

ctx = GlobalContext()
ctx.populate_from_modules(mods)

g = ModuleCodeGenerator(mods, main_method="simple.main")
g.generate()
print(g.get())
