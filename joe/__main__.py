def main() -> None:
    import argparse
    import typing as t
    from joe.compile import CompileVisitor
    from joe.emit import Emitter
    from joe.parse import Parser
    from joe.context import GlobalContext

    argparser = argparse.ArgumentParser("joe")
    argparser.add_argument("filename")
    argparser.add_argument("--main", "-m", dest="main_name")

    args = argparser.parse_args()
    filename: str = args.filename
    main_name: t.Optional[str] = args.main_name

    with open(filename, "r") as f:
        src = f.read()

    p = Parser(filename, src)
    mods = p.parse_file()

    ctx = GlobalContext()
    ctx.populate_from_modules(mods)

    vis = CompileVisitor(ctx)
    vis.visit(mods[0].class_decl)
    if main_name:
        vis.compile_main_function(main_name)

    e = Emitter()
    vis.code_unit.emit(e)

    print(e.get())


if __name__ == "__main__":
    main()
