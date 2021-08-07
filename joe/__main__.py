def main() -> int:
    import argparse
    import typing as t
    from joe.compile import CompileVisitor
    from joe.emit import Emitter
    from joe.parse import Parser
    from joe.context import GlobalContext

    argparser = argparse.ArgumentParser("joe")
    argparser.add_argument("filename")
    argparser.add_argument(
        "--main",
        "-m",
        dest="main_name",
        help="Specify the path to the main method",
    )
    argparser.add_argument(
        "--dump-ast",
        dest="dump_ast",
        action="store_true",
        help="Dump the AST for the given file and exit",
    )

    args = argparser.parse_args()
    filename: str = args.filename
    main_name: t.Optional[str] = args.main_name
    dump_ast: bool = args.dump_ast

    with open(filename, "r") as f:
        src = f.read()

    p = Parser(filename, src)
    mods = p.parse_file()

    if dump_ast:
        print(mods)
        return 0

    ctx = GlobalContext()
    ctx.populate_from_modules(mods)

    vis = CompileVisitor(ctx)
    for mod in mods:
        for class_decl in mod.class_decls:
            vis.visit(class_decl)
    if main_name:
        vis.compile_main_function(main_name)

    e = Emitter()
    vis.ctx.code_unit.emit(e)

    print(e.get())

    return 0


if __name__ == "__main__":
    exit(main())
