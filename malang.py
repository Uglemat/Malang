#!/usr/bin/env python3
"""
Copyright (C) 2014 Mattias Ugelvik

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
"""

from parser import parse
import builtin_funcs
from utils import Env, Node, change_directory, MalangError, readline_imported
import utils
from os import path
import sys

import evaluator


sys.setrecursionlimit(5000)


def eval_malang(code, env, filename):
    """
    (code: str, env: utils.Env, filename: str) -> (result: utils.Node)

    Evaluate `code` in the environment `env`.

    `filename` is used for reporting errors, it doesn't have to refer to an actual file.
    """
    return evaluator.trampoline(parse(code, filename), env, filename)



# The reason I don't define `require` in the builtin_funcs module is that I need access to
# `main_env` inside `require`.
def require(filename_to_open, env, filename, infonode):
    """
    @ = Filename

    Evaluate the program in the file `Filename`, and return a module.
    """
    utils.assert_type(filename_to_open, 'str', filename, infonode)
    with open(filename_to_open.content) as f:
        code = f.read()
    module_env = Env(parent=main_env)
    eval_malang(code, module_env, filename_to_open.content)
    return Node('module', module_env)


builtins_module = Node('module',
                      Env(bindings={name: node for name, node
                                    in builtin_funcs.builtins.items()}))
builtins_module.content.bind('Require', Node('builtin', require))
builtins_module.content.bind('Exhibit', Node('builtin', require))

# The main environment which all environments inherits from, with the "Builtins" module already present.
main_env = Env(bindings={'Builtins': builtins_module})

# The env in which expressions in the REPL is evaluated.
REPL_env = Env(parent=main_env)



stdlib_location = path.join(path.dirname(path.abspath(__file__)), "init.malang")
with open(stdlib_location) as f:
    with change_directory(path.dirname(path.abspath(__file__))):
        # Evaluates the init file in the main environment, thus filling it with the basic bindings
        eval_malang(f.read(), main_env, stdlib_location)







if __name__ == "__main__":

    if len(sys.argv) == 1:
        print("Usage: $ malang [-i] <file1> <file2> ... <fileN>")
        exit()


    interactive = "-i" in sys.argv
    if interactive:
        sys.argv.remove("-i")


    for filename in sys.argv[1:]:
        temp_env = Env(parent=main_env)
        with open(filename) as f:
            with change_directory(path.dirname(path.abspath(filename))):
                eval_malang(f.read(), temp_env, filename)


    if interactive:
        ansicode          = lambda n: "\x1b[{}m".format(n)
        readline_ansicode = ansicode
        if readline_imported:
            import readline
            readline.set_completer(utils.Completer(REPL_env))

            # The purpose of wrapping the ansi escape codes between \001 and \002
            # in `readline_ansicode`is to make readline not count those characters.
            # If I don't do that, things will go bad when the expression you write in
            # the REPL gets so long that it needs a new line in the terminal, because
            # readline will have an incorrect character count. Or something like that,
            # I'm not really sure. I don't really know why it works. Just trust me. Please.
            readline_ansicode = lambda n: "\001{}\002".format(ansicode(n))

        color_prompt = "{}Malang{}> {}".format(readline_ansicode(36),
                                               readline_ansicode(1)  + readline_ansicode(33),
                                               readline_ansicode(22) + readline_ansicode(32))

        while True:
            try:
                with utils.tab_completion():
                    inp = input(color_prompt)
                    print(ansicode(39), end="")
            except EOFError:
                print(ansicode(39))
                break
            if not inp:
                continue
            try:
                print(
                    builtin_funcs.tostr(
                        eval_malang(inp, REPL_env, "<REPL>"),
                        repr_str=True
                    ).content,
                )
            except MalangError as e:
                print("{}Error: {}".format(ansicode(31), e))
            finally:
                print(ansicode(39), end="")
