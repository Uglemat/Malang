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
from utils import Node, assert_type
import time
import random

def print_docstring(docstring):
    if docstring is None:
        print("That function doesn't have a docstring")
    else:
        print(" - Docstring:")
        for line in docstring.splitlines():
            print(line)

builtins = {}
def builtin(name):
    def _f(func):
        builtins[name] = Node('builtin', func)
        return func
    return _f


@builtin("Print")
def _print(s, env, filename, infonode):
    """
    @ = Val

    Print `Val`.
    """
    
    print(s.content if s._type == 'str' else
          tostr(s, env).content, end="")
    return s

@builtin("Help")
def _help(fun, env, filename, infonode):
    """
    @ = Func

    Prints information about the function `Func`.
    """
    assert_type(fun, ('function', 'builtin'), filename, infonode)
    if fun._type == 'builtin':
        print("This is a builtin function")
        print_docstring(fun.content.__doc__)
    elif fun._type == 'function':
        print("Function was defined in the file {!r}".format(fun.content['filename']))
        print_docstring(fun.content['docstring'])
    return Node('atom', 'ok')

@builtin("Input")
def _input(prompt, env, filename, infonode):
    """
    @ = Prompt

    Get input from the user, with the prompt `Prompt`.
    """
    assert_type(prompt, 'str', filename, infonode)
    return Node('str', input(prompt.content))

@builtin("Sleep")
def sleep(amount, env, filename, infonode):
    """
    @ = Ms

    Pause execution for `Ms` milliseconds.
    """
    assert_type(amount, 'number', filename, infonode)
    time.sleep(amount.content/1000.0)
    return Node('atom', 'ok')

@builtin("Random_Range")
def random_range(tup, env, filename, infonode):
    """
    @ = {Start, Stop}

    Returns a random integer between `Start` (inclusive) and `Stop` (exclusive).
    """
    assert_type(tup, 'tuple', filename, infonode, tuplelength=2)
    num1, num2 = tup.content
    assert_type(num1, 'number', filename, infonode)
    assert_type(num2, 'number', filename, infonode)
    return Node('number', random.randrange(num1.content,num2.content))

@builtin("ListEnv")
def list_env(_arg, env, filename, infonode):
    """
    @ = <whatever>

    Prints a listing of all the identifiers bound in the current environment.
    """
    for k, v in env.bindings.items():
        print("{:<15} => {}".format(k, tostr(v, env).content))
    if not env.parent is None:
        return list_env(None, env.parent)
    return Node('atom', 'ok')

@builtin("ClearEnv")
def clear_env(_arg, env, filename, infonode):
    """
    @ = <whatever>

    Removes the identifier bindings in the current environment.
    This doesn't remove identifier bindings from 'parent' environments.
    """
    env.clear()
    return Node('atom', 'ok')


@builtin("ToList")
def tolist(tup, env, filename, infonode):
    """
    @ = Tuple

    Convert `Tuple` to a malang list.
    """
    assert_type(tup, 'tuple', filename, infonode)
    if tup.content == ():
        return Node('atom', 'nil')
    else:
        return Node('tuple', (tup.content[0],
                              tolist(Node('tuple', tup.content[1:]), env)))

@builtin("ToStr")
def tostr(val, env=None, filename="", infonode=None, depth=0):
    """
    @ = Val

    Returns a string representation of `Val`.
    """
    T = val._type
    if T == 'tuple' and depth > 50:
        content = "{...}"
    elif T == 'tuple':
        content = "{" + ", ".join(tostr(elem, env, filename, infonode,
                                        depth=depth+1).content
                                  for elem in val.content) + "}"
    elif T in ('module', 'function', 'builtin'):
        content = '[{}]'.format(T)
    elif T == 'str':
        content = repr(val.content)
    else:
        content = str(val.content)

    return Node('str', content)
