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
from utils import Node, assert_type, MalangError
import time
import random

def print_docstring(fun):
    if fun._type == 'function':
        print("Function was defined in the file {!r}".format(fun.content['filename']))
        docstring = fun.content['docstring']
    elif fun._type == 'builtin':
        print("This is a builtin function")
        docstring = fun.content.__doc__


    if docstring is None:
        print("That function doesn't have a docstring")
    else:
        print(" - Docstring:")
        print(docstring)

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

@builtin("Writefile")
def writefile(arg, env, filename, infonode):
    """
    @ = {Str, Filename}

    Overwrite the contents of `Filename` to be `Str`.
    """
    assert_type(arg, 'tuple', filename, infonode, tuplelength=2)
    _str, fname = arg.content
    assert_type(_str, 'str',  filename, infonode)
    assert_type(fname, 'str', filename, infonode)

    try:
        with open(fname.content, mode="w") as f:
            f.write(_str.content)
    except IOError as e:
        raise MalangError(e.args[1], filename, infonode)

    return Node('atom', 'ok')


@builtin("Readfile")
def readfile(fname, env, filename, infonode):
    """
    @ = Filename

    Read the text in `Filename` and return the resulting string.
    """
    assert_type(fname, 'str', filename, infonode)
    try:
        with open(fname.content) as f:
            return Node('str', f.read())
    except IOError as e:
        raise MalangError(e.args[1], filename, infonode)

@builtin("Help")
def _help(fun, env, filename, infonode):
    """
    @ = Func

    Prints information about the function `Func`.
    Alternatively, if `Func` is a module, it will
    print all the information about all the functions
    inside that module.
    """
    assert_type(fun, ('function', 'builtin', 'module'), filename, infonode)

    if fun._type == 'module':
        mod_env = fun.content
        print("Functions in module:")
        for ident, val in mod_env.bindings.items():
            if val._type in ('function', 'builtin'):
                print("\nHelp for function bound to the name {!r}:".format(ident))
                _help(val, env, filename, infonode)
    elif fun._type in ('builtin', 'function'):
        print_docstring(fun)

    return Node('atom', 'ok')

@builtin("TypeOf")
def type_of(thing, env, filename, infonode):
    """
    @ = Thing

    Returns a string representation of the type of `Thing`
    """
    return Node('str', thing._type)

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
def list_env(arg, env, filename, infonode):
    """
    @ = Arg

    If `Arg` is a module, then print a listing of all the identifiers
    bound in the environment (and parent environments) of module.
    If `Arg` isn't a module, then do the same thing, but for the
    current environment.
    """
    if arg is not None:
        if arg._type == 'module':
            print("Identifier bindings in module environment:\n")
            return list_env(None, arg.content, filename, infonode)
        else:
            print("Identifier bindings in current environment:\n")

    for k, v in env.bindings.items():
        print("{:<15} => {}".format(k, tostr(v, env).content))
    if not env.parent is None:
        print("Showing bindings for parent environment below:\n")
        return list_env(None, env.parent, filename, infonode)
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
                              tolist(Node('tuple', tup.content[1:]), env,
                                     filename, infonode)))

@builtin("ToStr")
def tostr(val, env=None, filename="", infonode=None, depth=0, repr_str=False):
    """
    @ = Val

    Returns a string representation of `Val`.
    """
    T = val._type
    if T == 'tuple' and depth > 50:
        content = "{...}"
    elif T == 'tuple':
        content = "{" + ", ".join(tostr(elem, env, filename, infonode,
                                        depth=depth+1, repr_str=repr_str).content
                                  for elem in val.content) + "}"
    elif T in ('module', 'function', 'builtin'):
        content = '[{}]'.format(T)
    elif repr_str and T == 'str':
        content = repr(val.content)
    else:
        content = str(val.content)

    return Node('str', content)
