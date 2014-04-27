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
from utils import Node, assert_type, MalangError, python_list_to_malang_list, to_number
from evaluator import call_malang_func
import time
import random
import re
import functools

"""
Some of the builtin funcs are curried when they are placed in
modules, so the docstring might reflect that.
"""

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


@builtin("FuncsGetDocstring")
def getdocstring(func, env, filename, infonode):
    """
    @ = Function

    Return the docstring for `Function`, or `sorry` if there is no docstring for `Function`.
    """
    assert_type(func, ('builtin', 'function'), filename, infonode)

    docstring = func.content.__doc__ if func._type == 'builtin' else func.content['docstring'] 

    if docstring is None:
        return Node('atom', 'sorry')
    else:
        return Node('str', docstring)

@builtin("FuncsSetDocstring")
def setdocstring(tup, env, filename, infonode):
    """
    @ = Docstring Function

    Return a new version of `Function` where the docstring is `Docstring`. It
    does not mutate `Function`. `Function` must be a malang function, it cannot
    be a builtin function.
    """
    assert_type(tup, 'tuple', filename, infonode, tuplelength=2)

    new_docstring, func = tup.content

    assert_type(new_docstring, 'str', filename, infonode)
    assert_type(func, 'function', filename, infonode)


    function_content = func.content.copy()
    function_content['docstring'] = new_docstring.content
    return Node('function', function_content)


@builtin("Fmt")
def fmt(s, env, filename, infonode):
    """
    @ = String

    Format `String`. It will search for identifiers between curly
    braces in `String`, lookup the value in the environment of the caller
    (it can do that because it's a builtin function) and replace it with a
    string representation of the value.

    So `S := "test", Fmt "{S} hay"` should return "test hay"

    You can optionally put "|<fillchar><direction><width>" at the end of the
    identifier in the curly braces, where `width` will set a minimum width,
    `direction` must be either "<" or ">" to specify which way to justify
    the value when `width` is longer than the value, and `fillchar` specifies
    what character to pad with then `width` is longer than the value.

    So `N := 32, Fmt "{N|0>5}$"` should return "00032$".

    You can not nest these things.
    """
    assert_type(s, 'str', filename, infonode)
    def subfun(match):
        string = tostr(env.get(match.group('id').strip(), filename, infonode)).content

        if match.group('configs'):
            just = {'<': str.ljust, '>': str.rjust}[match.group('direction')]
            string = just(string, int(match.group('width')), match.group('fillchar'))
        return string

    pattern = (r"\{(?P<id>[^|{}]+)"
               r"(?P<configs>\|(?P<fillchar>.)(?P<direction>[<>])(?P<width>[0-9]+))?\}")
    return Node('str', re.sub(pattern, subfun, s.content))

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
    @ = Str Filename

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
        for ident, val in sorted(mod_env.bindings.items()):
            if val._type in ('function', 'builtin'):
                print("  ", ident)
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

@builtin("RandRange")
def randrange(tup, env, filename, infonode):
    """
    @ = Start Stop

    Returns a random integer between `Start` (inclusive) and `Stop` (exclusive).
    """
    assert_type(tup, 'tuple', filename, infonode, tuplelength=2)
    num1, num2 = tup.content
    assert_type(num1, 'number', filename, infonode)
    assert_type(num2, 'number', filename, infonode)
    return Node('number', random.randrange(num1.content,num2.content))

@builtin("Env")
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
            print("\tIdentifier bindings in module environment:")
            return list_env(None, arg.content, filename, infonode)
        else:
            print("\tIdentifier bindings in current environment:")

    if not len(env.bindings.keys()):
        print("No bindings in module", end="")
    else:
        for num, (identifier, value) in enumerate(env.bindings.items()):
            valstr = tostr(value, env, repr_str=True).content
            if len(valstr) > 15:
                valstr = valstr[:15] + "..."
            print("{:<40}".format(
                "{:<15} => {} ".format(identifier, valstr)
            ), end="\n" if num % 2 == 1 else "")
    print()

    if not env.parent is None:
        print("\tShowing bindings for parent environment below:")
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

    Convert `Val` to a string. Won't go deeper than ~50 for nested tuples.
    If `Val` already is a string, this does nothing.
    """
    T = val._type
    if T == 'tuple' and depth > 50:
        content = "{...}"
    elif T == 'tuple':
        content = "{" + ", ".join(tostr(elem, env, filename, infonode,
                                        depth=depth+1, repr_str=True).content
                                  for elem in val.content) + "}"
    elif T in ('module', 'function', 'builtin'):
        content = '[{}]'.format(T)
    elif repr_str and T == 'str':
        content = stringrepr(val, env, filename, infonode).content
    else:
        content = str(val.content)

    return Node('str', content)

@builtin("StringRepr")
def stringrepr(string, env, filename, infonode):
    """
    @ = String

    Replaces tabs with \\t, newlines with \\n, backslashes with \\\\
    and double-quotes with \\\" in `String`, wraps that in double-quotes
    and returns the result.
    """
    assert_type(string, 'str', filename, infonode)
    return Node('str', '"{}"'.format(
        string.content.replace('\\', r'\\').replace('"', r'\"').replace('\t', r'\t').replace('\n', r'\n')))

@builtin("TupleNth")
def tuplenth(arg, env, filename, infonode):
    """
    @ = N Tuple

    Returns the `N`th element of `Tuple`. (Starting from 1)
    """
    assert_type(arg, 'tuple', filename, infonode, tuplelength=2)
    N, actual_tuple = arg.content
    assert_type(N, 'number', filename, infonode)
    assert_type(actual_tuple, 'tuple', filename, infonode)
    
    try:
        idx = N.content-1
        if idx < 0:
            raise MalangError("Indices start at 1", filename, infonode)
        return actual_tuple.content[idx]
    except IndexError:
        raise MalangError("Tuple index out of range", filename, infonode)


@builtin("TupleSortwith")
def tuplesortwith(arg, env, filename, infonode):
    """
    @ = Func Tuple

    Sort `Tuple` with the comparison function `Func`.

    `Func` should take an argument of the form `{Val_1, Val_2}`, and return
    `0` if they should be considered equal, `1` if `Val_1` should be
    considered 'higher', and `-1` if `Val_1` should be considered 'lower'.
    'higher' values goes to the end of the new tuple, 'lower' values go to
    the beginning.
    """
    assert_type(arg, 'tuple', filename, infonode, tuplelength=2)
    func, tup = arg.content
    assert_type(func, ('function', 'builtin'), filename, infonode)
    assert_type(tup, 'tuple', filename, infonode)

    def _cmp(val_1, val_2):
        num = call_malang_func(func, Node('tuple', (val_1, val_2)),
                               env, filename, infonode)
        assert_type(num, 'number', filename, infonode)
        return num.content

    return Node('tuple', tuple(sorted(tup.content, key=functools.cmp_to_key(_cmp))))

@builtin("TupleLength")
def tuplelength(tup, env, filename, infonode):
    """
    @ = Tuple
    
    Return the length of `Tuple`
    """
    assert_type(tup, 'tuple', filename, infonode)
    return Node('number', len(tup.content))

@builtin("StringLength")
def stringlength(st, env, filename, infonode):
    """
    @ = String
    
    Return the length of `String`
    """
    assert_type(st, 'str', filename, infonode)
    return Node('number', len(st.content))

@builtin("StringNth")
def stringnth(tup, env, filename, infonode):
    """
    @ = N String
    
    Return the the `N`th character of `String`.
    """
    assert_type(tup, 'tuple', filename, infonode, tuplelength=2)
    N, string = tup.content
    assert_type(N, 'number', filename, infonode)
    assert_type(string, 'str', filename, infonode)


    try:
        idx = N.content-1
        if idx < 0:
            raise MalangError("Indices start at 1", filename, infonode)
        return Node('str', string.content[idx])
    except IndexError:
        raise MalangError("String index out of range", filename, infonode)

@builtin("StringUpper")
def stringupper(st, env, filename, infonode):
    """
    @ = String
    
    Return a version of `String` with uppercase characters.
    """
    assert_type(st, 'str', filename, infonode)
    return Node('str', st.content.upper())

@builtin("StringLower")
def stringlower(st, env, filename, infonode):
    """
    @ = String
    
    Return a version of `String` with lowercase characters.
    """
    assert_type(st, 'str', filename, infonode)
    return Node('str', st.content.lower())


@builtin("StringFind")
def stringFind(tup, env, filename, infonode):
    """
    @ = String1 String2
    
    Finds the first occurrence of `String1` in `String2`, and returns
    the index of that occurrence in `String2`.

    If no occurrence is found, it returns `sorry`.
    """
    assert_type(tup, 'tuple', filename, infonode, tuplelength=2)
    s1, s2 = tup.content
    assert_type(s1, 'str', filename, infonode)
    assert_type(s2, 'str', filename, infonode)

    result = s2.content.find(s1.content)
    if result == -1:
        return Node('atom', 'sorry')
    else:
        return Node('number', result + 1)


@builtin("StringSlice")
def stringslice(tup, env, filename, infonode):
    """
    @ = Start Stop String
    
    Returns a substring of `String` starting at index `Start` (inclusive),
    ending at index `Stop` (exclusive).
    """
    assert_type(tup, 'tuple', filename, infonode, tuplelength=3)
    start, stop, string = tup.content
    assert_type(start,  'number', filename, infonode)
    assert_type(stop,   'number', filename, infonode)
    assert_type(string, 'str', filename, infonode)

    if start.content < 1 or stop.content < 1:
        raise MalangError("Indices start at 1", filename, infonode)

    return Node('str', string.content[start.content-1:stop.content-1])

@builtin("TupleSlice")
def tupleslice(tup, env, filename, infonode):
    """
    @ = Start Stop Tuple
    
    Returns a subtuple of `Tuple` starting at index `Start` (inclusive),
    ending at index `Stop` (exclusive).
    """
    assert_type(tup, 'tuple', filename, infonode, tuplelength=3)
    start, stop, actual_tuple = tup.content
    assert_type(start,  'number', filename, infonode)
    assert_type(stop,   'number', filename, infonode)
    assert_type(actual_tuple, 'tuple',  filename, infonode)

    if start.content < 1 or stop.content < 1:
        raise MalangError("Indices start at 1", filename, infonode)

    return Node('tuple', actual_tuple.content[start.content-1:stop.content-1])

@builtin("StringRstrip")
def stringrstrip(tup, env, filename, infonode):
    """
    @ = Chars String
    
    Return `String` with all trailing characters that appear in the string `Chars` removed.
    """
    assert_type(tup, 'tuple', filename, infonode, tuplelength=2)
    chars, string = tup.content
    assert_type(chars,  'str', filename, infonode)
    assert_type(string, 'str', filename, infonode)

    return Node('str', string.content.rstrip(chars.content))

@builtin("StringLstrip")
def stringlstrip(tup, env, filename, infonode):
    """
    @ = Chars String
    
    Return `String` with all leading characters that appear in the string `Chars` removed.
    """
    assert_type(tup, 'tuple', filename, infonode, tuplelength=2)
    chars, string = tup.content
    assert_type(chars,  'str', filename, infonode)
    assert_type(string, 'str', filename, infonode)

    return Node('str', string.content.lstrip(chars.content))

@builtin("StringSplit")
def stringsplit(tup, env, filename, infonode):
    """
    @ = Sep String
    
    Splits `String` into a list of strings, using the non-empty string `Sep` as the delimiter.
    """
    assert_type(tup, 'tuple', filename, infonode, tuplelength=2)
    sep, string = tup.content
    assert_type(sep,    'str', filename, infonode)
    assert_type(string, 'str', filename, infonode)

    if sep.content == "":
        raise MalangError("The separator cannot be the empty string", filename, infonode)

    return python_list_to_malang_list(
        list(map(lambda s: Node('str', s), string.content.split(sep.content)))
    )

@builtin("Exit")
def exit_malang(_arg, env, filename, infonode):
    """
    @ = <whatever>

    Exit the interpreter.
    """
    exit()

@builtin("Error")
def error(arg, env, filename, infonode):
    """
    @ = Reason

    Stop executing because of `Reason`. `Reason` can be any type.
    """
    raise MalangError(tostr(arg).content, filename, infonode)

@builtin("StringToNumber")
def string_to_number(string, env, filename, infonode):
    assert_type(string, 'str', filename, infonode)
    try:
        return Node('number', to_number(string.content))
    except ValueError:
        raise MalangError("Can't convert string to number", filename, infonode)
