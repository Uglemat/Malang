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
def getdocstring(func, state):
    """
    @ = Function

    Return the docstring for `Function`, or `nope` if there is no docstring for `Function`.
    """
    assert_type(func, ('builtin', 'function'), state)

    docstring = func.content.__doc__ if func._type == 'builtin' else func.content['docstring'] 

    if docstring is None:
        return Node('atom', 'nope')
    else:
        return Node('str', docstring)

@builtin("FuncsSetDocstring")
def setdocstring(tup, state):
    """
    @ = Docstring Function

    Return a new version of `Function` where the docstring is `Docstring`. It
    does not mutate `Function`. `Function` must be a malang function, it cannot
    be a builtin function.
    """
    assert_type(tup, 'tuple', state, tuplelength=2)

    new_docstring, func = tup.content

    assert_type(new_docstring, 'str', state)
    assert_type(func, 'function', state)


    function_content = func.content.copy()
    function_content['docstring'] = new_docstring.content
    return Node('function', function_content)


@builtin("FuncsGetFilename")
def getfilename(func, state):
    """
    @ = Function

    Return the filename where `Function` was defined. Does not work for builtin functions.
    """
    assert_type(func, 'function', state)
    return Node('str', func.content['filename'])

@builtin("FuncsSetFilename")
def setfilename(tup, state):
    """
    @ = Filename Function

    Return a new version of `Function` where the filename where 'Function' was created
    is reported to be `Filename`. It does not mutate `Function`. `Function` must be a
    malang function, it cannot be a builtin function.
    """
    assert_type(tup, 'tuple', state, tuplelength=2)

    new_filename, func = tup.content

    assert_type(new_filename, 'str', state)
    assert_type(func, 'function', state)


    function_content = func.content.copy()
    function_content['filename'] = new_filename.content
    return Node('function', function_content)

@builtin("Fmt")
def fmt(tup, state):
    """
    @ = {String, Thing1, ..., ThingN}

    Format `String` using the `Thing<N>`s. It will search for numbers
    between curly braces in `String` and replace them with the thing in
    that position.

    `Fmt {"{1} hay", "test"}` should return "test hay".
    `Fmt {"{2}{1} {1} hay", 42, "?"}` should return "?42 42 hay".

    You can optionally put "|<fillchar><direction><width>" at the end of the
    index in the curly braces, where `width` will set a minimum width,
    `direction` must be either "<" or ">" to specify which way to justify
    the value when `width` is longer than the value, and `fillchar` specifies
    what character to pad with then `width` is longer than the value.

    So `Fmt {"{1|->5}$", 32}` should return "---32$".
    """
    assert_type(tup, 'tuple', state)
    if not len(tup.content) >= 2:
        raise MalangError("Fmt needs a string and at least 1 thing", state)

    string = tup.content[0]
    assert_type(string, 'str', state)

    things = Node('tuple', tup.content[1:])

    def subfun(match):
        idx = int(match.group('idx'))
        thing = tuplenth(Node('tuple', (Node('number', idx), things)), state)
        string = tostr(thing).content

        if match.group('configs'):
            just = {'<': str.ljust, '>': str.rjust}[match.group('direction')]
            string = just(string, int(match.group('width')), match.group('fillchar'))
        return string

    pattern = (r"\{(?P<idx>[0-9]+)"
               r"(?P<configs>\|(?P<fillchar>.)(?P<direction>[<>])(?P<width>[0-9]+))?\}")
    return Node('str', re.sub(pattern, subfun, string.content))

@builtin("Print")
def _print(s, state):
    """
    @ = Val

    Print `Val`.
    """
    
    print(s.content if s._type == 'str' else
          tostr(s, state).content, end="")
    return s

@builtin("Writefile")
def writefile(arg, state):
    """
    @ = Str Filename

    Overwrite the contents of `Filename` to be `Str`.
    """
    assert_type(arg, 'tuple', state, tuplelength=2)
    _str, fname = arg.content
    assert_type(_str, 'str',  state)
    assert_type(fname, 'str', state)

    try:
        with open(fname.content, mode="w") as f:
            f.write(_str.content)
    except IOError as e:
        raise MalangError(e.args[1], state)

    return Node('atom', 'ok')


@builtin("Readfile")
def readfile(fname, state):
    """
    @ = Filename

    Read the text in `Filename` and return the resulting string.
    """
    assert_type(fname, 'str', state)
    try:
        with open(fname.content) as f:
            return Node('str', f.read())
    except IOError as e:
        raise MalangError(e.args[1], state)

@builtin("Help")
def _help(fun, state):
    """
    @ = Func

    Prints information about the function `Func`.
    Alternatively, if `Func` is a module, it will list
    all the functions inside that module.
    """
    assert_type(fun, ('function', 'builtin', 'module'), state)

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
def type_of(thing, state):
    """
    @ = Thing

    Returns a string representation of the type of `Thing`
    """
    return Node('str', thing._type)

@builtin("Input")
def _input(prompt, state):
    """
    @ = Prompt

    Get input from the user, with the prompt `Prompt`.
    """
    assert_type(prompt, 'str', state)
    return Node('str', input(prompt.content))

@builtin("Sleep")
def sleep(amount, state):
    """
    @ = Ms

    Pause execution for `Ms` milliseconds.
    """
    assert_type(amount, 'number', state)
    time.sleep(amount.content/1000.0)
    return Node('atom', 'ok')

@builtin("RandRange")
def randrange(tup, state):
    """
    @ = Start Stop

    Returns a random integer between `Start` (inclusive) and `Stop` (exclusive).
    """
    assert_type(tup, 'tuple', state, tuplelength=2)
    num1, num2 = tup.content
    assert_type(num1, 'number', state)
    assert_type(num2, 'number', state)
    return Node('number', random.randrange(num1.content,num2.content))

@builtin("Env")
def list_env(arg, state):
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
            return list_env(None, state.newenv(arg.content))
        else:
            print("\tIdentifier bindings in current environment:")

    if not len(state.env.bindings.keys()):
        print("No bindings in module", end="")
    else:
        for num, (identifier, value) in enumerate(state.env.bindings.items()):
            valstr = tostr(value, state, repr_str=True).content
            if len(valstr) > 15:
                valstr = valstr[:15] + "..."
            print("{:<40}".format(
                "{:<15} => {} ".format(identifier, valstr)
            ), end="\n" if num % 2 == 1 else "")
    print()

    if not state.env.parent is None:
        print("\tShowing bindings for parent environment below:")
        return list_env(None, state.newenv(state.env.parent))
    return Node('atom', 'ok')

@builtin("ClearEnv")
def clear_env(_arg, state):
    """
    @ = <whatever>

    Removes the identifier bindings in the current environment.
    This doesn't remove identifier bindings from 'parent' environments.
    """
    state.env.clear()
    return Node('atom', 'ok')


@builtin("ToList")
def tolist(tup, state):
    """
    @ = Tuple

    Convert `Tuple` to a malang list.
    """
    assert_type(tup, 'tuple', state)
    if tup.content == ():
        return Node('atom', 'nil')
    else:
        return Node('tuple', (tup.content[0],
                              tolist(Node('tuple', tup.content[1:]), state)))

@builtin("ToStr")
def tostr(val, state=None, depth=0, repr_str=False):
    """
    @ = Val

    Convert `Val` to a string. Won't go deeper than ~50 for nested tuples.
    If `Val` already is a string, this does nothing.
    """
    T = val._type
    if T == 'tuple' and depth > 50:
        content = "{...}"
    elif T == 'tuple':
        content = "{" + ", ".join(tostr(elem, state, depth+1, repr_str=True).content
                                  for elem in val.content) + "}"
    elif T in ('module', 'function', 'builtin'):
        content = '[{}]'.format(T)
    elif repr_str and T == 'str':
        content = stringrepr(val, state).content
    else:
        content = str(val.content)

    return Node('str', content)

@builtin("StringRepr")
def stringrepr(string, state):
    """
    @ = String

    Replaces tabs with \\t, newlines with \\n, backslashes with \\\\
    and double-quotes with \\\" in `String`, wraps that in double-quotes
    and returns the result.
    """
    assert_type(string, 'str', state)
    return Node('str', '"{}"'.format(
        string.content.replace('\\', r'\\').replace('"', r'\"').replace('\t', r'\t').replace('\n', r'\n')))

@builtin("TupleNth")
def tuplenth(arg, state):
    """
    @ = N Tuple

    Returns the `N`th element of `Tuple`. (Starting from 1)
    """
    assert_type(arg, 'tuple', state, tuplelength=2)
    N, actual_tuple = arg.content
    assert_type(N, 'number', state)
    assert_type(actual_tuple, 'tuple', state)
    
    try:
        idx = N.content-1
        if idx < 0:
            raise MalangError("Indices start at 1", state)
        return actual_tuple.content[idx]
    except IndexError:
        raise MalangError("Tuple index out of range", state)


@builtin("TupleSortwith")
def tuplesortwith(arg, state):
    """
    @ = Func Tuple

    Sort `Tuple` with the comparison function `Func`.

    `Func` should take an argument of the form `{Val_1, Val_2}`, and return
    `0` if they should be considered equal, `1` if `Val_1` should be
    considered 'higher', and `-1` if `Val_1` should be considered 'lower'.
    'higher' values goes to the end of the new tuple, 'lower' values go to
    the beginning.
    """
    assert_type(arg, 'tuple', state, tuplelength=2)
    func, tup = arg.content
    assert_type(func, ('function', 'builtin'), state)
    assert_type(tup, 'tuple', state)

    def _cmp(val_1, val_2):
        num = call_malang_func(func, Node('tuple', (val_1, val_2)), state)
        assert_type(num, 'number', state)
        return num.content

    return Node('tuple', tuple(sorted(tup.content, key=functools.cmp_to_key(_cmp))))

@builtin("TupleLength")
def tuplelength(tup, state):
    """
    @ = Tuple
    
    Return the length of `Tuple`
    """
    assert_type(tup, 'tuple', state)
    return Node('number', len(tup.content))

@builtin("StringLength")
def stringlength(st, state):
    """
    @ = String
    
    Return the length of `String`
    """
    assert_type(st, 'str', state)
    return Node('number', len(st.content))

@builtin("StringNth")
def stringnth(tup, state):
    """
    @ = N String
    
    Return the the `N`th character of `String`.
    """
    assert_type(tup, 'tuple', state, tuplelength=2)
    N, string = tup.content
    assert_type(N, 'number', state)
    assert_type(string, 'str', state)


    try:
        idx = N.content-1
        if idx < 0:
            raise MalangError("Indices start at 1", state)
        return Node('str', string.content[idx])
    except IndexError:
        raise MalangError("String index out of range", state)

@builtin("StringUpper")
def stringupper(st, state):
    """
    @ = String
    
    Return a version of `String` with uppercase characters.
    """
    assert_type(st, 'str', state)
    return Node('str', st.content.upper())

@builtin("StringLower")
def stringlower(st, state):
    """
    @ = String
    
    Return a version of `String` with lowercase characters.
    """
    assert_type(st, 'str', state)
    return Node('str', st.content.lower())


@builtin("StringFind")
def stringFind(tup, state):
    """
    @ = String1 String2
    
    Finds the first occurrence of `String1` in `String2`, and returns
    the index of that occurrence in `String2`.

    If no occurrence is found, it returns `nope`.
    """
    assert_type(tup, 'tuple', state, tuplelength=2)
    s1, s2 = tup.content
    assert_type(s1, 'str', state)
    assert_type(s2, 'str', state)

    result = s2.content.find(s1.content)
    if result == -1:
        return Node('atom', 'nope')
    else:
        return Node('number', result + 1)


@builtin("StringSlice")
def stringslice(tup, state):
    """
    @ = Start Stop String
    
    Returns a substring of `String` starting at index `Start` (inclusive),
    ending at index `Stop` (exclusive).
    """
    assert_type(tup, 'tuple', state, tuplelength=3)
    start, stop, string = tup.content
    assert_type(start,  'number', state)
    assert_type(stop,   'number', state)
    assert_type(string, 'str', state)

    if start.content < 1 or stop.content < 1:
        raise MalangError("Indices start at 1", state)

    return Node('str', string.content[start.content-1:stop.content-1])

@builtin("TupleSlice")
def tupleslice(tup, state):
    """
    @ = Start Stop Tuple
    
    Returns a subtuple of `Tuple` starting at index `Start` (inclusive),
    ending at index `Stop` (exclusive).
    """
    assert_type(tup, 'tuple', state, tuplelength=3)
    start, stop, actual_tuple = tup.content
    assert_type(start,  'number', state)
    assert_type(stop,   'number', state)
    assert_type(actual_tuple, 'tuple',  state)

    if start.content < 1 or stop.content < 1:
        raise MalangError("Indices start at 1", state)

    return Node('tuple', actual_tuple.content[start.content-1:stop.content-1])

@builtin("StringRstrip")
def stringrstrip(tup, state):
    """
    @ = Chars String
    
    Return `String` with all trailing characters that appear in the string `Chars` removed.
    """
    assert_type(tup, 'tuple', state, tuplelength=2)
    chars, string = tup.content
    assert_type(chars,  'str', state)
    assert_type(string, 'str', state)

    return Node('str', string.content.rstrip(chars.content))

@builtin("StringLstrip")
def stringlstrip(tup, state):
    """
    @ = Chars String
    
    Return `String` with all leading characters that appear in the string `Chars` removed.
    """
    assert_type(tup, 'tuple', state, tuplelength=2)
    chars, string = tup.content
    assert_type(chars,  'str', state)
    assert_type(string, 'str', state)

    return Node('str', string.content.lstrip(chars.content))

@builtin("StringSplit")
def stringsplit(tup, state):
    """
    @ = Sep String
    
    Splits `String` into a list of strings, using the non-empty string `Sep` as the delimiter.
    """
    assert_type(tup, 'tuple', state, tuplelength=2)
    sep, string = tup.content
    assert_type(sep,    'str', state)
    assert_type(string, 'str', state)

    if sep.content == "":
        raise MalangError("The separator cannot be the empty string", state)

    return python_list_to_malang_list(
        list(map(lambda s: Node('str', s), string.content.split(sep.content)))
    )

@builtin("Exit")
def exit_malang(_arg, state):
    """
    @ = <whatever>

    Exit the interpreter.
    """
    exit()

@builtin("Error")
def error(arg, state):
    """
    @ = Reason

    Stop executing because of `Reason`. `Reason` can be any type.
    """
    raise MalangError(tostr(arg).content, state)

@builtin("StringToNumber")
def string_to_number(string, state):
    """
    @ = String

    Convert the `String` to a number. `String` can be of the form `<base>#<numeral>`
    where `base` can be from 2 (binary) to 16 (hexadecimal), or just `<numberal>`,
    where a base of 10 will be assumed. You can write numbers in your malang programs
    this way too.
    """
    assert_type(string, 'str', state)
    try:
        return Node('number', to_number(string.content))
    except ValueError:
        raise MalangError("Can't convert string to number", state)
