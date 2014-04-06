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
import re, operator, sys
from os import path


sys.setrecursionlimit(5000)
    



def equal(val_1, val_2):
    if val_1._type != val_2._type:
        return False
    T = val_1._type
    if T == 'tuple':
        return (len(val_1.content) == len(val_2.content) and
                all(equal(v1, v2) for (v1, v2) in zip(val_1.content, val_2.content)))
    else:
        return val_1.content == val_2.content

def greater_than(val_1, val_2):

    # Assumes val_1 and val_2 are of the same _type
    T = val_1._type

    if T == 'tuple':
        if len(val_1.content) == 0:
            return False
        elif len(val_2.content) == 0:
            return True
        else:
            return (greater_than(val_1.content[0], val_2.content[0]) or
                    greater_than(Node('tuple', val_1.content[1:]),
                                 Node('tuple', val_2.content[1:])))
    else:
        return val_1.content > val_2.content

def less_than(val_1, val_2):
    # Assumes val_1 and val_2 are of the same _type
    return not greater_than(val_1, val_2) and not equal(val_1, val_2)

def greater_than_or_eq(val_1, val_2):
    # Assumes val_1 and val_2 are of the same _type
    return greater_than(val_1, val_2) or equal(val_1, val_2)

def less_than_or_eq(val_1, val_2):
    # Assumes val_1 and val_2 are of the same _type
    return less_than(val_1, val_2) or equal(val_1, val_2)


def transform_list_to_tuple(elems, infonode):
    """
    `elems` may or may not have been evaluated, this function should work either way
    """
    if len(elems) == 0:
        return Node('atom', 'nil', infonode=infonode)
    else:
        return Node('tuple', (elems[0], transform_list_to_tuple(elems[1:], infonode)),
                    infonode=infonode)


def generate_items(malang_list, filename):
    """
    A generator that yields all the elements of a malang list
    """
    while not is_nil(malang_list):
        utils.assert_type(malang_list, 'tuple', filename, infonode=malang_list, tuplelength=2)
        head, malang_list = malang_list.content
        yield head

def is_nil(v):
    return hasattr(v, '_type') and v._type == 'atom' and v.content == 'nil'

def eval_list_comprehension(env, expr, emitters, filename, acc):
    """
    This evaluates the list comprehension, and mutates `acc`, which should be
    a python list. `expr` is the expression on the left hand side of the pipe
    in a list comprehension (like #[<expr> | ...]). Emitters is a python tuple
    of `emitters`, which are the things on the right hand side of a list comprehension,
    seperated by commas (like #[<expr> | <emitter1> , <emitter2>, ... <emitterN> ].
    An emitter can either be `filter`, which restricts what goes into the list, or
    or an actual emitter, which is a pattern combined with an expression that
    evaluates to a list, and the pattern is pattern matched with each element of the list
    one at a time.
    """

    emitter = emitters[0]
    last_emitter = len(emitters) <= 1

    if emitter._type == 'filter':
        result = trampoline(emitter.content, env, filename)
        if result._type == 'atom' and result.content == 'true':
            if last_emitter:
                acc.append(trampoline(expr, env, filename))
            else:
                eval_list_comprehension(env, expr, emitters[1:], filename, acc)

    elif emitter._type == 'emitter':
        pattern     = emitter.content['pattern']
        malang_list = trampoline(emitter.content['expr'], env, filename)


        if last_emitter:
            for item in generate_items(malang_list, filename):
                temp_env = env.shallow_copy()
                patternmatch(pattern, item, temp_env, filename)
                acc.append(trampoline(expr, temp_env, filename))
        else:
            for item in generate_items(malang_list, filename):
                temp_env = env.shallow_copy()
                patternmatch(pattern, item, temp_env, filename)
                eval_list_comprehension(temp_env, expr, emitters[1:], filename, acc=acc)


def patternmatch(pattern, expr, env, filename):
    """ `expr` should've been evaluated, `pattern` should not yet have been evaluated.
    Keep in mind that syntactially, 'pattern' can be any expression, like a function definition
    or whatever, even though patternmatching only works on simpler things like numbers and tuples.
    """
    exception = utils.InvalidMatch("Invalid match", filename, infonode=pattern)


    if pattern._type == 'uminus' and pattern.content._type == 'number':
        # Special rule for negative numbers, otherwise I wouldn't be able to match
        # them.
        pattern = pattern.content
        pattern.content = -pattern.content


    if pattern._type == 'list':
        return patternmatch(transform_list_to_tuple(pattern.content, infonode=pattern),
                            expr, env, filename)

    elif env.is_unbound_identifier(pattern):
        env.bind(pattern.content, expr)
        return

    elif pattern._type == 'id' and env.is_bound(pattern.content):
        val = env.get(pattern.content, filename, infonode=pattern)
        if not equal(val, expr):
            raise exception
        return

    elif pattern._type != expr._type:
        raise exception

    elif pattern._type == 'tuple':
        if not len(pattern.content) == len(expr.content):
            raise exception
        for pat, e in zip(pattern.content, expr.content):
            patternmatch(pat, e, env, filename)
        return

    elif pattern._type in ('number', 'str', 'atom'):
        if not pattern.content == expr.content:
            raise exception
        return 
        
    raise MalangError("Don't know how to pattern-match that. ", filename, infonode=pattern)

arithmetic_funcs = {
    'plus':   operator.add,
    'minus':  operator.sub,
    'divide': operator.floordiv,
    'times':  operator.mul,
    'modulo': operator.mod
}

cmp_funcs = {
    'gt': greater_than,
    'lt': less_than,
    'ge': greater_than_or_eq,
    'le': less_than_or_eq,
    'eq': equal,
    'ne': lambda op1, op2: not equal(op1, op2)
}

def thunk(func, *args, **kwargs):
    def _f():
        return func(*args, **kwargs)
    return _f

def eval_malang(code, env, filename):
    return trampoline(parse(code, filename), env, filename)

def trampoline(expr, env, filename):
    """
    This trampoline function is necessary for the purpose of implementing tail call elimination.
    When `maval' is about to evaluate an expression in tail position, it returns a thunk
    instead of recursing deeper into itself, thus making sure that the python call stack
    doesn't grow huge. So it's like `maval' is 'jumping' on this trampoline.
    """
    result = maval(expr, env, filename)
    while callable(result):
        result = result()
    return result

def maval(expr, env, filename):
    """
    This is the evaluation function. I didn't want to shadow the python builtin function `eval', so I called it
    `maval' instead. `expr' is the abstract syntax tree that has been created with Python Lex-Yacc.
    `env' is an Env instance, storing the identifier bindings for malang. It will be mutated inside `maval'.
    `maval' can return:
    (1) A 'Node' instance, signifying a value, it could for example return Node('number', 3), which could maybe
    be the result of evaluating the expression Node('plus', (Node('number' 2), Node('number', 1))).
    (2) A thunk (i.e. a function meant to be called later, delaying some computation).
    This is used to implement tail call elimination; `maval' can't just naively recurse deeper because python
    itself has no tail call elimination, and python's call stack would explode.

    Don't call `maval' directly, call `eval_malang' instead (because `eval_malang' never returns a thunk,
    only Node() values).
    """

    T = expr._type
    if T in ('number', 'str', 'atom', 'function', 'builtin'):
        return expr

    elif T == 'uminus':
        op = trampoline(expr.content, env, filename)
        if not op._type == 'number':
            raise MalangError("Invalid arithmetic expression", filename, infonode=expr)
        return Node('number', -op.content, infonode=expr)

    elif T in ('plus', 'minus', 'divide', 'times', 'modulo'):

        op1, op2 = (trampoline(op, env, filename) for op in expr.content)
        if op1._type == op2._type:
            _type = op1._type

            if _type == 'number':
                return Node('number', arithmetic_funcs[T](op1.content, op2.content), infonode=expr)
            elif _type == 'str' and T == 'plus':
                return Node('str', op1.content + op2.content, infonode=expr)
            elif _type == 'tuple' and T == 'plus':
                return Node('tuple', op1.content + op2.content, infonode=expr)
        else:
            raise MalangError("Invalid arithmetic expression", filename, infonode=expr)


    elif T in ('gt', 'lt', 'ge', 'le'):
        op1, op2 = (trampoline(op, env, filename) for op in expr.content)
        if op1._type == op2._type and op1._type in ('number', 'str', 'tuple', 'atom'):
            return Node('atom',
                        {True: 'true', False: 'false'}[cmp_funcs[T](op1, op2)],
                        infonode=expr
            )
        else:
            raise MalangError("Invalid comparison expression", filename, infonode=expr)
    elif T in ('eq', 'ne'):
        op1, op2 = (trampoline(op, env, filename) for op in expr.content)
        return Node('atom', {True: 'true', False: 'false'}[cmp_funcs[T](op1, op2)], infonode=expr)

    elif T == 'tuple':
        return Node('tuple', tuple(trampoline(e, env, filename) for e in expr.content), infonode=expr)

    elif T == 'list':
        elems = tuple(trampoline(e, env, filename) for e in expr.content)
        return transform_list_to_tuple(elems, expr)

    elif T == 'list_comprehension':
        result = []
        leftside_expr, emitters = expr.content

        eval_list_comprehension(env, leftside_expr, emitters, filename, acc=result)
        return utils.python_list_to_malang_list(result)


    elif T == 'bind':
        pattern, e = expr.content
        val = trampoline(e, env, filename)
        patternmatch(pattern, val, env, filename)
        return val

    elif T == 'id':
        return env.get(expr.content, filename, infonode=expr)

    elif T == 'module_access':
        module = trampoline(expr.content[0], env, filename)
        assert module._type == 'module', "That's not a module"
        val = trampoline(expr.content[1], module.content, filename)
        return val

    elif T == 'composition':
        f1, f2 = (trampoline(e, env, filename) for e in expr.content)
        utils.assert_type(f1, ('builtin', 'function'), filename, expr)
        utils.assert_type(f2, ('builtin', 'function'), filename, expr)

        code = Node('program', [
            Node('program', [
                Node('fncall', (
                    f1,
                    Node('fncall', (f2, Node('id', '@'))))
                 )
            ])
        ])

        return Node('function', {
            'code': code,
            'filename': filename,
            'docstring': None,
            'parent_env': env
        })

    elif T == 'fncall':
        func, arg = (trampoline(e, env, filename) for e in expr.content)
        utils.assert_type(func, ('builtin', 'function'), filename, expr)
        if func._type == 'builtin':
            return func.content(arg, env, filename, expr)
        elif func._type == 'function':
            return thunk(maval,
                         func.content['code'],
                         Env(parent=func.content['parent_env'], bindings={'@': arg}),
                         func.content['filename'])

    elif T == 'func_def':
        maybe_docstring = expr.content[0].content[0].content[0]
        if maybe_docstring._type == "str":
            docstring = maybe_docstring.content
        else:
            docstring = None
        return Node('function', {'code': expr.content[0],
                                 'filename': expr.content[1],
                                 'docstring': docstring,
                                 'parent_env': env}, infonode=expr)

    elif T == 'program':
        start = expr.content[:-1]
        last  = expr.content[-1]
        for e in start:
            trampoline(e, env, filename)
        return thunk(maval, last, env, filename)

    elif T == 'case_of':
        val = trampoline(expr.content['matched_expr'], env, filename)
        for arrow in expr.content['arrow_list']:
            env_copy = env.shallow_copy()
            try:
                patternmatch(arrow['pattern'], val, env_copy, filename)
            except utils.InvalidMatch:
                continue
            return thunk(maval, arrow['expr'], env_copy, filename)
        raise MalangError("No pattern matched in case_of expression", filename, infonode=expr)

    raise MalangError("Unknown expression {!r}".format(utils.AST_to_str(expr)), filename, infonode=expr)




# The reason I don't define `require` in the builtin_funcs module is that I need access to
# `main_env` inside `require`.
def require(filename_to_open, env, filename, infonode):
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
# The main environment, with the "Builtins" module already present.
main_env = Env(bindings={'Builtins': builtins_module})



stdlib_location = path.join(path.dirname(path.abspath(__file__)), "init.malang")
with open(stdlib_location) as f:
    with change_directory(path.dirname(path.abspath(__file__))):
        eval_malang(f.read(), main_env, stdlib_location)

if __name__ == "__main__":
    interpreter_env = Env(parent=main_env)

    if readline_imported:
        import readline
        readline.set_completer(utils.Completer(interpreter_env))

    if len(sys.argv) == 1:
        print("Usage: $ malang [-i] <file1> <file2> ... <fileN>")
        exit()


    interactive = "-i" in sys.argv
    if interactive:
        sys.argv.remove("-i")


    for filename in sys.argv[1:]:
        with open(filename) as f:
            with change_directory(path.dirname(path.abspath(filename))):
                eval_malang(f.read(), interpreter_env, filename)


    if interactive:

        color = lambda n: "\x1b[{}m".format(n)
        readlinecolor = lambda n: "\001{}\002".format(color(n))
        color_prompt = "{}Malang {}> {}".format(readlinecolor(36), readlinecolor(33), readlinecolor(32))

        while True:
            try:
                with utils.tab_completion():
                    inp = input(color_prompt)
            except EOFError:
                print(color(39))
                break
            if not inp:
                continue
            try:
                print(
                    builtin_funcs.tostr(
                        eval_malang(inp, interpreter_env, "<REPL>"),
                        repr_str=True
                    ).content,
                )
            except MalangError as e:
                print("{}Error: {}".format(color(31), e))
            finally:
                print(color(39), end="")
