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
from utils import Node
import time
import random

builtins = {}
def builtin(name):
    def _f(func):
        builtins[name] = Node('builtin', func)
        return func
    return _f


@builtin("Print")
def _print(s, env):
    
    print(s.content if s._type == 'str' else
          to_str(s, env).content, end="")
    return s

@builtin("Sleep")
def sleep(t, env):
    time.sleep(t.content/1000.0)
    return Node('atom', 'ok')

@builtin("Random_Range")
def random_range(tup, env):
    assert tup._type == 'tuple' and len(tup.content) == 2
    num1, num2 = tup.content
    assert (num1._type, num2._type) == ('number', 'number')
    return Node('number', random.randrange(num1.content,num2.content))

@builtin("List_Env")
def list_env(_a, env):
    for k, v in env.bindings.items():
        print("{:<20} => {}".format(k, to_str(v, env).content))
    if not env.parent is None:
        return list_env(None, env.parent)
    return Node('atom', 'ok')

@builtin("To_List")
def to_list(tup, env):
    assert tup._type == 'tuple'
    if tup.content == ():
        return Node('atom', 'nil')
    else:
        return Node('tuple', (tup.content[0],
                              to_list(Node('tuple', tup.content[1:]), env)))

@builtin("To_Str")
def to_str(val, env, depth=0):
    T = val._type
    if T == 'tuple' and depth > 50:
        content = "{...}"
    elif T == 'tuple':
        content = "{" + ", ".join(to_str(elem, env, depth=depth+1).content
                                  for elem in val.content) + "}"
    elif T in ('module', 'function', 'builtin'):
        content = '[{}]'.format(T)
    elif T == 'str':
        content = repr(val.content)
    else:
        content = str(val.content)

    return Node('str', content)
