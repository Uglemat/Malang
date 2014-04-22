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
import os


try:
    readline_imported = True
    import readline
except ImportError:
    readline_imported = False

def truthy(val):
    if val._type in ('tuple', 'str'):
        return len(val.content) != 0
    elif val._type == 'number':
        return val.content != 0
    elif val._type == 'atom' and val.content in ('nil', 'nope'):
        return False
    else:
        return True


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
    if val_1._type != val_2._type:
        return val_1._type > val_2._type

    elif val_1._type == 'tuple':
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
    return not greater_than(val_1, val_2) and not equal(val_1, val_2)


def greater_than_or_eq(val_1, val_2):
    return greater_than(val_1, val_2) or equal(val_1, val_2)


def less_than_or_eq(val_1, val_2):
    return not greater_than(val_1, val_2)




def get_lineno(infonode):
    if (hasattr(infonode, "lineno") and
        infonode.lineno is not None):
        return infonode.lineno
    return None

def AST_to_str(ast):
    if isinstance(ast, Node):
        if isinstance(ast.content, (list, tuple)):
            return "{}({})".format(ast._type, ", ".join(AST_to_str(elem) for elem in ast.content))
        elif isinstance(ast.content, Node):
            return "{}({})".format(ast._type, AST_to_str(ast.content))
        else:
            return "{}({})".format(ast._type, ast.content)
    else:
        return str(ast)



def is_nil(v):
    return hasattr(v, '_type') and v._type == 'atom' and v.content == 'nil'

def generate_items(malang_list, filename):
    """
    A generator that yields all the elements of a malang list
    """
    while not is_nil(malang_list):
        assert_type(malang_list, 'tuple', filename, infonode=malang_list, tuplelength=2)
        head, malang_list = malang_list.content
        yield head


def python_list_to_malang_list(_list):
    acc = Node('atom', 'nil')
    while _list:
        acc = Node('tuple', (_list.pop(), acc))
    
    return acc

def transform_list_to_tuple(elems, infonode):
    """
    `elems` may or may not have been evaluated, this function should work either way
    """
    if len(elems) == 0:
        return Node('atom', 'nil', infonode=infonode)
    else:
        return Node('tuple', (elems[0], transform_list_to_tuple(elems[1:], infonode)),
                    infonode=infonode)

def assert_type(node, types, filename, infonode, tuplelength=None):
    if isinstance(types, str):
        T = types
        if not node._type == T:
            raise MalangError("Incorrect type: got {!r}, expected {!r}.".format(node._type, T),
                              filename, infonode)
        elif node._type == 'tuple' and tuplelength is not None and len(node.content) != tuplelength:
            text = "Tuple has invalid length: got tuple of length {}, expected tuple of length {}".format(
                len(node.content), tuplelength)
            raise MalangError(text, filename, infonode)

    elif node._type not in types:
        raise MalangError("Incorrect type: got {!r}, expected one of {!r}.".format(
            node._type, types),
                          filename, infonode)

class MalangError(Exception):
    def __init__(self, text, filename, infonode=None):

        append = ""
        if get_lineno(infonode):
            append = " at line #{}".format(get_lineno(infonode))
        append += " in file {!r}".format(filename)

        value = Node('tuple', (Node('atom', 'error'), Node('str', text + append)))

        Exception.__init__(self, value)

class UnboundIdentifier(MalangError):
    pass

class InvalidMatch(MalangError):
    pass

class Node(object):
    def __init__(self, _type, content, lineno=None, infonode=None):
        self._type = _type
        self.content = content

        # `infonode` is used merely to track line numbers

        self.lineno = lineno
        if self.lineno is None:
            self.lineno = get_lineno(infonode)

    def __str__(self, recursed=False):
        if self._type in ('number', 'str'):
            return repr(self.content)
        elif self._type == 'atom':
            return self.content
        elif self._type == 'tuple':
            if recursed:
                return "{...}"
            else:
                return "{" + ", ".join(v.__str__(recursed=True) for v in self.content) + "}"

class Env(object):
    def __init__(self, parent=None, bindings=None):
        self.parent = parent
        self.bindings = {} if bindings is None else bindings
        
    def bind(self, name, val):
        if name != "_":
            self.bindings[name] = val

    def get(self, name, filename, infonode=None):
        if name in self.bindings:
            return self.bindings[name]
        elif self.parent is not None:
            return self.parent.get(name, filename, infonode=infonode)
        raise UnboundIdentifier("Identifier {!r} not bound to a value".format(name),
                                filename, infonode=infonode)

    def is_bound(self, name):
        try:
            self.get(name, filename=None)
            return True
        except UnboundIdentifier:
            return False

    def is_unbound_identifier(self, node):
        return node._type == 'id' and not self.is_bound(node.content)

    def shallow_copy(self):
        return Env(parent=self.parent, bindings=self.bindings.copy())
    
    def clear(self):
        self.bindings = {}

    def all_identifiers(self):
        return (set(self.bindings.keys()) | 
                (set() if self.parent is None else
                 self.parent.all_identifiers()))

class change_directory:
    """Context manager for changing the current working directory"""
    def __init__(self, new_path):
        self.new_path = new_path

    def __enter__(self):
        self.saved_path = os.getcwd()
        os.chdir(self.new_path)

    def __exit__(self, etype, value, traceback):
        os.chdir(self.saved_path)


class tab_completion:
    def __enter__(self):
        if readline_imported:
            readline.parse_and_bind("tab: complete")

    def __exit__(self, etype, value, traceback):
        """
        Couldn't find out how I'm supposed to disable tab completion,
        setting tab to something different seems to work.
        """
        if readline_imported:
            readline.parse_and_bind("tab: nothing")

class Completer:
    def __init__(self, env):
        self.env = env
        self.matches = []

    def __call__(self, text, state):
        if state == 0:
            self.matches = [var for var in self.env.all_identifiers()
                            if var.startswith(text)]
        try:
            return self.matches[state]
        except IndexError:
            return None

def cmp_to_key(mycmp):
    'Convert a cmp= function into a key= function'
    class K(object):
        def __init__(self, obj, *args):
            self.obj = obj
        def __lt__(self, other):
            return mycmp(self.obj, other.obj) < 0
        def __gt__(self, other):
            return mycmp(self.obj, other.obj) > 0
        def __eq__(self, other):
            return mycmp(self.obj, other.obj) == 0
        def __le__(self, other):
            return mycmp(self.obj, other.obj) <= 0
        def __ge__(self, other):
            return mycmp(self.obj, other.obj) >= 0
        def __ne__(self, other):
            return mycmp(self.obj, other.obj) != 0
    return K

