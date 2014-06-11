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
import itertools


try:
    readline_imported = True
    import readline
except ImportError:
    readline_imported = False

def to_number(string):
    if "#" in string:
        radix, val = string.split("#")
        radix = int(radix)
    else:
        val = string
        radix = 10

    return int(val, radix)

def tostr(val, depth=0):
    """
    returns a string representation of the malang value `val`. Won't go very deep for
    lists and tuples.
    """
    T = val._type
    if T == 'tuple' and depth > 50:
        return "{...}"
    elif T == 'tuple':
        return "{" + ", ".join(tostr(elem, depth+1) for elem in val.content) + "}"

    elif T == 'list':
        return "#[{}]".format(
            ", ".join(tostr(v, depth=depth+1) for v in generate_items(val)))

    elif T in ('module', 'function', 'builtin'):
        return '[{}]'.format(T)
    elif T == 'str':
        return stringrepr(val)
    else:
        return str(val.content)

def stringrepr(string):
    """
    return a string representation of the malang string `string`
    """
    return '"{}"'.format(
        string.content.replace('\\', r'\\').replace('"', r'\"').replace('\t', r'\t').replace('\n', r'\n'))

def truthy(val):
    if val._type in ('tuple', 'str'):
        return len(val.content) != 0
    elif val._type == 'number':
        return val.content != 0
    elif val._type == 'atom' and val.content == 'nope':
        return False
    elif val._type == 'list':
        return not is_nil(val)
    else:
        return True


def zip_lists(list1, list2):
    list1 = tuple(generate_items(list1))
    list2 = tuple(generate_items(list2))
    for z in zip(list1, list2):
        yield z

    if len(list1) > len(list2):
        yield list1[-1], None
    elif len(list2) > len(list1):
        yield None, list2[-1]


def zip_tuples(tuple1, tuple2):
    len1 = len(tuple1.content)
    len2 = len(tuple2.content)
    for i in range(max(len1, len2)):
        fst = None if i >= len1 else tuple1.content[i]
        snd = None if i >= len2 else tuple2.content[i]

        yield (fst, snd)

def flat_zip_malang_tuples(tuple1, tuple2):
    """
    tries to flatten the malang tuples completely
    and zip their values together, Padding with None
    when one tuple is shorter than another, and only
    flattening if *both* values are tuples. If a value
    is a tuple, and the corresponding value is not a tuple
    then those will just be zipped together, no more flattening.
    """
    pending = iter([  (tuple1, tuple2)  ])
    while True:
        current1, current2 = next(pending) # <- that might raise StopIteration, that is ok.
        if None in (current1, current2):
            yield (current1, current2)
        elif current1._type == current2._type == 'tuple':
            pending = itertools.chain(zip_tuples(current1, current2), pending)
        else:
            yield (current1, current2)


"""

The reason that I went through all the truble of defining the generators `zip_tuples` and
`flat_zip_malang_tuples` is that I wanted the comparison functions below to be able to
work on the 'old malang lists' that are very long (like 10k in length). I couldn't write
the functions recursive if I want to do that, because of python's recursion limit. So I
needed `flat_zip_malang_tuples` to flatten tuples that can be very deep 'trees' of tuples.

By 'old malang lists' I mean linked lists created out of two-tuples. This was how lists was implemented
previously in malang. I don't see a reason to remove the functionality described above, because it
might be useful for users who implement their own tree structure with tuples.

I used `None` in the generators above for padding, when one tuple is shorter than another.

"""

def compare_types(val1, val2):
    """
    compare two malang values of the *different* types, return (1, -1) if
    `val1` is (gt, lt) `val2`.
    """
    assert val1._type != val2._type
    return 1 if val1._type > val2._type else -1

def compare_vals(val1, val2):
    """
    compare two malang values, return (1, 0, -1) if `val1` is (gt, eq, lt) `val2`.
    """
    if val1._type != val2._type:
        return compare_types(val1, val2)

    T = val1._type

    if T in ('tuple', 'list'):
        zipped = flat_zip_malang_tuples(val1, val2) if T == 'tuple' else zip_lists(val1, val2)

        for fst, snd in zipped:
            if fst is None:   # `val1` is shorter, thus less than `val2`
                return -1
            elif snd is None: # `val1` is longer, thus greater than `val2`
                return 1
            comp = compare_vals(fst, snd)
            if comp != 0:
                return comp
        return 0
    elif T in ('function', 'builtin', 'module'):
        if val1.content == val2.content:
            return 0
        else:
            raise TypeError("Omfg!")
    else:
        return 1 if val1.content > val2.content else (-1 if val1.content < val2.content else 0)

def equal(val1, val2):
    return compare_vals(val1, val2) == 0


def greater_than(val1, val2):
    return compare_vals(val1, val2) == 1


def less_than(val1, val2):
    return compare_vals(val1, val2) == -1


def greater_than_or_eq(val1, val2):
    return compare_vals(val1, val2) in (1, 0)


def less_than_or_eq(val1, val2):
    return compare_vals(val1, val2) in (-1, 0)




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
    return v._type == 'list' and 'nil' == v.content

def generate_items(malang_list):
    """
    A generator that yields all the elements of a malang list
    """
    while not is_nil(malang_list):
        head, malang_list = malang_list.content
        yield head


def python_list_to_malang_list(_list):
    acc = Node('list', 'nil')
    while _list:
        acc = Node('list', (_list.pop(), acc))
    
    return acc

def transform_list_literal(elems, infonode):
    """
    `elems` may or may not have been evaluated, this function should work either way
    """
    if len(elems) == 0:
        return Node('list', 'nil', infonode=infonode)
    else:
        return Node('list', (elems[0], transform_list_literal(elems[1:], infonode)),
                    infonode=infonode)

def assert_type(node, types, state, tuplelength=None):
    if isinstance(types, str):
        T = types
        if not node._type == T:
            raise MalangError("Incorrect type: got {!r}, expected {!r}.".format(node._type, T), state)
        elif node._type == 'tuple' and tuplelength is not None and len(node.content) != tuplelength:
            text = "Tuple has invalid length: got tuple of length {}, expected tuple of length {}".format(
                len(node.content), tuplelength)
            raise MalangError(text, state)

    elif node._type not in types:
        raise MalangError("Incorrect type: got {!r}, expected one of {!r}.".format(
            node._type, types), state)

class MalangError(Exception):
    def __init__(self, text, state):

        append = ""
        if get_lineno(state.infonode):
            append = " at line #{}".format(get_lineno(state.infonode))
        append += " in file {!r}".format(state.filename)

        value = Node('tuple', (Node('atom', 'error'), Node('str', text + append)))

        Exception.__init__(self, value)

class Throw(MalangError):
    def __init__(self, value):
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

    def get(self, name, state):
        if name in self.bindings:
            return self.bindings[name]
        elif self.parent is not None:
            return self.parent.get(name, state)
        raise UnboundIdentifier("Identifier {!r} not bound to a value".format(name), state)

    def is_bound(self, name):
        try:
            self.get(name, State())
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

class State:
    def __init__(self, env=None, filename=None, infonode=None, readonly=False):
        self.env      = env
        self.filename = filename
        self.infonode = infonode
        self.readonly = readonly

    def newenv(self, env):
        return State(env, self.filename, self.infonode, self.readonly)
    def newfilename(self, filename):
        return State(self.env, filename, self.infonode, self.readonly)
    def newinfonode(self, infonode):
        return State(self.env, self.filename, infonode, self.readonly)
    def newreadonly(self, readonly):
        return State(self.env, self.filename, self.infonode, readonly)
