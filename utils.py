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


def python_list_to_malang_list(_list):
    acc = Node('atom', 'nil')
    while _list:
        acc = Node('tuple', (_list.pop(), acc))
    
    return acc


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

        Exception.__init__(self, text + append)

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
