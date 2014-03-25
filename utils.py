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



class MalangError(Exception):
    def __init__(self, text, filename):
        Exception.__init__(self,
                           text + " in file {!r}".format(filename))

class UnboundIdentifier(MalangError):
    pass

class InvalidMatch(MalangError):
    pass

class Node(object):
    def __init__(self, _type, content):
        self._type = _type
        self.content = content


class Env(object):
    def __init__(self, parent=None, bindings=None):
        self.parent = parent
        self.bindings = {} if bindings is None else bindings
        
    def bind(self, name, val):
        if name != "_":
            self.bindings[name] = val

    def get(self, name, filename):
        if name in self.bindings:
            return self.bindings[name]
        elif self.parent is not None:
            return self.parent.get(name, filename)
        raise UnboundIdentifier("Identifier {!r} not bound to a value".format(name),
                                filename)

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


class change_directory:
    """Context manager for changing the current working directory"""
    def __init__(self, new_path):
        self.new_path = new_path

    def __enter__(self):
        self.saved_path = os.getcwd()
        os.chdir(self.new_path)

    def __exit__(self, etype, value, traceback):
        os.chdir(self.saved_path)
