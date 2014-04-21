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
from ply import lex, yacc
import re
from utils import Node, MalangError

def unescape_str(s):
    if r'\\' in s:
        return '\\'.join(map(unescape_str, s.split(r'\\')))

    s = s.replace(r'\n', '\n').replace(r'\t', '\t')
    return re.sub(r'\\(.)', r'\1', s)

keywords = {kw: kw.upper()
            for kw in ['case', 'of', 'if', 'then', 'else', 'end']}

tokens = (
    'NUMBER',   'PLUS','MINUS','TIMES', 'POW',
    'DIVIDE',   'MODULO',
    'LPAREN',   'RPAREN',
    'LBRACE',   'RBRACE',
    'LBRACKET', 'RBRACKET',
    'STR',
    'DOT',      'ID',
    'COLON',    'STARTLIST',
    'COMMA',    'ATOM',
    'BIND',     'ARROW',
    'LEFTARROW','PIPE',
    'GT', 'LT', 'GE',  'LE', 'EQ', 'NE',
    'COMMENT', 'TILDE'
) + tuple(keywords.values())

states = (
  ('str','exclusive'),
)

precedence = (
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIVIDE', 'MODULO'),
    ('right', 'POW'),
    ('right', 'UMINUS'),
)


# Regular expression rules for simple tokens
t_PLUS    = r'\+'
t_MINUS   = r'-'
t_TIMES   = r'\*'
t_DIVIDE  = r'/'
t_MODULO = r'%'
t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_LBRACE = r'\{'
t_RBRACE = r'\}'
t_DOT = r'\.'
t_COMMA = r','
t_COLON = r':'
t_BIND = r':='
t_LBRACKET = r'\['
t_RBRACKET = r'\]'
t_ARROW = r'->'
t_LEFTARROW = r'<-'
t_PIPE = r'\|'
t_GE = r'>='
t_LE = r'<='
t_GT = r'>'
t_LT = r'<'
t_EQ = r'='
t_NE = r'!='
t_STARTLIST = r'\#\['
t_TILDE = r'~'
t_POW = r'\*\*'

def t_COMMENT(t):
    r'--(.*)'
    pass

def t_ATOM(t):
    r'[a-z][a-zA-Z0-9_]*'
    if t.value in keywords:
        t.type = keywords[t.value]
        t.value = Node('keyword', t.value, lineno=t.lineno)
    else:
        t.value = Node('atom', t.value, lineno=t.lineno)
    return t

def t_ID(t):
    r'([A-Z_][a-zA-Z0-9_]*|@)'
    t.value = Node('id', t.value, lineno=t.lineno)
    return t



def t_str(t):
    r'"'
    t.lexer.str_start = t.lexer.lexpos
    t.lexer.str_lineno = t.lineno
    t.lexer.begin("str")

def t_str_esc(t):
    r'\\.'

def t_str_STR(t):
    r'"'
    t.value = Node('str',
                   unescape_str(t.lexer.lexdata[t.lexer.str_start:t.lexer.lexpos-1]),
                   lineno=t.lexer.str_lineno)
    t.lexer.begin("INITIAL")
    return t

def t_str_fill(t):
    r'.'

def t_str_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_NUMBER(t):
    (r"(([2-9]|1[0-6])\#(\d|[a-fA-F])+"
     r"|\d+)")
    
    if "#" in t.value:
        radix, val = t.value.split("#")
        radix = int(radix)
    else:
        val = t.value
        radix = 10

    try:
        integer = int(val, radix)
    except ValueError:
        raise MalangError("Invalid number {!r}".format(t.value),
                          file_name, Node("dummynode", "", lineno=t.lineno))

    t.value = Node('number', integer, lineno=t.lineno)
    return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# A string containing ignored characters (spaces and tabs)
t_ignore  = ' \t'
t_str_ignore = ''


def t_error(t):
    print("Illegal character '{}'".format(t.value[0]))
    t.lexer.skip(1)

def t_str_error(t):
    print("Illegal character '{}'".format(t.value[0]))
    t.lexer.skip(1)
















def p_program(p):
    'program : program compound_expr'
    p[0] = Node('program', p[1].content + [p[2]], infonode=p[1])

def p_program_single(p):
    'program : compound_expr'
    p[0] = Node('program', [p[1]], infonode=p[1])


def p_compound_expr(p):
    'compound_expr : compound_expr_list DOT'
    p[0] = p[1]

def p_compound_expr_list(p):
    'compound_expr_list : compound_expr_list COMMA match_expr'
    p[0] = Node('program', p[1].content + [p[3]], infonode=p[1])

def p_compound_expr_list_single(p):
    'compound_expr_list : match_expr'
    p[0] = Node('program', [p[1]], infonode=p[1])


def p_match_expr_bind(p):
    'match_expr : cmp_expr BIND match_expr'
    p[0] = Node('bind', (p[1], p[3]), infonode=p[1])


def p_match_cmp_expr(p):
    'match_expr : cmp_expr'
    p[0] = p[1]


def p_cmp_expr_gt(p):
    'cmp_expr : cmp_expr GT arith_expr'
    p[0] = Node('gt', (p[1], p[3]), infonode=p[1])

def p_cmp_expr_lt(p):
    'cmp_expr : cmp_expr LT arith_expr'
    p[0] = Node('lt', (p[1], p[3]), infonode=p[1])

def p_cmp_expr_ge(p):
    'cmp_expr : cmp_expr GE arith_expr'
    p[0] = Node('ge', (p[1], p[3]), infonode=p[1])

def p_cmp_expr_le(p):
    'cmp_expr : cmp_expr LE arith_expr'
    p[0] = Node('le', (p[1], p[3]), infonode=p[1])

def p_cmp_expr_eq(p):
    'cmp_expr : cmp_expr EQ arith_expr'
    p[0] = Node('eq', (p[1], p[3]), infonode=p[1])

def p_cmp_expr_ne(p):
    'cmp_expr : cmp_expr NE arith_expr'
    p[0] = Node('ne', (p[1], p[3]), infonode=p[1])


def p_cmp_expr_expr(p):
    'cmp_expr : arith_expr'
    p[0] = p[1]

def p_arith_expr_plus(p):
    'arith_expr : arith_expr PLUS arith_expr'
    p[0] = Node('plus', (p[1], p[3]), infonode=p[1])


def p_arith_expr_minus(p):
    'arith_expr : arith_expr MINUS arith_expr'
    p[0] = Node('minus', (p[1], p[3]), infonode=p[1])


def p_arith_expr_uminus(p):
    'arith_expr : MINUS arith_expr %prec UMINUS'
    p[0] = Node('uminus', p[2], infonode=p[1])


def p_arith_expr_times(p):
    'arith_expr : arith_expr TIMES arith_expr'
    p[0] = Node('times', (p[1], p[3]), infonode=p[1])

def p_arith_expr_div(p):
    'arith_expr : arith_expr DIVIDE arith_expr'
    p[0] = Node('divide', (p[1], p[3]), infonode=p[1])

def p_arith_expr_mod(p):
    'arith_expr : arith_expr MODULO arith_expr'
    p[0] = Node('modulo', (p[1], p[3]), infonode=p[1])

def p_arith_expr_pow(p):
    'arith_expr : arith_expr POW arith_expr'
    p[0] = Node('pow', (p[1], p[3]), infonode=p[1])


def p_arith_expr_composition(p):
    'arith_expr : composition'
    p[0] = p[1]

def p_composition(p):
    'composition : composition TILDE fncall'
    p[0] = Node('composition', (p[1], p[3]), infonode=p[1])

def p_composition_fncall(p):
    'composition : fncall'
    p[0] = p[1]


def p_fncall(p):
    'fncall : fncall module_access'
    p[0] = Node('fncall', (p[1], p[2]), infonode=p[1])

def p_fncall_module_access(p):
    'fncall : module_access'
    p[0] = p[1]

def p_module_access(p):
    'module_access : module_access COLON item'
    p[0] = Node('module_access', (p[1], p[3]), infonode=p[1])

def p_module_access_item(p):
    'module_access : item'
    p[0] = p[1]

def p_item_num(p):
    '''item : NUMBER 
            | STR 
            | tuple
            | list
            | ATOM
            | ID
            | func_def
            | case_of
            | if'''
    p[0] = p[1]

def p_if(p):
    'if : IF match_expr THEN compound_expr ELSE compound_expr END'
    p[0] = Node('if', (p[2], p[4], p[6]), lineno=p.lineno(1))

def p_item_expr(p):
    'item : LPAREN match_expr RPAREN'
    p[0] = p[2]

def p_func_def(p):
    'func_def : LBRACKET program RBRACKET'
    p[0] = Node('func_def', (p[2], file_name), lineno=p.lineno(1))

def p_case_of(p):
    'case_of : CASE match_expr OF arrow_list END'
    p[0] = Node('case_of', {'matched_expr': p[2],
                            'arrow_list':    p[4]},
                lineno=p.lineno(1)
    )

def p_arrow_list(p):
    'arrow_list : arrow_list item ARROW compound_expr'
    p[0] = p[1] + [{'pattern': p[2], 'expr': p[4]}]

def p_arrow_list_single(p):
    'arrow_list : item ARROW compound_expr'
    p[0] = [{'pattern': p[1], 'expr': p[3]}]

def p_tuple(p):
    'tuple : LBRACE expr_list RBRACE'
    p[0] = Node('tuple', p[2], lineno=p.lineno(1))

def p_tuple_empty(p):
    'tuple : LBRACE RBRACE'
    p[0] = Node('tuple', (), lineno=p.lineno(1))

def p_list_comprehension(p):
    'list : STARTLIST match_expr PIPE comprehension_list RBRACKET'
    p[0] = Node('list_comprehension', (p[2], p[4]), lineno=p.lineno(1))

def p_comprehension_list(p):
    'comprehension_list : match_expr LEFTARROW match_expr COMMA comprehension_list'
    p[0] = (Node('emitter', {'pattern': p[1], 'expr': p[3]}, infonode=p[1]),) + p[5]

def p_comprehension_list_single(p):
    'comprehension_list : match_expr LEFTARROW match_expr'
    p[0] = (Node('emitter', {'pattern': p[1], 'expr': p[3]}, infonode=p[1]),)

def p_comprehension_list_filter(p):
    'comprehension_list : match_expr COMMA comprehension_list'
    p[0] = (Node('filter', p[1], infonode=p[1]),) + p[3]

def p_comprehension_list_filter_single(p):
    'comprehension_list : match_expr'
    p[0] = (Node('filter', p[1], infonode=p[1]),)


def p_list(p):
    'list : STARTLIST expr_list RBRACKET'
    p[0] = Node('list', p[2], lineno=p.lineno(1))

def p_list_empty(p):
    'list : STARTLIST RBRACKET'
    p[0] = Node('list', (), lineno=p.lineno(1))


def p_expr_list(p):
    'expr_list : expr_list COMMA match_expr'
    p[0] = p[1] + (p[3],)

def p_expr_list_single(p):
    'expr_list : match_expr'
    p[0] = (p[1],)


# Error rule for syntax errors
def p_error(p):
    lexer.begin("INITIAL")
    if p is None:
        raise SyntaxError("Syntax error (probably somewhere near the end)")
    else:
        raise SyntaxError("Syntax error at line #{}".format(p.lineno))

lexer  = lex.lex()
parser = yacc.yacc()

file_name = ""
def parse(s, filename):
    global file_name
    file_name = filename
    """
    I can't think of something much better than using a global variable here.
    I need function definitions to know which files they are in, to correctly
    report which file an error has occured in, when I'm in the evaluation function `maval`.
    By using the global variable `file_name`, I can store the file name a function definition
    was parsed in together with the code (look in the function `p_func_def` above to see).
    """

    lexer.lineno = 1
    try:
        return parser.parse(s, lexer=lexer)
    except SyntaxError as e:
        raise MalangError(e.args[0], filename)
