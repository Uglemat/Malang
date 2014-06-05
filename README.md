#Malang

Malang is a (very) minimalistic functional programming language. It's pretty useless, but it was fun to make.

It's made in Python 3 and depends upon Python Lex-Yacc. In Debian (and probably Ubuntu), you can install it
with `sudo apt-get install python3-ply`.

Enter this in the shell to start dashing out compound expressions in the REPL:

    $ ./malang.py -i

Here's an example session:

    Malang> 234, 923.
    923
    Malang> [@ * @.] 5.
    25
    Malang> List := #[one, two, three].
    {one, {two, {three, nil}}}
    Malang> Lists:Reverse List.
    {three, {two, {one, nil}}}
    Malang> Bools:And yeah nope.
    nope
    Malang> Ages := #[{peter, 20}, {mary, 22}, {george, 30}].
    {{peter, 20}, {{mary, 22}, {{george, 30}, nil}}}
    Malang> Dicts:Get george Ages.
    {ok, 30}
    Malang> Dicts:Get me Ages.
    nope
    Malang> Dicts:Set me 123 Ages.
    {{me, 123}, {{peter, 20}, {{mary, 22}, {{george, 30}, nil}}}}
    Malang> Dicts:Remove peter Ages.
    {{mary, 22}, {{george, 30}, nil}}
    Malang> Pairings := #[{P1, P2} | {P1, _} <- Ages, {P2, _} <- Ages, P1 > P2].
    {{peter, mary}, {{peter, george}, {{mary, george}, nil}}}
    Malang> Env {}.
    	Identifier bindings in current environment:
    Ages            => {{peter, 20}, {...   List            => {one, {two, {th...
    Pairings        => {{peter, mary},...
    	Showing bindings for parent environment below:
    Use             => [function]           Builtins        => [module]
    Tuples          => [module]             Exit            => [builtin]
    Env             => [builtin]            Dicts           => [module]
    Lists           => [module]             Objects         => [module]
    TypeOf          => [builtin]            Funcs           => [module]
    Cdr             => [function]           Bools           => [module]
    Car             => [function]           Require         => [builtin]
    Streams         => [module]             Error           => [builtin]
    Strings         => [module]             Cmp             => [function]
    Exhibit         => [builtin]            Help            => [builtin]
    ToStr           => [builtin]            Numbers         => [module]
    Io              => [module]
    ok
    Malang> Help Numbers.
    Functions in module:
       Even
       Odd
       RandRange
    ok
    Malang> Help Numbers:Odd.
    Function was defined in the file 'libs/numbers.malang'
     - Docstring:
    
      @ = Number
    
      Returns `yeah` if `Number` is odd, `false` otherwise.
      
    ok
    Malang> Numbers:Odd 43.
    yeah
    Malang> 


#Overview

It has function scope, closures, modules, tail call elimination, higher order functions and pattern matching.

There are no mutable values in Malang, so you can't change a value 'in place'.

All functions take exactly 1 argument (but they can take tuples as arguments, and use currying).

All functions are anonymous. Functions are values, and can be bound to a name.

Pattern matching is the only way to bind identifiers to values. You can't 'rebind' an
identifier, once you've said that A := 3, you can't change it. A is 3 always.

The only way to loop is to use recursion and with list comprehensions. The control flow
constructs are 'case of', 'throw/catch' and 'if then else' (plus the filters in list comprehensions).

#Values

There are 6 types of values: numbers (only integers), strings, atoms, tuples, functions and modules. Lists are
not a fundamental datatype, they are simply linked lists made up of two-tuples. Dicts are just lists of two-tuples
of the form `{<key>, <val>}`, so they are what you'd call an 'alist' in a language like Lisp.

##Numbers

Numbers have the syntax `(<base>#)?<number>` where base can be from 2 (for binary) to 16 (for hexadecimal),
and base defaults to 10 (decimal). Note that `<base>` and `<number>` above are not expressions.

##Strings

Strings are what you'd expect, like `"string with \" < quote and \\ < backslash "`. Any character following a
backslash is escaped, even if it doesn't have a special meaning like \n and \t, so "\hay" is the same string
as just "hay".

##Atoms

Atoms are tokens that match the regex `[a-z][a-zA-Z0-9_]*`, and are just values that you can use
in your programs (in tuples, et cetera).

##Identifiers

Identifiers are names which are bound to values, and they must match the regex `[A-Z_][a-zA-Z0-9_]*`.
The identifier `_` is special, it will match anything and is never bound to anything.

##Tuples

A tuple is an ordered collection of items, surrounded by `{` and `}`, like this: `{1, 2, 3}`.

##Lists

Lists have almost the same syntax as tuples: `#[1, 2, 3]`, however that is just syntactic sugar.
`#[1, 2, 3]` translates into `{1, {2, {3, nil}}}` (where `nil` is an atom).

#Syntax and semantics

A Malang program is a list of compound expressions.

A compound expression is 1 or more expression seperated by commas, ending with a dot. For example:

    1, 3 + 1, "hello".

or

    "Hello".

The value of a compound expression is the value of the last expression.

##Functions

A function is a list of compound expressions surrounded by square brackets. For example:

    [1, 3. 
     {atom, "string"}.]

The above function will always return the tuple `{atom, "string"}`, because functions returns the value
of the last compound expression.

If the first expression in a function body is a string, then that string will be the documentation string
for that function.

##Bind operator

You pattern-match with the 'bind' operator, ":=". Like this


    Three := 3.
    {One, {Two, Three}} := {1, {2, 3}}.

It will evaluate the right side expression and try to match it with the left side pattern (from left to right).
If it sees an unbound identifier (an identifier that hasn't been pattern-matched already)
it will just bind it to the respective value on the right hand side. 

This will only match tuples with two identical elements:

    Tuple := {"hay", "hay"}.
    {Same, Same} := Tuple.

Because the second time it sees `Same`, it is no longer an unbound identifier, and malang will insist
that it must match.

The resulting value is the same as the right hand side. Combine this with the fact that the operator
is right-associative and you can do stuff like this:

    Tup := {Fst, Snd} := {hello, yoyo}.

Which is parsed as `Tup := ({Fst, Snd} := {hello, yoyo}).` and thus does exactly what you'd expect.

##Function application

The syntax for function application is `<function> <argument>`.

##The @

As I said eariler, all functions take exactly one argument. It is automatically bound to the special
identifier `@`. The `@` will shadow any outside `@` that may exist.

##Case of

The `case of` construct is like the `:=` operator, except it tries to pattern match on several patterns,
and stops once a pattern matches, and executes the corresponding compound expression in a *new* environment
that inherits from the outer environment. Note it's a new environment, so the bindings made from the pattern
matching and in the compund expression are forgotten once the `case of` is done evaluating. The syntax is

    case <expr> of
      <pattern1> -> <compound_expr1>
      ...
      <patternN> -> <compound_exprN>
    end

The `<expr>` above is evaluated in the outer environment (so any identifier bindings will be remembered).

##If then else

The syntax for the `if then else` construct is

    if <expr>
      then <consequent_compound_expr>
      else <alternative_compound_expr>
    end

It will evaluate `<expr>` in a new environment that inherits from the outer environment, and then, depending on the
truthyness of the result, will decide which compound expr to evaluate (in the same environment that `<expr>` was evaluated
in). So, as with `case of`, any identifiers bound in the compund expressions will be forgotten once `if then else` is done.

##Operators and such

 Operators/other            | Associativity | Arity  | Explanation
 --------                   | ------------- | -----  | -------------
 `<` `>` `<=` `>=` `=` `!=` | Left          | Binary | Comparison operators. Values of different types has an inherent order, so they can be compared. Strings and tuples are compared lexicographically. The (not-)equals operator works for all types, but functions/modules/builtins aren't orderable.
 `+`                        | Left          | Binary | Add two things together. Works for numbers, strings, and tuples.
 `-`                        | Left          | Binary | Subtract one thing from another. Numbers only.
 `-`                        | Right         | Unary  | Negation operator for a number on the right.
 `*`                        | Left          | Binary | Multiply one thing by another. Works for numbers. Also works if one operand is a number, and the other operand is a tuple or a string.
 `/`                        | Left          | Binary | Divide one thing by another. Only works for numbers. Does floor division (Malang doesn't have real numbers. Real numbers are complicated, therefore dubious. fo real).
 `%`                        | Left          | Binary | Modulo operator. Works in the same way as the modulo in python. Numbers only.
 `~`                        | Left          | Binary | Function composition, creates a new function out if its operands. `(F ~ G) X = F(G(X))`.
 `**`                       | Right         | Binary | Raises an operand to some power.
 Function application       | Left          | Binary | Calling a function with an argument.
 `:`                        | Right         | Binary | Evaluating an expression in the context of some module.
 `:=`                       | Right         | Binary | Match a value agains a pattern.

###Precedence

The things at the top has higher precedence than the things at the bottom:

Prec.  | Operators/other
------ | ---------------
1      | `(...)` `#[...]` `{...}` `[...]` `case of` `if then else`
2      | `:` (Module access)
3      | Function application (`<func> <arg>`)
4      | `~` (Function composition)
5      | unary `-` (negation)
6      | `**`
7      | `*` `/` `%`
8      | `+` `-`
9      | `<` `>` `<=` `>=` `=` `!=`
10     | `:=`
11     | throw/catch
12     | `,` (seperator)
13     | `.` (end of compound expression)

##Modules

You can use the builtin function `Require` to import modules. Like this:

    My_Module := Require "src/module.malang".

It will evaluate the program in the file specified, and then return a module.

When you are evaluating top-level expressions in a file (i.e. not something that's inside a function), then the working
directory should be the directory that that file is located in, and so you can use relative path-names when using
`Require`.

'Module access' is used to make use of a module, this is the syntax

    <module>:<expr>

It will evaluate `<expr>` in the context of `<module>`, or in other words, in the same environment, and then return it.
So you can use all the identifiers in the module inside `<expr>`. `<expr>` is usually just an identifier that points to
a function in the module, but it could be a more complicated expression, such as a tuple or a function expression. Why not.
You cannot bind new identifiers in `<expr>`, it is said to be evaluated in "readonly mode".

So if `My_Module` contains a function bound to the identifier `Factorial` then you can do this to use it:

    My_Module:Factorial 20000.

It evaluates `Factorial` in the context of `My_Module`, the result of that is a function, and it is then called with the
argument 20000.

#Factorial example

Here is factorial written in Malang:

    Factorial := [
      case @ of
        1 -> 1.
        N -> N * Factorial (N-1).
      end.
    ].

The reason i write `Factorial (N-1)` is that function application has higher precedence than arithmetic operations
so `Factorial N-1` would be parsed as `(Factorial N) - 1`, which is not what I wanted.

That version of factorial is not tail recursive, however, and you can't really calculate `Factorial 20000` without
hitting the recursion limit. Here's a tail recursive version:

    Factorial_Tail_Recursive := [
      Helper := [
        {N, Acc} := @.
        if N <= 1
          then Acc.
          else Helper {N-1, N * Acc}.
        end.
      ].
      Helper {@, 1}.
    ].

With that version, you *can* calculate `Factorial 20000`.

#Navigating the REPL

You can use the functions `Help` and `Env` to get information about things. Run `Help Help.` to get started.

You can use the function `Exhibit` to go 'inside' of a module when you're in the REPL.

The function `Use` is quite *use*ful for testing out a file in the REPL.

# License

GNU GPLv3
