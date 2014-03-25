Malang is a (very) minimalistic functional programming language. It's pretty useless, but it was fun to make.

It has function scope, closures, modules, tail call elimination, higher order functions and pattern matching.

Everything is an expression in Malang. Absolutely everything.

All functions take exactly 1 argument. No more, no less.

All functions are anonymous. Functions/closures are values, and can be bound to a name.

Pattern matching is the only way to bind identifiers to values. You can't 'rebind' an
identifier, once you've said that A := 3, you can't change it. A is 3 always.

The only way to loop is to use recursion. The only control flow construct is 'case of'.

There are 6 types of values: numbers (only integers), strings, atoms, tuples, functions and modules.

numbers are what you would expect (malang can only parse positive integers, write 0 - <number> for
negative numbers), strings are also what you'd expect, like `"string with \" < quote "`, and also like `"""And then I just
"Helloo!, and then she said ... \n\n \t\t """` in triple quotes. Atoms are tokens that match the regex '[a-z][a-zA-Z0-9_]*',
and are just values that you can use in your programs (in tuples, et cetera). Identifiers are just then names which
are bound to values, and they must match the regexr '[A-Z_][a-zA-Z0-9_]*'


A Malang program is a list of compound expressions.

A compound expression is 1 or more expression seperated by commas, ending with a dot. For example:

    1, 3 + 1, "hello".

or

    "Hello".

The value of a compound expression is the value of the last expression.

A function is a list of compound expressions surrounded by square brackets. For example:

    [1, 3. 
     {atom, "string"}.]

The above function will always return the tuple `{atom, "string"}`, because functions returns the value
of the last compound expression.

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

The syntax for function application is <expr> <expr>.

As I said eariler, all functions take exactly one argument. It is automatically bound to the special
identifier `@`.

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
        case N <= 1 of
          true  -> Acc.
          false -> Helper {N-1, N * Acc}.
        end.
      ].
      Helper {@, 1}.
    ].

With that version, you *can* calculate `Factorial 20000`.

The `case of` construct is just like the `:=` operator, except it tries to pattern match on several patterns,
and stops once a pattern matches, and executes the corresponding compound expression. The syntax is

    case <expr> of
      <pattern1> -> <compund_expr1>
      ...
      <patternN> -> <compound_exprN>
    end

You can use the builtin function `Require` to import modules. Like this:

    My_Module := Require "src/module.malang".

It will evaluate the program in the file specified, and then return a module.

'Module access' is used to make use of a module, this is the syntax

    <module>:<expr>

It will evaluate `<expr>` in the context of `<module>`, or in other words, in the same environment, and then return it.
So you can use all the identifiers in the module inside `<expr>`. `<expr>` is usually just an identifier that points to
a function in the module, but it could be a more complicated expression, such as a tuple or a function expression. Why not.

So if `My_Module` contains a function bound to the identifier `Factorial` then you can do this to use it:

    My_Module:Factorial 20000.

It evaluates `Factorial` in the context of `My_Module`, the result of that is a function, and it is then called with the
argument 20000.

# License

GNU GPLv3