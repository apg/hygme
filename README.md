# Hygme

## Overview

Hygme implements a simple interpreter with hygienic macro expansion, as
described by [Kohlbecker](http://www.cs.ucdavis.edu/~devanbu/teaching/260/kohlbecker.pdf).

## Syntax

Hygme uses s-expressions, but has the following BNF-ish:

    Abs := (fn var body)
    Var := <symbol>
    Const := <string> | <symbol> | <number> | <boolean> ...
    Cond := (if <condition> <consequent> <alternate>)
    Assm := (set! var value)
    App := (<expr> <expr> ...)

Abs (abstractions), you'll notice take, and *must* have a single argument. Multiple
arguments can be obtained via currying, though zeroadic functions simply are not
supported.

Where applicable, the host scheme reader is deferred too, which means that <boolean>
is either #f or #t. 

## Initial environment

Procedures: `cons, car, cdr, list, not, +, -, *, /`

Macros: `unless, let, or, and`

## Expansion

`unless` expands as follows:

    hygme> (hygme-expand '(unless foo bar))
    => (if (not foo) bar)

`let` expands as follows:

    hygme> (hygme-expand '(let foo bar (cons foo '())))
    => ((fn foo2835 (cons foo2835 (quote ()))) bar)

`and` expands as follows:

    hygme> (hygme-expand '(and foo bar))
    => (if foo bar #f)

`or` expands as follows:

    hygme> (hygme-expand '(or foo bar))
    => ((fn v2995 (if v2995 v2995 bar)) foo)

## Copyright

Copyright 2013 Andrew Gwozdziewycz. Licensed under the MIT license.


## References

[Kohlbecker]: <http://www.cs.ucdavis.edu/~devanbu/teaching/260/kohlbecker.pdf> Kohlbecker, E.; Friedman, D. P.; Felleisen, M.; Duba, B. (1986). "Hygienic Macro Expansion". ACM conference on LISP and functional programming.
