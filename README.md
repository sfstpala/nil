nil
===

## Functional programming language

Nil (which stands for `'(not in lisp)`) is a small, functional language with
a bytecode compiler and virtual machine. It's very much a work in progress,
with no fixed specification for even the semantics.

### Types

Types are integers (bignums), floats (double precision), strings, 
lists and functions. The literals for the first three are what you'd expect.
Lists are created using the builtin `list` function.

### Forms

 - Atom

        12.5
        "hello"
        ...

 - Function call

        (function argument)
        (function argument argument)
        ...

 - Definition

        (define name value)

 - Function

        (lambda (x) (* x x))
        (lambda (x y) (* y x))
        (lambda xs (car xs))

 - Conditional

        (if condition true-case false-case)

 - List constructor

        (list)
        (list 1)
        (list 1 2 3 4)

To define a function f(x) = x², you'd say

        (define f (lambda (x)
            (^ x 2)))

This is a function that calculates the product of a list of numbers:

        (define product (lambda (m)
            (if m
                    (* (car m) (product (cdr m)))
                1)))

Where `(car x)` returns the first item of the non-empty list x (the *head*)
and `(cdr x)` returns a new list containing all but that first item (the *tail*).

For a more complicated example, here's the McCarthy 91 function and a loop that
calls it 16 times with a value of 99:

    (define M (lambda (n)
        (if (> n 100.0)
                (- n 10.0)
            (M (M (+ n 11.0))))))

    (define loop (lambda (n)
        (print (M 99.0))
        (if (> n 1.0) (loop (- n 1.0)) 0.0)))

    (loop 16.0)

### Bultins

    (> a b)     - defined on numbers
    (= a b)     - defined on numbers
    (+ a b)     - defined on numbers and lists
    (- a b)     - defined on numbers
    (* a b)     - defined on numbers
    (/ a b)     - defined on numbers
    (^ a b)     - defined on numbers
    (print a)   - defined on everything
    (car a)     - defined on lists
    (cdr a)     - defined on lists


### Running

There isn't a facility to execute source code files right now. If you're brave,
open up "test.go" and change the code at the bottom of the file, then run:

    go run test.go
