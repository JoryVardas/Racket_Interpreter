# Description

This is an interpreter written in Racket to interpret a subset of the Racket language.

This was a school group project, however all the code in this repository is my own.

## Implementation Overview
Since Racket uses a prefix notation then the program passed to `startEval` can be recursively evaluated with each evaluation step being passed the remainder of the program and the context. At each evaluation step the context consists of the bindings which are present at the time of invocation, this will include the builtin bindings and any additional binds from `let`, `letrec`, or `lambda`.

For more details on how this works see the comments in the code.

## Implemented Functions
The following is a list of functions which are part of the Racket language and which the interpreter implements:

- control flow operations: if
- math operations: + - * /
- comparison operations: \> < \>= <= = equal?
- list operators: cons car cdr pair?
- binding operators: let letrec
- constants:
	- true: bound to #t
	- false: bound to #f
	- null: bound to ()
- lambda
- quote

Like in Racket quote can be called directly as `(quote <expr>)` or as `'<expr>`.



#Usage

Open and run the startEval.rkt file in DrRacket, then in the terminal call the startEval function with a literal representing your program.

A few examples:

To simply return a literal:
```
(startEval 1)
```

To return the first element of a list:
```
(startEval '(car '(1 2 3)))
```

To create and call a lambda which takes a value x and adds 20 to x:
```
(startEval '((lambda(x) (+ x 20)) 1))
```

To bind a value to y and then create and call a lambda which takes a value x adds x and y:
```
(startEval '(let ([y 21]) ((lambda(x) (+ x y)) 1)))
```