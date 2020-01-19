# hasktran - A compiler for FRACTRAN written in Haskell.

This was inspired by [Malisper's Common Lisp assembler for
FRACTRAN](https://malisper.me/building-fizzbuzz-fractran-bottom/), but
of course, in a purely functional manner.

FRACTRAN programs have the type `[repr [Rational]]`, where `repr` is
an instance of the `FracComp` typeclass, from which the special forms
(`subi`, `while`, `goto` etc.) can be derived, making the language
Turing complete.

Here is an example FRACTRAN program written in Haskell, rather, a
function that takes an `Integer` and _produces_ a FRACTRAN program,
not unlike Lisp macros.

```haskell
sumTo :: FracComp repr => Integer -> [repr [Rational]]
sumTo n = [addi "n" n, while (jge "n" 0) [adds "c" "n", subi "n" 1]]
```

The following show the invocation of the assembler and naïve
interpreter, respectively.
```haskell
λ> runAssembler (sumTo 10)
Right [29296875 % 2,11 % 3,17 % 11,7 % 3,7 % 23,29 % 7,37 % 29,19 % 7,
19 % 43,1927 % 19,53 % 205,53 % 37,61 % 265,215 % 61,59 % 53,71 % 59,
79 % 71,67 % 59,67 % 89,8051 % 67,505 % 83,103 % 4747,103 % 79,
109 % 4841, 4183 % 109,107 % 103,113 % 535,113 % 17,131 % 113,23 % 131,
127 % 113]
λ> runAsm (sumTo 10)
[(Prime 97,55),(Prime 107,1)]
```

Thus showing the sum of the numbers from 1 to 10 inclusive is 55.

A pretty printer for FRACTRAN programs was also added.
```haskell
λ> :t pretty
pretty :: Traversable t => t (S a) -> String
λ> putStr (pretty (sumTo 10))
addi n 10
jge g2 0 g1
label g0
jge g6 0 g5
label g4
addi g3 1
addi n -1
label g5
jge n 1 g4
jge g9 0 g8
label g7
addi c 1
addi n 1
addi g3 -1
label g8
jge g3 1 g7
addi n -1
label g1
jge n 0 g0
```
