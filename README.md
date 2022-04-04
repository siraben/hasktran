# hasktran - A compiler for FRACTRAN written in Haskell.

See my
[writeup](https://siraben.dev/2020/02/26/translating_cl.html) of
this compiler.

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

## Features
- Embedding of DSL into Haskell using tagless final style
- Support for high-level control flow constructs
- Peephole optimization of compiled programs
```haskell
sumTo :: FracComp repr => Integer -> [repr [Rational]]
sumTo n = [addi "c" 0, addi "n" n, while (jge "n" 0) [adds "c" "n", subi "n" 1]]
```

The following show the invocation of the assembler and naïve
interpreter and peephole optimizer.
```haskell
λ> runAssembler (sumTo 10)                      -- Program length: 31
Right [847425747 % 2,13 % 3,19 % 13,11 % 3,11 % 29,31 % 11,41 % 31,23
% 11,23 % 47,2279 % 23,59 % 301,59 % 41,67 % 413,329 % 67,61 % 59,73 %
61,83 % 73,71 % 61,71 % 97,445 % 71,707 % 89,103 % 5353,103 % 83,109 %
5459,5141 % 109,107 % 103,113 % 749,113 % 19,131 % 113,29 % 131,127 %
113]
λ> peepholeOptimize <$> runAssembler (sumTo 10) -- Program length: 25
Right [847425747 % 2,19 % 3,11 % 3,11 % 29,41 % 11,23 % 11,2279 %
47,59 % 301,59 % 41,67 % 413,329 % 67,61 % 59,83 % 61,71 % 61,445 %
97,707 % 89,103 % 5353,103 % 83,109 % 5459,5141 % 109,107 % 103,113 %
749,113 % 19,29 % 113,127 % 113]
λ> runAsm (sumTo 10)
[(Prime 97,55),(Prime 107,1)]
```

Thus showing the sum of the numbers from 1 to 10 inclusive is 55.

A pretty printer for FRACTRAN programs was also added, and thanks to
the tagless final style, we are able to print high level constructs
like while loops by writing the corresponding printer.
```haskell
λ> :t pretty
pretty :: Traversable t => t (S a) -> Doc
λ> pretty (sumTo 10)
c += 0
n += 10
while n >= 0 {
  c += n
  n -= 1
}
```
## Future plans
- [x] Pretty printer
- [x] Peephole optimization
- [ ] Partial evaluator
