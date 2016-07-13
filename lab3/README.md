Unification.hs
---
Author: Jonathan S. Prieto. C.

The file `Unification.hs` implements the unification algorithm for two atoms in
first-order logic (FOL) given by
[Martelli and Montanari (1982)](http://goo.gl/SS8DeA). This algorithm
is the function `unify`. It transforms two atoms in two new atoms as long
as the unification algorithm found the most general unifier (`mgu`).
When the algorithm reports some failure such a unifier does not exit, and
we call that these atoms are *not unifiable*.
In that case, the function `unify` returns an error of type `UnifyError`.
We have four possible failure, each of one associated with the steps in
the algorithm.

- `DifferentPredArity`
- `DifferentFn`
- `DifferentFnArity`
- `FailRule4`


Usage
---
In the ghci console, load the library.

```Haskell
Prelude>:load Unification.hs
```
then try the following examples, the main method is `unify`.
When it succeed, it shows you a list with the original after apply
the substitution `mgu`.

```Haskell
Prelude> let x = Var "x"
Prelude> let y = Var "y"
Prelude> let p = Pred "p" [F "g" [y], F "f" [x, F "h" [x], y]]
Prelude> let p' = Pred "p" [x, F "f" [F "g" [z], w, z]]
Prelude> unify p p'
Right [p[g[z],f[g[z],h[g[z]],z]],p[g[z],f[g[z],h[g[z]],z]]]
```

As we can see above, the output is pretty formatted in the interactive
console. The output of the method unify is the type `UnifyResult`, an
abbreviation of `Either UnifyError`. We always get an answer with the
prefix `Right` if the function succeed, or with the prefix `Left` if
something was wrong.

More cases can be found in the test file (`Test.hs`).

Testing
---
Run some test cases using HUnit.

```Haskell
Prelude> :load test
*Test> runTestTT tests
Cases: 6  Tried: 6  Errors: 0  Failures: 0
Counts {cases = 6, tried = 6, errors = 0, failures = 0}
*Test> unify a b
Left The atoms have different predicates.
```
Use the eight base cases:

```
runTestTT tests
```

If you don't have `hunit` library, you can go for it and install it as
follows. In your console, invoke `cabal`.

```Haskell
cabal install hunit
```
Note: This code was tested in GHCi, version 8.0.1.