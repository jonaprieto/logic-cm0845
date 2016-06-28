Unify.hs
---
Author: Jonathan S. Prieto. C.

The file `Unify.hs` implements the unification algorithm for two atoms in
first-order logic (FOL) given by
[Martelli and Montanari (1982)](http://goo.gl/SS8DeA). This algorithm
is the function `unify`. It transforms two atoms in two new
atoms depends on if the unification algorithm found the most general unifier.
If this algorithm did not find out a substitution, such a unifier does not exit,
and then these two atoms are *not unifiable*, in that case, the function `unify`
returns one of the errors of type `UnifyError` .

Usage
---
In the ghci console, load the library.

```Haskell
Prelude>:load Unify.hs
```
then try the following examples, the main method is `unify`, this method gives
the prenex conjunctive normal form.

```Haskell
Prelude> let x = Var "x"
Prelude> let y = Var "y"
Prelude> let p = Pred "p" [F "g" [y], F "f" [x, F "h" [x], y]]
Prelude> let p' = Pred "p" [x, F "f" [F "g" [z], w, z]]
Prelude> unify p p'
Right [p[g[z],f[g[z],h[g[z]],z]],p[g[z],f[g[z],h[g[z]],z]]]
```

More cases can be found in the test file (`Test.hs`).

Testing
---
Run some test cases using HUnit.

```Haskell
Prelude> :load test
*Test> runTestTT tests
Cases: 6  Tried: 6  Errors: 0  Failures: 0
Counts {cases = 6, tried = 6, errors = 0, failures = 0}
```
Use the base cases:

```
runTestTT tests
```

If you don't have `hunit` library, you can go for it and install it as
follows. In your console, invoke cabal.

```Haskell
cabal install hunit
```