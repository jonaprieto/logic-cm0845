PCNF.hs
---
Author: Jonathan S. Prieto. C.

The file `Unify.hs` implements the unification algorithm for two atoms in
first-order logic (FOL) given by Martelli and Montanari (1982). The algorithm
is implemented in the function `unify`. It transforms two atoms in two new
atoms depends on if the unification algorithm found the most general unifier.
If this algorithm did not find out a substitution it because it does not exit,
and then these two atoms are not unifable, in that case the function `unify`
returns `[]`.


Usage
---
In the ghci console, load the library.

```Haskell
Prelude>:load PCNF.hs
```
then try the following examples, the main method is `pcnf`, this method gives
the prenex conjunctive normal form.

```Haskell
Prelude> let px = Pred 0 [Var "x"]
Prelude> let qx = Pred 1 [Var "y"]
Prelude> unify px qx
[Pred 0 [Var "y"], Pred 1 [Var "y"]]
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