PCNF.hs
---
Author: Jonathan S. Prieto. C.

The program `PCNF.hs` translates first-order logic (FOL) formulae to the
prenex conjunctive normal form (PCNF) with the `pcnf` method.

This library has the following files:

- The file `PCNF.hs` includes tree methods.
  - The `pcnf` methods is the main one. It gives theprenex conjunctive normal
  form of a formula.
  - The `extract` method puts the quantifiers as prefix in the formula.
  - The `combineExtract` is that, combine in the D&Q `extract` method.
- The file `CNF.hs` has the method `dist`, it is delegated to apply the
distributive laws in a formula. Also, in this file, we have `demorgan` for
the DeMorgan Laws, a generalization of this rules.
- The file `Utils.hs` has many methods for that the `PCNF.rename` method
works.
  - The `rectify` method changes all bound variables.
  - The `replace` method implements a substitution F[y/x].
  - The `boundIndex` method finds a suitable index for a new variable
  unused in the formula.
  - Other methods that helps to the above ones works.
- The file `FOL.hs` defines the first-order logic language.
- The file `Test.hs` includes at least five different test cases as examples.

Usage
---
In the ghci console, load the library.

```Haskell
Prelude>:load PCNF.hs
```
then try the following examples, the main method is `pcnf`, this method gives
the prenex conjunctive normal form.

```Haskell
Prelude> let x = Var 1
Prelude> let y = Var 2
Prelude> let px = Pred 0 [x]
Prelude> let qx = Pred 1 [x]
Prelude> let f1 = Not (Exists x (Imp px (Forall x px)))
Prelude> pcnf f1
Forall (Var 2) (Exists (Var 1) (And (Pred 0 [Var 2]) (Not (Pred 0 [Var 1]))))
```

You can write whatever formula in FOL follows the grammar given on the 
file `FOL.hs`:

```Haskell
-- A term in First-Order Logic
data Term = Cons Int | Var Int | Func Int [Term]
  deriving (Show, Eq)

-- A formula in First-Order Logic
data Formula =
    Pred Int [Term]
  | Not Formula
  | And Formula Formula
  | Or Formula Formula
  | Imp Formula Formula
  | Biimp Formula Formula
  | Forall Term Formula
  | Exists Term Formula
    deriving (Show, Eq)
```

On this way, we can build up any formula. Then, in a file after import
the module pcnf you can do some computations.

Import the library.
```Haskell
import PCNF
```

Defining a term (variable):
```Haskell
x,y,z :: Term
x   = Var 0
y   = Var 1
z   = Var 2
```

Defining a predicate.
```Haskell
px  = Pred 0 [x]
py  = Pred 1 [y]
pxz = Pred 2 [xz]
```

Definining a formula.
```Haskell
f5 = Not
    (Imp
        (And
            (Forall x (Imp px qx))
            (Forall x (Imp qx rx)))
       (Forall x (Imp px rx)))
```

Finally, if you need the prenex conjuctive normal form of a formula,
the library provides the method `pcnf :: Formula -> Formula`.

Last example:

```Haskell
let f1 = Not (Exists x (Imp px (Forall x px)))
```

For instance, `pcnf` applies to the last example `f1`:

```Haskell
Prelude> pcnf Not (Exists x (Imp px (Forall x px)))
```

gives you (pretty format):

```Haskell
Forall (Var 2)
    (Exists (Var 1)
            (And
                (Pred 0 [Var 2])
                (Not (Pred 0 [Var 1]))))
```

You can find more usage cases in (`Test.hs`).

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
or

```
runTestTT beta
```

If you don't have `hunit` library, you can go for it and install it as 
follows. In your console, invoke cabal.

```Haskell
cabal install hunit
```