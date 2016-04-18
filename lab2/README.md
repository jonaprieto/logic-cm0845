PCNF.hs
---
Jonathan S. Prieto. C.

Translates first-order logic (FOL) formulae to the prenex
conjunctive normal form (PCNF).

Usage
---
In the ghci console, load the library.

```Haskell
Prelude>:load PCNF
```
then try the following example.

```Haskell
Prelude> let x = Var 1
Prelude> let y = Var 2
Prelude> let px = Pred 0 [x]
Prelude> let qx = Pred 1 [x]
Prelude> let f1 = (Not (Exists x (Imp px (Forall x px))))
Prelude> pcnf f1
Forall (Var 2) (Exists (Var 1) (And (Pred 0 [Var 2]) (Not (Pred 0 [Var 1]))))
```

The context-free grammar is the following (`FOL.hs`) for the language:

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

On this way, we can build up any formula given by the language, 
like these ones:

Defining some basic terms (variables):

```Haskell
x,y,z :: Term
x   = Var 0
y   = Var 1
z   = Var 2
```

Defining a predicate
```Haskell
px  = Pred 0 [x]
py  = Pred 1 [y]
pxz = Pred 2 [xz]
```

Some other examples:

```Haskell
let f0 = (Forall x px)
```

```Haskell
let f1  = (Not (Exists x (Imp px (Forall x px))))
```

Finally, if you need the prenex conjuctive normal form of a formula,
the library provides you the method `pcnf::Formula -> Formula`. 

For instance, `pcnf` applies to the last example `f1`:

```Haskell
Prelude> pcnf  (Not (Exists x (Imp px (Forall x px))))
```
gives you:

```Haskell
Forall (Var 2) 
    (Exists (Var 1) 
            (And 
                (Pred 0 [Var 2]) 
                (Not (Pred 0 [Var 1]))))
```

You can find more usage cases in (`Test.hs`)

Testing
---
Run some test cases using HUnit.

```Haskell
Prelude> :load test
*Test> runTestTT tests
Cases: 6  Tried: 6  Errors: 0  Failures: 0
Counts {cases = 6, tried = 6, errors = 0, failures = 0}
```

If you don't have `hunit` library, you can install it as follows in the shell:

```Haskell
cabal install hunit
```