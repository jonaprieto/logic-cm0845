Program name: `cnf.pr`
Name: Jonathan S. Prieto. C.
Compiler used: SWI-Prolog (Multi-threaded, 64 bits, Version 7.2.3)
Description: This program aims to translate any propositional
formula fbfs to CNF equivalent. The main method is named `cnf`.
The syntax is the following.

The call `cnf(A,B)` where `A` is a logical preposition formula and `B` his 
equivalente in CNF representation.

I mapped the binary and unary logic operators as follows.

- Negation of literal A: neg(A).
- Conjunction between A,B: c(A,B) 
- Disjunction between A,B: d(A,B) 
- Implication between A,B: if(A,B)
- Equivalent between A,B: iff(A,B)
- Symbols. Atoms.

For instance, here are some calls.

```Prolog
?- cnf(neg(d(neg(p), neg(q))), X).
X = c(p, q)

?- cnf(c(a,d(p,q)), X).
X = c(a, d(p, q)) .

?- cnf(if(a,if(b,c)), X).
X = d(neg(a), d(neg(b), c))
```