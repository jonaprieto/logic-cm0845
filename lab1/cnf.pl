%%
%% CNF Traslator.
%% Author: Jonathan S. Prieto C.
%% Date: 6/03/16
%% Course: Logic-CM0845
%% Basic syntax: cnf(A,B). B is the cnf form for the formula A.
%%


% The following lines are the base cases of operators.
% So, we give the explict solutions to aim correctness of program.

cnf(X, X) :- atom(X).
cnf(F, F) :- F = neg(X), atom(X).
cnf(F, X) :- F = neg(neg(X)), atom(X).
cnf(F, B) :- F = neg(X), cnf(neg(X),B).

cnf(if(X,Y), d(neg(X), Y)) :- atom(X), atom(Y).
cnf(if(X,Y), d(neg(A), B)) :- cnf(X, A), cnf(Y,B).
cnf(iff(X,Y), c(d(neg(X), Y),d(A, neg(B)) :- atom(X), atom(Y).
cnf(iff(X,Y), c(d(neg(A), B),d(A, neg(B)) :- cnf(X, A), cnf(Y,B).

%% %% % dealing with negation, and double apply of it.

cnf(F, d(neg(X), neg(Y))) :- F = neg(c(X,Y)), atom(X), atom(Y).
cnf(F, c(neg(X), neg(Y))) :- F = neg(d(X,Y)), atom(X), atom(Y).
cnf(F, d(c(A, B),c(A, C))):- F = d(X,c(Y,Z)), cnf(X,A), cnf(Y,B), cnf(Z, C).
cnf(F, d(c(A, B),c(A, C))):- F = d(c(Y,Z),X), cnf(X,A), cnf(Y,B), cnf(Z, C).


%% %% % conjunction, and disjunction binary operators
cnf(F, c(A,B)) :- F = c(X,Y), cnf(X,A), cnf(Y,B).
cnf(F, d(A,B)) :- F = d(X,Y), cnf(X,A), cnf(Y,B).

