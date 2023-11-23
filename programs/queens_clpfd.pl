% Copied from https://swish.swi-prolog.org/example/clpfd_queens.pl
% Author: Markus Triska

top :- n_queens(50,_), fail.
top.

:- use_module(library(clpfd)).
:- op(700, xfx, ins).

n_queens(N, Qs) :-
	length(Qs, N),
	Qs ins 1..N,
	safe_queens(Qs).

safe_queens([]).
safe_queens([Q|Qs]) :-
	safe_queens(Qs, Q, 1),
	safe_queens(Qs).

safe_queens([], _, _).
safe_queens([Q|Qs], Q0, D0) :-
	Q0 #\= Q,
	abs(Q0 - Q) #\= D0,
	D1 #= D0 + 1,
	safe_queens(Qs, Q0, D1).

:- if(\+current_predicate(ins/2)).
ins([], _).
ins([H|T], D) :-
	H in D,
	ins(T, D).
:- endif.
