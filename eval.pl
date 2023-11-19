% Author: Jan Wielemaker
% This code is in the public domain

:- if(current_prolog_flag(dialect, sicstus)).
:- use_module(library(between)).
:- endif.

top :-
    t_(1000, 1).

t(D,N) :-
    time(t_(D,N)).

t_(D,N) :-
    add(D, Expr),
    (   between(1, N, _),
        _ is Expr,
        fail
    ;   true
    ).

add(0, 1) :- !.
add(N, (Expr+N)) :-
    N2 is N - 1,
    add(N2, Expr).
