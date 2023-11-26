% Tabling is also important for the memoisation it offers.
% The standard example is calculating Fibonacci numbers.
%
% As an example, this benchmark  calculates   the  N-th Fibonacci number
% without tabling. You can compare  its   execution  time to the version
% with tabling in fib-no-tabling-hprolog.pl
%
% This is also interesting for a paper (Marko, Tom).
%
% Copyright: This code is in the public domain.

top :-
    abolish_all_tables, fib(1000,F),
    F == 70330367711422815821835254877183549770181269836358732742604905087154537118196933579742249494562611733487750449241765991088186363265450223647106012053374121273867339111198139373125598767690091902245245323403501.

enable_tabling.                         % so we can expand
:- table fib/2.

fib(0, 1) :- !.
fib(1, 1) :- !.
fib(N, F) :-
    N>1,
    N1 is N-1,
    N2 is N-2,
    fib(N1, F1),
    fib(N2, F2),
    F is F1 + F2.
