:- use_module('../programs/scryer/include_all.pl').
:- use_module('../../programs.pl').
:- use_module(library(lists)).

:- dynamic(result/3).

run(F) :-
    run(user_output, F, csv).

run(S, F, Format):-
    \+ (retract(result(_,_,_)), fail),
    header(S, Format),
    (   use_program(P, N, F),
	run_program(P, N, S, Format),
	fail
    ;   true
    ),
    findall(t(Time, GC), retract(result(_,Time,GC)), List),
    split(List, Times, GCs),	% avoid library preds for portability
    suml(Times, Time),
    suml(GCs, GC),
    length(List, Count),
    AvgT is Time/Count,
    AvgGC is GC/Count,
    footer(S, AvgT, AvgGC, Format).

split([], [], []).
split([t(T,G)|M], [T|Ta], [G|Tb]) :-
    split(M, Ta, Tb).

suml(List, N) :-
    suml_(List, 0, N).

suml_([], N, N).
suml_([H|T], N0, N) :-
    N1 is N0+H,
    suml_(T, N1, N).

header(S, csv) :-
    write_term(S, 'program,time,gc\n', []).

footer(S, AvgT, AvgGC, csv) :-
    report_time(S, average, AvgT, AvgGC, csv).

report_time(S, Program, Time, GC, csv) :-
    write_term(S, Program, []), write(','),
    write_term(S, Time, []), write(','),
    write_term(S, GC, []), nl.

use_program(Program, N, F) :-
    program(Program, N0),
    has_program(Program),
    N is max(1, round(N0*F)).

run_program(Program, N, S, Format) :-
%    write_term(S, Program, []), nl,
    ntimes(Program, N, Time, GC), !,
    assertz(result(Program, Time, GC)),
    report_time(S, Program, Time, GC, Format).

ntimes(M, N, T, GC):-
    top(M, Goal),
    get_performance_stats(GC0, T0),
    ntimes(Goal, N),
    get_performance_stats(GC1, T1),
    ntimes_dummy(N),
    get_performance_stats(GC2, T2),
    T  is (T1-T0) - (T2-T1),
    GC is (GC1-GC0) - (GC2-GC1).

top(M, Goal) :-
    atom_concat(M, ':top', Goal).

ntimes(_, N) :- N=:=0, !.
ntimes(M, N) :- not_not_top(M), !, N1 is N-1, ntimes(M, N1).

ntimes_dummy(N) :- N=:=0, !.
ntimes_dummy(N) :- not_not_dummy, !, N1 is N-1, ntimes_dummy(N1).

not_not_top(M) :- not_top(M), !, fail.
not_not_top(_).

not_top(Goal) :- call(Goal), !, fail.
not_top(_).

not_not_dummy :- not_dummy, !, fail.
not_not_dummy.

not_dummy :- dummy, !, fail.
not_dummy.

dummy.

get_performance_stats(0, T0) :-
    '$cpu_now'(T0).
