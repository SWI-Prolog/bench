:- include('../programs/gprolog/include_all').
:- include('../../programs').

run(F) :-
    run(user_output, F, csv).

run(S, F, Format):-
    header(S, Format),
    (   use_program(P, N, F),
	run_program(P, N, S, Format),
	fail
    ;   true
    ).

header(S, csv) :-
    write_term(S, 'program,time,gc\n', []).

report_time(S, Program, Time, GC, csv) :-
    write_term(S, Program, []), write(','),
    write_term(S, Time, []), write(','),
    write_term(S, GC, []), nl.

use_program(Program, N, F) :-
    program(Program, N0),
    has_program(Program),
    N is max(1, round(N0*F)).

run_program(Program, N, S, Format) :-
    ntimes(Program, N, Time, GC), !,
    report_time(S, Program, Time, GC, Format).

ntimes(M, N, T, GC):-
    top(M, Goal),
    get_performance_stats(GC0, T0),
    ntimes(Goal, N),
    get_performance_stats(GC1, T1),
    ntimes(dummy, N),
    get_performance_stats(GC2, T2),
    T  is (T1-T0) - (T2-T1),
    GC is (GC1-GC0) - (GC2-GC1).

top(M, Goal) :-
    atom_concat(M, ':top', Goal).

ntimes(_, N) :- N=:=0, !.
ntimes(M, N) :- not_not_top(M), !, N1 is N-1, ntimes(M, N1).

not_not_top(M) :- not_top(M), !, fail.
not_not_top(_).

not_top(Goal) :- call(Goal), !, fail.
not_top(_).

dummy.

get_performance_stats(0, T0) :-
    statistics(runtime, [RT|_]),
    T0 is RT/1000.
