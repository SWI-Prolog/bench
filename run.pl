:- style_check(-singleton).

run(F) :-
	run(current_output, F).

run(S, F):-
	compile_programs,
	format(S, '~p~t~18| ~t~w~25| ~t~w~32|~n', ['Program', 'Time', 'GC']),
	format(S, '~`=t~32|~n', []),
	forall(program(P, N),
	       run_program(P, N, F, S)).

compile_programs :-
	style_check(-singleton),
	forall(program(P, _),
	       load_files(P:P, [silent(true), if(changed)])).


run_program(P, N0, F, S) :-
	N is integer(N0*F),
	run_program(P, N, S).

run_program(Program, N, S) :-
	ntimes(Program, N, Time, GC), !,
	format(S, '~p~t~18| ~t~3f~25| ~t~3f~32|~n', [Program, Time, GC]).

ntimes(M, N, T, GC):-
	statistics(gctime, GC0),
	statistics(cputime, T0),
	ntimes(M, N),
	statistics(cputime, T1),
	statistics(gctime, GC1),
	ntimes_dummy(N),
	statistics(cputime, T2),
	statistics(gctime, GC2),
	T  is (T1-T0) - (T2-T1),
	GC is (GC1-GC0) - (GC2-GC1).

ntimes(_, N) :- N=:=0, !.
ntimes(M, N) :- not_not_top(M), !, N1 is N-1, ntimes(M, N1).

ntimes_dummy(N) :- N=:=0, !.
ntimes_dummy(N) :- not_not_dummy, !, N1 is N-1, ntimes_dummy(N1).

not_not_top(M) :- not_top(M), !, fail.
not_not_top(_).

not_top(M) :- M:top, !, fail.
not_top(_).

not_not_dummy :- not_dummy, !, fail.
not_not_dummy.

not_dummy :- dummy, !, fail.
not_dummy.

dummy.

%%	tune_counts
%
%	Write the program/2 table below, tuning all counts such that the
%	test runs for about 1 second.

tune_counts :-
	forall(program(P, _),
	       (   tune_count(P, C),
		   format('~q.~n', [program(P, C)]))).

tune_count(Program, Count) :-
	between(1, 100, I),
	C is 1<<I,
	ntimes(Program, C, T, _),
	T > 0.5, !,
	Count is round(C * (1/T)).

%%	program(?Program, ?Times)
%
%	Times are tuned on Jan 24, 2010, using SWI-Prolog 5.9.7 on
%	AMD 5400+ (gcc 4.4.1; AMD64 mode)

program(boyer,		 8).
program(browse,		 7).
program(chat_parser,	 46).
program(crypt,		 868).
program(fast_mu,	 4819).
program(flatten,	 8275).
program(meta_qsort,	 966).
program(mu,		 6827).
program(nreverse,	 11378).
program(poly_10,	 105).
program(prover,		 6400).
program(qsort,		 8445).
program(queens_8,	 63).
program(query,		 1219).
program(reducer,	 164).
program(sendmore,	 44).
program(simple_analyzer, 320).
program(tak,		 35).
program(zebra,		 166).


