/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2018-2021, VU University Amsterdam
			      CWI, Amsterdam
			      SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

% disable threading. This is needed for PGO (Profile Guided
% Optimization)

swi :-
	current_prolog_flag(version_data, swi(_,_,_,_)).
yap :-
	current_prolog_flag(version_data, yap(_,_,_,_)).
sicstus :-
	current_prolog_flag(version_data, sicstus(_,_,_,_,_)).

have_tabling :-
	\+ sicstus.

:- dynamic
	output_format/1.

:- if(swi).
:- use_module(library(statistics), [time/1]).
:- use_module(library(backcomp), [current_thread/2]).
:- use_module(library(main), [argv_options/3]).
:- use_module(library(option), [option/2, option/3]).

:- if(current_prolog_flag(threads, true)).
:- set_prolog_flag(gc_thread, false).
:- (   current_thread(gc, running)
   ->  set_prolog_gc_thread(false)
   ;   true
   ).
:- endif.

:- style_check(-singleton).

:- initialization(bench, main).

bench :-
	current_prolog_flag(argv, Argv),
	argv_options(Argv, _, Options),
	(   option(csv(true), Options)
	->  asserta(output_format(csv))
	;   true
	),
	option(speedup(N), Options, 1),
	F is 1/N,
	run(F).

opt_type(csv,     csv,     boolean).
opt_type(speedup, speedup, number).

opt_help(csv,     "Use CSV output format").
opt_help(speedup, "Speedup tests (10 means 10 times faster)").

opt_meta(speedup, 'TIMES').

:- endif.

run(F) :-
	current_output(Out),
	run(Out, F).

run(S, F):-
	compile_programs,
	header(S),
	Total = total(0,0,0),
	(   program(P, N, F),
	    current_predicate(P:top/0),	% only if really loaded
	    run_program(P, N, S, Total),
	    fail
	;   true
	),
	Total = total(Count, Time, GC),
	(   Count =:= 0
	->  true
	;   AvgT is Time/Count,
	    AvgGC is GC/Count,
	    footer(S, AvgT, AvgGC)
	).

header(S) :-
	output_format(csv),
	!,
	format(S, 'program,time,gc~n', []).
header(S) :-
	format(S, '~p~t~18| ~t~w~25| ~t~w~32|~n', ['Program', 'Time', 'GC']),
	format(S, '~`=t~32|~n', []).

footer(S, AvgT, AvgGC) :-
	output_format(csv),
	!,
	report_time(S, average, AvgT, AvgGC).
footer(S, AvgT, AvgGC) :-
	format(S, '~t~w~18| ~t~3f~25| ~t~3f~32|~n', [average, AvgT, AvgGC]).


:- multifile user:file_search_path/2.
:- dynamic   user:file_search_path/2.

:- (   file_search_path(bench, _)
   ->  true
   ;   prolog_load_context(directory, Dir),
       assert(user:file_search_path(bench, Dir))
   ).

compile_programs :-
	forall(program(P, _),
	       compile_program(P)).

compile_program(P) :-
	no_singletons,
	(   program(P, _),
	    absolute_file_name(bench(P), AbsFile,
			       [ file_type(prolog),
				 access(read),
				 file_errors(fail)
			       ]),
	    load_files(P:AbsFile, [if(changed)]),
	    fail
	;   true
	).

:- if(sicstus).
no_singletons :-
	set_prolog_flag(single_var_warnings, off).
:- else.
no_singletons :-
	style_check(-singleton).
:- endif.

run_program(Program, N, S, Total) :-
	ntimes(Program, N, Time, GC), !,
	add(1, Total, 1),
	add(2, Total, Time),
	add(3, Total, GC),
	report_time(S, Program, Time, GC).

report_time(S, Program, Time, GC) :-
	output_format(csv),
	!,
	format(S, '~w,~3f,~3f~n', [Program, Time, GC]).
report_time(S, Program, Time, GC) :-
	format(S, '~p~t~18| ~t~3f~25| ~t~3f~32|~n', [Program, Time, GC]).


:- if(sicstus).
add(_, _, _).
:- else.
add(Arg, Term, Time) :-
	arg(Arg, Term, T0),
	T is T0+Time,
	nb_setarg(Arg, Term, T).
:- endif.

:- if(swi).
:- if(current_prolog_flag(wine_version, _)).
get_performance_stats(GC, T):-
	statistics(gctime, GC),		% SWI-Prolog under Wine
	statistics(process_cputime, T).
:- elif(statistics(gctime, _)).
get_performance_stats(GC, T):-
	statistics(gctime, GC),		% SWI-Prolog
	statistics(cputime, T).
:- endif.
:- elif(yap).
get_performance_stats(GC, T):-
	statistics(garbage_collection, [_,_,TGC]),
	statistics(cputime, [TT,_]),
	GC is TGC / 1000,
	T is TT / 1000.
:- else.
get_performance_stats(GC, T):-
	statistics(garbage_collection, [_,_,TGC]),
	statistics(runtime, [TT,_]),
	GC is TGC / 1000,
	T is TT / 1000.
:- endif.

ntimes(M, N, T, GC):-
	get_performance_stats(GC0, T0),
	ntimes(M, N),
	get_performance_stats(GC1, T1),
	ntimes_dummy(N),
	get_performance_stats(GC2, T2),
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
	compile_programs,
	forall(program(P, _),
	       (   tune_count(P, C),
		   format('~q.~n', [program(P, C)]))).

tune_count(Program, Count) :-
	between(1, 100, I),
	C is 1<<I,
	ntimes(Program, C, T, _),
	T > 0.5, !,
	Count is round(C * (1/T)).

program(P, N, F) :-
	program(P, N0),
	N is max(1, round(N0*F)).

%!	program(?Program, ?Times)
%
%	Times are tuned on Nov 24,  2021 using SWI-Prolog 8.5.2 compiled
%	using GCC-11 on AMD 3950X  to  run   for  1  second.  Tuning did
%	__not__ use -O and used the normal ``RelWithDebInfo`` build.

program(boyer,		 47).
program(browse,		 32).
program(chat_parser,	 128).
program(crypt,		 3480).
program(derive,		 279547).
program(fast_mu,	 17354).
program(flatten,	 33146).
program(log10,		 1199682).
program(meta_qsort,	 3923).
program(mu,		 23549).
program(nand,		 1005).
program(nreverse,	 71340).
program(ops8,		 744744).
program(perfect,	 1423) :-
	current_prolog_flag(bounded, false).
program(poly_10,	 420).
program(prover,		 21909).
program(qsort,		 27207).
program(queens_8,	 232).
program(query,		 4192).
program(reducer,	 567).
program(sendmore,	 127).
program(serialise,	 53129).
program(simple_analyzer, 1144).
program(tak,		 128).
program(times10,	 704988).
program(unify,		 8363).
program(zebra,		 576).

% Later additions
program(sieve,		 56).
program(queens_clpfd,	 67) :-
	\+ yap,				% clpfd is broken in YAP 6.5.0
	\+ sicstus.			% Requires some porting
program(pingpong,	 25) :-
	have_tabling.
program(fib,	         266) :-
	have_tabling,
	current_prolog_flag(bounded,false).
program(moded_path,      37773) :-
	have_tabling,
	\+ yap.				% Yap lacks lattice answer subsumption
program(det,	         169) :-
	swi.


		 /*******************************
		 *	    INTERLEAVED		*
		 *******************************/

:- dynamic rni/0.

run_interleaved(F) :-
	compile_programs,
	findall(N-P, program(P, N, F), Pairs),
	phrase(seq_interleaved(Pairs), Sequence),
	seq_clause(Sequence, Body),
	retractall(rni),
	assert((rni :- Body), Ref),
	garbage_collect,
	time(rni),
	erase(Ref).

seq_interleaved([]) --> !.
seq_interleaved(Pairs) -->
	seq_interleaved(Pairs, Rest),
	seq_interleaved(Rest).

seq_interleaved([], []) -->
	[].
seq_interleaved([1-P|T0], T) --> !,
	[P],
	seq_interleaved(T0, T).
seq_interleaved([N-P|T0], [N1-P|T]) -->
	[P],
	{ N1 is N - 1 },
	seq_interleaved(T0, T).

seq_clause([], true).
seq_clause([H|T], (\+ \+ H:top, G)) :-
	seq_clause(T, G).

run_non_interleaved(F) :-
	compile_programs,
	findall(N-P, program(P, N, F), Pairs),
	phrase(seq_non_interleaved(Pairs), Sequence),
	seq_clause(Sequence, Body),
	assert((rni :- Body), Ref),
	garbage_collect,
	time(rni),
	erase(Ref).

seq_non_interleaved([]) -->
	[].
seq_non_interleaved([0-_|T]) --> !,
	seq_non_interleaved(T).
seq_non_interleaved([N-P|T]) -->
	[P],
	{ N1 is N - 1 },
	seq_non_interleaved([N1-P|T]).
