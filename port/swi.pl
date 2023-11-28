/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2023, SWI-Prolog Solutions b.v.
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

:- if(current_prolog_flag(dialect, swi)).
:- use_module(library(statistics), [time/1]).
:- use_module(library(backcomp), [current_thread/2]).
:- use_module(library(main), [argv_options/3]).
:- use_module(library(option), [option/2, option/3]).

% disable threading. This is needed for PGO (Profile Guided
% Optimization)
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
	bench(Options).

bench(Options) :-
	option(calibrate(true), Options),
	!,
	tune_counts.
bench(Options) :-
	option(interleaved(Bool), Options),
	option(speedup(N), Options, 1),
	!,
	Factor is 1.0/N,
	run_interleaved(Bool, Factor).
bench(Options) :-
	option(speedup(N), Options, 1),
	F is 1/N,
	(   option(csv(true), Options)
	->  Format = csv
	;   Format = default
	),
	current_output(Out),
	run(Out, F, Format).

opt_type(csv,	      csv,	   boolean).
opt_type(speedup,     speedup,	   number).
opt_type(s,	      speedup,	   number).
opt_type(calibrate,   calibrate,   boolean).
opt_type(interleaved, interleaved, boolean).

opt_help(csv,	      "Use CSV output format").
opt_help(speedup,     "Speedup tests (10 means 10 times faster)").
opt_help(calibrate,   "Generate program/2 clauses from current timing").
opt_help(interleaved, "Time all tests interleaved or sequential").

opt_meta(speedup, 'TIMES').

:- if(current_prolog_flag(wine_version, _)).
get_performance_stats(GC, T):-
	statistics(gctime, GC),		% SWI-Prolog under Wine
	statistics(process_cputime, T).
:- elif(statistics(gctime, _)).
get_performance_stats(GC, T):-
	statistics(gctime, GC),		% SWI-Prolog
	statistics(cputime, T).
:- endif.

no_singletons :-
	style_check(-singleton).

:- thread_local(result/3).		% Program, Time, GC


		 /*******************************
		 *	      CALIBRATE		*
		 *******************************/

%%	tune_counts
%
%	Write the program/2 table below, tuning all counts such that the
%	test runs for about 1 second.

tune_counts :-
	compile_programs,
	forall(use_program(P, _),
	       (   tune_count(P, C),
		   format('~q.~n', [program(P, C)]))).

tune_count(Program, Count) :-
	between(1, 100, I),
	C is 1<<I,
	ntimes(Program, C, T, _),
	T > 0.5, !,
	Count is round(C * (1/T)).

		 /*******************************
		 *	    INTERLEAVED		*
		 *******************************/

:- dynamic(rni/0).
:- discontiguous(run_interleaved/2).

run_interleaved(true, F) :-
	compile_programs,
	findall(N-P, use_program(P, N, F), Pairs),
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

run_interleaved(false, F) :-
	compile_programs,
	findall(N-P, use_program(P, N, F), Pairs),
	phrase(seq_non_interleaved(Pairs), Sequence),
	seq_clause(Sequence, Body),
	retractall(rni),
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
