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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Not working.  See ../prepare/trealla.pl
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- include(include_all).
:- include(programs).

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
%    write_term(S, Program, []), nl,
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

not_dummy :- dummy, !, fail.
not_dummy.

dummy.

get_performance_stats(0, RT) :-
    statistics(runtime, [RT|_]).

