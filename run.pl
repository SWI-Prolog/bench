/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (c)  2018-2024, VU University Amsterdam
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

:- if(current_prolog_flag(dialect, swi)).
:- include('port/swi').
:- elif(current_prolog_flag(dialect, yap)).
:- include('port/yap').
:- elif(current_prolog_flag(dialect, sicstus)).
:- include('port/sicstus').
:- endif.

:- ensure_loaded(programs).
:- initialization(compile_programs).

%!  run(+Factor) is det.
%!  run(+Factor, +Format) is det.
%
%   Run all tests, rescaling the number  of iterations using Factor. For
%   example, run(0.1) runs the benchmrks 10 times faster.
%
%   @arg Format is the  reporting  format.   Using  `csv`,  it emits the
%   results as CSV. Otherwise  the  results   are  formatted  for  human
%   consumption.

run(F) :-
    current_output(Out),
    run(Out, F, default).

run(F, Format) :-
    current_output(Out),
    run(Out, F, Format).

run(S, F, Format):-
    retractall(result(_,_,_)),
    header(S, Format),
    (   use_program(P, N, F),
        run_program(P, N, S, Format),
        fail
    ;   true
    ),
    findall(t(Time, GC), retract(result(_,Time,GC)), List),
    summary(S, List, Format).

summary(S, [], Format) :-
    !,
    footer(S, nan, nan, Format).
summary(S, List, Format) :-
    split(List, Times, GCs),        % avoid library preds for portability
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
    !,
    format(S, 'program,time,gc~n', []).
header(S, _) :-
    format(S, '~p~t~18| ~t~w~25| ~t~w~32|~n', ['Program', 'Time', 'GC']),
    format(S, '~`=t~32|~n', []).

footer(_, _AvgT, _AvgGC, csv) :-               % average is handled outside
    !.
footer(S, nan, nameAvgGC, _) :-
    !,
    format(S, 'No tests where executed~n', []).
footer(S, AvgT, AvgGC, _) :-
    format(S, '~t~w~18| ~t~3f~25| ~t~3f~32|~n', [average, AvgT, AvgGC]).

use_program(Program, Times) :-
    program(Program, Times),
    \+ ( program_condition(Program, Cond), % = forall/2
         \+ system_satisfies(Cond)).

use_program(P, N, F) :-
    use_program(P, N0),
    has_program(P),
    N is max(1, round(N0*F)).

:- if(current_prolog_flag(static, true)).
% Allows compiling to a single SWI-Prolog .qlf file.  First,
% create the include files using
%
%    swipl port/tools/modularize.pl --dir=port/programs/swi \
%                  --include-all=include_all.pl programs/*.pl
%
% Next, compile using (--no-threads if you want to use this in e.g.
% WASM)
%
%    swipl -Dstatic --no-threads -O qlf compile --include run.pl
compile_programs.
:- multifile(has_program/1).
:- include('port/programs/swi/include_all').
:- else.
compile_programs :-
    \+ ( use_program(P, _),                % = forall/2.
         \+ compile_program(P)).

:- if(current_prolog_flag(dialect, gprolog)).
compile_program(P) :-
    atom_concat(P, '.pl', File),
    consult(File).
:- elif(current_prolog_flag(dialect, ciao)).
compile_program(P) :-
    atom_concat(P, '.pl', File),
    ensure_loaded(File).
:- else.
:- multifile(user:file_search_path/2).
:- dynamic(user:file_search_path/2).

:- (   file_search_path(bench, _)
   ->  true
   ;   prolog_load_context(directory, Dir),
       atom_concat(Dir, '/programs', ProgDir),
       assert(user:file_search_path(bench, ProgDir))
   ).

compile_program(P) :-
    no_singletons,
    absolute_file_name(bench(P), AbsFile,
                       [ file_type(source),
                         access(read),
                         file_errors(fail)
                       ]),
    load_files(P:AbsFile, [if(changed)]).

has_program(P) :-
    current_predicate(P:top/0).

:- endif.
:- endif.

run_program(Program, N, S, Format) :-
    ntimes(Program, N, Time, GC),
    !,
    assertz(result(Program, Time, GC)),
    report_time(S, Program, Time, GC, Format).

report_time(S, Program, Time, GC, csv) :-
    !,
    format(S, '~w,~3f,~3f~n', [Program, Time, GC]).
report_time(S, Program, Time, GC, _) :-
    format(S, '~p~t~18| ~t~3f~25| ~t~3f~32|~n', [Program, Time, GC]).

:- meta_predicate ntimes(0,?).

ntimes(M, N, T, GC):-
    top(M, Goal),
    get_performance_stats(GC0, T0),
    ntimes(Goal, N),
    get_performance_stats(GC1, T1),
    ntimes(dummy, N),
    get_performance_stats(GC2, T2),
    T  is (T1-T0) - (T2-T1),
    GC is (GC1-GC0) - (GC2-GC1).

:- if(current_prolog_flag(static, true)).
top(M, Goal) :-
    atom_concat(M, ':top', Goal).
:- else.
top(M, M:top).
:- endif.

ntimes(_, N) :- N=:=0, !.
ntimes(M, N) :- not_not_top(M), !, N1 is N-1, ntimes(M, N1).

not_not_top(M) :- not_top(M), !, fail.
not_not_top(_).

not_top(Goal) :- call(Goal), !, fail.
not_top(_).

dummy.
