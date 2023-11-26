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

:- module(trealla,
          [ prepare/1
          ]).
:- use_module('../tools/modularize').
:- use_module(library(main)).
:- use_module(library(strings)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(filesex)).
:- use_module(library(option)).
:- use_module(library(process)).

:- initialization(main, main).

/** <module> Prepare benchmarks for Trealla

This  part  works  and  we   can    run   `tpl   -l  include_all.pl`  in
`port/programs/trealla` afterwards with  a   working  has_program/1  and
working run_top/1. When started from   `port/run/trealla.pl`  though, we
get

```
Error: using module file: chat_parser.pl
Error: using module file: /home/janw/src/swipl-devel/bench/port/run/../programs/trealla/include_all
```

Running `include_all.pl` from any other directory seems broken this way.
*/


main(Argv) :-
    argv_options(Argv, _Pos, Options),
    prepare(Options).

prepare(Options) :-
    verbose(Options),
    option(dir(Dir), Options, 'port/programs/trealla'),
    expand_file_name('programs/*.pl', Files),
    (   exists_directory(Dir)
    ->  delete_directory_contents(Dir)
    ;   true
    ),
    modularize_files(Files,
                     [ dir(Dir),
                       has_program(false)
                     ]),
    generate_include_all(Dir).

opt_type(dir,     dir,     directory).
opt_type(verbose, verbose, boolean).
opt_type(v,       verbose, boolean).

verbose(Options) :-
    option(verbose(true), Options),
    !,
    debug(port).
verbose(_).

generate_include_all(Dir) :-
    atom_concat(Dir, '/*.pl', Pattern),
    expand_file_name(Pattern, Files),
    include(test_file, Files, OkFiles),
    directory_file_path(Dir, 'include_all.pl', File),
    setup_call_cleanup(
        open(File, write, Out),
        write_include(Out, OkFiles),
        close(Out)).

%!  write_include(+Out, +OkFiles:list(atom)) is det.
%
%   Generate the output  program.   Note  that current_predicate(Pred/0)
%   claims Pred/0 is not a predicate indicator.

write_include(Out, OkFiles) :-
    format(Out, '~s',
           {|string||
            :- module(programs,
                      [ run/1
                      ]).

            has_program(P) :-
                atom_concat(P, ':top', Pred),
                predicate_property(Pred, _),
                !.

           |}),
    include_file_contents(Out, '../../programs.pl'),
    include_file_contents(Out, '../run/trealla.pl'),
    maplist(top_link_clause(Out), OkFiles),
    maplist(include_file(Out), OkFiles).

here.

include_file_contents(Stream, File) :-
    source_file(here, Here),
    file_directory_name(Here, ThisDir),
    directory_file_path(ThisDir, File, AbsFile),
    setup_call_cleanup(
        open(AbsFile, read, In),
        copy_stream_data(In, Stream),
        close(In)).

%!  top_link_clause(+Out, +File) is det.
%
%   `run_top(Goal) :- call(Goal).` crashes. This seems caused by call/1.
%   Using explicit link clauses works.

top_link_clause(Out, File) :-
    file_base_name(File, RelFile),
    file_name_extension(Prog, _, RelFile),
    atom_concat(Prog, ':top', Goal),
    portray_clause(Out, (run_top(Goal) :- Goal)).

include_file(Out, File) :-
    file_base_name(File, RelFile),
    format(Out, ':- use_module(~q).~n', [RelFile]).

test_file(File) :-
    debug(port, "Testing whether Trealla Prolog can load ~q ...", [File]),
    file_prog(File, Prog),
    Prog \== sieve,                     % too slow
    format(string(Goal), "'~w:top',halt", [Prog]),
    process_create(path('tpl'),
                   [ '-g', Goal, '-l', File ],
                   [ stdout(pipe(Err)),
                     process(PID)
                   ]),
    read_string(Err, _, ErrString),
    close(Err),
    process_wait(PID, Status),
    (   Status == exit(0),
        ErrString == ""
    ->  debug(port, "   --> ok", [])
    ;   debug(port, "   --> FAILED: ~s", [ErrString]),
        fail
    ).

file_prog(File, Prog) :-
    file_base_name(File, Base),
    file_name_extension(Prog, _, Base).

