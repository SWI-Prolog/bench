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

:- module(scryer,
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

main(Argv) :-
    argv_options(Argv, _Pos, Options),
    prepare(Options).

term_expansion((:- use_module(library(clpfd))),
               [ (:- use_module(library(clpz))),
                 (:- op(700, xfx, in)),
                 (:- op(700, xfx, my_ins)),
                 (:- op(700, xfx, #=)),
                 (:- op(700, xfx, #\=)),
                 (:- op(450, xfx, ..))
               ]).

prepare(Options) :-
    verbose(Options),
    option(dir(Dir), Options, 'port/programs/scryer'),
    expand_file_name('programs/*.pl', Files),
    (   exists_directory(Dir)
    ->  delete_directory_contents(Dir)
    ;   true
    ),
    context_module(M),
    setup_call_cleanup(
        '$set_source_module'(Old, M),
        modularize_files(Files,
                         [ dir(Dir),
                           has_program(false),
                           module(scryer)
                         ]),
        '$set_source_module'(Old)),
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

write_include(Out, OkFiles) :-
    maplist(mk_has_program(Out), OkFiles),
    maplist(include_file(Out), OkFiles).

mk_has_program(Out, File) :-
    file_prog(File, Prog),
    portray_clause(Out, has_program(Prog)).

include_file(Out, File) :-
    file_base_name(File, RelFile),
    format(Out, ':- use_module(~q).~n', [RelFile]).

test_file(File) :-
    debug(port, "Testing whether scryer-prolog can load ~q ...", [File]),
    file_prog(File, Prog),
    format(string(Goal), "'~w:top'", [Prog]),
    process_create(path('scryer-prolog'),
                   [ File, '-g', Goal, '-g', halt ],
                   [ stdout(pipe(Err)),
                     process(PID)
                   ]),
    read_string(Err, _, ErrString),
    close(Err),
    process_wait(PID, Status),
    (   Status == exit(0),
        ErrString == "",
        debug(port, "   --> ok", [])
    ;   debug(port, "   --> FAILED: ~s", [ErrString]),
        fail
    ).

file_prog(File, Prog) :-
    file_base_name(File, Base),
    file_name_extension(Prog, _, Base).

