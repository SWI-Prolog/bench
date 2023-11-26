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

:- module(xsb,
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
Error: using module file: /home/janw/src/swipl-devel/bench/port/run/../programs/scryer/include_all

```

Running `include_all.pl` from any other directory seems broken this way.
*/

file_prefix(bench_).

main(Argv) :-
    argv_options(Argv, _Pos, Options),
    prepare(Options).

prepare(Options) :-
    verbose(Options),
    option(dir(Dir), Options, 'port/programs/xsb'),
    expand_file_name('programs/*.pl', Files),
    (   exists_directory(Dir)
    ->  delete_directory_contents(Dir)
    ;   true
    ),
    file_prefix(FilePrefix),
    modularize_files(Files,
                     [ dir(Dir),
                       file_prefix(FilePrefix)
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
    maplist(include_file(Out), OkFiles).

include_file(Out, File) :-
    file_base_name(File, RelFile),
    file_name_extension(Module, _, RelFile),
    format(Out, ':- include(~q).~n', [Module]).

test_file(File) :-
    debug(port, "Testing whether XSB can load ~q ...", [File]),
    process_create(path('xsb'),
                   [ '--quietload', '--noprompt' ],
                   [ stderr(pipe(Err)),
                     stdout(pipe(Out)),
                     stdin(pipe(Stdin)),
                     process(PID)
                   ]),
    thread_create(call_cleanup(write_script(Stdin, File),
                               close(Stdin)), TID),
    call_cleanup(read_string(Err, _, ErrString), close(Err)),
    call_cleanup(read_string(Out, _, OutString), close(Out)),
    thread_join(TID),
    process_wait(PID, Status),
    (   Status == exit(0),
        \+ sub_string(ErrString, _, _, _, "Error"),
        \+ sub_string(OutString, _, _, _, "Error")
    ->  debug(port, "   --> ok", [])
    ;   debug(port, "   --> FAILED: ~s", [ErrString]),
        fail
    ).

write_script(Stdin, File) :-
    file_program(File, Program),
    format(string(Load), "~q", [[File]]),
    format(string(Goal), "'~w:top'->true;halt(1)", [Program]),
    Script =  {|string(Load, Goal)||
                {Load}.
                {Goal}.
               |},
    write(Stdin, Script),
    flush_output(Stdin).

file_program(File, Program) :-
    file_prefix(FilePrefix),
    file_name_extension(Plain, _, File),
    file_base_name(Plain, Base),
    atom_concat(FilePrefix, Program, Base).
