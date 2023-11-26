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

:- module(modularize,
          [ modularize_file/3,
            modularize_files/2 % +Files, +Options
          ]).
:- use_module(library(apply)).
:- use_module(library(filesex)).
:- use_module(library(listing)).
:- use_module(library(lists)).
:- use_module(library(main)).
:- use_module(library(option)).
:- use_module(library(prolog_code)).
:- use_module(library(terms)).

/** <module> Modularize benchmarks

This script allows to prefix all predicates in   a  file, so they can be
loaded together without conflicts.
*/

:- initialization(main, main).

main(Argv) :-
    argv_options(Argv, Files, Options),
    modularize_files(Files, [argv(Argv)|Options]).

opt_type(prefix,      prefix,      atom).
opt_type(file_prefix, file_prefix, atom).
opt_type(dir,         dir,         atom).
opt_type(has_program, has_program, boolean).
opt_type(multifile,   multifile,   boolean).
opt_type(include_all, include_all, file).

opt_help(prefix,      "Prefix to use rather than `<file>:`").
opt_help(file_prefix, "Rename all files by adding a prefix").
opt_help(dir,         "Directory for results").
opt_help(has_program, "Add has_program/1 multifile clause").
opt_help(multifile,   "Declare has_program/1 multifile in each file").
opt_help(include_all, "Generate file to include add").

%!  modularize_files(+Files, +Options) is det.

modularize_files(Files, Options) :-
    maplist(modularize(Options), Files),
    add_include_all(Files, Options).

modularize(Options, File) :-
    (   option(prefix(Prefix), Options)
    ->  true
    ;   file_program(File, Program),
        atom_concat(Program, :, Prefix)
    ),
    (   option(dir(Dir), Options)
    ->  ensure_directory(Dir),
        file_base_name(File, Base),
        prefix_file_name(Base, OutBase, Options),
        directory_file_path(Dir, OutBase, OutFile),
        setup_call_cleanup(
            open(OutFile, write, Out),
            with_output_to(Out,
                           ( add_header(File, Options),
                             modularize_file(File, Prefix, Options))),
            close(Out))
    ;   modularize_file(File, Prefix, Options)
    ).

file_program(File, Program) :-
    file_name_extension(Plain, _, File),
    file_base_name(Plain, Program).

prefix_file_name(Name, PrefixedName, Options) :-
    option(file_prefix(Prefix), Options, ''),
    atom_concat(Prefix, Name, PrefixedName).

add_header(File, Options) :-
    option(argv(Argv), Options, []),
    get_time(Now),
    format_time(string(Date), '%+', Now),
    format('% Created from "../~w" at ~w using~n', [File, Date]),
    format('%~n'),
    atomics_to_string(Argv, " ", ArgvS),
    format('%    swipl modularize.pl ~s~n', [ArgvS]),
    format('%~n'),
    format('% Copyright from the original file applies.~n'),
    format('% Do not edit.~n~n').

modularize_file(File, Prefix, Options) :-
    read_source_file(File, Terms, Options),
    convlist(pred, Terms, Preds0),
    sort(Preds0, Preds),
    maplist(prefix(Prefix, Preds), Terms, TermsOut),
    (   option(has_program(true), Options, true),
        memberchk(top/0, Preds)
    ->  format('% Make the fact that we have this program public.~n~n'),
        file_program(File, Program),
        (   option(multifile(true), Options)
        ->  my_portray_clause((:- multifile(has_program/1)))
        ;   true
        ),
        my_portray_clause(has_program(Program)),
        nl
    ;   true
    ),
    maplist(my_portray_clause, TermsOut).

my_portray_clause((:- dynamic(Terms))) =>
    format(':- dynamic((~q)).~n', [Terms]).
my_portray_clause((:- discontiguous(Terms))) =>
    format(':- discontiguous((~q)).~n', [Terms]).
my_portray_clause((:- multifile(Terms))) =>
    format(':- multifile((~q)).~n', [Terms]).
my_portray_clause(Term) =>
    portray_clause(Term).

read_source_file(File, Terms, Options) :-
    style_check(-singleton),
    setup_call_cleanup(
        (   push_operators([]),
            prolog_open_source(File, In)
        ),
        read_source(In, Terms, Options),
        (   prolog_close_source(In),
            pop_operators
        )).

read_source(In, Terms, Options) :-
    prolog_read_source_term(In, Term, Expanded, []),
    (   Term == end_of_file
    ->  Terms = []
    ;   hide_term(Term)
    ->  read_source(In, Terms, Options)
    ;   noexpand(Term)
    ->  Terms = [Term|Tail],
        read_source(In, Tail, Options)
    ;   (   is_list(Expanded)
        ->  exclude(hide_term, Expanded, Expanded1),
            append(Expanded1, Tail, Terms)
        ;   Terms = [Expanded|Tail]
        ),
        read_source(In, Tail, Options)
    ).

noexpand((:- table(_))).

hide_term((:-op(_,_,_))) => true.
hide_term((:- non_terminal(_))) => true.
hide_term(_) => fail.

pred((Head :- _), PI) =>
    pi_head(PI, Head).
pred((Head,_Guard => PI), _) =>
    pi_head(PI, Head).
pred((Head => _), PI) =>
    pi_head(PI, Head).
pred((:- _), _) =>
     fail.
pred(Head, PI) =>
    pi_head(PI, Head).

prefix(Prefix, Preds, TermIn, TermOut) :-
    foldsubterms(t_prefix(Prefix, Preds), TermIn, TermOut, _, _).

                                        % rewrite compounds
t_prefix(Prefix, Preds, TermIn, TermOut, _, _) :-
    callable(TermIn),
    pi_head(PI, TermIn),
    memberchk(PI, Preds),
    !,
    TermIn =.. [Name|Args0],
    prefix(Prefix, Preds, Args0, Args),
    atom_concat(Prefix, Name, PName),
    TermOut =.. [PName|Args].
                                        % rewrite predicate indicators
t_prefix(Prefix, Preds, PI, PIOut, _, _) :-
    PI = Name/Arity,
    ground(PI),
    memberchk(PI, Preds),
    atom_concat(Prefix, Name, PName),
    PIOut = PName/Arity.

add_include_all(Files, Options) :-
    option(include_all(InclFile), Options),
    option(dir(Dir), Options),
    !,
    directory_file_path(Dir, InclFile, File),
    setup_call_cleanup(
        open(File, write, Out),
        add_includes(Options, Out, Files),
        close(Out)).
add_include_all(_, _).

add_includes(Options, Out, Files) :-
    (   option(has_program(true), Options, true),
        option(multifile(false), Options, false)
    ->  format(Out, ':- discontiguous(has_program/1).~n~n', [])
    ;   true
    ),
    maplist(add_include(Out), Files).

add_include(Out, File) :-
    file_base_name(File, Base),
    portray_clause(Out, (:- include(Base))).
