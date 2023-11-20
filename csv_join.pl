:- module(csv_join,
          []).
:- use_module(library(main)).
:- use_module(library(csv)).

:- initialization(main, main).

main(Argv) :-
    argv_options(Argv, Files, Options),
    option(out(Out), Options),
    maplist(system, Files, Systems),
    maplist(csv_read_file, Files, Data),
    pairs_keys_values(Pairs, Systems, Data),
    findall(P, distinct(P, program(Data, P)), Programs),
    maplist(make_row(Systems, Pairs), Programs, Rows),
    Hdr =.. [row, program | Systems],
    csv_write_file(Out, [Hdr|Rows]).

opt_type(out, out, file(write)).
opt_type(o,   out, file(write)).

system(File, System) :-
    file_base_name(File, Plain),
    file_name_extension(System, _, Plain).

program(Data, P) :-
    member(System, Data),
    member(row(P, _, _), System),
    P \== program.

make_row(Systems, Pairs, Program, Row) :-
    maplist(system_time(Program, Pairs), Systems, SysTimes),
    Row =.. [row, Program | SysTimes].

system_time(Program, Pairs, System, Time) :-
    memberchk(System-Data, Pairs),
    (   memberchk(row(Program, UTime, GC), Data)
    ->  Time is UTime+GC
    ;   Time = (-)
    ).
