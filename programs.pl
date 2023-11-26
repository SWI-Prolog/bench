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
program(perfect,	 1423).
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
program(divide10,	 698324).
program(unify,		 8363).
program(zebra,		 576).

% Later additions
program(sieve,		 56).
program(queens_clpfd,	 285).
program(pingpong,	 25).
program(fib,	         266).
program(moded_path,      37773).
program(det,	         169).
program(eval,		 10000).

program_condition(perfect,      unbounded).
program_condition(queens_clpfd, not(yap)).      % clpfd is broken in YAP 6.5.0
program_condition(pingpong,     tabling).
program_condition(fib,          tabling).
program_condition(fib,          unbounded).
program_condition(moded_path,   tabling).
program_condition(moded_path,   not(yap)).      % No lattice answer subsumption
program_condition(det,          ssu).

system_satisfies(unbounded) :-
    current_prolog_flag(bounded, false).
system_satisfies(not(Dialect)) :-
    \+ current_prolog_flag(dialect, Dialect).
system_satisfies(tabling) :-
    \+ current_prolog_flag(dialect, sicstus).
system_satisfies(ssu) :-
    current_prolog_flag(dialect, swi).
