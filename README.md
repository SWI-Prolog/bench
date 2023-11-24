# SWI-Prolog benchmark suite

This set of benchmarks started as the   `van Roy' benchmarks for Prolog.
They are designed to cover a large   number of aspects, both small-scale
and large(r) scale programs. All benchmarks are pure Prolog; i.e., there
are no constraints, etc. Another problem  with   this  data  is that the
programs are rather old, generally not  using modern libraries and often
avoiding GC because some old implementations   did  not provide it. Also
many todays application manage a lot of text, often using atoms. None of
the benchmarks is concerned with that.

Each test is a non-modular  program   that  provides  a single predicate
top/0. The driver run.pl loads  all  programs   in  their  own module. I
modified most of the driver. The toplevel  is run(+Factor). Each test is
callibrated to be run approximately  the   same  time. Factor scales the
number of iterations. Factor=1 is callibrated for 1 second per benchmark
on  an  AMD3950X,  SWI-Prolog  8.5.2,  GCC-11  using  CMake  build  type
`RelWithDebInfo`

In addition to the `van Roy'   benchmarks,  various benchmarks have been
added to extend the coverage of  this   benchmark  set. Good coverage is
important to maximize the benefits of Program Guided Optimization (PGO).
See CMAKE.md, CMAKE_BUILD_TYPE=PGO

## Running

To run  the benchmark  suite with SWI-Prolog,  simply run  the command
below.  The benchmarks  are calibrated to take about 1  second each on
SWI-Prolog 8.5.2.   You can  speedup the run  using `--speedup=Times`,
e.g.

    swipl run.pl --speedup=10

To create a clustered histogram comparing multiple systems run e.g.

    swipl compare.pl --speedup=10 swi gprolog

Any number of  systems may be specified.  Use `wipl  compare.pl -l` to
see the available  systems and the _Porting_ section  below for adding
systems.  The  results are  saved into  `bench.svg`.  Use  the command
below for other options.

    swipl compare.pl --help



## Porting

The  current version  runs on  SWI-Prolog, YAP,  SICStus Prolog,  Ciao
lang, Scryer  Prolog and GNU-Prolog,  XSB and Trealla Prolog.   To get
the actual  list, run this command  to get the system  identifiers and
their version.

    swipl compare.pl -l

There are two routes for running the benchmarks.

  - For  systems with a  Quintus-derived module systems,  `run.pl` can
    usually  deal with  loading and  configuring the  benchmark suite.
    Currently this supports SWI-Prolog, YAP and SICStus Prolog.

  - For  other  systems we  use `tools/modularize.pl`  to qualify  all
    local predicates with  their file (base name), such  that they can
    be   loaded    into   the   same   Prolog    instance.    We   add
    `ports/prepare/<system>.pl` to call the modularization, test which
    benchmarks can  be loaded  and executed on  the target  system and
    finally generate  a file  that includes all  tests and  provides a
    predicate has_program/1 that provides  all programs that have been
    loaded.      The     prepared     system     is     created     in
    `port/programs/<system>`

	Next, we  provide `port/run/<system>.pl` that loads  the above and
	provides `run(Factor)`  which runs  all benchmarks and  writes the
	CSV output.

When using the second route, proceeds as follows.  First, run

    swipl port/tools/modularize.pl --dir=port/programs/<system> \
		  --include-all=include_all.pl programs/*.pl

Next, you should  have `port/programs/<system>/include_all.pl`.  Tweak
to  get  this running  on  the  target  system  such that  e.g.,  this
benchmark runs:

    ?- 'qsort:top'.

After loading the file, has_program/1  should succeed for every Prolog
that can  be loaded.  Once you  know how to  do that, copy one  of the
`port/prepare/<system>.pl` files.  Most contain code that

  - Call modularize.pl correctly to get the proper programs
  - Evaluate whether a benchmark can be loaded.
  - Generate `include_all.pl` to only include the benchmarks we
    can run.

Now,  copy   and  edit   `port/run/<system>.pl`  and   finally  extend
`compare.pl` to include the new system.


## Legal status

Most of these benchmarks have been written  and shared within the Prolog
community for a very long  time.  Unfortunately   many  of  them have no
official claim that they can  be   redistributed  under established open
source conditions. Note that these files  are   not  part  of any binary
distribution and are only  used  for   drive  PGO  optimization.  Binary
distributions are thus __not__ legally affected by these files.


### Non-free

The following programs  lack  explicit   clear  conditions  that  allows
redistribution.

Source packages that wish to be sure   to  only distribute material that
complies with open source standards  may   remove  these files using the
command below.

    rm $(grep '^\s*- [a-z0-9_]*\.pl' README.md | awk '{print $2}')

  - boyer.pl
  - browse.pl
  - crypt.pl
  - fast_mu.pl
  - flatten.pl
  - fib.pl
  - meta_qsort.pl
  - mu.pl
  - nand.pl
  - perfect.pl
  - pingpong.pl
  - poly_10.pl
  - prover.pl
  - queens_8.pl
  - queens_clpfd.pl
  - reducer.pl
  - sendmore.pl
  - simple_analyzer.pl
  - tak.pl
  - unify.pl
  - zebra.pl
