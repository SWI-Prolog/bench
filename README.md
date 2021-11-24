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
command below. MAKE SURE NOT TO  ADD   ANY  OTHER  MARKDOWN LIST TO THIS
FILE.

    rm $(grep '^\s*-' README.md | awk '{print $2}')

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


