bench_swi()
{ swipl -O run.pl --csv > swi.csv
}

bench_yap()
{ yap -l run.pl -g 'run(1,csv),halt' > yap.csv
}

bench_sicstus()
{ sicstus -l run.pl > sicstus.csv <<EOF
run(1,csv).
halt.
EOF
}

bench_swi
bench_yap
bench_sicstus
