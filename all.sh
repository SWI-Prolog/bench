if [ -z "$1" ]; then
    systems="swi yap sicstus"
else
    systems="$*"
fi

run()
{ case $1 in
     swi)
	 swipl -O run.pl --csv
	 ;;
     yap)
	 yap -l run.pl -g 'run(1,csv),halt' 2>/dev/null
	 ;;
     sicstus)
	 sicstus -l run.pl <<EOF 2>/dev/null
run(1,csv).
halt.
EOF
	 ;;
  esac
}

csv=
for s in $systems; do
    echo -n "$s ... "
    run $s > $s.csv
    echo "done"
    csv+=" $s.csv"
done

echo -n "Merging to all.csv ... "
swipl csv_join.pl -o all.csv $csv
echo "done"

echo -n "Generating bar chart ... "
gnuplot all.gnuplot
echo "Wrote bench.svg"
