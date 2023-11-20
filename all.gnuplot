set term svg
set output "bench.svg"

set datafile separator ','
set boxwidth 0.9 absolute
set style fill solid 1.00 border lt -1
set key fixed right top vertical Right noreverse noenhanced autotitle nobox
set style histogram clustered gap 2 title textcolor lt -1
set datafile missing '-'
set style data histograms
set xtics border in scale 0,0 nomirror rotate by -45 autojustify noenhanced
set xtics  norangelimit
set xtics   ()
set title "Prolog benchmark suite from https://github.com/SWI-Prolog/bench.git"
set colorbox vertical origin screen 0.9, 0.2 size screen 0.05, 0.6 front noinvert bdefault
set linetype 1 lc rgb "red"
set linetype 2 lc rgb "green"
set linetype 3 lc rgb "blue"

plot 'all.csv' using 2:xtic(1) ti col, '' u 3 ti col, '' u 4 ti col

# pause -1 "Hit return to continue"
# reset
