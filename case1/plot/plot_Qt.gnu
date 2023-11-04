set xlabel "t"
set ylabel "Q"
set title "Q_t"

set terminal wxt size 1066,600

# set xrange [0:90] 
# set yrange [0:1] 

set pointsize 1

plot 'data/fQt.dat' using 1:2 with lines title "Q" lt rgb "red" lw 2 
# plot '..\data/fQt.dat' using 1:2 with lines title "Q" lt rgb "red" lw 2 

pause -1  "Press any key to exit..."