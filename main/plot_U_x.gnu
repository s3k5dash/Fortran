set xlabel "x-axis"
set ylabel "y-axis"
set title "U_x"

set terminal wxt size 1066,600

set xrange [0:90] 
set yrange [0:1] 

set pointsize 1

plot 'fU_x.dat' using 1:2 with linespoint title "u" lt rgb "red" pt 1 lw 2 

pause -1  "Press any key to exit..."