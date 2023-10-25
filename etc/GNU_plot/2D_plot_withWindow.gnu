set xlabel "Time [sec]"
set ylabel "Radius [m]"
set title "My first plot with gnuplot"


f(x) = 8*tan(x)
fs(x) = tan(x*8)

set xrange [-1:1] 
set yrange [-1:1] 


plot f(x) with lines, 0 with lines, fs(x)
pause -1 # "Press any key to exit..."