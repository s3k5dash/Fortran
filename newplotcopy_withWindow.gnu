set xlabel "Time [sec]"
set ylabel "Radius [m]"
set title "My first plot with gnuplot"

m = "./testimage.det"
plot sin(x)
pause -1 "Press any key to exit..."
