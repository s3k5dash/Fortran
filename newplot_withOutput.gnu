set terminal png size 1280,941
set output 'testimage.png'

set xlabel "Time [sec]"
set ylabel "Radius [m]"
set zlabel "Radius [m]"
set title "My first plot with gnuplot"

splot sin(x)