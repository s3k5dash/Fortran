set terminal png size 1280,941
set output 'testimage.png'

set xlabel "Time [sec]"
set ylabel "Radius [m]"
set zlabel "Radius [m]"
set title "My first plot with gnuplot"

set isosamples 80
set xrange [-5:2] 
set yrange [-5:2] 
set zrange [-1:0.0] 
set hidden3d



f(x) = cos(x)

f(y) = cos(y)

splot f(x)*f(y) 