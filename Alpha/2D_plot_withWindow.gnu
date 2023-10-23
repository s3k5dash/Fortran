set xlabel "Time [sec]"
set ylabel "Radius [m]"
set title "My first plot with gnuplot"

#m = "./testimage.det"


f(x) = (11 * x) / (x**2 - 11) - 1 * tan(x)
df(x) = (((11) * (x**2 - (10)) - (11) * (2 * x**2)) / ((x**2 - (10))**2)) - 1 / cos(x)**2
fdf(x)= f(x)/df(x)
set xrange [-1:1] 
set yrange [-1:1] 


plot fdf(x) with lines, 0 with lines, 'data.csv' lt rgb "black"

pause -1 # "Press any key to exit..."