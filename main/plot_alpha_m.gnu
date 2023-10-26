set xlabel "x-axis"
set ylabel "y-axis"
set title "Newton Raphson"

bit = 10
bib = 1
length = 1

f(x) = ((x * (bib + bit) )/ (x**2 - (bib * bit))) - (length * tan(x))
df(x) = ((((bib + bit) * (x**2 - (bib * bit))) - ((bib + bit) * (2.0 * x**2))) / ((x**2 - (bib * bit))**2)) -( length/cos(x)**2)
fdf(x)= f(x)/df(x)

set terminal wxt size 1066,600

set xrange [-0:16] 
set yrange [-9:9] 

set pointsize 2

plot \
0 with lines title "y = 0" lt rgb "black" pt 2, \
f(x) with lines title "f(x)" lt rgb "green" lw 2, \
df(x) with lines title "f'(x)" lt rgb "gold" lw 2, \
fdf(x) with lines title "Error = f(x)/f'(x)" lt rgb "blue" lw 2, \
'alpha_m.dat' title "x" lt rgb "red" pt 9 

pause -1 # "Press any key to exit..."