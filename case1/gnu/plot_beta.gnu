set xlabel "(x)"
set ylabel "f(x)"
set title "Beta"

biT = 1

f(x) = ((x * tan(x)) - biT)
df(x) = (tan(x) + (x * (1 / cos(x)**2)))
fdf(x)= f(x)/df(x)

set terminal wxt size 1066,600

set xrange [-0:16] 
set yrange [-5:5] 

set pointsize 2

plot \
0 with lines title "0" lt rgb "black", \
f(x) with lines title "f(Beta)" lt rgb "green" lw 2, \
df(x) with lines title "f'(Beta)" lt rgb "gold" lw 2, \
fdf(x) with lines title "Error = f(Beta)/f'(Beta)" lt rgb "blue" lw 2, \
'data/beta.dat' title "Roots" lt rgb "red" pt 9 
# '..\data/beta.dat' title "Roots" lt rgb "red" pt 9 

pause -1 "Press any key to exit..."