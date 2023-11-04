# plot_alpha
    set xlabel "(x)"
    set ylabel "f(x)"
    set title "Alpha"

    bit = 10
    bib = 1
    length = 1

    f(x) = ((x * tan(x)) - bit)
    df(x) = tan(x) + (x * (1/cos(x)**2))
    fdf(x)= f(x)/df(x)

    set terminal wxt size 1066,600

    set xrange [-0:16] 
    set yrange [-5:5] 

    set pointsize 2

    plot \
    0 with lines title "0" lt rgb "black" pt 2, \
    f(x) with lines title "f(Alpha)" lt rgb "green" lw 2, \
    df(x) with lines title "f'(Alpha)" lt rgb "gold" lw 2, \
    fdf(x) with lines title "Error = f(Alpha)/f'(Alpha)" lt rgb "blue" lw 2, \
    '..\data/alpha.dat' title "Roots" lt rgb "red" pt 9 

# pause -60 #"Press any key to exit..."
