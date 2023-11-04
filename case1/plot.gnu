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
    'data/alpha.dat' title "Roots" lt rgb "red" pt 9 

pause -1 "Press any key to exit..."

# plot_beta
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
    0 with lines title "0" lt rgb "black" pt 2, \
    f(x) with lines title "f(Beta)" lt rgb "green" lw 2, \
    df(x) with lines title "f'(Beta)" lt rgb "gold" lw 2, \
    fdf(x) with lines title "Error = f(Beta)/f'(Beta)" lt rgb "blue" lw 2, \
    'data/beta.dat' title "Roots" lt rgb "red" pt 9 

pause -1 "Press any key to exit..."

# plot_Qt


    set xlabel "t"
    set ylabel "Q"
    set title "Q_t"

    set terminal wxt size 1066,600

    unset xrange
    unset yrange

    set pointsize 1

    plot 'data/fQt.dat' using 1:2 with lines title "Q" lt rgb "red" lw 2 

pause -1  "Press any key to exit..."

# plot_Ux

    set xlabel "X"
    set ylabel "U"
    set title "U_x"

    set terminal wxt size 1066,600

    set pointsize 1

    plot 'data/fUx.dat' using 1:2 with lines title "u" lt rgb "red" lw 2 

pause -1  "Press any key to exit..."