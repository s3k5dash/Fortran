# a1 = 1
f1(x) = a1*tan(x)

plot \
a1 = 1, f1(x) title "a1 = 1" with lines, \
a1 = 10, f1(x) title "a1 = 10" with lines, \
a1 = 50, f1(x) title "a1 = 50" with lines

pause -1 "Press any key to exit..."