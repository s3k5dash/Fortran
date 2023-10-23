set grid
# set pm3d
set samples 200

set isosamples 80
set hidden3d

# f(x) = (11 * x) / (x**2 - 11) - 1 * tan(x)
# f(y) = (((11) * (y**2 - (10)) - (11) * (2 * y**2)) / ((y**2 - (10))**2)) - 1 / cos(y)**2


f(x) = cos(x)

f(y) =  sin(y)

splot f(x)*f(y) lt rgb "red" , f(x)*f(x) lt rgb "black"

pause -1 "Press any key to exit..."