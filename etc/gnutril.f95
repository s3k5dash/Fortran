program GnuplotDemo
  implicit none
  integer :: i
  real(8) :: x
  real(8), dimension(100) :: y
  character(100) :: cmd

  ! Generate data (sine wave)
  do i = 1, 100
    x = 0.1 * i
    y(i) = sin(x)
  end do

  ! Create a data file
  open(unit=10, file='data.txt', status='replace')
  do i = 1, 100
    write(10, *) x, y(i)
  end do
  close(10)

  ! Create a Gnuplot script file
  open(unit=20, file='gnuplot_script.plt', status='replace')
  write(20, *) "set terminal pngcairo"
  write(20, *) "set output 'plot.png'"
  write(20, *) "plot 'data.txt' using 1:2 with lines"
  close(20)

  ! Execute Gnuplot to create the plot
  cmd = "gnuplot gnuplot_script.plt"
  call execute_command_line(cmd)

end program GnuplotDemo