program variables
  implicit none

  integer :: amount
  real :: pi
  complex :: frequency
  character :: initial
  logical :: isOkay

  amount = 10
  pi = 3.1415927
  frequency = (1.0, -0.5)
  initial = 'A'
  isOkay = .false.

  print*, amount
end program variables