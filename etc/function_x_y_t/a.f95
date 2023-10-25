program Calculate_A
  implicit none
  real(8) :: b_m, BiT, A

  ! Assign values to the variables
  b_m = 1.0     ! Replace with your desired value
  BiT = 2.0     ! Replace with your desired value

  ! Calculate A
  A = b_m + b_m / BiT

  ! Print the result
  print *, "A = ", A

end program Calculate_A