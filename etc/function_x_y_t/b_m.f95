program Calculate_b_m
  implicit none
  real(8) :: alpha_m, bib, length, c

  ! Assign values to the variables
  alpha_m = 1.0    ! Replace with your desired value
  bib = 2.0        ! Replace with your desired value
  length = 3.0          ! Replace with your desired value
  c = 4.0          ! Replace with your desired value

  ! Calculate b_m
  b_m = 2.0 * (((alpha_m**2)*(sin(alpha_m*length))) - (bib*alpha_m)*(cos(alpha_m*length) - 1.0) )/ c

  ! Print the result
  print *, "b_m = ", b_m

end program Calculate_b_m
