program Calculate_b_m
  implicit none
  real(8) :: alpha_m, Bib, L, C, b_m

  ! Assign values to the variables
  alpha_m = 1.0    ! Replace with your desired value
  Bib = 2.0        ! Replace with your desired value
  L = 3.0          ! Replace with your desired value
  C = 4.0          ! Replace with your desired value

  ! Calculate b_m
  b_m = 2.0 * (((alpha_m**2)*L*(sin(alpha_m))) - (Bib*alpha_m)*(cos(alpha_m*L) - 1.0) )/ C

  ! Print the result
  print *, "b_m = ", b_m

end program Calculate_b_m
