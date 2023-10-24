program Calculate_C
  implicit none
  real(8) :: length, alpha_m, bib, c

  ! Assign values to the variables
  length = 1.0      ! Replace with your desired value
  alpha_m = 2.0    ! Replace with your desired value
  bib = 3.0        ! Replace with your desired value

  ! Calculate c
  c = length * (alpha_m**2 + alpha_m * (bib**2)) + &
      sin(2.0 * alpha_m * length) * ((alpha_m**2 - bib**2) / 2.0) - &
      2.0 * alpha_m * bib * (COS(alpha_m * length)**2 - 1.0)

  ! Print the result
  print *, "c = ", c

end program Calculate_C
