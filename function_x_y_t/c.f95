program Calculate_C
  implicit none
  real(8) :: length, alpha(m), bib, c

  ! Assign values to the variables
  length = 1.0      ! Replace with your desired value
  alpha(m) = 2.0    ! Replace with your desired value
  bib = 3.0        ! Replace with your desired value

  ! Calculate c
  c = length * (alpha(m)**2 + alpha(m) * (bib**2)) + &
      sin(2.0 * alpha(m) * length) * ((alpha(m)**2 - bib**2) / 2.0) - &
      2.0 * alpha(m) * bib * (COS(alpha(m) * length)**2 - 1.0)

  ! Print the result
  print *, "c = ", c

end program Calculate_C
