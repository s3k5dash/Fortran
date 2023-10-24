program Calculate_b_mn
  implicit none
  real(8) :: b_m, beta_n, BiT, b_mn

  ! Assign values to the variables
  b_m = 1.0     ! Replace with your desired value
  beta_n = 2.0  ! Replace with your desired value
  BiT = 3.0     ! Replace with your desired value

  ! Calculate b_mn
  b_mn = (-2.0 * b_m * ((beta_n * sin(beta_n)) - (BiT * cos(beta_n)) + BiT)) / (beta_n * BiT * (cos(beta_n) * sin(beta_n) + beta_n))

  ! Print the result
  print *, "b_mn = ", b_mn

end program Calculate_b_mn
