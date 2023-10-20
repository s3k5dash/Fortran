program name

    implicit none
    
    real(8) :: alpha, Bib, Bit, L, result,df
    integer :: i

    real(8) :: alphaList(50) !one dimensional integer array

  ! Initialize the variables with appropriate values
  alpha = 1.0   ! Replace with your desired values
  Bib = 1.0     ! Replace with your desired values
  Bit = 10.0     ! Replace with your desired values
  L = 1.0       ! Replace with your desired values

  ! Calculate the expression
  df = (alpha * (Bib + Bit)) / (alpha**2 - (Bib * Bit)) - L * tan(alpha)

  ! Print the result
  print *, "f(x) = ", df

do i = 1, 5
    
    alphaList(i) = i * 2.0

    df = (alpha * (Bib + Bit)) / (alpha**2 - (Bib * Bit)) - L * tan(alpha)

    ! Print the result for each value of x
    print *, "f(", i, ") = ", df
  end do

end program name
