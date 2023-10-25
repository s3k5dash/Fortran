program Calculate_b_mn
  implicit none
    integer, parameter :: max = 50
    integer :: i, w, m, n 
    real(8) :: alpha(max), beta(max), cm(max), bm(max), am(max), capitalBm(max)
    real(8) :: length, bib, biT, bmn, u, v, x, y, t
  ! Assign values to the variables
  m=1
  n=1
  bm(m) = 1.0     ! Replace with your desired value
  beta(n) = 2.0  ! Replace with your desired value
  biT = 3.0     ! Replace with your desired value

  ! Calculate bmn
  bmn = (-2*(bm(m))*(((beta(n))*(sin(beta(n))))-(biT*(cos(beta(n))))+biT)) / ((beta(n))*biT)* &
  (((cos(beta(n)))*(sin(beta(n))))+(beta(n)))

  ! Print the result
  print *, "bmn = ", bmn

end program Calculate_b_mn
