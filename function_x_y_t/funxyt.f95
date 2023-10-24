program FunctionEvaluation

  implicit none
  
  integer, parameter :: max_m = 2  ! Maximum value for m
  integer, parameter :: max_n = 2  ! Maximum value for n
  real(8) :: x, y, t
  real(8) :: u,v,temp1,temp2,temp3
  integer :: m, n
  real(8) :: A, B, Bib  ! Constants
  real(8) :: alpha(max_m), beta(max_n), b_mn (max_m) 

  A = 1.0
  B = 2.0
  Bib = 3.0
  
    alpha(1)= 2
    alpha(2)= 3
    beta(1)= 2
    beta(2)= 3
    b_mn(1)= 2
    b_mn(2)= 3
    x = 2
    y = 3
    t = 1

  ! Initialize the result
  u = 0.0

  ! Double summation
  
  do m = 1, max_m

    u = u + (((1 - x)*A) + (x*B)) + v

    do n = 1, max_n

      v = v + (((beta(n)**2)*exp(-((beta(n)**2)+(alpha(m)**2))*t)+(alpha(m)**2))/((beta(n)**2)+(alpha(m)**2))) * &
              (x*cos(beta(n))*b_mn(m)) * &
              ((y*(cos(alpha(m))))+((Bib/(alpha(m)))*y*(sin(alpha(m)))))
      

    end do
  end do

  print *, "u(x, y, t) = ", u

end program FunctionEvaluation

      ! v = v + (((beta(n)**2)*exp(-((beta(n)**2)+(alpha(m)**2))*t)+(alpha(m)**2))/((beta(n)**2)+(alpha(m)**2)))*(x*cos(beta(n))*b_mn(m))*((y*(cos(alpha(m))))+((Bib/(alpha(m)))*y*(sin(alpha(m)))))
