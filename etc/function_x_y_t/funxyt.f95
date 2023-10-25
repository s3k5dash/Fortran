program FunctionEvaluation

  implicit none
  
    integer, parameter :: max = 50
    integer :: i, w, m, n 
    real(8) :: alpha(max), beta(max), cm(max), bm(max), am(max), capitalBm(max)
    real(8) :: length, bib, biT, bmn, u, v, x, y, t

  am = 1.0
  bm = 2.0
  bib = 3.0
  
    alpha(1)= 2
    alpha(2)= 3
    beta(1)= 2
    beta(2)= 3
    capitalBm(1)= 3
    capitalBm(2)= 3
    bmn= 2
    x = 2
    y = 3
    t = 1

  ! Initialize the result
  u = 0.0
  
  do m = 1, 2
    do n = 1, 2



      v = v + (((beta(n)**2)*exp(-((beta(n)**2)+(alpha(m)**2))*t)+(alpha(m)**2))/((beta(n)**2)+(alpha(m)**2))) * &
              (x*cos(beta(n))*bmn) * &
              ((y*(cos(alpha(m))))+((bib/(alpha(m)))*y*(sin(alpha(m)))))
      

    end do
  u = u + (((1 - x)*am(m)) + (x*capitalBm(m))) + v
  end do

  print *, "u(x, y, t) = ", u

end program FunctionEvaluation

      ! v = v + (((beta(n)**2)*exp(-((beta(n)**2)+(alpha(m)**2))*t)+(alpha(m)**2))/((beta(n)**2)+(alpha(m)**2)))*(x*cos(beta(n))*bmn(m))*((y*(cos(alpha(m))))+((bib/(alpha(m)))*y*(sin(alpha(m)))))
