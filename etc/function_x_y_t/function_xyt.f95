program FunctionEvaluation

    implicit none
    
    integer, parameter :: max = 50
    integer :: i, w, m, n 
    real(8) :: alpha(max), beta(max), cm(max), bm(max), am(max), capitalBm(max)
    real(8) :: length, bib, biT, bmn, u, v, x, y, t


! -------------------------------------------------------------------------------------------
    !! Assign values to the variables

    length = 1.0      ! Replace with desired value
    bib = 3.0         ! Replace with desired value
    biT = 3.0         ! Replace with desired value
    x = 1.0           ! Replace with desired value
    y = 1.0           ! Replace with desired value
    t = 1.0           ! Replace with desired value

! -------------------------------------------------------------------------------------------
    !! 'alpha_m.dat' file Input operations

    open(unit=10, file="alpha_m.dat", status='old', action='read')
    i = 0
    do
        read(10, *, iostat=w) alpha(i + 1)
        if (w /= 0) then
            exit
        end if
        i = i + 1
    end do

    close(10)

! -------------------------------------------------------------------------------------------
    !! 'beta_n.dat' file Input operations

    open(unit=11, file="beta_n.dat", status='old', action='read')
    i = 0
    do
        read(11, *, iostat=w) beta(i + 1)
        if (w /= 0) then
            exit
        end if
        i = i + 1
    end do

    close(11)

! ----------------------------------------------
    !! Debugging print statements 

    ! do i = 1, max
    !     write(*,*) "alpha(", i, ") = ", alpha(i)
    !     write(*,*) "beta(", i, ") = ", beta(i)
    ! end do


! -------------------------------------------------------------------------------------------
    !! Calculation of cm(m), bm(m), am(m), capitalBm(m), bmn, u

    do m = 1, max 

        cm(m) = length * (alpha(m)**2 + alpha(m) * (bib**2)) + &
            sin(2.0 * alpha(m) * length) * ((alpha(m)**2 - bib**2) / 2.0) - &
            2.0 * alpha(m) * bib * (cos(alpha(m) * length)**2 - 1.0)

        bm(m) = 2.0 * (((alpha(m)**2)*(sin(alpha(m)*length))) - &
            (bib*alpha(m))*(cos(alpha(m)*length) - 1.0) )/ cm(m)

        am(m) = bm(m) + bm(m) / biT

        capitalBm(m) = bm(m) / biT

        do n = 1, max

            bmn = (-2*(bm(m))*(((beta(n))*(sin(beta(n))))-(biT*(cos(beta(n))))+biT)) / ((beta(n))*biT)* &
                (((cos(beta(n)))*(sin(beta(n))))+(beta(n)))

            v = v + (((beta(n)**2)*exp(-((beta(n)**2)+(alpha(m)**2))*t)+(alpha(m)**2))/((beta(n)**2)+(alpha(m)**2))) * &
                (x*cos(beta(n))*bmn) * &
                ((y*(cos(alpha(m))))+((bib/(alpha(m)))*y*(sin(alpha(m)))))
            
            write(*,*) "bmn(", m, ") = ", bmn

        end do

    u = u + (((1 - x)*am(m)) + (x*capitalBm(m))) + v

  end do

  
! ----------------------------------------------------------
    !! Debugging print statements 
    
    do i = 1, max
        write(*,*) "cm(", i, ") = ", cm(i)
        write(*,*) "bm(", i, ") = ", bm(i)
        write(*,*) "am(", i, ") = ", am(i)
        write(*,*) "capitalBm(", i, ") = ", capitalBm(i)

    end do
    
    write(*,*) "u = ", u
    
! -------------------------------------------------------------------------------------------

end program FunctionEvaluation