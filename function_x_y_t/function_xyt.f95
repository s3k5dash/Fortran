program FunctionEvaluation

    implicit none
    
    integer, parameter :: max = 50
    integer :: i, w, m, n 
    real(8) :: alpha(max), beta(max), c(max), b(max), a(max), capitalB(max)
    real(8), dimension(max, max) :: bmn
    real(8) :: length, bib, biT


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
    !! Assign values to the variables

    length = 1.0      ! Replace with desired value
    bib = 3.0         ! Replace with desired value
    biT = 3.0         ! Replace with desired value

! -------------------------------------------------------------------------------------------
    !! Calculate c(m) & b(m)

    do m = 1, max
        c(m) = length * (alpha(m)**2 + alpha(m) * (bib**2)) + &
            sin(2.0 * alpha(m) * length) * ((alpha(m)**2 - bib**2) / 2.0) - &
            2.0 * alpha(m) * bib * (cos(alpha(m) * length)**2 - 1.0)

        b(m) = 2.0 * (((alpha(m)**2)*(sin(alpha(m)*length))) - &
            (bib*alpha(m))*(cos(alpha(m)*length) - 1.0) )/ c(m)

        a(m) = b(m) + b(m) / biT

        capitalB(m) = b(m) / biT


    end do

! ----------------------------------------------
    !! Debugging print statements 
    
    do i = 1, max
        write(*,*) "c(", i, ") = ", c(i)
        write(*,*) "b(", i, ") = ", b(i)
        write(*,*) "a(", i, ") = ", a(i)
        write(*,*) "capitalB(", i, ") = ", capitalB(i)

    end do

end program FunctionEvaluation