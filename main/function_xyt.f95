program FunctionEvaluation

    implicit none

    integer, parameter :: max = 50
    integer :: i, w, m, n 
    real(8) :: alpha(max), beta(max), cm(max), bm(max), am(max), B_m(max)
    real(8) :: length, bib, biT, bmn, u, v, x, y, t, bi_t


! -------------------------------------------------------------------------------------------
    !! Assign values to the constants

    length = 1.0      ! Replace with desired value
    bi_t = 10.0       ! Replace with desired value
    bib = 1.0         ! Replace with desired value
    biT = 1.0         ! Replace with desired value
    x = 1.0           ! Replace with desired value
    y = 0.5           ! Replace with desired value
    t = 1.0           ! Replace with desired value

! -------------------------------------------------------------------------------------------
    !! Assign values of the constants to the 'constants.dat' file

    open(unit=1, file='constants.dat', status='replace')

        write(1,*) length, bi_t, bib, biT, x, y, t

    close(unit=1)

! -------------------------------------------------------------------------------------
    !! gnuplot -> plot from file 

    call execute_command_line("gfortran alpha_m.f95 -o  alpha_m")
    call execute_command_line(".\alpha_m ")

    call execute_command_line("gfortran beta_n.f95 -o beta_n")
    call execute_command_line(".\beta_n ")

    call execute_command_line("gnuplot plot_alpha_m.gnu")
    call execute_command_line("gnuplot plot_beta_n.gnu")


! -------------------------------------------------------------------------------------------
    !! 'alpha_m.dat' file read operations

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
    !! 'beta_n.dat' file read operations

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
    !! Calculation of cm(m), bm(m), am(m), B_m(m), bmn, u

    do m = 1, max 

        cm(m) = length * (alpha(m)**2 + alpha(m) * (bib**2)) + &
            sin(2.0 * alpha(m) * length) * ((alpha(m)**2 - bib**2) / 2.0) - &
            2.0 * alpha(m) * bib * (cos(alpha(m) * length)**2 - 1.0)

        bm(m) = 2.0 * (((alpha(m)**2)*(sin(alpha(m)*length))) - &
            (bib*alpha(m))*(cos(alpha(m)*length) - 1.0) )/ cm(m)

        am(m) = bm(m) + bm(m) / biT

        B_m(m) = bm(m) / biT

        do n = 1, max

            bmn = (-2*(bm(m))*(((beta(n))*(sin(beta(n))))-(biT*(cos(beta(n))))+biT)) / ((beta(n))*biT)* &
                (((cos(beta(n)))*(sin(beta(n))))+(beta(n)))

            v = v + (((beta(n)**2)*exp(-((beta(n)**2)+(alpha(m)**2))*t)+(alpha(m)**2))/((beta(n)**2)+(alpha(m)**2))) * &
                (cos(beta(n)*x)*bmn) * &
                (((cos(alpha(m)*y)))+((bib/(alpha(m)))*(sin(alpha(m)*y))))
            
        end do

    u = u + (((1 - x)*am(m)) + (x*B_m(m))) + v

    v = 0

  end do

  
! ----------------------------------------------------------
    !! Debugging print statements 
    
    ! do i = 1, max
    !     write(*,*) "cm(", i, ") = ", cm(i)
    !     write(*,*) "bm(", i, ") = ", bm(i)
    !     write(*,*) "am(", i, ") = ", am(i)
    !     write(*,*) "B_m(", i, ") = ", B_m(i)

    ! end do
    
    write(*,*) "u = ", u
    
! -------------------------------------------------------------------------------------------

end program FunctionEvaluation