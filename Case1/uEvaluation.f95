program uEvaluation

    implicit none

    integer, parameter :: max = 200
    integer :: i, w, ux_length, k
    real(8) :: u_x(max), x_i(max), q_t(max), t_i(max), alpha(50), beta(50)
    real(8) :: u, q, length, bi_t, bib, biT, x, y, t


! -------------------------------------------------------------------------------------------
    !! Assign values to the constants

    length = 0.5      ! Replace with desired value
    bi_t = 10.0       ! Replace with desired value
    bib = 1.0         ! Replace with desired value
    biT = 1.0         ! Replace with desired value
    x = 1.0           ! Replace with desired value
    y = 0.5           ! Replace with desired value
    t = 2.0           ! Replace with desired value

    ! Manually change the value of "length, bi_t, bib, biT" in 'plot_alpha_m.gnu' & 'plot_beta_n.gnu'

! -------------------------------------------------------------------------------------------
    !! Assign values of the constants to the 'constants.dat' file

    open(unit=1, file='constants.dat', status='replace')

        write(1,*) length, bi_t, bib, biT, x, y, t

    close(unit=1)

! -------------------------------------------------------------------------------------
    !! gnuplot -> plot from file 

    call execute_command_line("gfortran alphaFinder.f95 -o  alphaFinder")
    call execute_command_line(".\alphaFinder ")

    call execute_command_line("gfortran betaFinder.f95 -o betaFinder")
    call execute_command_line(".\betaFinder ")

    ! call execute_command_line("gnuplot plot_alpha_m.gnu")
    ! call execute_command_line("gnuplot plot_beta_n.gnu")

! -------------------------------------------------------------------------------------------
    !! 'alpha_m.(m).dat' file read operations

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
    !! U Finder

    i = 1
    x = 0

    13  if ( x .gt. 1) then
            goto 14
        end if
        
        call fu_xyt(alpha, beta, length, bi_t, bib, biT, x, y, t, u)

        u_x(i) = u
        x_i(i) = x

        x = x + 0.1
        i = i + 1  

        goto 13
    14  ux_length = i-1
    
! ----------------------------------------------
    !! Debugging print statements 

    ! print*, ux_length

    ! do i = 1, ux_length
    !     print*, "u_x(", i, ") = ", u_x(i)
    ! end do
    ! do i = 1, ux_length
    !     print*, "x_i(", i, ") = ", x_i(i)
    ! end do

! -------------------------------------------------------------------------------------------
    !! 'fU_x.dat' file Output operations

    open(unit=1, file='fU_x.dat', status='replace')

    do k = 1, ux_length
        write(1,*) x_i(k),u_x(k)
    end do

    close(unit=1)

    print*, "U Finder executed successfully"

! -------------------------------------------------------------------------------------
    !! gnuplot -> plot from file 

    call execute_command_line("gnuplot plot_U_x.gnu")

! -------------------------------------------------------------------------------------------

end program uEvaluation

! =========================================================================================================

subroutine fu_xyt(alpha, beta, length, bi_t, bib, biT, x, y, t, u)

    implicit none
    
    integer, parameter :: max = 50
    integer :: i, w, m, n 
    real(8) :: alpha(max), beta(max), cm, bm, am, B_m
    real(8) :: bmn, u, temp_u, length, bi_t, bib, biT, x, y, t

! -------------------------------------------------------------------------------------------
    !! Calculation of cm, bm, am, B_m, bmn, u

    do m = 1, max 

        bm = ((4.0 * (sin(alpha(m)*length))) / &
            ((2.0 * alpha(m)*length)+(sin(2 * alpha(m)*length))))

        am = bm + (bm / biT)

        B_m = bm / biT

        do n = 1, max

            bmn = ((-2*(bm)*(((beta(n))*(sin(beta(n))))-(biT*(cos(beta(n))))+biT)) / (((beta(n))*biT)* &
                (((cos(beta(n)))*(sin(beta(n))))+(beta(n)))))

            temp_u = temp_u + (((((beta(n)**2)*exp(-1*((beta(n)**2)+(alpha(m)**2))*t))+(alpha(m)**2))/ &
                ((beta(n)**2)+(alpha(m)**2))) * (cos(beta(n)*x)*bmn))
                
        end do

        u = u + ((((1 - x)*am) + (x*B_m) + (temp_u))* &
                ((cos(alpha(m)*y))))


        temp_u = 0

    end do

end subroutine fu_xyt
