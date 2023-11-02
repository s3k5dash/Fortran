program qEvaluation

    implicit none

    integer, parameter :: max = 500
    integer :: i, w, qt_length, k
    real(8) :: q_t(max), t_i(max), alpha(50), beta(50)
    real(8) :: q, length, bi_t, bib, biT, x, y, t


! -------------------------------------------------------------------------------------------
    !! Assign values to the constants

    length = 1        ! Replace with desired value
    bi_t = 1.0        ! Replace with desired value
    bib = 0.0         ! Replace with desired value
    biT = 1.0         ! Replace with desired value
    x = 0.1           ! Replace with desired value
    y = 1.0           ! Replace with desired value
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
    !! Q Finder

    i = 1
    t = 0

    15  if ( t .gt. 2) then
            goto 16
        end if
        
        call Q_2D_t(alpha, beta, length, bi_t, bib, biT, x, y, t, q)

        q_t(i) = q
        t_i(i) = t

        t = t + 0.01
        i = i + 1  

        goto 15
    16  qt_length = i-1
    
! ----------------------------------------------
    !! Debugging print statements 

    ! print*, qt_length

    ! do i = 1, qt_length
    !     print*, "q_t(", i, ") = ", q_t(i)
    ! end do
    ! do i = 1, qt_length
    !     print*, "t_i(", i, ") = ", t_i(i)
    ! end do

! -------------------------------------------------------------------------------------------
    !! 'fQ_t.dat' file Output operations

    open(unit=2, file='fQ_t.dat', status='replace')

    do k = 1, qt_length
        write(2,*) t_i(k), q_t(k)
    end do

    close(unit=2)

    print*, "U Finder executed successfully"

! -------------------------------------------------------------------------------------
    !! gnuplot -> plot from file 

    call execute_command_line("gnuplot plot_Q_t.gnu")

! -------------------------------------------------------------------------------------------

end program qEvaluation

! =========================================================================================================

subroutine Q_2D_t(alpha, beta, length, bi_t, bib, biT, x, y, t, q)

    implicit none
    
    integer, parameter :: max = 50
    integer :: i, m, n 
    real(8) :: alpha(max), beta(max), cm, bm
    real(8) :: bmn, q, temp_q, length, bi_t, bib, biT, x, y, t

    !! Calculation of cm, bm, bmn, q

    do m = 1, max 

        bm = ((4.0 * (sin(alpha(m)*length))) / &
            ((2.0 * alpha(m)*length)+(sin(2 * alpha(m)*length))))

        do n = 1, max

            bmn = ((-2*(bm)*(((beta(n))*(sin(beta(n))))-(biT*(cos(beta(n))))+biT)) / (((beta(n))*biT)* &
                (((cos(beta(n)))*(sin(beta(n))))+(beta(n)))))

            temp_q = temp_q + (((((beta(n)**2)*exp(-1*((beta(n)**2)+(alpha(m)**2))*t))+(alpha(m)**2))/ &
                ((beta(n)**2)+(alpha(m)**2))) * (beta(n)*sin(beta(n))*bmn))
                
        end do

        q = q + (((bm)+(temp_q)) * &
                ((-alpha(m)*sin(alpha(m)*length)) / (alpha(m)**2)))

        temp_q = 0

    end do
    
end subroutine Q_2D_t
