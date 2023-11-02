program alphaBetaFinder

    implicit none

    integer, parameter :: elements = 50
    integer :: equation, i
    real(8), dimension(elements) :: alpha, beta
    real(8) :: bi_t, bib, biT, length

    bi_t = 10
    biT = 1
    bib = 0
    length = 1
    
    equation = 1 ! 1 for alpha, 2 for beta
    call alphaBeta (equation, elements, bi_t, bib, biT, length, alpha)
    equation = 2 ! 1 for alpha, 2 for beta
    call alphaBeta (equation, elements, bi_t, bib, biT, length, beta)

    ! do i = 1, elements
    !     print*,"alpha",i,"=", alpha(i)
    !     print*,"alpha",i,"=", beta(i)
    ! end do
    
    ! -------------------------------------------------------------------------------------
    !! 'alpha_m.dat' file Output operations

    open(unit=1, file='alpha_m.dat', status='replace')

    do i = 1, elements
        write(1,*) alpha(i), 0
    end do

    close(unit=1)
    ! -------------------------------------------------------------------------------------
    !! 'beta_n.dat' file Output operations

    open(unit=2, file='beta_n.dat', status='replace')

    do i = 1, elements
        write(2,*) beta(i), 0
    end do

    close(unit=2)

! -------------------------------------------------------------------------------------
    !! gnuplot -> plot from file 
    ! ( Remember to change the Value of { Bi_t, Bib and L } in "plot_alpha_m.gnu" file )

    ! call execute_command_line("gnuplot plot_alpha_m.gnu")
    ! call execute_command_line("gnuplot plot_beta_n.gnu")

! -------------------------------------------------------------------------------------
    print*, "Alpha Finder executed successfully"

end program alphaBetaFinder

! =========================================================================================================

subroutine alphaBeta (equation, elements, bi_t, bib, biT, length, result)
    
    implicit none
    
    integer, intent(in) :: equation, elements
    real(8), intent(in) :: bi_t, bib, biT, length
    real(8), dimension(elements), intent(out) :: result
    
    integer :: i
    real(8) :: xin, xresult, dx
    
    ! -------------------------------------------------------------------------------------------

    xin = 1.0d0

    do i = 1, elements
    
    dx = 0.5
    xin = xin + dx
        
        11 continue
            
            call newtonRaphson(equation, bi_t, bib, biT, length, xin, xresult)

            if ( i .eq. 1 ) then 

                result(i) =  xresult
                cycle

            endif
            
            if (  result(i-1) .eq. xresult ) then

                dx = 1.1*dx
                xin = xin + dx

                goto 11

            endif

        result(i) = xresult 

        !! Debugging print statements 
        ! print*,"The root of the equation is", xresult
        ! print*, "the value of xin is", xin
        
        xin = xresult

    end do
    
end subroutine alphaBeta

! =========================================================================================================

subroutine newtonRaphson (equation, bi_t, bib, biT, length, xin, xresult)

    implicit none

    integer, intent(in) :: equation
    real(8), intent(in) :: xin
    real(8), intent(in) :: bi_t, bib, biT, length
    real(8), intent(out) :: xresult

    real(8) :: xtemp, fx, fx_prime, error
    real(8) :: tol = 0.00000001d0
    
    ! -------------------------------------------------------------------------------------------
    xtemp = xin

    10 continue
    
    select case (equation)
        case (1)
            fx = ((xtemp * tan(xtemp)) - bi_t)
            fx_prime = (tan(xtemp) + (xtemp * (1 / cos(xtemp)**2)))
        case (2)
            fx = ((xtemp * tan(xtemp)) - biT)
            fx_prime = (tan(xtemp) + (xtemp * (1 / cos(xtemp)**2)))
        case default
            write(*,*)  "invalid subroutine call => check for the selected equation, equation = (1 or 2) only"
            stop
    end select

    error = fx / fx_prime
    xtemp = xtemp - error

    if (abs(error) .gt. tol) goto 10

    xresult = xtemp
    
    
    ! print*,"xresult is",xtemp

end subroutine newtonRaphson
