program alphaFinder

    implicit none

    real(8) :: x, alpha, dx
    real(8) :: alpha_m(50)
    integer :: i


    integer :: k
    integer :: num_elements = 50


    x = 1

    do i = 1, 50, 1

    dx = 1

        11  x = x + dx

            call newtonRaphson(x, alpha)

            if ( i .eq. 1 ) then 

                alpha_m(i) =  alpha
                cycle

            endif
            
            if (  alpha_m(i-1) .eq. alpha ) then

                dx = dx + 1
                goto 11

            endif

        alpha_m(i) = alpha 

        !! Debugging print statements 
        ! print*,"The root of the equation is", alpha
        ! print*, "the value of x is", x
        
        x = alpha

    end do

! -------------------------------------------------------------------------------------
    !! Manual Input 

    ! do i = 1, 100, 1

    !     print*,"Please insert the initial guess of the root of equation."
    !     read*,x
    
    !     call newtonRaphson(x, alpha)

    !     print*,"The root of the equation is",x

    ! end do
    
! -------------------------------------------------------------------------------------
    !! 'alpha_m.dat' file Output operations

    open(unit=1, file='alpha_m.dat', status='replace')

    do k = 1, num_elements
        write(1,*) alpha_m(k), 0
    end do

    close(unit=1)

    print*, "Alpha Finder executed successfully"

! -------------------------------------------------------------------------------------
    !! gnuplot -> plot from file 
    ! ( Remember to change the Value of { Bi_t, Bib and L } in "plot_alpha_m.gnu" file )

    ! call execute_command_line("gnuplot plot_alpha_m.gnu")

! -------------------------------------------------------------------------------------

end program alphaFinder

! =========================================================================================================

subroutine newtonRaphson(xin, xout)

    implicit none

    real(8) :: xin, fx, fx_prime, error, xout, length, bi_t, bib
    real(8) :: tol = 0.000000001d0
    
! -------------------------------------------------------------------------------------------
    !! 'constants.dat' file Input operations

    open(unit=1, file="constants.dat", status='old', action='read')
    
        read(1, *) length, bi_t, bib

    close(1)
! -------------------------------------------------------------------------------------------


    10  fx = ((xin * tan(xin)) - bi_t)
    
        fx_prime = (tan(xin) + (xin * (1 / cos(xin)**2)))


        error=fx/fx_prime
        xin=xin-error

        if (abs(error) .gt. tol) then

            !! Debugging print statements 
            ! print*,"The root of the equation is",xin
            ! print*,"Tolerance is",tol

        goto 10

    endif

    xout = xin
    
    
    ! print*,"Tolerance is",tol

end
