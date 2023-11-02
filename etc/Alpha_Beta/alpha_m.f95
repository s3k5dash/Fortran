program alphaFinder

    implicit none

    real :: xin, xout, beta, dx, equation 
    integer :: iteration, i, k
    integer, parameter :: max = 50
    real :: alpha_m(max), beta_n(max)

    xin = 1
    equation = 1

    do i = 1, max, 1

    dx = 1

        11  xin = xin + dx

            call newtonRaphson(xin, xout, equation)

            if ( i .eq. 1 ) then 

                alpha_m(i) =  xout
                cycle

            endif
            
            if (  xout(i-1) .eq. xout ) then

                dx = dx + 1
                goto 11

            endif

        alpha_m(i) = xout 

        !! Debugging print statements 
        ! print*,"The root of the equation is", xout
        ! print*, "the value of xin is", xin
        ! Print*,"Iteration is",iteration
        
        xin = xout

    end do

! -------------------------------------------------------------------------------------
    !! Manual Input 

    ! do i = 1, 100, 1

    !     print*,"Please insert the initial guess of the root of equation."
    !     read*,xin
    
    !     call newtonRaphson(xin, xout, iteration)

    !     print*,"The root of the equation is",xin
    !     Print*,"Iteration is",iteration

    ! end do
    
! -------------------------------------------------------------------------------------
    !! 'data.dat' file Output operations

    open(unit=1, file='xout.dat', status='old', action='write', position='append')
    ! open(unit=1, file='xout.dat', status='replace')

    do k = 1, max
        write(1,*) xout(k), 0
    end do

    close(unit=1)

    print*, "end"

! -------------------------------------------------------------------------------------
    !! gnuplot -> plot from file 
    ! ( Remember to change the Value of { Bit, Bib and L } in "plot_alpha_m.gnu" file )

    ! call execute_command_line("gnuplot plot_alpha_m.gnu")

! -------------------------------------------------------------------------------------

end program alphaFinder


subroutine newtonRaphson(xin, xout, equation)

    implicit none

    real :: xin, xout, fx, fx_prime, error, bit, bib, biT, length
    real :: equation
    real :: tol = 0.00001
    
    ! Value of Bit Bib and L
    bit = 10.0
    bib = 1.0 
    bib = 1.0 
    biT = 1.0
    length = 1.0



    10  select case (equation)
            case (1)

            fx = ((xin * (bib + bit) )/ (xin**2 - (bib*bit))) - (length * tan(xin))

            fx_prime = (((bib + bit) * (xin**2 - (bib*bit))) - ((bib + bit) * (2.0 * xin**2))) &
                        / ((xin**2 - (bib*bit))**2) - length/cos(xin)**2
            
            case (2)

            fx = xin * tan(xin) - BiT

            fx_prime = tan(xin) + xin * (1 / cos(xin)**2)

            case DEFAULT
                write(*,*)  "invalid function call => check for the selected equation, equation = (1 or 2) only"
        end select
    
        error=fx/fx_prime
        xin=xin-error

        if (abs(error) .gt. tol) then

            !! Debugging print statements 
            ! print*,"The root of the equation is",xin
            ! print*,"Tolerance is",tol

        goto 10

    endif

    xout = xin

end
