program alphaFinder

    implicit none

    real(8) :: xin, xout, dx
    real(8) :: alpha_m(50), beta_n(50)
    integer :: i


    integer :: k, equation
    integer :: num_elements = 50


    xin = 1.0d0
    equation = 1

    do i = 1, 50, 1

    dx = 1.0d0

        11  xin = xin + dx

            call newtonRaphson(equation, xin, xout)

            if ( i .eq. 1 ) then 

                alpha_m(i) =  xout
                cycle

            endif
            
            if (  alpha_m(i-1) .eq. xout ) then

                dx = dx + 1.0d0
                goto 11

            endif

        alpha_m(i) = xout 

        !! Debugging print statements 
        print*,"The root of the equation is", xout
        print*, "the value of xin is", xin
        
        xin = xout

    end do


! -------------------------------------------------------------------------------------
    !! 'alpha_m.dat' file Output operations

    open(unit=1, file='alpha_m.dat', status='replace')

    do k = 1, num_elements
        write(1,*) alpha_m(k), 0
    end do

    close(unit=1)

    print*, "xout Finder executed successfully"

! -------------------------------------------------------------------------------------

end program alphaFinder

subroutine newtonRaphson(equation, xin, xout)
    
    implicit none

    integer, intent(in) :: equation
    real(8), intent(in) :: xin
    real(8), intent(inout) :: xout


    real(8) :: xtemp, fx, fx_prime, error, bi_t, bib, biT, length
    real(8), parameter :: tol = 0.000010d0

    ! Value of Bi_t Bib and L
    bi_t = 10.0d0
    bib = 1.0d0
    biT = 1.0d0
    length = 1.0d0


10  continue
    select case (equation)
        case (1)

            fx = ((xin * (bib + bi_t) )/ (xin**2.0d0 - (bib*bi_t))) - (length * tan(xin))

            fx_prime = (((bib + bi_t) * (xin**2.0d0 - (bib*bi_t))) - ((bib + bi_t) * (2.0d0 * xin**2.0d0))) &
                        / ((xin**2.0d0 - (bib*bi_t))**2.0d0) - length/cos(xin)**2.0d0
            
        case (2)

            fx = xin * tan(xin) - BiT

            fx_prime = tan(xin) + xin * (1.0d0/ cos(xin)**2.0d0)

        case DEFAULT
                write(*,*)  "invalid function call => check for the selected equation, equation = (1 or 2) only"
        end select


        error=fx/fx_prime
        xtemp=xtemp-error

        !! Debugging print statements 
            print*,"The root of the equation is",xtemp
            print*,"Tolerance is",tol

        if (abs(error) .gt. tol) then
            goto 10
        endif

    xout = xtemp

end subroutine newtonRaphson


