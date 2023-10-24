subroutine newtonRaphson(xin, xout, equation)

    implicit none

    real(8) :: xin, xout, fx, fx_prime, error, bit, bib, biT, length
    integer :: equation
    real(8) :: tol = 0.00001
    
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

            fx = x * tan(x) - BiT

            fx_prime = tan(x) + x * (1 / cos(x)**2)

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
