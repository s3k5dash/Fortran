program newton

    implicit none

    real :: x, alpha, dx
    real :: alpha_m(50)
    integer :: iteration,i

    x = -2
    dx = 1

    do i = 1, 50, 1

        11  x = x + dx

            call ntrap(x, alpha, iteration)

            if ( i .eq. 1 ) then 

                alpha_m(i) =  alpha
                cycle

            endif
            
            if (  alpha_m(i-1) .eq. alpha ) then

                dx = dx + 1
                goto 11

            endif

            if (  alpha_m(i-1) .ne. alpha ) dx = 1

        alpha_m(i) = alpha 

        
        ! print*,"The root of the equation is", alpha
        ! print*, "the value of x is", x
        ! Print*,"Iteration is",iteration
        
        x = alpha

    end do




    ! do i = 1, 100, 1

    !     print*,"Please insert the initial guess of the root of equation."
    !     read*,x
    
    !     call ntrap(x, alpha, iteration)

    !     print*,"The root of the equation is",x
    !     Print*,"Iteration is",iteration

    ! end do
    
    print*, "end"


end


subroutine ntrap(x, alpha, iteration)

    implicit none

    real :: x, fx, fx_prime, error, alpha
    real :: tol = 0.00001
    integer :: iteration

    iteration = 0

    10  fx = (11 * x) / (x**2 - 11) - 1 * tan(x)
        fx_prime = (((11) * (x**2 - (10)) - (11) * (2 * x**2)) / ((x**2 - (10))**2)) - 1 / cos(x)**2
        
        error=fx/fx_prime
        x=x-error
        iteration=iteration+1

        if (abs(error) .gt. tol) then

            ! print*,"The root of the equation is",x
            ! print*,"Tolerance is",tol
            ! Print*,"Iteration is",iteration

        goto 10

    endif

    alpha = x
    ! print*,"Tolerance is",tol
    ! Print*,"Iteration is",iteration

end
