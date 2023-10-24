program newton

    implicit none

    real :: x, alpha, dx
    real :: alpha_m(50)
    integer :: iteration,i


    integer :: k
    integer :: num_elements = 50


    x = 0

    do i = 1, 50, 1

    dx = 0.5

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

        alpha_m(i) = alpha 

        !! Debugging print statements 
        ! print*,"The root of the equation is", alpha
        ! print*, "the value of x is", x
        ! Print*,"Iteration is",iteration
        
        x = alpha

    end do

! -------------------------------------------------------------------------------------
    !! Manual Input 

    ! do i = 1, 100, 1

    !     print*,"Please insert the initial guess of the root of equation."
    !     read*,x
    
    !     call ntrap(x, alpha, iteration)

    !     print*,"The root of the equation is",x
    !     Print*,"Iteration is",iteration

    ! end do
    
! -------------------------------------------------------------------------------------
    !! 'data.dat' file Output operations

    open(unit=1, file='data.dat', status='replace')

    do k = 1, num_elements
        write(1,*) alpha_m(k), 0
    end do

    close(unit=1)

    print*, "end"

! -------------------------------------------------------------------------------------
    !! Plotting Data with gnuplot
    call execute_command_line("gnuplot 2D_plot_withWindow.gnu")

! -------------------------------------------------------------------------------------

end program newton


subroutine ntrap(x, alpha, iteration)

    implicit none

    real :: x, fx, fx_prime, error, alpha, bit, bib, length
    real :: tol = 0.00001
    integer :: iteration

    iteration = 0
    
    bit = 10.0
    bib = 1.0 
    length = 1.0

    ! fx = ((x * (bib + bit) )/ (x**2 - (bib * bit))) - (length * tan(x))
    ! fx_prime =  ((bib + bit) * (x**2 - (bib * bit)) - (bib + bit) * (2 * x**2)) / ((x**2 - (bib * bit))**2) - ( length/cos(x)**2)


    10  fx = ((x * (bib + bit) )/ (x**2 - (bib*bit))) - (length * tan(x))
        fx_prime = (((bib + bit) * (x**2 - (bib*bit))) - ((bib + bit) * (2.0 * x**2))) / ((x**2 - (bib*bit))**2) - length/cos(x)**2


        error=fx/fx_prime
        x=x-error
        iteration=iteration+1

        if (abs(error) .gt. tol) then

            !! Debugging print statements 
            ! print*,"The root of the equation is",x
            ! print*,"Tolerance is",tol
            ! Print*,"Iteration is",iteration

        goto 10

    endif

    alpha = x
    
    
    ! print*,"Tolerance is",tol
    ! Print*,"Iteration is",iteration

end
