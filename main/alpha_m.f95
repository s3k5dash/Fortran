program alphaFinder

    implicit none

    real(8) :: x, alpha, dx
    real(8) :: alpha_m(50)
    integer :: iteration,i


    integer :: k
    integer :: num_elements = 50


    x = 1

    do i = 1, 50, 1

    dx = 1

        11  x = x + dx

            call newtonRaphson(x, alpha, iteration)

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
    
    !     call newtonRaphson(x, alpha, iteration)

    !     print*,"The root of the equation is",x
    !     Print*,"Iteration is",iteration

    ! end do
    
! -------------------------------------------------------------------------------------
    !! 'data.dat' file Output operations

    open(unit=1, file='alpha_m.dat', status='replace')

    do k = 1, num_elements
        write(1,*) alpha_m(k), 0
    end do

    close(unit=1)

    print*, "end"

! -------------------------------------------------------------------------------------
    !! gnuplot -> plot from file 
    ! ( Remember to change the Value of { Bi_t, Bib and L } in "plot_alpha_m.gnu" file )

    ! call execute_command_line("gnuplot plot_alpha_m.gnu")

! -------------------------------------------------------------------------------------

end program alphaFinder


subroutine newtonRaphson(x, alpha, iteration)

    implicit none

    real(8) :: x, fx, fx_prime, error, alpha, length, bi_t, bib
    real(8) :: tol = 0.00001
    integer :: iteration

    iteration = 0
    
! -------------------------------------------------------------------------------------------
    !! 'constants.dat' file Input operations

    open(unit=1, file="constants.dat", status='old', action='read')
    
        read(1, *) length, bi_t, bib

    close(1)
! -------------------------------------------------------------------------------------------


    10  fx = ((x * (bib + bi_t) )/ (x**2 - (bib*bi_t))) - (1 * tan(x*length))
    
        fx_prime = (((bib + bi_t) * (x**2 - (bib*bi_t))) - ((bib + bi_t) * (2.0 * x**2))) &
                    / ((x**2 - (bib*bi_t))**2) - 1/cos(x*length)**2


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
