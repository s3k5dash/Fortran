program betaFinder

    implicit none

    real(8) :: x, beta, dx
    real(8) :: beta_n(50)
    integer :: iteration, i


    integer :: k
    integer :: num_elements = 50


    x = 1

    do i = 1, 50, 1

    dx = 1

        11  x = x + dx

            call newtonRaphson(x, beta, iteration)

            if ( i .eq. 1 ) then 

                beta_n(i) =  beta
                cycle

            endif
            
            if (  beta_n(i-1) .eq. beta ) then

                dx = dx + 1
                goto 11

            endif

        beta_n(i) = beta 

        !! Debugging print statements 
        ! print*,"The root of the equation is", beta
        ! print*, "the value of x is", x
        ! Print*,"Iteration is",iteration
        
        x = beta

    end do

! -------------------------------------------------------------------------------------
    !! Manual Input 

    ! do i = 1, 100, 1

    !     print*,"Please insert the initial guess of the root of equation."
    !     read*,x
    
    !     call newtonRaphson(x, beta, iteration)

    !     print*,"The root of the equation is",x
    !     Print*,"Iteration is",iteration

    ! end do
    
! -------------------------------------------------------------------------------------
    !! 'beta_n.dat' file Output operations

    open(unit=1, file='beta_n.dat', status='replace')

    do k = 1, num_elements
        write(1,*) beta_n(k), 0
    end do

    close(unit=1)

    print*, "Beta Finder executed successfully"

! -------------------------------------------------------------------------------------
    ! gnuplot -> plot from file 
    ! ( Remember to change the Value of { BiT } in "plot_beta_n.gnu" file )


    ! call execute_command_line("gnuplot plot_beta_n.gnu")

! -------------------------------------------------------------------------------------

end program betaFinder

! =========================================================================================================

subroutine newtonRaphson(x, beta, iteration)

    implicit none

    real(8) :: x, fx, fx_prime, error, beta, biT, tmp
    real(8) :: tol = 0.00001
    integer :: iteration

    iteration = 0
    
    
! -------------------------------------------------------------------------------------------
    !! 'constants.dat' file Input operations

    open(unit=1, file="constants.dat", status='old', action='read')
    
        read(1, *) tmp, tmp, tmp, biT

    close(1)
! -------------------------------------------------------------------------------------------

    

    10  fx = x * tan(x) - BiT
        fx_prime = tan(x) + x * (1 / cos(x)**2)

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

    beta = x
    
    
    ! print*,"Tolerance is",tol
    ! Print*,"Iteration is",iteration

end
