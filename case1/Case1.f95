program Case1

    implicit none

    integer, parameter :: elements = 100
    integer :: equation, i
    real(8), dimension(elements) :: alpha, beta
    real(8) :: bi_t, bib, biT, length, x, y, t, q, u
    
    integer, parameter :: maxresolution = 200
    integer :: resolution
    real(8), dimension(maxresolution)  :: ux, xi, qt, ti
    
    real(8) :: dx, dt, ranget


!! Assign values to the constants

    bi_t = 10.0       ! Replace with desired value
    bib = 1.0         ! Replace with desired value
    biT = 1.0         ! Replace with desired value
    length = 1        ! Replace with desired value
    x = 1.0           ! Replace with desired value
    y = 1.0           ! Replace with desired value
    t = 2.0           ! Replace with desired value

    ! Manually change the value of "length, bi_t, bib, biT" in 'plot_alpha.gnu' & 'plot_beta.gnu'

! -------------------------------------------------------------------------------------

!! Alpha Beta Finder

    equation = 1 ! 1 for alpha, 2 for beta
    call alphaBeta (equation, elements, bi_t, bib, biT, length, alpha)
    equation = 2 ! 1 for alpha, 2 for beta
    call alphaBeta (equation, elements, bi_t, bib, biT, length, beta)

    
    !! Debugging print statements 
        ! do i = 1, elements
        !     print*,"alpha",i,"=", alpha(i)
        !     print*,"alpha",i,"=", beta(i)
        ! end do
! -------------------------------------------------------------------------------------

!! 'alpha.dat' file Output operations

    open(unit=1, file='data/alpha.dat', status='replace')

    do i = 1, elements
        write(1,*) alpha(i), 0
    end do

    close(unit=1)

! -------------------------------------------------------------------------------------

!! 'beta.dat' file Output operations

    open(unit=2, file='data/beta.dat', status='replace')

    do i = 1, elements
        write(2,*) beta(i), 0
    end do

    close(unit=2)

! -------------------------------------------------------------------------------------
    print*, "Alpha Beta Finder executed successfully"

!! U Finder

    i = 1
    x = 0
    dx = 1.d0/(maxresolution-1)
        
    13  continue

        call fuxyt(elements, alpha, beta, bi_t, bib, biT, length, x, y, t, u)

        ux(i) = u
        xi(i) = x
        
        i = i + 1
        x = x + dx

        if ( x .ge. 1) then
            goto 14
        end if

        goto 13
    14  continue
    resolution = i-1
    
    !! Debugging print statements 

        ! print*, resolution

        ! do i = 1, resolution
        !     print*, "ux(", i, ") = ", ux(i)
        ! end do
        ! do i = 1, resolution
        !     print*, "xi(", i, ") = ", xi(i)
        ! end do

! -------------------------------------------------------------------------------------

!! 'fUx.dat' file Output operations

    open(unit=4, file='data/fUx.dat', status='replace')

    do i = 1, resolution
        write(4,*) xi(i),ux(i)
    end do

    close(unit=4)

! -------------------------------------------------------------------------------------
    print*, "U Finder executed successfully"

!! Q Finder

    ranget = 1.d0      ! Replace with desired value

    dt = ranget/(maxresolution-1)
    i = 1
    t = 0

    15 continue

        call Q2Dt(elements, alpha, beta, bi_t, bib, biT, length, x, y, t, q)

        ti(i) = t
        qt(i) = q

        t = t + dt
        i = i + 1  

        if ( t .ge. ranget) then
            goto 16
        end if

        goto 15
    16 continue
    resolution = i-1
    
    !! Debugging print statements 

        ! print*, resolution

        ! do i = 1, resolution
        !     print*, "qt(", i, ") = ", qt(i)
        ! end do
        ! do i = 1, resolution
        !     print*, "ti(", i, ") = ", ti(i)
        ! end do

! -------------------------------------------------------------------------------------

!! 'fQt.dat' file Output operations

    open(unit=3, file='data/fQt.dat', status='replace')
    do i = 1, resolution
        write(3,*) ti(i), qt(i)
    end do

    close(unit=3)

! -------------------------------------------------------------------------------------
    print*, "Q Finder executed successfully"

!! gnuplot -> plot from file 

    ! ( Remember to change the Value of { Bi_t, Bib and L } in "plot_alpha.gnu" file )

        ! call execute_command_line("gnuplot gnu/plot_alpha.gnu")
        ! call execute_command_line("gnuplot gnu/plot_beta.gnu")
        ! call execute_command_line("gnuplot gnu/plot_Ux.gnu")
        ! call execute_command_line("gnuplot gnu/plot_Qt.gnu")

! -------------------------------------------------------------------------------------


end program Case1

! =====================================================================================

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

! =====================================================================================

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

! =====================================================================================

subroutine Q2Dt(elements, alpha, beta, bi_t, bib, biT, length, x, y, t, q)

    implicit none
    
    integer, intent(in) :: elements
    real(8), dimension(elements), intent(in) :: alpha, beta
    real(8), intent(in) :: bi_t, bib, biT, length, x, y, t
    real(8), intent(out) :: q


    integer :: m, n
    real(8) :: bm, bmn, temp_q

    !! Calculation of bm, bmn, q

    do m = 1, elements 

        bm = ((4.0 * (sin(alpha(m)*length))) / &
            ((2.0 * alpha(m)*length)+(sin(2 * alpha(m)*length))))

        do n = 1, elements

            bmn = ((-2*(bm)*(((beta(n))*(sin(beta(n))))-(biT*(cos(beta(n))))+biT)) / (((beta(n))*biT)* &
                (((cos(beta(n)))*(sin(beta(n))))+(beta(n)))))

            temp_q = temp_q + (((((beta(n)**2)*exp(-1*((beta(n)**2)+(alpha(m)**2))*t))+(alpha(m)**2))/ &
                ((beta(n)**2)+(alpha(m)**2))) * (beta(n)*sin(beta(n))*bmn))
                
        end do

        q = q + (((bm)+(temp_q)) * &
                ((-alpha(m)*sin(alpha(m)*length)) / (alpha(m)**2)))

        temp_q = 0

    end do
    
end subroutine Q2Dt

! =====================================================================================

subroutine fuxyt(elements, alpha, beta, bi_t, bib, biT, length, x, y, t, u)

    implicit none
    
    integer, intent(in) :: elements
    real(8), dimension(elements), intent(in) :: alpha, beta
    real(8), intent(in) :: bi_t, bib, biT, length, x, y, t
    real(8), intent(out) :: u


    integer :: m, n 
    real(8) :: am, B_m, bm, bmn, temp_u

    !! Calculation of bm, am, B_m, bmn, u

    do m = 1, elements 

        bm = ((4.0 * (sin(alpha(m)*length))) / &
            ((2.0 * alpha(m)*length)+(sin(2 * alpha(m)*length))))

        am = bm + (bm / biT)

        B_m = bm / biT

        do n = 1, elements

            bmn = ((-2*(bm)*(((beta(n))*(sin(beta(n))))-(biT*(cos(beta(n))))+biT)) / (((beta(n))*biT)* &
                (((cos(beta(n)))*(sin(beta(n))))+(beta(n)))))

            temp_u = temp_u + (((((beta(n)**2)*exp(-1*((beta(n)**2)+(alpha(m)**2))*t))+(alpha(m)**2))/ &
                ((beta(n)**2)+(alpha(m)**2))) * (cos(beta(n)*x)*bmn))
                
        end do

        u = u + ((((1 - x)*am) + (x*B_m) + (temp_u))* &
                ((cos(alpha(m)*y))))


        temp_u = 0

    end do

end subroutine fuxyt