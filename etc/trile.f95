program name

    implicit none
    ! real(kind=8), parameter :: tol = 1.d-14
    ! print*, tol 

    integer :: x,a,b

    x = 1
    
  
    10 print*, "10"
    print*, "1st"
    pause

    if ( x .eq. 1 ) then
        print*, "2nd"
        cycle
    end if

    if ( x .eq. 2 ) then
        print*, "3rd"
        goto 10
    end if

end program name
