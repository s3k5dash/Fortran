program automat
    implicit none
    integer :: x, y, i
    character(100) :: command

    do i = 1, 100
        ! Read input values
        read(*, *) x
        read(*, *) y
        
        ! Create a command string for the arithmetic operation
        write(command, "(A,I0,A,I0)") "echo ", x, " + ", y
        
        ! Call the external command and capture its output
        call execute_command_line(command)
    end do
end program automat
