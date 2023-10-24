program ReplaceSpecificLines
    implicit none

    character(20) :: filename
    integer :: status
    character(100) :: line
    character(100), dimension(100) :: newContent
    integer :: i

    ! Specify the filename
    filename = 'data.txt'

    ! Open the existing file for reading
    open(unit=10, file=filename, status='old', action='read', iostat=status)
    
    ! Check for errors when opening the file
    if (status /= 0) then
        write(*, *) 'Error opening the file'
        stop
    end if

    ! Read the lines from the existing file
    i = 1
    do
        read(10, '(A)', iostat=status) line
        if (status /= 0) exit
        newContent(i) = line

        ! Check which lines to replace (2 and 4)
        if (i == 2 .or. i == 4) then
            ! Replace the lines with new content
            select case(i)
                case(2)
                    newContent(i) = 'Line 2: This is the new second line.'
                case(4)
                    newContent(i) = 'Line 4: This is the new fourth line.'
            end select
        end if

        i = i + 1
    end do

    ! Close the existing file
    close(10)

    ! Open the file for writing (which replaces the existing file)
    open(unit=20, file=filename, status='replace', action='write', iostat=status)
    
    ! Check for errors when opening the file
    if (status /= 0) then
        write(*, *) 'Error opening the file for writing'
        stop
    end if

    ! Write the modified content back to the file
    do i = 1, i - 1
        write(20, *) newContent(i)
    end do

    ! Close the file
    close(20)

    write(*, *) 'Specific lines replaced in "', filename, '"'
end program ReplaceSpecificLines
