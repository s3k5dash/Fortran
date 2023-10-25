program ReadDataWithTwoRows
  implicit none
  integer :: i, n
  real(8) :: data1(100), data2(100) , temp(100) ! Assuming you have at most 100 data points
  logical :: end_of_file

  ! Open the file
  open(unit=10, file="alpha_m.dat", status='old', action='read')

  ! Initialize the end-of-file flag
  end_of_file = .false.

  ! Read the data from the file
  i = 0
  do
    read(10, *, iostat=n) data1(i + 1), temp(i + 1)
    if (n /= 0) then
      end_of_file = .true.
      exit
    end if
    i = i + 1
  end do

  ! Close the file
   close(10)

  ! Process the data
  do i = 1, i
    write(*,*) "Data1(", i, ") = ", data1(i)
    write(*,*) "Data2(", i, ") = ", data2(i)
  end do

  if (end_of_file) then
    write(*,*) "End of file reached."
  else
    write(*,*) "All data read."
  end if

end program ReadDataWithTwoRows
