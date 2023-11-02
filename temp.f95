program RealOperationSelector
  implicit none
  integer :: choice
  real(8) :: result

  ! Prompt the user for an operation choice
  write(*, *) "Choose an operation (1 for addition, 2 for subtraction): "
  read(*, *) choice

  ! Call the function to perform the selected operation
  result = PerformRealOperation(choice, 10.0d0, 5.0d0)

  ! Print the result
  write(*, *) "Result =", result
contains

  function PerformRealOperation(operation, a, b) result(result)
    integer, intent(in) :: operation
    real(8), intent(in) :: a, b
    real(8) :: result

    ! Perform the selected operation
    select case (operation)
    case (1)
      result = a + b
    case (2)
      result = a - b
    case default
      result = 0.0d0 ! Default operation (you can change this as needed)
    end select

  end function PerformRealOperation
end program RealOperationSelector
