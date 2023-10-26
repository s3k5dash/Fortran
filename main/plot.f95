program name

    implicit none

    character(len=, kind=), attributes :: name
    call execute_command_line("gfortran alphaFinder.f95 -o  alphaFinder")
    call execute_command_line(".\alphaFinder ")

    call execute_command_line("gfortran betaFinder.f95 -o betaFinder")
    call execute_command_line(".\betaFinder ")

end program name
