i = 1, 100, 1
    print*,"Please insert the initial guess of the root of equation."
    read*,x
                call ntrap(x, alpha, iteration)

    
    print*,"The root of the equation is",x
    Print*,"Iteration is",iteration
end do
    
    print*, "