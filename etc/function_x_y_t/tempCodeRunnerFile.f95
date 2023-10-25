do m = 1, max
    !     cm(m) = length * (alpha(m)**2 + alpha(m) * (bib**2)) + &
    !         sin(2.0 * alpha(m) * length) * ((alpha(m)**2 - bib**2) / 2.0) - &
    !         2.0 * alpha(m) * bib * (cos(alpha(m) * length)**2 - 1.0)

    !     bm(m) = 2.0 * (((alpha(m)**2)*(sin(alpha(m)*length))) - &
    !         (bib*alpha(m))*(cos(alpha(m)*length) - 1.0) )/ cm(m)

    !     am(m) = bm(m) + bm(m) / biT

    !     capitalBm(m) = bm(m) / biT


    ! end do