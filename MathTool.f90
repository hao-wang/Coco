!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        function rombint(f,a,b,tol)
!  Rombint returns the integral from a to b of using Romberg integration.
!  The method converges provided that f(x) is continuous in (a,b).
!  f must be real(hp) and must be declared external in the calling
!  routine.  tol indicates the desired relative accuracy in the integral.
!
		use Precision, only : hp
        implicit none
        integer, parameter :: MAXITER=20
        integer, parameter :: MAXJ=5
        dimension g(MAXJ+1)
        real(hp) f
        external f
        real(hp) :: rombint
        real(hp), intent(in) :: a,b,tol
        integer :: nint, i, k, jmax, j
        real(hp) :: h, gmax, error, g, g0, g1, fourj
!
        h=0.5d0*(b-a)
        gmax=h*(f(a)+f(b))
        g(1)=gmax
        nint=1
        error=1.0d20
        i=0
10        i=i+1
          if (i.gt.MAXITER.or.(i.gt.5.and.abs(error).lt.tol)) &
            go to 40
!  Calculate next trapezoidal rule approximation to integral.
          g0=0._hp
            do 20 k=1,nint
            g0=g0+f(a+(k+k-1)*h)
20        continue
          g0=0.5d0*g(1)+h*g0
          h=0.5d0*h
          nint=nint+nint
          jmax=min(i,MAXJ)
          fourj=1._hp
            do 30 j=1,jmax
!  Use Richardson extrapolation.
            fourj=4._hp*fourj
            g1=g0+(g0-g(j))/(fourj-1._hp)
            g(j)=g0
            g0=g1
30        continue
          if (abs(g0).gt.tol) then
            error=1._hp-gmax/g0
          else
            error=gmax
          end if
          gmax=g0
          g(jmax+1)=g0
        go to 10
40      rombint=g0
        if (i.gt.MAXITER.and.abs(error).gt.tol)  then
          write(*,*) 'Warning: Rombint failed to converge; '
          write (*,*)'integral, error, tol:', rombint,error, tol
        end if
        
        end function rombint
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        function rombint2(f,a,b,tol)
!THE SAME AS ABOVE, ONLY BECAUSE THAT UNWANTED 'RECURSIVE USAGE' WOULD CAUSE PROBLEMS

!  Rombint returns the integral from a to b of using Romberg integration.
!  The method converges provided that f(x) is continuous in (a,b).
!  f must be real(hp) and must be declared external in the calling
!  routine.  tol indicates the desired relative accuracy in the integral.
!
		use Precision, only : hp
        implicit none
        integer, parameter :: MAXITER=20
        integer, parameter :: MAXJ=5
        dimension g(MAXJ+1)
        real(hp) f
        external f
        real(hp) :: rombint2
        real(hp), intent(in) :: a,b,tol
        integer :: nint, i, k, jmax, j
        real(hp) :: h, gmax, error, g, g0, g1, fourj
!
        h=0.5d0*(b-a)
        gmax=h*(f(a)+f(b))
        g(1)=gmax
        nint=1
        error=1.0d20
        i=0
10        i=i+1
          if (i.gt.MAXITER.or.(i.gt.5.and.abs(error).lt.tol)) &
            go to 40
!  Calculate next trapezoidal rule approximation to integral.
          g0=0._hp
            do 20 k=1,nint
            g0=g0+f(a+(k+k-1)*h)
20        continue
          g0=0.5d0*g(1)+h*g0
          h=0.5d0*h
          nint=nint+nint
          jmax=min(i,MAXJ)
          fourj=1._hp
            do 30 j=1,jmax
!  Use Richardson extrapolation.
            fourj=4._hp*fourj
            g1=g0+(g0-g(j))/(fourj-1._hp)
            g(j)=g0
            g0=g1
30        continue
          if (abs(g0).gt.tol) then
            error=1._hp-gmax/g0
          else
            error=gmax
          end if
          gmax=g0
          g(jmax+1)=g0
        go to 10
40      rombint2=g0
        if (i.gt.MAXITER.and.abs(error).gt.tol)  then
          write(*,*) 'Warning: Rombint2 failed to converge; '
          write (*,*)'integral, error, tol:', rombint2,error, tol, a, b
call sleep(10)
        end if
        
        end function rombint2
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc