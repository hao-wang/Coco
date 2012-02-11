!----------------------From Antony Lewis' CosmoMC----------------------
MODULE Ziggurat
! Marsaglia & Tsang generator for random normals & random exponentials.
! Translated from C by Alan Miller (amiller@bigpond.net.au)

! Marsaglia, G. & Tsang, W.W. (2000) `The ziggurat method for generating
! random variables', J. Statist. Software, v5(8).

! This is an electronic journal which can be downloaded from:
! http://www.jstatsoft.org/v05/i08

! N.B. It is assumed that all integers are 32-bit.
! N.B. The value of M2 has been halved to compensate for the lack of
!      unsigned integers in Fortran.

! Latest version - 1 January 2001
!
! AL: useful material at http://en.wikipedia.org/wiki/Ziggurat_algorithm
   IMPLICIT NONE

   PRIVATE

   INTEGER,  PARAMETER  ::  DP=SELECTED_REAL_KIND( 12, 60 )
   REAL(DP), PARAMETER  ::  m1=2147483648.0_DP,   m2=2147483648.0_DP,      &
                            half=0.5_DP
   REAL(DP)             ::  dn=3.442619855899_DP, tn=3.442619855899_DP,    &
                            vn=0.00991256303526217_DP,                     &
                            q,                    de=7.697117470131487_DP, &
                            te=7.697117470131487_DP,                       &
                            ve=0.003949659822581572_DP
   INTEGER,  SAVE       ::  iz, jz, jsr=123456789, kn(0:127),              &
                            ke(0:255), hz
   REAL(DP), SAVE       ::  wn(0:127), fn(0:127), we(0:255), fe(0:255)
   LOGICAL,  SAVE       ::  initialized=.FALSE.

   PUBLIC  :: zigset, shr3, uni, rnor, rexp


CONTAINS


SUBROUTINE zigset( jsrseed )

   INTEGER, INTENT(IN)  :: jsrseed

   INTEGER  :: i

   !  Set the seed
   jsr = jsrseed

   !  Tables for RNOR
   q = vn*EXP(half*dn*dn)
   kn(0) = (dn/q)*m1
   kn(1) = 0
   wn(0) = q/m1
   wn(127) = dn/m1
   fn(0) = 1.0_DP
   fn(127) = EXP( -half*dn*dn )
   DO  i = 126, 1, -1
      dn = SQRT( -2.0_DP * LOG( vn/dn + EXP( -half*dn*dn ) ) )
      kn(i+1) = (dn/tn)*m1
      tn = dn
      fn(i) = EXP(-half*dn*dn)
      wn(i) = dn/m1
   END DO

   !  Tables for REXP
   q = ve*EXP( de )
   ke(0) = (de/q)*m2
   ke(1) = 0
   we(0) = q/m2
   we(255) = de/m2
   fe(0) = 1.0_DP
   fe(255) = EXP( -de )
   DO  i = 254, 1, -1
      de = -LOG( ve/de + EXP( -de ) )
      ke(i+1) = m2 * (de/te)
      te = de
      fe(i) = EXP( -de )
      we(i) = de/m2
   END DO
   initialized = .TRUE.
   RETURN
END SUBROUTINE zigset



!  Generate random 32-bit integers
FUNCTION shr3( ) RESULT( ival )
   INTEGER  ::  ival

   jz = jsr
   jsr = IEOR( jsr, ISHFT( jsr,  13 ) )
   jsr = IEOR( jsr, ISHFT( jsr, -17 ) )
   jsr = IEOR( jsr, ISHFT( jsr,   5 ) )
   ival = jz + jsr
   RETURN
END FUNCTION shr3



!  Generate uniformly distributed random numbers
FUNCTION uni( ) RESULT( fn_val )
   REAL(DP)  ::  fn_val

   fn_val = half + 0.2328306e-9_DP * shr3( )
   RETURN
END FUNCTION uni



!  Generate random normals
FUNCTION rnor( ) RESULT( fn_val )
   REAL(DP)             ::  fn_val

   REAL(DP), PARAMETER  ::  r = 3.442620_DP
   REAL(DP)             ::  x, y

   IF( .NOT. initialized ) CALL zigset( jsr )
   hz = shr3( )
   iz = IAND( hz, 127 )
   IF( ABS( hz ) < kn(iz) ) THEN
      fn_val = hz * wn(iz)
   ELSE
      DO
         IF( iz == 0 ) THEN
            DO
               x = -0.2904764_DP* LOG( uni( ) )
               y = -LOG( uni( ) )
               IF( y+y >= x*x ) EXIT
            END DO
            fn_val = r+x
            IF( hz <= 0 ) fn_val = -fn_val
            RETURN
         END IF
         x = hz * wn(iz)
         IF( fn(iz) + uni( )*(fn(iz-1)-fn(iz)) < EXP(-half*x*x) ) THEN
            fn_val = x
            RETURN
         END IF
         hz = shr3( )
         iz = IAND( hz, 127 )
         IF( ABS( hz ) < kn(iz) ) THEN
            fn_val = hz * wn(iz)
            RETURN
         END IF
      END DO
   END IF
   RETURN
END FUNCTION rnor



!  Generate random exponentials
FUNCTION rexp( ) RESULT( fn_val )
   REAL(DP)  ::  fn_val

   REAL(DP)  ::  x

   IF( .NOT. initialized ) CALL Zigset( jsr )
   jz = shr3( )
   iz = IAND( jz, 255 )
   IF( ABS( jz ) < ke(iz) ) THEN
      fn_val = ABS(jz) * we(iz)
      RETURN
   END IF
   DO
      IF( iz == 0 ) THEN
         fn_val = 7.69711 - LOG( uni( ) )
         RETURN
      END IF
      x = ABS( jz ) * we(iz)
      IF( fe(iz) + uni( )*(fe(iz-1) - fe(iz)) < EXP( -x ) ) THEN
         fn_val = x
         RETURN
      END IF
      jz = shr3( )
      iz = IAND( jz, 255 )
      IF( ABS( jz ) < ke(iz) ) THEN
         fn_val = ABS( jz ) * we(iz)
         RETURN
      END IF
   END DO
   RETURN
END FUNCTION rexp

END MODULE ziggurat

  

module Random
 integer :: rand_inst = 0 
 logical, parameter :: use_ziggurat = .false.
  !Ziggurat is significantly (3-4x) faster, see Wikipedia for details
  !Have seem some suspicious things, though couldn't replicate; may be OK..

contains
   
  subroutine initRandom(i)
  use Ziggurat
  implicit none
  integer, optional, intent(IN) :: i
  integer seed_in,kl,ij
  character(len=10) :: fred
  real :: klr
  
   if (present(i)) then
    seed_in = i
   else
    seed_in = -1
   end if
      if (seed_in /=-1) then
       kl = 9373
       ij = i
      else
       call system_clock(count=ij)
       ij = mod(ij + rand_inst*100, 31328)
       call date_and_time(time=fred)
       read (fred,'(e10.3)') klr
       kl = mod(int(klr*1000), 30081)       
      end if

      call rmarin(ij,kl)
      if (use_ziggurat) call zigset(ij)
  end subroutine initRandom

  subroutine RandRotation(R, N)
   !this is most certainly not the world's most efficient or robust random rotation generator
    integer, intent(in) :: N
    real R(N,N), vec(N), norm
    integer i,j
    
    do j = 1, N
     do
         do i = 1, N
          vec(i) = Gaussian1()
         end do
         do i = 1, j-1
           vec = vec - sum(vec*R(i,:))*R(i,:)
         end do
         norm = sum(vec**2)
         if (norm > 1e-3) exit
     end do
     R(j,:) = vec / sqrt(norm)
    end do
    
  end subroutine RandRotation


  double precision function GAUSSIAN1()

    use Ziggurat

    implicit none
    double precision R, V1, V2, FAC
    integer, save :: iset = 0
    double precision, save :: gset

    if (use_ziggurat) then

     Gaussian1 = rnor( )

    else
     !Box muller
     if (ISET==0) then
        R=2
        do while (R >= 1.d0)
        V1=2.d0*ranmar()-1.d0
        V2=2.d0*ranmar()-1.d0
        R=V1**2+V2**2
        end do
        FAC=sqrt(-2.d0*log(R)/R)
        GSET=V1*FAC
        GAUSSIAN1=V2*FAC
        ISET=1
      else
        GAUSSIAN1=GSET
        ISET=0
      endif
      end if
      end function GAUSSIAN1


     double precision function CAUCHY1()
      implicit none

      Cauchy1 = Gaussian1()/max(1d-15,abs(Gaussian1()))

     end function CAUCHY1


     real FUNCTION RANDEXP1()
!
!     Random-number generator for the exponential distribution
!     Algorithm EA from J. H. Ahrens and U. Dieter,
!     Communications of the ACM, 31 (1988) 1330--1337.
!     Coded by K. G. Hamilton, December 1996, with corrections.
!
      real u, up, g, y
  
      real, parameter ::   alog2= 0.6931471805599453
      real, parameter ::      a = 5.7133631526454228
      real, parameter ::      b = 3.4142135623730950
      real, parameter ::     c = -1.6734053240284925
      real, parameter ::      p = 0.9802581434685472
      real, parameter ::     aa = 5.6005707569738080
      real, parameter ::     bb = 3.3468106480569850
      real, parameter ::     hh = 0.0026106723602095
      real, parameter ::     dd = 0.0857864376269050

      u = ranmar()
      do while (u.le.0)                 ! Comment out this block 
        u = ranmar()                    ! if your RNG can never
      enddo                             ! return exact zero
      g = c
      u = u+u
      do while (u.lt.1.0)
         g = g + alog2
         u = u+u
      enddo
      u = u-1.0
      if (u.le.p) then
        randexp1 = g + aa/(bb-u)
        return
      endif
      do
        u = ranmar()
        y = a/(b-u)
        up = ranmar()
        if ((up*hh+dd)*(b-u)**2 .le. exp(-(y+c))) then
          randexp1 = g+y
          return
        endif
      enddo

      end function randexp1


! This random number generator originally appeared in ''Toward a Universal 
! Random Number Generator'' by George Marsaglia and Arif Zaman. 
! Florida State University Report: FSU-SCRI-87-50 (1987)
! 
! It was later modified by F. James and published in ''A Review of Pseudo-
! random Number Generators'' 
! 
! THIS IS THE BEST KNOWN RANDOM NUMBER GENERATOR AVAILABLE.
!    (However, a newly discovered technique can yield 
!        a period of 10^600. But that is still in the development stage.)
!
! It passes ALL of the tests for random number generators and has a period 
!   of 2^144, is completely portable (gives bit identical results on all 
!   machines with at least 24-bit mantissas in the floating point 
!   representation). 
! 
! The algorithm is a combination of a Fibonacci sequence (with lags of 97
!   and 33, and operation "subtraction plus one, modulo one") and an 
!   "arithmetic sequence" (using subtraction).
!
! On a Vax 11/780, this random number generator can produce a number in 
!    13 microseconds. 
!======================================================================== 
!
!      PROGRAM TstRAN
!     INTEGER IJ, KL, I
! Thee are the seeds needed to produce the test case results
!      IJ = 1802
!      KL = 9373
!
!
! Do the initialization
!      call rmarin(ij,kl)
!
! Generate 20000 random numbers
!      do 10 I = 1, 20000
!         x = RANMAR()
!10    continue
!
! If the random number generator is working properly, the next six random
!    numbers should be:
!          6533892.0  14220222.0  7275067.0
!    6172232.0  8354498.0   10633180.0
!           
!           
!        
!      write(6,20) (4096.0*4096.0*RANMAR(), I=1,6)
!20    format (3f12.1)
!      end
!
      subroutine RMARIN(IJ,KL)
! This is the initialization routine for the random number generator RANMAR()
! NOTE: The seed variables can have values between:    0 <= IJ <= 31328
!                                                      0 <= KL <= 30081
!The random number sequences created by these two seeds are of sufficient 
! length to complete an entire calculation with. For example, if sveral 
! different groups are working on different parts of the same calculation,
! each group could be assigned its own IJ seed. This would leave each group
! with 30000 choices for the second seed. That is to say, this random 
! number generator can create 900 million different subsequences -- with 
! each subsequence having a length of approximately 10^30.
!
! Use IJ = 1802 & KL = 9373 to test the random number generator. The
! subroutine RANMAR should be used to generate 20000 random numbers.
! Then display the next six random numbers generated multiplied by 4096*4096
! If the random number generator is working properly, the random numbers
!    should be:
!           6533892.0  14220222.0  7275067.0
!           6172232.0  8354498.0   10633180.0
      double precision U(97), C, CD, CM, S, T
      integer I97, J97,i,j,k,l,m
      integer ij,kl
      integer ii,jj
           
    
!      INTEGER IRM(103)
      
      common /RASET1/ U, C, CD, CM, I97, J97
      if( IJ .lt. 0  .or.  IJ .gt. 31328  .or. &
         KL .lt. 0  .or.  KL .gt. 30081 ) then
          print '(A)', ' The first random number seed must have a value  between 0 and 31328'
          print '(A)',' The second seed must have a value between 0 and   30081'
            stop
      endif
      I = mod(IJ/177, 177) + 2
      J = mod(IJ    , 177) + 2
      K = mod(KL/169, 178) + 1
      L = mod(KL,     169) 
      do 2 II = 1, 97
         S = 0.0
         T = 0.5
         do 3 JJ = 1, 24
            M = mod(mod(I*J, 179)*K, 179)
            I = J
            J = K
            K = M
            L = mod(53*L+1, 169)
            if (mod(L*M, 64) .ge. 32) then
               S = S + T
            endif
            T = 0.5 * T
3        continue
         U(II) = S
2     continue
      C = 362436.0 / 16777216.0
      CD = 7654321.0 / 16777216.0
      CM = 16777213.0 /16777216.0
      I97 = 97
      J97 = 33
    
      end subroutine RMARIN

      double precision function RANMAR()
! This is the random number generator proposed by George Marsaglia in 
! Florida State University Report: FSU-SCRI-87-50
! It was slightly modified by F. James to produce an array of pseudorandom
! numbers.
      double precision U(97), C, CD, CM
      integer I97, J97
       double precision uni
    
      common /RASET1/ U, C, CD, CM, I97, J97
!      INTEGER IVEC
         UNI = U(I97) - U(J97)
         if( UNI .lt. 0.0 ) UNI = UNI + 1.0
         U(I97) = UNI
         I97 = I97 - 1
         if(I97 .eq. 0) I97 = 97
         J97 = J97 - 1
         if(J97 .eq. 0) J97 = 97
         C = C - CD
         if( C .lt. 0.d0 ) C = C + CM
         UNI = UNI - C
         if( UNI .lt. 0.d0 ) UNI = UNI + 1.0 ! bug?
         RANMAR = UNI
      
      end function RANMAR


end module Random


 

