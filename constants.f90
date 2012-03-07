module Precision
  implicit none

  integer, parameter :: hp = KIND(1.d0)

end module Precision

module Constants
   use precision
   implicit none
  
   real(hp), parameter :: c = 2.99792458e5_hp !km/s
   real(hp), parameter :: PI=3.14159
   real(hp), parameter :: epsilon=1e-5
   real(hp), parameter :: tol=1e-5
   integer, parameter :: mtxt=100
   integer :: verbose=1
	real(hp) :: infini = 1e20

end module Constants