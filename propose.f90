!---------------------From Antony Lewis' CosmoMC-----------------------------
!Proposal density 
!Using the covariance matrix to give the proposal directions typically
!significantly increases the acceptance rate and gives faster movement 
!around parameter space.
!We generate a random basis in the eigenvectors, then cycle through them
!proposing changes to each, then generate a new random basis
!The distance proposal in the random direction is given by a two-D Gaussian 
!radial function mixed with an exponential, which is quite robust to wrong width estimates
!See http://cosmologist.info/notes/cosmomc.ps.gz

module propose
	use random
	use cosmoSetUp
	implicit none

	logical :: propose_rand_directions = .true.
	real, allocatable, dimension(:,:), save :: Rot_slow
	real(hp) :: propose_scale  = 2.4 

contains

	subroutine RotMatrix(M,n)
		integer, intent(in) :: n
		real M(n,n)
		integer i

		if (propose_rand_directions .and. n > 1) then
		  call RandRotation(M, n)      
		else
		  M = 0
		  do i = 1, n
			   M(i,i) = sign(1., real(ranmar())-0.5)
		  end do
		end if

	end  subroutine RotMatrix


	function Propose_r(in_n) result(r_fac)
	!distance proposal function (scale is 1)
		integer, intent(in) :: in_n
		integer i,n
		real r_fac

		  if (ranmar() < 0.33d0) then
		   r_fac = randexp1()
		  else
		   n = min(in_n,2)
		   r_fac = 0
		   do i = 1, n
			r_fac = r_fac + Gaussian1()**2
		   end do
		   r_fac = sqrt( r_fac / n )
		  end if

	end function Propose_r

	subroutine UpdateParamsDirection(tmp, dist, i)
	!Change parameters in tmp by dist in the direction of the ith random e-vector
		implicit none
		real(hp), dimension(nparam) :: tmp
		real vec(nparam)
		integer, intent(in) :: i
		real(hp), intent(in) :: dist

		tmp = tmp +  Rot_slow(:,i) * dist *  pset%Width

	end subroutine UpdateParamsDirection

	subroutine GetProposalProjSlow(In, Out)
		implicit none
		real(hp), dimension(nparam) :: In, Out
		real(hp) :: wid
		integer, save :: loopix = 0

		Out= In
		wid = propose_scale
		if (mod(loopix,nparam)==0) then
			if (.not. allocated(Rot_slow)) allocate(Rot_slow(nparam,nparam))
			call RotMatrix(Rot_slow, nparam)      
			loopix = 0
		end if
		loopix = loopix + 1

		call UpdateParamsDirection(Out, Propose_r(nparam) * wid, loopix)

	end subroutine GetProposalProjSlow

	subroutine GetProposal(InPlus, OutPlus)
		implicit none
		real(hp), dimension(nparam+1) :: InPlus, OutPlus
		real(hp), dimension(nparam) :: before, after
		before=InPlus(1:nparam)

		call GetProposalProjSlow(before, after)
		current_param(1:nparam)=after
		call getOk
		OutPlus=current_param
	
	end subroutine GetProposal

end module propose
