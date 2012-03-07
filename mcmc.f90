!----------MCMC Sampling Settings-----------------------
Module MCMC
	use Chi2
	use precision
	use CosmoSetUp
	use constants
	use IO
	integer, parameter :: samples_num=300000
	real(hp), parameter:: maxsample=9e6 
	integer, parameter :: combine=2
	integer, parameter :: burn_in=15000
	integer, parameter :: freq_output=10
	real(hp), dimension(nparam+1) :: iniMCParam
	integer, parameter :: has_propose_matrix=0

	contains

	subroutine MCMCScanParams
		use ReadData
!		use MCMC
!		use IO
		use constants, only : mtxt
		implicit none
		integer :: iobs
		character(len=mtxt) :: name
		integer :: multi

		if(combine == 0 .or. combine == 2) then
			multi=0
			do iobs=1, nobs
				name=obs(iobs)%name
				open(unit_mc, file=trim(plotdir)//trim(modelname)//(trim(name)//'_mc.dat'))
				call MCMCsample(iobs, multi)
				close(unit_mc)
			enddo
		endif
		if(combine == 1 .or. combine == 2) then
			multi=1
			if(combine==1) name=''
			if(combine==2) name=obs(1)%name
			do iobs=combine, nobs
				if(iobs==1) name=trim(name)//obs(iobs)%name
				if(iobs/=1) name=trim(name)//(trim('_')//obs(iobs)%name)
				open(unit_mc, file=trim(plotdir)//trim(modelname)//(trim(name)//'_mc.dat'))
				call MCMCsample(iobs, multi)
				close(unit_mc)
			enddo
		endif
	end subroutine MCMCScanParams

	subroutine initMCMCParam
		current_param(1:nparam)=pset%center+pset%width
		call getOk
		iniMCParam=current_param
	end subroutine initMCMCParam

!Modified from MCMC.f90 file of Antony Lewis' CosmoMC
!Use Metropolis-Hastings sampling only. 
	subroutine MCMCsample(obsidx, multi)
		use propose
		use random
		use ReadData
		implicit none

		real(hp), dimension(nparam+1) :: Trial, CurParams
		logical accpt
		integer num
		integer num_accept
		integer num_metropolis,  num_metropolis_accept
		real(hp) Like, CurLike
		real(hp) testlike, testCurLike
!		character(len=mtxt) :: name
		real(hp) :: MaxLike
		integer :: obsidx, iobs
		integer :: multi

		integer, dimension(8) :: values
!initialize random generator with system time 	
		call date_and_time(values=values)
		call initrandom(values(6)*values(7))

		call initMCMCParam

		MaxLike = infini 
		CurLike = infini
		testCurLike = infini
		CurParams = iniMCParam
		num=0
		num_accept = 0

		do while (((num_accept-burn_in) .le. samples_num) .and. (num .le. maxsample))

			num = num + 1

			if (num_accept>= burn_in .and. mod(num_accept, freq_output)==0) then
				if (CurLike /= infini) call WriteMCSample(CurParams(1:nparam), CurLike)
			end if

!Do metropolis
			call GetProposal(CurParams, Trial) 
			current_param=Trial

!Like is logLike, so is just -1./2*chi2. But seems they use 1./2 or 1--> chi2
			if(multi==1) then 
				Like=0
				do iobs=1, obsidx
					Like = Like+chi2obs(obs(iobs)%name)
				end do	 
			else
				Like = chi2obs(obs(obsidx)%name)
			endif
if (verbose > 1) write (*,*) 'Likelihood: ', Like, 'Current Like:', CurLike

!Include the min() so that compilers not doing optimal compilation don't complain

			if (Like /= infini) then
				testLike = Like
				accpt = (testCurLike > testLike .or. randexp1() > testLike - testCurLike) 
			else
				accpt = .false.
			end if

!mark to output the acceptance ratio and likelihood 

			if (accpt) then
   				CurLike = Like
				testCurLike = testLike
				CurParams = Trial
				num_accept = num_accept + 1
				if (Like < MaxLike) MaxLike = Like

				if (verbose > 1) write (*,*) num, ' accepting. ratio:', real(num_accept)/num

				if (mod(num_accept,10) ==0) then
				  write (*,'(a8, f8.3, a8, I8, a8, e8.3, a8, e8.3)') 'r:',real(num_accept)/num, ' in ',num, &
					   ' minX: ',real(MaxLike), ' curX:', CurLike
				end  if
			else
				if (verbose > 1) write (*,*) num,'th rejecting. accept ratio:', real(num_accept)/num
			end if

			if (CurLike == infini .and. num>1000) then
					print *, 'Error : chain got trapped the first 1000 loops. Check the starting range.'
					stop
			end if
		end do

		if (verbose > 0) then
		 if(num>maxsample) write(*, *) 'Stopping as have total trial exceeding ', maxsample, ' samples.' 
		 if(num_accept>samples_num) write(*,*) 'Stopping as have ', samples_num ,' accepted samples. '
		end if

	end subroutine MCMCsample

	subroutine WriteMCSample(param, like)
		implicit none
		integer :: ip
		character(len=mtxt) :: fmt
		real(hp), dimension(nparam) :: param
		real(hp) :: like
		fmt = '('
		do ip=1,nparam+1
			if (ip .eq. 1) then
			fmt=trim(fmt)//'f14.6'
			else
			fmt = trim(fmt)//',f14.6'
			endif
		enddo
		fmt = trim(fmt)//')'
		write(unit_mc, fmt) param, like

	end subroutine WriteMCSample	
!--------------------------------------------------------------------------------------
end Module MCMC
