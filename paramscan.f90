!Do parameter scan in a normal Grid way, or  MCMC way. Setting use_MCMC=1 would lead to the latter.
Module paramScan
	use Chi2
	use Precision
	USE cosmoSetUp,  only : nparam, pset, current_param, modelname
!Variable for recursive usage can't be locally defined	
	integer, dimension(nparam) :: stack, start, stop

	contains

!-------------update the parameters
	SUBROUTINE update_params(ip)
		implicit none
		integer, intent(in), dimension(nparam) :: ip
		integer                                :: i
		! set parameters
		do i=1,nparam
			if (pset(i)%n==1) then
				current_param(i)=pset(i)%min
			else	 
				current_param(i) = (ip(i) - 1._hp)/(pset(i)%n - 1._hp)*(pset(i)%max - pset(i)%min) + pset(i)%min
			endif
		enddo
		call getOk
	END SUBROUTINE update_params

!Recursively search inside the parameter space.
!Modified from a subroutine in 'easyLTB' written by Garcia-Bellido, Juan and Haugbolle, Troels.
	recursive SUBROUTINE GridScanParams(lvl)
		USE IO
		use ReadData, only : nobs, obs
		use Constants, only : verbose
		implicit none 
		integer, intent(in) :: lvl
		integer             :: ip
		character(len=200)	:: fmt=''
		real(hp), dimension(nobs) :: xobs
        integer :: tmp, i, iobs
		                                                                                                                                                   
		if (lvl .eq. nparam) then
		  fmt = '('
		  do ip=1,nparam+nobs-1
			fmt = trim(fmt)//'e14.4,'
		  enddo
		  fmt = trim(fmt)//'e14.4)'
		endif
		
		start=1
		stop=pset%n

		do ip=start(lvl), stop(lvl)
			stack(lvl) = ip
			if (lvl .lt. nparam) then
			  call GridScanParams(lvl+1)
			  cycle
			endif
			call update_params(stack)

			do iobs=1, nobs
!******************************
!abondoned part, attached below
!******************************
				xobs(iobs)=chi2obs(obs(iobs)%name)
			enddo

			write(unit_chi,fmt) current_param(1:nparam), xobs

if(verbose > 0 .and. stack(nparam-1)==2) then
	print *, '(', stack, ')'
	write(*, trim(fmt)//', e14.4') current_param, xobs
endif
		end do

	end subroutine GridScanParams

	subroutine ScanParams(usemcmc)
		use IO
		use MCMC
		implicit none
		integer :: usemcmc
		integer :: ip
		
		if (usemcmc==1) then
			write(unit_log, *) 'Use MCMC Method'
			write(unit_log, *) 'MCMC Sample Number: ', samples_num
			write(unit_log, *) 'MCMC Burn-In Number: ', burn_in
			write(unit_log, *) '---Param Set---: '
			write(unit_log, *) trim(modelname)
			do ip=1, nparam
				write(unit_log, '(a6, a6, f12.5, a6, f12.5, a8, f12.5, a8, f12.5)') &
					pset(ip)%name, '|Min ', pset(ip)%min, '|Max ', pset(ip)%max, &
					'|Center ', pset(ip)%center, '|Width ', pset(ip)%width
			end do
			call MCMCScanParams
		else
			write(unit_log, *) 'Use Normal(Grid Evaluation) Method'
			write(unit_log, *) '---Param Set---: '
			write(unit_log, *) trim(modelname)
			do ip=1, nparam
				write(unit_log, '(a6, a6, i4, a6, f12.5, a6, f12.5)') &
						pset(ip)%name, '|n= ', pset(ip)%n, '|from ', pset(ip)%min, '|to ', pset(ip)%max
			end do
			open(unit_chi, file=trim(plotdir)//(trim(modelname)//chi2file))
			call GridScanParams(1)
			close(unit_chi)
		endif
	end subroutine ScanParams

end Module paramScan

!This part of code was intended to reduce the unnecessary computation time spent on 
!unnecessary work, for example, chi2 of SN on different wb. But it did not work 
!				tmp=0
!!Supernovae is uncorrelated with wb or h. Calculate only once if only wb&h change.
!				if (trim(obs(iobs)%name)=='SN') then
!					do i=1, nparam-1
!						tmp=tmp+abs(effparasn(i)-stack(i))
!					enddo
!					if(tmp/=0) then
!						xsne=chi2obs(obs(iobs)%name)
!						effparasn=stack
!						lastxsne=xsne
!					else
!						xsne=lastxsne
!					endif
!					continue
!				endif
!!Hz is uncorrelated with wb. Calculate only once if only wb changes. 
!				if (trim(obs(iobs)%name)=='Hz') then
!					do i=1, nparam-1
!						tmp=tmp+abs(effparahz(i)-stack(i))
!					enddo
!					if (tmp/=0) then
!						xhz=chi2obs(obs(iobs)%name)
!						effparahz=stack
!						lastxhz=xhz
!					else
!						xhz=lastxhz
!					endif				
!					continue
!				endif
