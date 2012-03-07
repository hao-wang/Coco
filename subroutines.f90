Module subroutines
	use cosmoSetUp
	implicit none

	contains
!---------------------------Cosmo Subroutines---------------------------------
	subroutine CheckParam
		implicit none
		integer :: icheck, jcheck	
		do icheck=1, nparam
			if(pset(icheck)%n==1) then
				print *, 'Error: Parameters should have a range to vary. Or just comment this parameter out. '
			endif
			if(pset(icheck)%n==0) then
				print *, 'Error: NPARAM is larger than that you have assigned'
				stop
			endif
			do jcheck=icheck+1, nparam
				if(pset(jcheck)%name==pset(icheck)%name) then
					print *, 'Error: Two parameter', icheck, jcheck, 'have the same name!'
					stop
				endif
			enddo
		 enddo
	end subroutine CheckParam

	function invHubble(a)
		implicit none
		real(hp) a, invHubble
		real(hp) z
		z=1._hp/a-1
		invHubble=1._hp/(HubbleParam(z)*a**2)
	end function invHubble

	subroutine ProperDist(nz, zs, rs)
		use Precision
		use constants, only : tol
		implicit none
		real(hp) :: rombint
		external rombint

		integer :: nz
		real(hp), dimension(nz) :: zs, rs
		integer :: i
		real(hp) :: a
		real(hp) :: Ok
		Ok=current_param(nparam+1)
		do i=1, nz
			a=1._hp/(1+zs(i))
			rs(i)=rombint(invHubble, a, 1._hp, tol)
		enddo
		if(Ok<0)  rs=1._hp/sqrt(abs(Ok))*sin(sqrt(abs(Ok))*rs)
		if(Ok>0)  rs=1._hp/sqrt(abs(Ok))*sinh(sqrt(abs(Ok))*rs)

	end subroutine ProperDist

!CMB related observables-------------------
!the integration core to get Sound Horizon
	function dSHda(a)
		implicit none
		real(hp) :: wb
		real(hp) :: rb
		real(hp) :: a, z
		real(hp) :: cs, dSHda
		wb=current_param(nparam)
		rb=3.*wb/(4.*wr)
		cs=1._hp/sqrt(3.*(1+rb*a))
		z=1._hp/a-1
		dSHda=cs/(a**2*HubbleParam(z))
	end function dSHda

	subroutine SoundHorizon(z, r)
		use Precision
		use constants, only : tol
		implicit none
		real(hp) :: rombint
		external rombint

		real(hp) :: z, r
		integer :: i
		real(hp) :: a
		a=1._hp/(1._hp+z)
		r=rombint(dSHda, 1e-8_hp, a, tol)

	end subroutine SoundHorizon

	subroutine CMBth(obsvec)
		use constants, only : PI, tol
		implicit none
		real(hp) :: obsvec(3)
		real(hp) :: la, rparam
		real(hp) :: g1, g2
		real(hp), dimension(1) :: rlss, zlss
		real(hp) :: scalrlss, scalzlss
		real(hp) :: rcs
		real(hp) :: wb, Om, wm, h
		
		Om=current_param(1)
		h=current_param(nparam-1)
		wm=Om*h**2
		wb=current_param(nparam)

!decoupling z, Hu&Sugiyama(1996) 
		g1=0.0783*wb**(-0.238)/(1+39.5*wb**0.763)
		g2=0.560/(1+21.1*wb**1.81)
		zlss=1048*(1+0.00124*wb**(-0.738))*(1+g1*wm**g2)
		call ProperDist(1, zlss, rlss)
		scalzlss=zlss(1)
		scalrlss=rlss(1)

		call SoundHorizon(scalzlss, rcs)
		la=PI*scalrlss/rcs
!shift parameter: R
		rparam=sqrt(Om)*scalrlss
		obsvec=[la, rparam, scalzlss]

	end subroutine CMBth

!BAO related observables-----------------------
	subroutine zdrag(zd)
		implicit none
		real(hp) :: b1, b2
		real(hp) :: wm, h, Om, wb
		real(hp) :: zd

!Baryon drag epoch, Eisenstein&Hu(1998)
		Om=current_param(1)
		h=current_param(nparam-1)
		wm=Om*h**2
		wb=current_param(nparam)

		b1=0.313*wm**(-0.419)*(1+0.607*wm**0.674)
		b2=0.238*wm**0.223
		zd=1291*wm**0.251/(1+0.659*wm**0.828)*(1+b1*wb**b2) 
	end subroutine zdrag

	subroutine BAOth(dz)
		use ReadData, only : nbao, bao
		implicit none
		real(hp) :: rcsdrag
		real(hp), dimension(nbao) :: dz, Dv
		real(hp) :: zd
		real(hp), dimension(nbao) :: rbao
		real(hp), dimension(nbao) :: Hbao
		integer :: i

		call zdrag(zd)
		do i=1, nbao
			Hbao(i)=Hubbleparam(bao(i)%z)
		enddo				
		call SoundHorizon(zd, rcsdrag)
		call ProperDist(nbao, bao%z, rbao)
		Dv=(rbao**2*bao%z/Hbao)**(1._hp/3)
		dz=rcsdrag/Dv
	end subroutine BAOth

!Parallelled BAO-BAOP
	subroutine BAOPth(dz)
		use ReadData, Only : nbaop, baop
		implicit none
		real(hp), dimension(nbaop) :: dz, hubz
		real(hp) :: rcsdrag, zd
		integer :: i

		call zdrag(zd)
		do i=1, nbaop
			hubz(i)=HubbleParam(baop(i)%z)
		enddo
		call SoundHorizon(zd, rcsdrag)
		dz=rcsdrag*hubz
	end subroutine BAOPth

end Module subroutines
 
