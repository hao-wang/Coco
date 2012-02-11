Module Chi2
	use subroutines
	use CosmoSetUp
	use ReadData
	contains

	function chi2obs(name)
		use IO, only : unit_data
		use constants, only : infini
		implicit none
		real(hp) :: chi2obs
		real(hp) :: offset
		real(hp), dimension(nsne) :: zs, rs, mu
		real(hp) :: h
		integer :: iz
		real(hp), dimension(nhz) :: hs
		real(hp), dimension(nbao) :: bao_thvec
		real(hp), dimension(nbaop) :: baop_thvec 
		real(hp), dimension(3) :: cmb_thvec
		real(hp) :: AT, BT
		integer :: OutOfBound
		character(len=mtxt) :: name
 
	call CheckBound(OutOfBound)
    if (OutOfBound==1) then
       chi2obs = infini
       return
    end if

!chi2 of SuperNovae--------------------
	if(trim(name)=='SN') then
		zs=sne%z
		call ProperDist(nsne, zs, rs)
		mu     = 5.0_hp*log10((1.0_hp + zs)*rs) + 25.0_hp
		AT = dot_product(mu - sne%mu,matmul(snecov_inv,mu - sne%mu))
		BT = sum(matmul(snecov_inv,mu - sne%mu))
		chi2obs = (AT-BT**2/snecov_sum)
 !   !! H0 normalisation alla Bridle and co. 
!		offset = sum((mu - sne%mu)*1._hp/sne%mu_err**2) / sum(1._hp/sne%mu_err**2)
!		chi2obs= sum((sne%mu - mu + offset)**2*1./sne%mu_err**2)
	endif

!chi2 of Hz------------------------------
	if(trim(name)=='Hz') then
		h=current_param(nparam-1)
		do iz=1, nhz
			hs(iz)=(100*h)*HubbleParam(hz(iz)%z)
		enddo
		chi2obs=sum((hs-hz%h)**2/hz%h_err**2)
	endif

!chi2 of BAO-----------------------------
	if(trim(name)=='BAO') then
		call BAOth(bao_thvec)
		chi2obs=dot_product(bao_thvec-bao%dz, matmul(bao_invcov, bao_thvec-bao%dz))
!		chi2obs=sum((bao_thvec-bao%dz)**2/bao%dzerr**2)
	endif

!chi2 of BAOP----------------------------
	if(trim(name)=='BAOP') then
		call BAOPth(baop_thvec)     
		chi2obs=sum((baop_thvec-baop%dz)**2/baop%dz_err**2)
	endif

!chi2 of CMB-----------------------------
	if(trim(name)=='CMB') then
		call CMBth(cmb_thvec)
		chi2obs=dot_product(cmb_thvec - cmb%di,matmul(cmb_invcov,cmb_thvec-cmb%di))
	endif

!chi2 of hubble constant-----------------
	if(trim(name)=='H0') then
		chi2obs=(current_param(nparam-1)-h0%h0)**2/h0%dh0**2
	endif

	end function chi2obs			
end Module Chi2
