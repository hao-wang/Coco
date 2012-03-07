!Read in SN data(Union2), BAO, CMB(the R parameter) 
!you can add others in a similar way(like SN or Hz),
!or comment out the observations that you do not want to include in,
!In the latter case, all that you need to do is commenting out the corresponding
! ' call read** ' line. 

Module ReadData
	use Precision
	use Constants, only : mtxt, verbose
	use IO, only	: unit_data, datadir

	implicit none
	integer :: nobs=0
	character :: co
	integer :: nroflines, nrofcomments
	integer :: use_sne=0, use_hz=0

!OBS info
	type obsinfo	
		character(len=mtxt) :: name
		integer :: num
	end type obsinfo
	type(obsinfo), dimension(10) :: obs

!BAO type
	integer, parameter :: nbao=2
	type baodata
		real(hp) :: z
		real(hp) :: dz
		real(hp) :: dzerr
	end type baodata
	type(baodata), dimension(nbao) :: bao
	real(hp), dimension(nbao, nbao) :: bao_invcov

!BAOP type
	integer, parameter :: nbaop=2
	type baopdata
		real(hp) :: dz_err
		real(hp) :: dz
		real(hp) :: z
	end type baopdata
	type(baopdata), dimension(nbaop) :: baop

!CMB type 
	integer, parameter :: ncmb=3
	type cmbdata
		real(hp) :: di !distance indicator: R, La, z
	end type cmbdata
	type(cmbdata), dimension(ncmb) :: cmb
	real(hp), dimension(ncmb, ncmb) :: cmb_invcov

!SN data type
	type sndata
		real(hp)       :: z
		real(hp)       :: mu
		real(hp)       :: mu_err
		character(len=mtxt) :: name
	end type
	integer               :: nsne
	type(sndata), dimension(:), allocatable :: sne
	real(hp), dimension(:, :), allocatable :: snecov_inv
	real(hp) :: snecov_sum

!Hz data type
	type hzdata
		real(hp)       :: z
		real(hp)       :: h
		real(hp)       :: h_err
	end type
	integer               :: nhz
	type(hzdata), dimension(:), allocatable :: hz

!H0 
	type h0data
		real(hp)	:: h0
		real(hp)	:: dh0
	end type 
	type(h0data) :: h0 
contains
			
!read in SN data
!modified from a subroutine in 'easyLTB' written by Garcia-Bellido, Juan and Haugbolle, Troels.
	subroutine read_sn
		integer :: iostat
		character(len=100) :: sn_data, sn_cov
		character :: comment
		integer :: isne, il, i
		logical :: sort
		real(hp), allocatable, dimension(:) :: tmp_rc
		type(sndata) :: snswap
		use_sne=1

		comment='#'

		sn_data=trim(datadir)//'SCPUnion2_mu_vs_z.txt'
		open(unit_data, file=sn_data, form='formatted', status='old')
		nroflines = 0 ; nrofcomments = 0                                      ! nr of lines and comments
		do
			read(unit_data,'(a1)', iostat=iostat) co  !read in one line each time
			if (iostat .gt. 0) then 
				print *, 'open sn data error!'
				return
			endif
			if (iostat .lt. 0) exit                                             ! end-of-file
			nroflines    = nroflines    + 1
			nrofcomments = nrofcomments + merge(1,0,co.eq.comment)              ! comment line?
		enddo
		nsne = nroflines - nrofcomments                                       ! nr of sne in data file
		allocate(sne(nsne))
		rewind(unit_data)                                                     ! rewind file
		isne = 0                                                              ! read in sne data
		do il = 1,nroflines
			read(unit_data,'(a1)',advance='no') co !read in one line, but stay at this line 
			if (co .eq. comment) then                                           ! check for comment
			  read(unit_data,'(a1)') co                                         ! if comment, skip line
			  cycle
			endif
			isne = isne + 1
if (verbose .gt. 1) print *, "il,isne,co:", il,isne,co
		read(unit_data,*) sne(isne)%name, sne(isne)%z, sne(isne)%mu , sne(isne)%mu_err
if (verbose > 0) write(*, '(a10, 3f13.4)'),sne(isne)%name,sne(isne)%z, sne(isne)%mu,sne(isne)%mu_err

	enddo
!if (verbose > 0) call sleep(1)
		close(unit_data)

	allocate(snecov_inv(nsne, nsne), tmp_rc(nsne))	
	sn_cov=trim(datadir)//'iunion2.bin'
	open(unit_data,file=sn_cov,form='unformatted',status='old')
	read(unit_data) snecov_inv
	close (unit_data)

!	union_inv = tmp_mat
	snecov_sum = sum(snecov_inv)
  ! very simple inline sort, but good enough for few SNe
	sort = .true.
	do while(sort)
	    sort = .false.
	    do i=1,nsne-1
		  if (sne(i)%z .gt. sne(i+1)%z) then
			snswap = sne(i+1)
			sne(i+1) = sne(i)
	        sne(i)   = snswap
	        tmp_rc = snecov_inv(:,i+1)
	       snecov_inv(:,i+1) = snecov_inv(:,i)
	       snecov_inv(:,i)   = tmp_rc
	       tmp_rc = snecov_inv(i+1,:)
	       snecov_inv(i+1,:) = snecov_inv(i,:)
	       snecov_inv(i,:)   = tmp_rc
	       sort = .true.
	     endif
	   enddo
	 enddo
	nobs=nobs+1
	obs(nobs)%name='SN'
	obs(nobs)%num=nsne
	end subroutine read_sn

!read in Hz data
	subroutine read_hz
		integer :: iostat
		character(len=100) :: hz_data
		character :: comment
		integer :: ihz, il
		use_hz=1

		comment='#'
!hz_data="hz.txt"
		hz_data=trim(datadir)//'hz.txt'
		open(unit_data, file=hz_data, form='formatted', status='old')
		nroflines = 0 ; nrofcomments = 0                                      ! nr of lines and comments
		do
			read(unit_data,'(a1)', iostat=iostat) co  !read in one line each time
			if (iostat .gt. 0) then 
				print *, 'open hz data error!'
				return
			endif
			if (iostat .lt. 0) exit                                          
			nroflines    = nroflines    + 1
			nrofcomments = nrofcomments + merge(1,0,co.eq.comment)             
		enddo
		nhz = nroflines - nrofcomments                                       ! nr of hz in data file
		allocate(hz(nhz))
		rewind(unit_data)                                                     ! rewind file
		ihz = 0                                                              ! read in hz data
		do il = 1,nroflines
			read(unit_data,'(a1)',advance='no') co !read in one line, but stay at this line 
			if (co .eq. comment) then                                           ! check for comment
			  read(unit_data,'(a1)') co                                         ! if comment, skip line
			  cycle
			endif
			ihz = ihz + 1
print *, nroflines, nhz, ihz, il
		read(unit_data,*) hz(ihz)%z, hz(ihz)%h, hz(ihz)%h_err
if(verbose > 0) print *,  hz(ihz)%z, hz(ihz)%h, hz(ihz)%h_err
		enddo
if(verbose > 0) call sleep(1)
		close(unit_data)

		nobs=nobs+1
		obs(nobs)%name='Hz'
		obs(nobs)%num=nhz
	end subroutine read_hz

	subroutine read_cmb
		nobs=nobs+1
		obs(nobs)%name='CMB'
		obs(nobs)%num=ncmb

		cmb_invcov=reshape([2.305, 29.698, -1.333, &
						29.698, 6825.270, -113.180, &
						-1.333, -113.180, 3.414], shape(cmb_invcov))
		cmb%di=[302.09, 1.725, 1091.3]
	end subroutine read_cmb

	subroutine read_bao
		nobs=nobs+1
		obs(nobs)%name='BAO'
		obs(nobs)%num=nbao
		bao%z=[0.2, 0.35] !0.275 !
		bao%dz=[0.1905, 0.1097] !0.139 !
		!bao%dzerr=0.0037
		bao_invcov=reshape([30124, -17227, &
						-17227, 86977], shape(bao_invcov))
	end subroutine read_bao

	subroutine read_baop
		nobs=nobs+1
		obs(nobs)%name='BAOP'
		obs(nobs)%num=nbaop

		baop%z=[0.24, 0.43]
		baop%dz=[0.0407, 0.0442]
		baop%dz_err=[0.0011, 0.0015]
	end subroutine read_baop

	subroutine read_h0
		nobs=nobs+1
		obs(nobs)%name='H0'
		obs(nobs)%num=1

		h0%h0=0.72
		h0%dh0=0.10
	end subroutine read_h0

!An example to add new data
!if a file is used(like SN or Hz), then remember to deallocate space in 'freeMemory'
!	subroutine read_somedata
!		nobs=nobs+1   ---always add this one
!		obs(nobs)%name='dataname' ---always assign the data a name 
!		obs(nobs)%num=num_of_datapoints  
!
!		dataVariable%*=[...]  
!		dataVariable%**=[...] 
!	end subroutine read_somedata

	subroutine ReadInObsData
		use IO, only : unit_log
		integer :: iobs

		call read_sn
!		call read_hz
!		call read_cmb
!		call read_bao
!		call read_baop
!		call read_h0
		
		write(unit_log, *) "---The data sets used--- :"
		do iobs=1, nobs
			write(unit_log, '(a10, a8, i5, a8)') trim(obs(iobs)%name), &
								'with', obs(iobs)%num, 'data points'
		end do

	end subroutine ReadInObsData

	subroutine freeMemory
		integer :: iobs
		do iobs=1, nobs
			if(trim(obs(iobs)%name)=='SN') &
				deallocate(sne)
			if(trim(obs(iobs)%name)=='Hz') &
				deallocate(hz)
		enddo
	end subroutine freeMemory
end Module ReadData
