!ioSetUp--place and name of the final .ps file; keep them unchanged will be ok: .dat files and   
!		  .ps files will be generated in Coco directory.
Module IO
	use Precision
	use constants, only : mtxt
	
	integer , parameter :: use_multi=1 !If you want to plot in a 'SN', 'SN+Hz', 'SN+Hz+..' way
	character(len=mtxt), parameter:: version='CoCo Version 1.0.4' !to keep track with modification	
	character(len=mtxt), parameter:: datadir='./Data/'  !the observational data(SN, Hz,...) that you use
!	character(len=mtxt), parameter:: datadir='Data\'
	character(len=mtxt), parameter:: plotdir=''  
	character(len=mtxt), parameter:: logdir=''
	real(hp) :: time_begin, time_end

	integer, parameter :: unit_data=13, unit_chi=14, unit_idl=15, unit_log=16, unit_mc=17
	character(len=mtxt), parameter :: chi2file='chi2.dat'
	character(len=mtxt), parameter :: psfile='contour.ps'
	character(len=mtxt), parameter :: IDLGridfile='_grid'
	character(len=mtxt), parameter :: IDLMCMCfile='_mcmc'
	character(len=mtxt), parameter :: LOGfile='info.txt'

	!set up the ps file coordinates(used in GenIDL.f90)
	real(hp), parameter	:: cxori=0.10, cyori=0.10, cxend=0.95, cyend=0.90, charsize=0.4, num_color=200
	!set up the legend position
	real(hp), parameter  :: lxori=0.75, lyori=0.95
	real(hp), parameter  :: xgap=0.02, ygap=0.02

	contains

	subroutine IOinit
		implicit none
		integer dt (8)
		character (LEN = 12) REAL_CLOCK (3)

		open(unit_log, file=trim(logdir)//logfile, position="append")
		call cpu_time(time_begin)
		call date_and_time(REAL_CLOCK (1), REAL_CLOCK (2), &
						REAL_CLOCK (3), dt)
		write(unit_log, '(i4,a1,i2,a1,i2,a1,i2,a1,i2,a1,i2)') &
				dt(1), '/', dt(2), '/', dt(3), '/', &
				dt(5), ':', dt(6), ':', dt(7)
		write(unit_log, *) trim(version), '--------------------------'
		if (dt(3)==27) write(unit_log, *) '---COCO. SINCE 2011/01/01---'
	end subroutine
	
	subroutine IOend
		implicit none
		call cpu_time(time_end)
		write(unit_log, *), 'Succeed! Time Elapse:  ', time_end-time_begin, 's'
		write(unit_log, '(a50)') '------------------------------------------------'
		close(unit_log)
	end subroutine	

end Module IO
