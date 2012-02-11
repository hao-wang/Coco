!All settings (except for choosing observations--in ReadInData.f90) are 
!set here. Main Modules and Subroutines:
!cosmoSetUp--all info of the parameter set will be set here: name, 
!		  minimum value,  maximum value, number of grid points for each parameter(used only
!		  when use_MCMC is set to 0- see Coco.f90), center value and step
!		  width(used only when use_MCMC=1). 
!		  Note that the number of parameters, nparam, is set to be n+3, n being the number of parameters
!		  BESIDES 'Om', 'h' and 'wb', which are 'reserved' as parameters(I believe it is the proper way, but 
!		  you can always make a modification if you really want to-- then you need to read through
!		  subroutines.f90 to make some more modifications to keep consistent; It is not complicated
!		  actually); 
!		  Om: exists in all sensible cosmology models, pls keep it as one parameter and 
!		  as the first parameter;
!		  Ok: never treated as a free parameter, but its value is used from time to time, so Ok is
!		  absorbed to the global parameter array Current_Param, being its (nparam+1)th member;
!		  wr: Or*h^2, precisely observed by the temperature of the Cosmic Microwave Background, so its
!		  value is taken as a constant, and Or is therefore not a free parameter, but is given by wr/h^2.
!		  wb: crutial in calculating sound horizon for CMB and BAO, exists in any sensible mode; keep
!		  wb and h as the last two parameters and keep their sequence unchanged. 
!HubbleParam--the Friedman equation. Set the way to calculate 'H' with
!		  your model.  
!		  several cosmology models are listed here as examples.   

!------------------Cosmological Model settings--------------------
Module cosmoSetUp
	use Precision
	use constants, only : epsilon, mtxt
	implicit none
	integer, parameter :: nparam=3+1 !n+'Om'+'h'+'wb'
	type param
		integer :: n
		real(hp) :: min, max, center
		real(hp) :: width
		character(len=10) :: name
	end type param
	character(len=mtxt) :: modelname
	type(param), dimension(nparam) :: pset
	real(hp), dimension(nparam+1) :: current_param
	real(hp) :: wr=2.469e-5_hp !Or*h**2
	integer :: use_MCMC

	contains

	subroutine SetModelParam

	modelname='LCDMnonflat'
	pset(1)%name='Om';	pset(1)%n=30;  pset(1)%min=0.05; pset(1)%max=1.2;	pset(1)%center=0.25; pset(1)%width=0.01

	pset(2)%name='Ol';pset(2)%n=25; pset(2)%min=0; pset(2)%max=1.2;	pset(2)%center=0.75; pset(2)%width=0.01
!	pset(2)%name='Ophi';pset(2)%n=25; pset(2)%min=0; pset(2)%max=1.2;	pset(2)%center=0.75; pset(2)%width=0.01
!	pset(2)%name='Ophi';pset(2)%n=25; pset(2)%min=0; pset(2)%max=1.2;	pset(2)%center=0.75; pset(2)%width=0.01
!	pset(2)%name='ksi'; pset(2)%n=25; pset(2)%min=1.;	pset(2)%max=8.;		pset(2)%center=3.;	 pset(2)%width=0.1
!	pset(2)%name='w' ;	pset(2)%n=2;  pset(2)%min=-1.3; pset(2)%max=-0.7;	pset(2)%center=-1.0; pset(2)%width=0.1
!	pset(2)%name='w0' ;	pset(2)%n=26; pset(2)%min=-2;	pset(2)%max=-1./3;	pset(2)%center=-1.0; pset(2)%width=0.1
!	pset(3)%name='w1' ;	pset(3)%n=26; pset(3)%min=-2;	pset(3)%max=3;		pset(3)%center=0;	 pset(3)%width=0.1
!	pset(3)%name='f' ;	pset(3)%n=15;  pset(3)%min=-0.15; pset(3)%max=0.15;	pset(3)%center=0.0; pset(3)%width=0.000001
!	pset(3)%name='wx';	pset(3)%n=25; pset(3)%min=-3.;	pset(3)%max=-0.5;	pset(3)%center=-1.;	 pset(3)%width=0.1

	pset(nparam-1)%name='h'; pset(nparam-1)%n=20; pset(nparam-1)%min=0.50; pset(nparam-1)%max=0.88; 
		  				 pset(nparam-1)%center=0.73; pset(nparam-1)%width=0.01
	pset(nparam)%name='wb';	pset(nparam)%n=20; pset(nparam)%min=0.005; pset(nparam)%max=0.1; 
						pset(nparam)%center=0.025; pset(nparam)%width=0.001

	end subroutine SetModelParam 

!Friedman Equation, or the Hubble Parameter
	function HubbleParam(z)
		use Precision
		implicit none
		real(hp), dimension(nparam+1) :: param
		real(hp) :: Om, Ol, Or, Ok
		real(hp) :: w, Ophi, f
		real(hp) :: HubbleParam, h
		real(hp) :: z, a
		real(hp) :: Ep
		real(hp) :: ksi, wx
		real(hp) :: w0, w1
		real(hp) :: tmp, maxzforDE
		real(hp) :: rombint2
		external rombint2

		param=current_param
		a=1._hp/(1+z)
		Om=param(1); 
		h=param(nparam-1)
		Ok=param(nparam+1)
		Or=wr/h**2
!Pls Keep The Above Part Unchanged

!-----------------------------------Model Specification--------------------------------
!!---param=(om, ol) --LCDMnonflat
		Ol=param(2)
		Ep=Om*(1+z)**3+Ol*(1+z)**(3*(1+w))+Ok*(1+z)**2+Or*(1+z)**4

!!---param=(om, w, ol) --FRW-wCDM
!		Ol=param(2)
!		w=param(3)
!		Ep=Om*(1+z)**3+Ol*(1+z)**(3*(1+w))+Ok*(1+z)**2+Or*(1+z)**4

!!---param=(om, w) --FRW-LCDM
!		Ol=param(2) 
!		Ep=Om*(1+z)**3+Ol				  +Ok*(1+z)**2+Or*(1+z)**4

!----param=(om, w0, w1); w=w0+w1*z/(1+z)
!		w0=param(2)
!		w1=param(3)
!		Ol=1-Om
!		!Dark Energy at High z would be negligible even for w0=-1./3-->(1+z)^2 (1% for ~3e2) 
!		if(z>3e2_hp) then
!			tmp=0
!		else
!			tmp=exp(3*rombint2(f_wz, a, 1._hp, 1e-5_hp))
!		endif
!		Ep=Om*(1+z)**3+Ol*tmp 			  +Ok*(1+z)**2+Or*(1+z)**4
!---param=(om, ophi, f)
!		Ophi=param(2)
!		f=param(3)
!		Ep=Om*(1+z)**3+Ophi+f*Ophi*(1+z)**4	  +Ok*(1+z)**2+Or*(1+z)**4

!----check if Ep>0, otherwise, there will be no big bang-----------------------------------		
		if(Ep <= 0) then
			print *, 'Hubble Param is not a real number now. Parameter Range has a Problem'
			print *, 'current params are :', param
			stop
		endif

		HubbleParam=sqrt(Ep)		

	end function HubbleParam

	function f_wz(a)
		implicit none
		real(hp) :: w0, w1
		real(hp) :: a
		real(hp) :: f_wz
		w0=current_param(2)
		w1=current_param(3)
		f_wz=(1._hp+w0+w1*(1._hp-a))/a
	end function f_wz

	subroutine getOk
		real(hp) :: Ok, h
		Ok=-1e20
		h=current_param(nparam-1) 
!------------------------!-- how to calculate  Ok--!---------------------------!-----------
		if(trim(modelname)=='TEST')		Ok=0
		if(trim(modelname)=='LCDMflat')		Ok=0
		if(trim(modelname)=='LCDMnonflat')  Ok=1-wr/h**2-current_param(1)-current_param(2)
		if(trim(modelname)=='DarkRnonflat') Ok=1-wr/h**2-current_param(1)-current_param(2)
!------------------------!-------------------------!---------------------------!---------
		if(Ok==-1e20) then
			print *, 'Error. Should set Ok manually for each model.'
			stop
		endif
		current_param(nparam+1)=Ok
	end subroutine getOk

	subroutine CheckBound(OutOfBound)
		integer OutOfBound
		OutOfBound=0
!----add boundary condition of your parameterization if there is any-----
		if(trim(modelname)=='wzCDM' .and. current_param(2)+current_param(3) > -1./3) &
			OutOfBound=1
		if(trim(modelname)=='DarkRflat' .and. (1-current_param(1))*current_param(2) < -wr/(current_param(nparam-1)**2)) &
			OutOfBound=1
		if(trim(modelname)=='DarkRnonflat' .and. current_param(2)*current_param(3) < -wr/(current_param(nparam-1)**2)) &
			OutOfBound=1

!------------------------------------------------------------------------
!		if(current_param(nparam)/current_param(nparam-1)**2 > current_param(1)) &
!			OutOfBound=1
		if(any(current_param(1:nparam) > pset%Max) .or. any(current_param(1:nparam) < pset%Min)) &
			OutOfBound=1
	end subroutine CheckBound	
end Module cosmoSetUp
