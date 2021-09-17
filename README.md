Coco ¨Cosmos cOnstraint COde£©combines and in a way extends the functionalities of easyLTB, CosmoMC(the MCMC part), and is intended to simplify the dark energy model constraining work.

Features:
- Support two modes of paramter scanning, Grid/MCMC;
- Automatic IDL file generation for plotting;
- Well structured, easy for modification;
- Running info (parameters, Grid/MCMC, running time, etc) is logged properly.

Files you may want to modify:
- coco.f90: main file; use_MCMC = 1 if using MCMC, 0 otherwise;
- settings.f90: setting parameters and everything about your dark energy model; 
- readdata.f90: add or comment out an observation data, e.g., in the case that supernovae is not needed, it will be sufficient to just comment out the line 'call read_sn'in 'subroutine ReadInObsData';
- genidl.f90: generate IDL .pro file; the corresponding IDL program has an parameter(0 or 1). Read the comments inside the .pro file before running. 

Notes:
1. The code is tested in IDL 7.1;  
1. You need textoidl and Coyote (http://www.ast.cam.ac.uk/~vasily/idl.htm) installed and added to the path of IDL.

A sketch of the workflow with coco:
1. cd into coco:
	1. Edit coco.f90£¬that is, set the value of use_MCMC;	
	1. Edit readdata.f90, add data set or comment out some data;
	1. Edit io.f90, set desired plotdir;
	1. Edit settings.f90£¬set the model and its parameters. You might need to read carefully of this file: I tried my best to put everything that does not need to be changed out of this file;
	1. make
1. Now a .pro file is generated. Start IDL, compile and run the file.
1. One or more .ps files containing the confidence regions (contours) are generated.	
