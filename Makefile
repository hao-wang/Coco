#F90C=ifort
F90C=gfortran
FFLAGS = -openmp -O3 -w -fpp2 -DMPI
LINKFLAGS = -lm
SOURCES = Settings.f90 \
					ParamScan.f90 \
					Constants.f90 \
					GenIDL.f90 \
					Subroutines.f90 \
					ReadInData.f90 \
					propose.f90 \
					Coco.f90 \
					Chi2.f90 \
					RandGen.f90\
					MathTool.f90\
					io.f90\
					MCMC.f90
					
OBJS = ${SOURCES:.f90=.o}					

default: coco
all : coco
coco : ${OBJS}
	${F90C} -o coco $(OBJS) ${LINKFLAGS}
	
Coco.o : io.o Settings.o Subroutines.o ParamScan.o GenIDL.o
Chi2.o : Subroutines.o ReadInData.o Settings.o io.o
ParamScan.o : Chi2.o Constants.o Settings.o RandGen.o ReadInData.o MCMC.o
GenIDL.o : Settings.o ReadInData.o Constants.o io.o
MCMC.o : Chi2.o Settings.o propose.o Constants.o io.o RandGen.o
propose.o : Settings.o RandGen.o
ReadInData.o : Constants.o Settings.o io.o
io.o : Constants.o
Settings.o : Constants.o MathTool.o
Subroutines.o : ReadInData.o Settings.o Constants.o

%.o: %.f90
	${F90C} -c $*.f90		

clean:
	rm -f *.o *.mod coco
