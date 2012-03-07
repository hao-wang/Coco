#F90C=ifort
F90C=gfortran
FFLAGS = -openmp -O3 -w -fpp2 -DMPI
LINKFLAGS = -lm
SOURCES = settings.f90 \
					paramscan.f90 \
					constants.f90 \
					genidl.f90 \
					subroutines.f90 \
					readdata.f90 \
					propose.f90 \
					coco.f90 \
					chi2.f90 \
					randgen.f90\
					mathtool.f90\
					io.f90\
					mcmc.f90
					
OBJS = ${SOURCES:.f90=.o}					

default: coco
all : coco
coco : ${OBJS}
	${F90C} -o coco $(OBJS) ${LINKFLAGS}
	
coco.o : io.o settings.o subroutines.o paramscan.o genidl.o
chi2.o : subroutines.o readdata.o settings.o io.o
paramscan.o : chi2.o constants.o settings.o randgen.o readdata.o mcmc.o
genidl.o : settings.o readdata.o constants.o io.o
mcmc.o : chi2.o settings.o propose.o constants.o io.o randgen.o
propose.o : settings.o randgen.o
readdata.o : constants.o settings.o io.o
io.o : constants.o
settings.o : constants.o mathtool.o
subroutines.o : readdata.o settings.o constants.o

%.o: %.f90
	${F90C} -c $*.f90		

clean:
	rm -f *.o *.mod coco
