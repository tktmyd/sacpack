# makefile for sacpack

#FC    = ifort
#FFLAGS = -fast

FC = gfortran
FFLAGS = -O3

all: sac2ascii.x ascii2sac.x sac2bin.x sacdiff.x rsachead.x stacksac.x catsac.x

.SUFFIXES:
.SUFFIXES:.F90 .o .f

.F90.o:
	$(FC) $(FFLAGS) -c $< -o $@

m_std.o   : m_std.F90
m_daytim.o: m_daytim.F90 m_std.o
m_endian.o: m_endian.F90 m_std.o
m_system.o: m_system.F90 m_std.o
m_getopt.o: m_getopt.F90 m_system.o m_std.o
m_sac.o   : m_sac.F90    m_daytim.o m_endian.o m_std.o

sac2ascii.x: sac2ascii.F90 m_sac.o m_daytim.o m_endian.o m_std.o m_system.o
	$(FC) $(FFLAGS) $^ -o $@

ascii2sac.x: ascii2sac.F90 m_sac.o m_daytim.o m_endian.o m_std.o m_system.o
	$(FC) $(FFLAGS) $^ -o $@

sac2bin.x: sac2bin.F90 m_sac.o m_daytim.o m_endian.o m_std.o m_system.o
	$(FC) $(FFLAGS) $^ -o $@

sacdiff.x: sacdiff.F90 m_sac.o m_daytim.o m_endian.o m_std.o m_system.o
	$(FC) $(FFLAGS) $^ -o $@

rsachead.x: rsachead.F90 m_sac.o m_daytim.o m_endian.o m_std.o m_system.o m_getopt.o
	$(FC) $(FFLAGS) $^ -o $@

stacksac.x: stacksac.F90 m_sac.o m_daytim.o m_endian.o m_std.o m_system.o m_getopt.o
	$(FC) $(FFLAGS) $^ -o $@

catsac.x: catsac.F90 m_sac.o m_daytim.o m_endian.o m_std.o m_system.o m_getopt.o
	$(FC) $(FFLAGS) $^ -o $@

clean: 
	rm -f *.o *.mod *~ sac2ascii.x ascii2sac.x sac2bin.x sacdiff.x rsachead.x stacksac.x catsac.x
