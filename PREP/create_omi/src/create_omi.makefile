# make file to build program

PROGRAM = create_omi_v54.exe

ifndef compiler
  compiler = intel
# compiler = pgi
# compiler = gcc
endif

#Helps diagnose crashes if they occur
DEBUG = TRUE

ifeq ($(compiler),intel)

  FC = ifort
  CC = icc

# netcdf = /usr/local/apps/netcdf-4.8.1/intel-21.4
  netcdf = /usr/local/apps/netcdf-4.9.2/intel-23.1
  
  include_path = -I $(netcdf)/include -I .


  WARN = 
  FSTD = -O2 -traceback
  DBG  = -O0 -g -check bounds -check uninit -fpe0 -fno-alias -ftrapuv -traceback

  ifneq (,$(filter $(DEBUG), TRUE true ))
     f_FLAGS   = -fixed -132 $(DBG) $(include_path)
     f90_FLAGS = -free $(DBG) $(include_path)
  else
      f_FLAGS   = -fixed -132 $(FSTD) $(include_path)
      f90_FLAGS = -free $(FSTD) $(include_path)
  endif
 
   LINK_FLAGS = -i-static

else ifeq ($(compiler),pgi)

  FC = pgf90
  CC = pgcc
 
  netcdf = /usr/local/apps/netcdf-4.9.2/nvhpc-22.11
  
  include_path = -I $(netcdf)/include -I .

  WARN = 
  FSTD = -O3 -Mextend
  DBG  = -O0 -g -Mbounds -Mchkptr -traceback -Ktrap=fp -Mextend

  ifneq (,$(filter $(DEBUG), TRUE true ))
     f_FLAGS   = -Mfixed $(DBG) $(include_path)
     f90_FLAGS = -Mfree $(DBG) $(include_path)
  else
      f_FLAGS   = -Mfixed $(FSTD) $(include_path)
      f90_FLAGS = -Mfree $(FSTD) $(include_path)
  endif

else ifeq ($(compiler),gcc)

 FC = gfortran
 CC = cc

 netcdf = /usr/local/apps/netcdf-4.9.2/gcc-12.2
 
 include_path = -I $(netcdf)/include -I .

 WARN = 
 FSTD = -O2 -funroll-loops -finit-character=32 -Wconversion-extra -Wtabs -Wsurprising
 DBG  = -Wall -O0 -g -fcheck=all -ffpe-trap=invalid,zero,overflow -fbounds-check -fbacktrace -Wno-zerotrip -Wno-unused-function

 ifneq (,$(filter $(DEBUG), TRUE true ))
     f_FLAGS   = -ffixed-form -ffixed-line-length-132 -funroll-loops -finit-character=32 $(DBG) $(include_path)
     f90_FLAGS = -ffree-form -ffree-line-length-none -funroll-loops -finit-character=32 $(DBG) $(include_path)
 else
     f_FLAGS   = -ffixed-form -ffixed-line-length-132 -funroll-loops -finit-character=32 $(FSTD) $(include_path)
     f90_FLAGS = -ffree-form -ffree-line-length-none -funroll-loops -finit-character=32 $(FSTD) $(include_path)
 endif

else
 ERROR1 = "Makefile not configured to support the specified compiler, $(compiler). USER MUST USE intel, pgi or gcc options or MODIFY Makefile"
endif

ifdef ERROR1
 $(error $(ERROR1))
endif

F_FLAGS   = $(f_FLAGS)
F90_FLAGS = $(f90_FLAGS)
C_FLAGS   = -I.

LINKER     = $(FC)
LINK_FLAGS = 

CPP = $(FC)
CPP_FLAGS = 

NETCDF = -L$(netcdf)/lib -lnetcdf -lnetcdff
LIBRARIES = $(NETCDF)


SRC = \
  get_env_vars.o \
  module_envvar.o \
  outncf_mod.o \
  module_utilities.o \
  create_CMAQ_OMI.o \
  create_ioapi_OMI.o \
  driver.o 

OBJS = \
  $() \
  $(SRC)

.SUFFIXES: .F .f .c .F90 .f90

$(PROGRAM): $(OBJS)
	$(LINKER) $(LINK_FLAGS) $(OBJS) $(LIBRARIES) -o $@

.F.o:
	$(FC) -c $(F_FLAGS) $(CPP_FLAGS) $(INCLUDES) $<

.f.o:
	$(FC) -c $(F_FLAGS) $<

.F90.o:
	$(FC) -c $(F90_FLAGS) $(CPP_FLAGS) $(INCLUDES) $<

.f90.o:
	$(FC) -c $(F90_FLAGS) $<

.c.o:
	$(CC) -c $(C_FLAGS) $<

clean:
	\rm -f $(OBJS) *.mod


# dependencies

create_CMAQ_OMI.o:	get_env_vars.o module_utilities.o outncf_mod.o
create_ioapi_OMI.o:	get_env_vars.o outncf_mod.o
driver.o:	module_envvar.o module_utilities.o outncf_mod.o
module_envvar.o:	get_env_vars.o
module_utilities.o:	module_envvar.o outncf_mod.o

