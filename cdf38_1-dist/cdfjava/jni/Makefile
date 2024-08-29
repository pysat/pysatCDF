#------------------------------------------------------------------------------
# Copyright 1996-2016 United States Government as represented by the
# Administrator of the National Aeronautics and Space Administration.
# All Rights Reserved.
#------------------------------------------------------------------------------
#
#  SPDF/CDF						JNI library makefile.
#
#  Version 1.0, 23-Jun-2020
#
#  Modification history:
#
#   V1.0  23-Jun-20, M Liu 	Original version.
#
#------------------------------------------------------------------------------
#

.SILENT:

SHELL=/bin/sh
NULL=true

SHARED=
PIC=
CCx=
LDx=
COPTIONS=
SYSLIBS=
JNIEXT=
SHAREDEXT=
LDOPTIONS=
VERSION=
JAVA_HOME=
JNIOS=
LDLIB=
MAKE=make

INCLUDEcdf=../../src/include
INCLUDEjni1=.
INCLUDEjni2=$(JAVA_HOME)/include
INCLUDEjni3=$(INCLUDEjni2)/$(JNIOS)

CC=$(CCx)
LD=$(LDx)

CFLAGS=$(COPTIONS) $(PIC) -I$(INCLUDEcdf) -I$(INCLUDEjni1) -I$(INCLUDEjni2) -I$(INCLUDEjni3)
LDFLAGS=$(LDOPTIONS)

#OBJs=cdfNativeLibrary.o ../../src/lib/libcdf.$(SHAREDEXT)
OBJs=cdfNativeLibrary.o ../../src/lib/libcdf.a

CDFh=$(INCLUDEcdf)/cdf.h $(INCLUDEcdf)/cdfconfig.h $(INCLUDEcdf)/cdfdist.h $(INCLUDEcdf)/cdflib.h $(INCLUDEcdf)/cdflib64.h
JNIh=cdfNativeLibrary.h cdfjava.h

#------------------------------------------------------------------------------
# Compile/link.
#------------------------------------------------------------------------------

all: shared.$(SHARED)

shared.yes: libcdfNativeLibrary.$(JNIEXT)

libcdfNativeLibrary.so \
libcdfNativeLibrary.jnilib \
libcdfNativeLibrary.dll \
libcdfNativeLibrary.sl: $(OBJs)
	@if [ -f $@ ] ; then rm $@ ; else $(NULL) ; fi
	@echo $(LD) $(LDFLAGS) -o $@ $(OBJs) $(LDLIB) -lc -lm
	      $(LD) $(LDFLAGS) -o $@ $(OBJs) $(LDLIB) -lc -lm
.c.o:
	@echo $(CC) $(CFLAGS) -c $< ; \
	      $(CC) $(CFLAGS) -c $< ; \

cdfNativeLibrary.o:	cdfNativeLibrary.c $(CDFh) $(JNIh)

shared.no:

#------------------------------------------------------------------------------
# Clean/purge.
#------------------------------------------------------------------------------

clean:
	@-rm -f *.o
	@-rm -f core
	@-rm -f libcdfNativeLibrary.so
	@-rm -f libcdfNativeLibrary.sl
	@-rm -f libcdfNativeLibrary.jnilib
	@-rm -f libcdfNativeLibrary.dll

purge:
	@-rm -f *~
	@-rm -f *#

