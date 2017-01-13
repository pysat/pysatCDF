#-----------------------------------------------------------------------
# Copyright 1996-2016 United States Government as represented by the
# Administrator of the National Aeronautics and Space Administration.
# All Rights Reserved.
#-----------------------------------------------------------------------
#------------------------------------------------------------------------------
#
#  NSSDC/CDF						CDF tests makefile.
#
#  Version 1.5, 20-Feb-96, Hughes STX.
#
#  Modification history:
#
#   V1.0  26-Jan-94, J Love	Original version.
#   V1.0a 23-Feb-94, J Love	Added `CC='.
#   V1.0b  6-Apr-94, J Love	Finished `CC='.
#   V1.1  19-Dec-94, J Love	CDF V2.5.
#   V1.2   5-Jan-95, J Love	Updated dependencies, etc.
#   V1.2a 20-Jan-95, J Love	IRIX 6.x.
#   V1.3  16-Mar-95, J Love	Multi-compiler support, etc.
#   V1.4   3-Apr-95, J Love	POSIX.
#   V1.5  20-Feb-96, J Love	CDF V2.6.
#   V1.6  12-Feb-07, D Berger   Added existing LD_LIBRARY_PATH to temp 
#                               definition.
#   V1.7  22-Feb-16, M Liu      Modified to handle Mac's El Captain's not 
#                               (Mac OS 10.11) handling DYLD_LIBRARY_PATH
#
#------------------------------------------------------------------------------
#
# Notes:
#
# `make' on DECstations complains (prints an error message) if an if-then-else
# construct evaluates to FALSE but there is no `false' part.  To prevent this,
# the `NULL' command is executed as the `false' part.
#
#------------------------------------------------------------------------------

.SILENT:

SHELL=/bin/sh
NULL=true

FORTRAN=
FCx=
CCx=
SHARED=
FOPTIONS=
FOPTIONSld=
COPTIONS=
COPTIONSld=
SYSLIBS=
SHAREDEXT=
MAKE=make

INCLUDEcdf=../include

CC=$(CCx)
CFLAGS=$(COPTIONS) -I$(INCLUDEcdf)
CFLAGSld=$(COPTIONSld)

FC=$(FCx)
FFLAGS=$(FOPTIONS)
FFLAGSld=$(FOPTIONSld)
MACos=Darwin

LIBCDFa=../lib/libcdf.a
LIBCDFshare=../lib/libcdf.$(SHAREDEXT)

LIBsARCHIVE=$(LIBCDFa) $(SYSLIBS)
LIBsSHARE=$(LIBCDFshare) $(SYSLIBS)

CDFh=$(INCLUDEcdf)/cdf.h
CDFinc=$(INCLUDEcdf)/cdf.inc

BINsC=qst2c qst2ic qst2icEPOCH16 qst2cOrig qst2icTT2000
BINsF=qst2f qst2if qst2ifEPOCH16 qst2fOrig qst2ifTT2000
BINsCshare=qst2VersionS qst2cS qst2icS qst2icEPOCH16S qst2cOrigS qst2icTT2000S
BINsFshare=qst2fS qst2ifS qst2ifEPOCH16S qst2fOrigS qst2ifTT2000S

WHICHOS=$(shell uname)
ifeq ("$(WHICHOS)","Darwin")
  MACVERSION=$(shell uname -r | cut -f1 -d.)
  MACLIB=$(shell echo $$HOME)/lib/libcdf.dylib
  MACLIBH=$(shell echo $$HOME)/lib
else
  MACVERSION=0
  MACLIB=
  MACLIBH=
endif

CURDIR=$(shell pwd)

#------------------------------------------------------------------------------
# Compile/link.
#------------------------------------------------------------------------------

all: c.yes cS.$(SHARED) f.$(FORTRAN) fS.$(FORTRAN).$(SHARED)

%.o : %.mod

c.yes: $(BINsC)

cS.yes: $(BINsCshare)
cS.no:

f.yes: $(BINsF)
f.no:

fS.yes.yes: $(BINsFshare)
fS.yes.no:
fS.no.yes:
fS.no.no:

qst2c:	qst2c.o $(LIBCDFa)
	@echo $(CURDIR)
	@echo $(CC) $(CFLAGSld) -o $@ qst2c.o $(LIBsARCHIVE)
	      $(CC) $(CFLAGSld) -o $@ qst2c.o $(LIBsARCHIVE)

qst2cOrig:	qst2cOrig.o $(LIBCDFa)
	@echo $(CC) $(CFLAGSld) -o $@ qst2cOrig.o $(LIBsARCHIVE)
	      $(CC) $(CFLAGSld) -o $@ qst2cOrig.o $(LIBsARCHIVE)

qst2ic:	qst2ic.o $(LIBCDFa)
	@echo $(CC) $(CFLAGSld) -o $@ qst2ic.o $(LIBsARCHIVE)
	      $(CC) $(CFLAGSld) -o $@ qst2ic.o $(LIBsARCHIVE)

qst2icEPOCH16: qst2icEPOCH16.o $(LIBCDFa)
	@echo $(CC) $(CFLAGSld) -o $@ qst2icEPOCH16.o $(LIBsARCHIVE)
	      $(CC) $(CFLAGSld) -o $@ qst2icEPOCH16.o $(LIBsARCHIVE)

qst2icTT2000: qst2icTT2000.o $(LIBCDFa)
	@echo $(CC) $(CFLAGSld) -o $@ qst2icTT2000.o $(LIBsARCHIVE)
	      $(CC) $(CFLAGSld) -o $@ qst2icTT2000.o $(LIBsARCHIVE)

qst2VersionS: qst2Version.o $(LIBCDFshare)
	@echo $(CC) $(CFLAGSld) -o $@ qst2Version.o $(LIBsSHARE)
	      $(CC) $(CFLAGSld) -o $@ qst2Version.o $(LIBsSHARE)

qst2cS: qst2c.o $(LIBCDFshare)
	@echo $(CC) $(CFLAGSld) -o $@ qst2c.o $(LIBsSHARE)
	      $(CC) $(CFLAGSld) -o $@ qst2c.o $(LIBsSHARE)

qst2cOrigS: qst2cOrig.o $(LIBCDFshare)
	@echo $(CC) $(CFLAGSld) -o $@ qst2cOrig.o $(LIBsSHARE)
	      $(CC) $(CFLAGSld) -o $@ qst2cOrig.o $(LIBsSHARE)

qst2icS: qst2ic.o $(LIBCDFshare)
	@echo $(CC) $(CFLAGSld) -o $@ qst2ic.o $(LIBsSHARE)
	      $(CC) $(CFLAGSld) -o $@ qst2ic.o $(LIBsSHARE)

qst2icEPOCH16S: qst2icEPOCH16.o $(LIBCDFshare)
	@echo $(CC) $(CFLAGSld) -o $@ qst2icEPOCH16.o $(LIBsSHARE)
	      $(CC) $(CFLAGSld) -o $@ qst2icEPOCH16.o $(LIBsSHARE)

qst2icTT2000S: qst2icTT2000.o $(LIBCDFshare)
	@echo $(CC) $(CFLAGSld) -o $@ qst2icTT2000.o $(LIBsSHARE)
	      $(CC) $(CFLAGSld) -o $@ qst2icTT2000.o $(LIBsSHARE)

qst2f:	qst2f.o $(LIBCDFa)
	@if [ $(MACos) =  `uname` ] ; then \
	   if [ `uname -r | cut -f1 -d.` = 8 ]; then \
	     echo $(FC) $(FFLAGSld) -o $@ qst2f.o $(LIBsARCHIVE) -lSystemStubs; \
	          $(FC) $(FFLAGSld) -o $@ qst2f.o $(LIBsARCHIVE) -lSystemStubs; \
	   else \
	     echo $(FC) $(FFLAGSld) -o $@ qst2f.o $(LIBsARCHIVE); \
	          $(FC) $(FFLAGSld) -o $@ qst2f.o $(LIBsARCHIVE); \
	   fi \
	else \
	  echo $(FC) $(FFLAGSld) -o $@ qst2f.o $(LIBsARCHIVE); \
	       $(FC) $(FFLAGSld) -o $@ qst2f.o $(LIBsARCHIVE); \
	fi

qst2if:	qst2if.o $(LIBCDFa)
	@if [ $(MACos) =  `uname` ] ; then \
	   if [ `uname -r | cut -f1 -d.` = 8 ]; then \
	     echo $(FC) $(FFLAGSld) -o $@ qst2if.o $(LIBsARCHIVE) -lSystemStubs; \
	          $(FC) $(FFLAGSld) -o $@ qst2if.o $(LIBsARCHIVE) -lSystemStubs; \
	   else \
	     echo $(FC) $(FFLAGSld) -o $@ qst2if.o $(LIBsARCHIVE); \
	          $(FC) $(FFLAGSld) -o $@ qst2if.o $(LIBsARCHIVE); \
	   fi \
	else \
	  echo $(FC) $(FFLAGSld) -o $@ qst2if.o $(LIBsARCHIVE); \
	       $(FC) $(FFLAGSld) -o $@ qst2if.o $(LIBsARCHIVE); \
	fi

qst2fOrig: qst2fOrig.o $(LIBCDFa)
	@if [ $(MACos) =  `uname` ] ; then \
	   if [ `uname -r | cut -f1 -d.` = 8 ]; then \
	     echo $(FC) $(FFLAGSld) -o $@ qst2fOrig.o $(LIBsARCHIVE) -lSystemStubs; \
	          $(FC) $(FFLAGSld) -o $@ qst2fOrig.o $(LIBsARCHIVE) -lSystemStubs; \
	   else \
	     echo $(FC) $(FFLAGSld) -o $@ qst2fOrig.o $(LIBsARCHIVE); \
	          $(FC) $(FFLAGSld) -o $@ qst2fOrig.o $(LIBsARCHIVE); \
	   fi \
	else \
	  echo $(FC) $(FFLAGSld) -o $@ qst2fOrig.o $(LIBsARCHIVE); \
	       $(FC) $(FFLAGSld) -o $@ qst2fOrig.o $(LIBsARCHIVE); \
	fi

qst2ifEPOCH16: qst2ifEPOCH16.o $(LIBCDFa)
	@if [ $(MACos) =  `uname` ] ; then \
	   if [ `uname -r | cut -f1 -d.` = 8 ]; then \
	     echo $(FC) $(FFLAGSld) -o $@ qst2ifEPOCH16.o $(LIBsARCHIVE) -lSystemStubs; \
	          $(FC) $(FFLAGSld) -o $@ qst2ifEPOCH16.o $(LIBsARCHIVE) -lSystemStubs; \
	   else \
	     echo $(FC) $(FFLAGSld) -o $@ qst2ifEPOCH16.o $(LIBsARCHIVE); \
	          $(FC) $(FFLAGSld) -o $@ qst2ifEPOCH16.o $(LIBsARCHIVE); \
	   fi \
	else \
	  echo $(FC) $(FFLAGSld) -o $@ qst2ifEPOCH16.o $(LIBsARCHIVE); \
	       $(FC) $(FFLAGSld) -o $@ qst2ifEPOCH16.o $(LIBsARCHIVE); \
	fi

qst2ifTT2000: qst2ifTT2000.o $(LIBCDFa)
	@if [ $(MACos) =  `uname` ] ; then \
	   if [ `uname -r | cut -f1 -d.` = 8 ]; then \
	     echo $(FC) $(FFLAGSld) -o $@ qst2ifTT2000.o $(LIBsARCHIVE) -lSystemStubs; \
	          $(FC) $(FFLAGSld) -o $@ qst2ifTT2000.o $(LIBsARCHIVE) -lSystemStubs; \
	   else \
	     echo $(FC) $(FFLAGSld) -o $@ qst2ifTT2000.o $(LIBsARCHIVE); \
	          $(FC) $(FFLAGSld) -o $@ qst2ifTT2000.o $(LIBsARCHIVE); \
	   fi \
	else \
	  echo $(FC) $(FFLAGSld) -o $@ qst2ifTT2000.o $(LIBsARCHIVE); \
	       $(FC) $(FFLAGSld) -o $@ qst2ifTT2000.o $(LIBsARCHIVE); \
	fi

qst2fS:	qst2f.o $(LIBCDFshare)
	@if [ $(MACos) =  `uname` ] ; then \
	   if [ `uname -r | cut -f1 -d.` = 8 ]; then \
	     echo $(FC) $(FFLAGSld) -o $@ qst2f.o $(LIBsSHARE) -lSystemStubs; \
	          $(FC) $(FFLAGSld) -o $@ qst2f.o $(LIBsSHARE) -lSystemStubs; \
	   else \
	     echo $(FC) $(FFLAGSld) -o $@ qst2f.o $(LIBsSHARE); \
	          $(FC) $(FFLAGSld) -o $@ qst2f.o $(LIBsSHARE); \
	   fi \
	else \
	  echo $(FC) $(FFLAGSld) -o $@ qst2f.o $(LIBsSHARE); \
	       $(FC) $(FFLAGSld) -o $@ qst2f.o $(LIBsSHARE); \
	fi

qst2ifS: qst2if.o $(LIBCDFshare)
	@if [ $(MACos) =  `uname` ] ; then \
	   if [ `uname -r | cut -f1 -d.` = 8 ]; then \
	     echo $(FC) $(FFLAGSld) -o $@ qst2if.o $(LIBsSHARE) -lSystemStubs; \
	          $(FC) $(FFLAGSld) -o $@ qst2if.o $(LIBsSHARE) -lSystemStubs; \
	   else \
	     echo $(FC) $(FFLAGSld) -o $@ qst2if.o $(LIBsSHARE); \
	          $(FC) $(FFLAGSld) -o $@ qst2if.o $(LIBsSHARE); \
	   fi \
	else \
	  echo $(FC) $(FFLAGSld) -o $@ qst2if.o $(LIBsSHARE); \
	       $(FC) $(FFLAGSld) -o $@ qst2if.o $(LIBsSHARE); \
	fi

qst2fOrigS: qst2fOrig.o $(LIBCDFshare)
	@if [ $(MACos) =  `uname` ] ; then \
	   if [ `uname -r | cut -f1 -d.` = 8 ]; then \
	     echo $(FC) $(FFLAGSld) -o $@ qst2fOrig.o $(LIBsSHARE) -lSystemStubs; \
	          $(FC) $(FFLAGSld) -o $@ qst2fOrig.o $(LIBsSHARE) -lSystemStubs; \
	   else \
	     echo $(FC) $(FFLAGSld) -o $@ qst2fOrig.o $(LIBsSHARE); \
	          $(FC) $(FFLAGSld) -o $@ qst2fOrig.o $(LIBsSHARE); \
	   fi \
	else \
	  echo $(FC) $(FFLAGSld) -o $@ qst2fOrig.o $(LIBsSHARE); \
	       $(FC) $(FFLAGSld) -o $@ qst2fOrig.o $(LIBsSHARE); \
	fi

qst2ifEPOCH16S: qst2ifEPOCH16.o $(LIBCDFshare)
	@if [ $(MACos) =  `uname` ] ; then \
	   if [ `uname -r | cut -f1 -d.` = 8 ]; then \
	     echo $(FC) $(FFLAGSld) -o $@ qst2ifEPOCH16.o $(LIBsSHARE) -lSystemStubs; \
	          $(FC) $(FFLAGSld) -o $@ qst2ifEPOCH16.o $(LIBsSHARE) -lSystemStubs; \
	   else \
	     echo $(FC) $(FFLAGSld) -o $@ qst2ifEPOCH16.o $(LIBsSHARE); \
	          $(FC) $(FFLAGSld) -o $@ qst2ifEPOCH16.o $(LIBsSHARE); \
	   fi \
	else \
	  echo $(FC) $(FFLAGSld) -o $@ qst2ifEPOCH16.o $(LIBsSHARE); \
	       $(FC) $(FFLAGSld) -o $@ qst2ifEPOCH16.o $(LIBsSHARE); \
	fi

qst2ifTT2000S: qst2ifTT2000.o $(LIBCDFshare)
	@if [ $(MACos) =  `uname` ] ; then \
	   if [ `uname -r | cut -f1 -d.` = 8 ]; then \
	     echo $(FC) $(FFLAGSld) -o $@ qst2ifTT2000.o $(LIBsSHARE) -lSystemStubs; \
	          $(FC) $(FFLAGSld) -o $@ qst2ifTT2000.o $(LIBsSHARE) -lSystemStubs; \
	   else \
	     echo $(FC) $(FFLAGSld) -o $@ qst2ifTT2000.o $(LIBsSHARE); \
	          $(FC) $(FFLAGSld) -o $@ qst2ifTT2000.o $(LIBsSHARE); \
	   fi \
	else \
	  echo $(FC) $(FFLAGSld) -o $@ qst2ifTT2000.o $(LIBsSHARE); \
	       $(FC) $(FFLAGSld) -o $@ qst2ifTT2000.o $(LIBsSHARE); \
	fi
.c.o:
	@echo $(CC) $(CFLAGS) -c $<
	      $(CC) $(CFLAGS) -c $<

.f.o:
	@cp $(CDFinc) CDF.INC
	@echo $(FC) $(FFLAGS) -c $<
	      $(FC) $(FFLAGS) -c $<
	@rm CDF.INC

qst2Version.o: qst2Version.c $(CDFh)
qst2c.o: qst2c.c $(CDFh)
qst2cOrig.o: qst2cOrig.c $(CDFh)
qst2ic.o: qst2ic.c $(CDFh)
qst2icEPOCH16.o: qst2icEPOCH16.c $(CDFh)
qst2icTT2000.o: qst2icTT2000.c $(CDFh)
qst2f.o: qst2f.f $(CDFh) $(CDFinc)
qst2if.o: qst2if.f $(CDFh) $(CDFinc)
qst2fOrig.o: qst2fOrig.f $(CDFh) $(CDFinc)
qst2ifEPOCH16.o: qst2ifEPOCH16.f $(CDFh) $(CDFinc)
qst2ifTT2000.o: qst2ifTT2000.f $(CDFh) $(CDFinc)

#------------------------------------------------------------------------------
# Tests.
#     Each executable name is preceeded by `./' in case the current directory
# is not in the path.
#------------------------------------------------------------------------------

test:
	@if [ -f TEST.cdf ] ; then \rm TEST.* ; else $(NULL) ; fi
	@echo ""
	@echo "Testing CDF library..."
	@echo ""
	@./qst2c
	@./qst2cOrig
	@./qst2ic
	@./qst2icEPOCH16
	@if [ -f qst2icTT2000 ] ; then $(MAKE) test.qst2icTT2000 ; else $(NULL) ; fi
#	@./qst2icTT2000
	@if [ -f qst2f ] ; then $(MAKE) test.qst2f ; else $(NULL) ; fi
	@if [ -f qst2if ] ; then $(MAKE) test.qst2if ; else $(NULL) ; fi
	@if [ -f qst2fOrig ] ; then $(MAKE) test.qst2fOrig ; else $(NULL) ; fi
	@if [ -f qst2ifEPOCH16 ] ; then $(MAKE) test.qst2ifEPOCH16 ; else $(NULL) ; fi
#	@if [ -f qst2ifTT2000 ] ; then ./qst2ifTT2000 ; else $(NULL) ; fi
	@if [ -f qst2ifTT2000 ] ; then $(MAKE) test.qst2ifTT2000 ; else $(NULL) ; fi
	@if [ -f qst2VersionS ] ; then \
	  echo ""; echo "Testing shareable CDF library..."; echo "" ; \
	 else $(NULL) ; \
	fi
# If Mac OX 10.11 (version 15) and later... use special handling
	@if [ $(MACVERSION) -lt 15 ] ; then \
	        $(MAKE) MAKE=$(MAKE) test.normalShare ; \
	else \
	        $(MAKE) MAKE=$(MAKE) test.Mac15Share ; \
	fi

test.normalShare:
	@if [ -f qst2VersionS ] ; then $(MAKE) test.qst2VersionS ; else $(NULL) ; fi
	@if [ -f qst2cS ] ; then $(MAKE) test.qst2cS ; else $(NULL) ; fi
	@if [ -f qst2cOrigS ] ; then $(MAKE) test.qst2cOrigS ; else $(NULL) ; fi
	@if [ -f qst2icS ] ; then $(MAKE) test.qst2icS ; else $(NULL) ; fi
	@if [ -f qst2icEPOCH16S ] ; then $(MAKE) test.qst2icEPOCH16S ; else $(NULL) ; fi
	@if [ -f qst2icTT2000S ] ; then $(MAKE) test.qst2icTT2000S ; else $(NULL) ; fi
	@if [ -f qst2fS ] ; then $(MAKE) test.qst2fS ; else $(NULL) ; fi
	@if [ -f qst2ifS ] ; then $(MAKE) test.qst2ifS ; else $(NULL) ; fi
	@if [ -f qst2fOrigS ] ; then $(MAKE) test.qst2fOrigS ; else $(NULL) ; fi
	@if [ -f qst2ifEPOCH16S ] ; then $(MAKE) test.qst2ifEPOCH16S ; else $(NULL) ; fi
	@if [ -f qst2ifTT2000S ] ; then $(MAKE) test.qst2ifTT2000S ; else $(NULL) ; fi

test.Mac15Share:
	@if [ -f qst2VersionS ] ; then $(MAKE) test.qst2VersionS15 ; else $(NULL) ; fi
	@if [ -f qst2cS ] ; then $(MAKE) test.qst2cS15 ; else $(NULL) ; fi
	@if [ -f qst2cOrigS ] ; then $(MAKE) test.qst2cOrigS15 ; else $(NULL) ; fi
	@if [ -f qst2icS ] ; then $(MAKE) test.qst2icS15 ; else $(NULL) ; fi
	@if [ -f qst2icEPOCH16S ] ; then $(MAKE) test.qst2icEPOCH16S15 ; else $(NULL) ; fi
	@if [ -f qst2icTT2000S ] ; then $(MAKE) test.qst2icTT2000S15 ; else $(NULL) ; fi
	@if [ -f qst2fS ] ; then $(MAKE) test.qst2fS15 ; else $(NULL) ; fi
	@if [ -f qst2ifS ] ; then $(MAKE) test.qst2ifS15 ; else $(NULL) ; fi
	@if [ -f qst2fOrigS ] ; then $(MAKE) test.qst2fOrigS15 ; else $(NULL) ; fi
	@if [ -f qst2ifEPOCH16S ] ; then $(MAKE) test.qst2ifEPOCH16S15 ; else $(NULL) ; fi
	@if [ -f qst2ifTT2000S ] ; then $(MAKE) test.qst2ifTT2000S15 ; else $(NULL) ; fi

test.qst2VersionS:
	@env LD_LIBRARY_PATH="`pwd`/../lib:$(LD_LIBRARY_PATH)" \
	env DYLD_LIBRARY_PATH="`pwd`/../lib:$(DYLD_LIBRARY_PATH)" \
	env PATH="`pwd`/../lib:$(PATH)" \
	env CDF_LEAPSECONDSTABLE="`pwd`/../../CDFLeapSeconds.txt" \
	./qst2VersionS

test.qst2cS:
	@env LD_LIBRARY_PATH="`pwd`/../lib:$(LD_LIBRARY_PATH)" \
	env DYLD_LIBRARY_PATH="`pwd`/../lib:$(DYLD_LIBRARY_PATH)" \
	env PATH="`pwd`/../lib:$(PATH)" \
	env CDF_LEAPSECONDSTABLE="`pwd`/../../CDFLeapSeconds.txt" \
	./qst2cS

test.qst2cOrigS:
	@env LD_LIBRARY_PATH="`pwd`/../lib:$(LD_LIBRARY_PATH)" \
	env DYLD_LIBRARY_PATH="`pwd`/../lib:$(DYLD_LIBRARY_PATH)" \
	env PATH="`pwd`/../lib:$(PATH)" \
	env CDF_LEAPSECONDSTABLE="`pwd`/../../CDFLeapSeconds.txt" \
	./qst2cOrigS

test.qst2icS:
	@env LD_LIBRARY_PATH="`pwd`/../lib:$(LD_LIBRARY_PATH)" \
	env DYLD_LIBRARY_PATH="`pwd`/../lib:$(DYLD_LIBRARY_PATH)" \
	env PATH="`pwd`/../lib:$(PATH)" \
	env CDF_LEAPSECONDSTABLE="`pwd`/../../CDFLeapSeconds.txt" \
	./qst2icS

test.qst2icEPOCH16S:
	@env LD_LIBRARY_PATH="`pwd`/../lib:$(LD_LIBRARY_PATH)" \
	env DYLD_LIBRARY_PATH="`pwd`/../lib:$(DYLD_LIBRARY_PATH)" \
	env PATH="`pwd`/../lib:$(PATH)" \
	env CDF_LEAPSECONDSTABLE="`pwd`/../../CDFLeapSeconds.txt" \
	./qst2icEPOCH16S

test.qst2icTT2000:
	@env CDF_LEAPSECONDSTABLE="`pwd`/../../CDFLeapSeconds.txt" \
	./qst2icTT2000

test.qst2icTT2000S:
	@env LD_LIBRARY_PATH="`pwd`/../lib:$(LD_LIBRARY_PATH)" \
	env DYLD_LIBRARY_PATH="`pwd`/../lib:$(DYLD_LIBRARY_PATH)" \
	env PATH="`pwd`/../lib:$(PATH)" \
	env CDF_LEAPSECONDSTABLE="`pwd`/../../CDFLeapSeconds.txt" \
	./qst2icTT2000S

test.qst2f:
	@env CDF_LEAPSECONDSTABLE="`pwd`/../../CDFLeapSeconds.txt" \
	./qst2f

test.qst2fS:
	@env LD_LIBRARY_PATH="`pwd`/../lib:$(LD_LIBRARY_PATH)" \
	env DYLD_LIBRARY_PATH="`pwd`/../lib:$(DYLD_LIBRARY_PATH)" \
	env PATH="`pwd`/../lib:$(PATH)" \
	env CDF_LEAPSECONDSTABLE="`pwd`/../../CDFLeapSeconds.txt" \
	./qst2fS

test.qst2if:
	@env CDF_LEAPSECONDSTABLE="`pwd`/../../CDFLeapSeconds.txt" \
	./qst2if

test.qst2ifS:
	@env LD_LIBRARY_PATH="`pwd`/../lib:$(LD_LIBRARY_PATH)" \
	env DYLD_LIBRARY_PATH="`pwd`/../lib:$(DYLD_LIBRARY_PATH)" \
	env PATH="`pwd`/../lib:$(PATH)" \
	env CDF_LEAPSECONDSTABLE="`pwd`/../../CDFLeapSeconds.txt" \
	./qst2ifS

test.qst2fOrigS:
	@env LD_LIBRARY_PATH="`pwd`/../lib:$(LD_LIBRARY_PATH)" \
	env DYLD_LIBRARY_PATH="`pwd`/../lib:$(DYLD_LIBRARY_PATH)" \
	env PATH="`pwd`/../lib:$(PATH)" \
	env CDF_LEAPSECONDSTABLE="`pwd`/../../CDFLeapSeconds.txt" \
	./qst2fOrigS

test.qst2fOrig:
	@env CDF_LEAPSECONDSTABLE="`pwd`/../../CDFLeapSeconds.txt" \
	./qst2fOrig

test.qst2ifEPOCH16:
	@env CDF_LEAPSECONDSTABLE="`pwd`/../../CDFLeapSeconds.txt" \
	./qst2ifEPOCH16

test.qst2ifEPOCH16S:
	@env LD_LIBRARY_PATH="`pwd`/../lib:$(LD_LIBRARY_PATH)" \
	env DYLD_LIBRARY_PATH="`pwd`/../lib:$(DYLD_LIBRARY_PATH)" \
	env PATH="`pwd`/../lib:$(PATH)" \
	env CDF_LEAPSECONDSTABLE="`pwd`/../../CDFLeapSeconds.txt" \
	./qst2ifEPOCH16S

test.qst2ifTT2000:
	@env CDF_LEAPSECONDSTABLE="`pwd`/../../CDFLeapSeconds.txt" \
	./qst2ifTT2000

test.qst2ifTT2000S:
	@env LD_LIBRARY_PATH="`pwd`/../lib:$(LD_LIBRARY_PATH)" \
	env DYLD_LIBRARY_PATH="`pwd`/../lib:$(DYLD_LIBRARY_PATH)" \
	env PATH="`pwd`/../lib:$(PATH)" \
	env CDF_LEAPSECONDSTABLE="`pwd`/../../CDFLeapSeconds.txt" \
	./qst2ifTT2000S

test.qst2VersionS15:
	@if [ -e $(MACLIB) ] ; then \
	    mv $(MACLIB) "$(MACLIB)-old" ; \
	    cp -f ../lib/libcdf.dylib $(MACLIB) ; \
	    export CDF_LEAPSECONDSTABLE="`pwd`/../../CDFLeapSeconds.txt" ; \
	    ./qst2VersionS ; \
	    rm -f $(MACLIB) ; \
	    mv "$(MACLIB)-old" $(MACLIB) ; \
	else \
	    if [ ! -e $(MACLIBH) ] ; then \
	      mkdir -p $(MACLIBH) ; \
	    fi ; \
	    cp -f ../lib/libcdf.dylib $(MACLIB) ; \
	    export CDF_LEAPSECONDSTABLE="`pwd`/../../CDFLeapSeconds.txt" ; \
	    ./qst2VersionS ; \
	    rm -f $(MACLIB) ; \
	fi

test.qst2cS15:
	@if [ -e $(MACLIB) ] ; then \
	    mv $(MACLIB) "$(MACLIB)-old" ; \
	    cp -f ../lib/libcdf.dylib $(MACLIB) ; \
	    export CDF_LEAPSECONDSTABLE="`pwd`/../../CDFLeapSeconds.txt" ; \
	    ./qst2cS ; \
	    rm -f $(MACLIB) ; \
	    mv "$(MACLIB)-old" $(MACLIB) ; \
	else \
	    if [ ! -e $(MACLIBH) ] ; then \
	      mkdir -p $(MACLIBH) ; \
	    fi ; \
	    cp -f ../lib/libcdf.dylib $(MACLIB) ; \
	    export CDF_LEAPSECONDSTABLE="`pwd`/../../CDFLeapSeconds.txt" ; \
	    ./qst2cS ; \
	    rm -f $(MACLIB) ; \
	fi

test.qst2cOrigS15:
	@if [ -e $(MACLIB) ] ; then \
	    mv $(MACLIB) "$(MACLIB)-old" ; \
	    cp -f ../lib/libcdf.dylib $(MACLIB) ; \
	    export CDF_LEAPSECONDSTABLE="`pwd`/../../CDFLeapSeconds.txt" ; \
	    ./qst2cOrigS ; \
	    rm -f $(MACLIB) ; \
	    mv "$(MACLIB)-old" $(MACLIB) ; \
	else \
	    if [ ! -e $(MACLIBH) ] ; then \
	      mkdir -p $(MACLIBH) ; \
	    fi ; \
	    cp -f ../lib/libcdf.dylib $(MACLIB) ; \
	    export CDF_LEAPSECONDSTABLE="`pwd`/../../CDFLeapSeconds.txt" ; \
	    ./qst2cOrigS ; \
	    rm -f $(MACLIB) ; \
	fi

test.qst2icS15:
	@if [ -e $(MACLIB) ] ; then \
	    mv $(MACLIB) "$(MACLIB)-old" ; \
	    cp -f ../lib/libcdf.dylib $(MACLIB) ; \
	    export CDF_LEAPSECONDSTABLE="`pwd`/../../CDFLeapSeconds.txt" ; \
	    ./qst2icS ; \
	    rm -f $(MACLIB) ; \
	    mv "$(MACLIB)-old" $(MACLIB) ; \
	else \
	    if [ ! -e $(MACLIBH) ] ; then \
	      mkdir -p $(MACLIBH) ; \
	    fi ; \
	    cp -f ../lib/libcdf.dylib $(MACLIB) ; \
	    export CDF_LEAPSECONDSTABLE="`pwd`/../../CDFLeapSeconds.txt" ; \
	    ./qst2icS ; \
	    rm -f $(MACLIB) ; \
	fi

test.qst2icEPOCH16S15:
	@if [ -e $(MACLIB) ] ; then \
	    mv $(MACLIB) "$(MACLIB)-old" ; \
	    cp -f ../lib/libcdf.dylib $(MACLIB) ; \
	    export CDF_LEAPSECONDSTABLE="`pwd`/../../CDFLeapSeconds.txt" ; \
	    ./qst2icEPOCH16S ; \
	    rm -f $(MACLIB) ; \
	    mv "$(MACLIB)-old" $(MACLIB) ; \
	else \
	    if [ ! -e $(MACLIBH) ] ; then \
	      mkdir -p $(MACLIBH) ; \
	    fi ; \
	    cp -f ../lib/libcdf.dylib $(MACLIB) ; \
	    export CDF_LEAPSECONDSTABLE="`pwd`/../../CDFLeapSeconds.txt" ; \
	    ./qst2icEPOCH16S ; \
	    rm -f $(MACLIB) ; \
	fi

test.qst2icTT2000S15:
	@if [ -e $(MACLIB) ] ; then \
	    mv $(MACLIB) "$(MACLIB)-old" ; \
	    cp -f ../lib/libcdf.dylib $(MACLIB) ; \
	    export CDF_LEAPSECONDSTABLE="`pwd`/../../CDFLeapSeconds.txt" ; \
	    ./qst2icTT2000S ; \
	    rm -f $(MACLIB) ; \
	    mv "$(MACLIB)-old" $(MACLIB) ; \
	else \
	    if [ ! -e $(MACLIBH) ] ; then \
	      mkdir -p $(MACLIBH) ; \
	    fi ; \
	    cp -f ../lib/libcdf.dylib $(MACLIB) ; \
	    export CDF_LEAPSECONDSTABLE="`pwd`/../../CDFLeapSeconds.txt" ; \
	    ./qst2icTT2000S ; \
	    rm -f $(MACLIB) ; \
	fi

test.qst2fS15:
	@if [ -e $(MACLIB) ] ; then \
	    mv $(MACLIB) "$(MACLIB)-old" ; \
	    cp -f ../lib/libcdf.dylib $(MACLIB) ; \
	    export CDF_LEAPSECONDSTABLE="`pwd`/../../CDFLeapSeconds.txt" ; \
	    ./qst2fS ; \
	    rm -f $(MACLIB) ; \
	    mv "$(MACLIB)-old" $(MACLIB) ; \
	else \
	    if [ ! -e $(MACLIBH) ] ; then \
	      mkdir -p $(MACLIBH) ; \
	    fi ; \
	    cp -f ../lib/libcdf.dylib $(MACLIB) ; \
	    export CDF_LEAPSECONDSTABLE="`pwd`/../../CDFLeapSeconds.txt" ; \
	    ./qst2fS ; \
	    rm -f $(MACLIB) ; \
	fi

test.qst2ifS15:
	@if [ -e $(MACLIB) ] ; then \
	    mv $(MACLIB) "$(MACLIB)-old" ; \
	    cp -f ../lib/libcdf.dylib $(MACLIB) ; \
	    export CDF_LEAPSECONDSTABLE="`pwd`/../../CDFLeapSeconds.txt" ; \
	    ./qst2ifS ; \
	    rm -f $(MACLIB) ; \
	    mv "$(MACLIB)-old" $(MACLIB) ; \
	else \
	    if [ ! -e $(MACLIBH) ] ; then \
	      mkdir -p $(MACLIBH) ; \
	    fi ; \
	    cp -f ../lib/libcdf.dylib $(MACLIB) ; \
	    export CDF_LEAPSECONDSTABLE="`pwd`/../../CDFLeapSeconds.txt" ; \
	    ./qst2ifS ; \
	    rm -f $(MACLIB) ; \
	fi

test.qst2fOrigS15:
	@if [ -e $(MACLIB) ] ; then \
	    mv $(MACLIB) "$(MACLIB)-old" ; \
	    cp -f ../lib/libcdf.dylib $(MACLIB) ; \
	    export CDF_LEAPSECONDSTABLE="`pwd`/../../CDFLeapSeconds.txt" ; \
	    ./qst2fOrigS ; \
	    rm -f $(MACLIB) ; \
	    mv "$(MACLIB)-old" $(MACLIB) ; \
	else \
	    if [ ! -e $(MACLIBH) ] ; then \
	      mkdir -p $(MACLIBH) ; \
	    fi ; \
	    cp -f ../lib/libcdf.dylib $(MACLIB) ; \
	    export CDF_LEAPSECONDSTABLE="`pwd`/../../CDFLeapSeconds.txt" ; \
	    ./qst2fOrigS ; \
	    rm -f $(MACLIB) ; \
	fi

test.qst2ifEPOCH16S15:
	@if [ -e $(MACLIB) ] ; then \
	    mv $(MACLIB) "$(MACLIB)-old" ; \
	    cp -f ../lib/libcdf.dylib $(MACLIB) ; \
	    export CDF_LEAPSECONDSTABLE="`pwd`/../../CDFLeapSeconds.txt" ; \
	    ./qst2ifEPOCH16S ; \
	    rm -f $(MACLIB) ; \
	    mv "$(MACLIB)-old" $(MACLIB) ; \
	else \
	    if [ ! -e $(MACLIBH) ] ; then \
	      mkdir -p $(MACLIBH) ; \
	    fi ; \
	    cp -f ../lib/libcdf.dylib $(MACLIB) ; \
	    export CDF_LEAPSECONDSTABLE="`pwd`/../../CDFLeapSeconds.txt" ; \
	    ./qst2ifEPOCH16S ; \
	    rm -f $(MACLIB) ; \
	fi

test.qst2ifTT2000S15:
	@if [ -e $(MACLIB) ] ; then \
	    mv $(MACLIB) "$(MACLIB)-old" ; \
	    cp -f ../lib/libcdf.dylib $(MACLIB) ; \
	    export CDF_LEAPSECONDSTABLE="`pwd`/../../CDFLeapSeconds.txt" ; \
	    ./qst2ifTT2000S ; \
	    rm -f $(MACLIB) ; \
	    mv "$(MACLIB)-old" $(MACLIB) ; \
	else \
	    if [ ! -e $(MACLIBH) ] ; then \
	      mkdir -p $(MACLIBH) ; \
	    fi ; \
	    cp -f ../lib/libcdf.dylib $(MACLIB) ; \
	    export CDF_LEAPSECONDSTABLE="`pwd`/../../CDFLeapSeconds.txt" ; \
	    ./qst2ifTT2000S ; \
	    rm -f $(MACLIB) ; \
	fi

#------------------------------------------------------------------------------

#------------------------------------------------------------------------------

clean:
	@-rm -f *.o
	@-rm -f *.exe
	@-rm -f qst2c
	@-rm -f qst2cOrig
	@-rm -f qst2ic
	@-rm -f qst2iEPOCH16c
	@-rm -f qst2f
	@-rm -f qst2if
	@-rm -f qst2fOrig
	@-rm -f qst2iEPOCH16f
	@-rm -f qst2VersionS
	@-rm -f qst2cS
	@-rm -f qst2cOrigS
	@-rm -f qst2icS
	@-rm -f qst2icEPOCH16
	@-rm -f qst2icEPOCH16S
	@-rm -f qst2icTT2000
	@-rm -f qst2icTT2000S
	@-rm -f qst2fS
	@-rm -f qst2ifS
	@-rm -f qst2fOrigS
	@-rm -f qst2ifEPOCH16
	@-rm -f qst2ifEPOCH16S
	@-rm -f qst2ifTT2000
	@-rm -f qst2ifTT2000S
	@-rm -f qstleapsecondsinfo
	@-rm -f *.trace
	@-rm -f core
	@-rm -f *.cdf

purge:
	@-rm -f *~
	@-rm -f *#
