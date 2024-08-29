#------------------------------------------------------------------------------
# Copyright 1996-2013 United States Government as represented by the
# Administrator of the National Aeronautics and Space Administration.
# All Rights Reserved.
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#
#  NSSDC/CDF						CDF tools makefile.
#
#  Version 1.5a, 17-Feb-97, Hughes STX.
#
#  Modification history:
#
#   V1.0  25-Jan-94, J Love	Original version.
#   V1.0a 23-Feb-94, J Love	Added `CC='.
#   V1.0b  6-Apr-94, J Love	Finished `CC=', Solaris using Gnu C compiler.
#   V1.1  28-Nov-94, J Love	CDF V2.5.
#   V1.2  30-Dec-94, J Love	Update dependencies.
#   V1.2a 19-Jan-95, J Love	IRIX 6.x.
#   V1.3  16-Mar-95, J Love	Multi-compiler support, etc.
#   V1.4   3-Apr-95, J Love	POSIX.
#   V1.4a 18-Jul-95, J Love	Added CDFexport.
#   V1.5  12-Aug-96, J Love	CDF V2.6.
#   V1.5a 17-Feb-97, J Love	Added `libcdf.a' to dependencies for `cdfinq'.
#   V1.6  23-Aug-06, M Liu      Added cdfirdump and cdfmerge.
#   V2.0  20-Feb-16, M Liu      Mac OS to build both 32 and 64-bit universal
#                               binary.
#
#------------------------------------------------------------------------------

.SILENT:

SHELL=/bin/sh

CURSES=yes
CCx=gcc
COPTIONS=-I/usr/include/ncurses -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -D_LARGEFILE_SOURCE -O2
COPTIONSld=
SYSLIBS=-lgcc
CURSESLIB=-lncurses
MAKE=make

INCLUDEcdf= ../include

CC=$(CCx)
CFLAGS=$(COPTIONS) -I$(INCLUDEcdf)
CFLAGSld=$(COPTIONSld)

WHICHOS=$(shell uname -a | cut -f1 -d" ")
ifeq ("$(WHICHOS)","Darwin")
  LIBCDFa=../lib/libcdf.dylib
else
  WHICHOS=$(shell uname -a | cut -f7 -d" ")
  ifeq ("$(WHICHOS)","Cygwin")
    LIBCDFa=../lib/libcdf.dll
  else
    ifeq ("$(WHICHOS)","Msys")
      LIBCDFa=../lib/libcdf.dll
    else
      LIBCDFa=../lib/libcdf.a
    endif
  endif
endif

LIBs1=$(LIBCDFa) $(SYSLIBS)
LIBs2=$(LIBCDFa) $(CURSESLIB) $(SYSLIBS)

BINsCURSES=cdfxp cdfedit 
BINsNOTCURSES=cdfcvt cdfstats cdfcmp cdf2skt skt2cdf cdfinq cdfdump cdfirsdump cdfmerge cdfvalidate cdfleapsecondsinfo

CONFIGh=$(INCLUDEcdf)/cdfconfig.h
CDFh=$(INCLUDEcdf)/cdf.h
CDFDISTh=$(INCLUDEcdf)/cdfdist.h $(CDFh) $(CONFIGh)
CDFLIBh=$(INCLUDEcdf)/cdflib.h $(INCLUDEcdf)/cdflib64.h $(CDFDISTh)
CDFTOOLSh=$(INCLUDEcdf)/cdftools.h $(CDFDISTh)

WINDOZh=$(INCLUDEcdf)/windoz.h $(CDFTOOLSh)
WIDGETSh=$(INCLUDEcdf)/widgets.h $(WINDOZh)
CDFCVTh=$(INCLUDEcdf)/cdfcvt.h $(CDFTOOLSh)
CDFEDITh=$(INCLUDEcdf)/cdfedit.h $(WIDGETSh)
CDFXPh=$(INCLUDEcdf)/cdfxp.h $(WIDGETSh)
CDFINQh=$(INCLUDEcdf)/cdfinq.h $(CDFTOOLSh)
CDFSTATSh=$(INCLUDEcdf)/cdfstats.h $(CDFTOOLSh)
CDFCMPh=$(INCLUDEcdf)/cdfcmp.h $(CDFTOOLSh)
CDF2SKTh=$(INCLUDEcdf)/cdf2skt.h $(CDFTOOLSh)
SKT2CDFh=$(INCLUDEcdf)/skt2cdf.h $(CDFTOOLSh)
CDFDUMPh=$(INCLUDEcdf)/cdfdump.h $(CDFTOOLSh) $(INCLUDEcdf)/cdflib.h 
CDFIRsDUMPh=$(INCLUDEcdf)/cdfirsdump.h $(CDFTOOLSh) $(INCLUDEcdf)/cdflib.h
CDFMERGEh=$(INCLUDEcdf)/cdfmerge.h $(CDFTOOLSh) $(INCLUDEcdf)/cdflib.h
CDFVALIDATEh=$(INCLUDEcdf)/cdfvalidate.h $(CDFTOOLSh) $(INCLUDEcdf)/cdflib.h $(INCLUDEcdf)/cdflib64.h
CDFLEAPSECONDSINFOh=$(INCLUDEcdf)/cdfleapsecondsinfo.h $(CDFTOOLSh) $(INCLUDEcdf)/cdflib.h

CDFEDITobjs=cdfedit.o cdfedit1.o cdfedit2.o cdfedit3.o windoz.o widgets.o \
toolbox1.o toolbox3.o
CDFXPobjs=cdfxp.o cdfxp1.o cdfxp2.o windoz.o widgets.o toolbox1.o toolbox3.o
CDFCVTobjs=cdfcvt.o toolbox1.o toolbox3.o
CDF2SKTobjs=cdf2skt.o toolbox1.o toolbox3.o
SKT2CDFobjs=skt2cdf.o toolbox1.o toolbox3.o
CDFINQobjs=cdfinq.o toolbox1.o toolbox3.o
CDFSTATSobjs=cdfstats.o cdfstat1.o cdfstat2.o toolbox1.o toolbox3.o
CDFCMPobjs=cdfcmp.o toolbox1.o toolbox3.o
CDFDUMPobjs=cdfdump.o toolbox1.o toolbox3.o
CDFIRsDUMPobjs=cdfirsdump.o toolbox1.o toolbox3.o
CDFMERGEobjs=cdfmerge.o toolbox1.o toolbox3.o
CDFVALIDATEobjs=cdfvalidate.o toolbox1.o toolbox3.o
CDFLEAPSECONDSINFOobjs=cdfleapsecondsinfo.o toolbox1.o toolbox3.o

#------------------------------------------------------------------------------
# Compile/link.
#------------------------------------------------------------------------------

all: curses.$(CURSES) $(BINsNOTCURSES)

curses.yes: $(BINsCURSES)
curses.no:

cdfcvt: $(CDFCVTobjs) $(LIBCDFa)
	@echo $(CC) $(CFLAGSld) -o $@ $(CDFCVTobjs) $(LIBs1)
	      $(CC) $(CFLAGSld) -o $@ $(CDFCVTobjs) $(LIBs1)

cdf2skt: $(CDF2SKTobjs) $(LIBCDFa)
	@echo $(CC) $(CFLAGSld) -o $@ $(CDF2SKTobjs) $(LIBs1)
	      $(CC) $(CFLAGSld) -o $@ $(CDF2SKTobjs) $(LIBs1)

skt2cdf: $(SKT2CDFobjs) $(LIBCDFa)
	@echo $(CC) $(CFLAGSld) -o $@ $(SKT2CDFobjs) $(LIBs1)
	      $(CC) $(CFLAGSld) -o $@ $(SKT2CDFobjs) $(LIBs1)

cdfinq: $(CDFINQobjs) $(LIBCDFa)
	@echo $(CC) $(CFLAGSld) -o $@ $(CDFINQobjs) $(LIBs1)
	      $(CC) $(CFLAGSld) -o $@ $(CDFINQobjs) $(LIBs1)

cdfstats: $(CDFSTATSobjs) $(LIBCDFa)
	@echo $(CC) $(CFLAGSld) -o $@ $(CDFSTATSobjs) $(LIBs1)
	      $(CC) $(CFLAGSld) -o $@ $(CDFSTATSobjs) $(LIBs1)

cdfcmp: $(CDFCMPobjs) $(LIBCDFa)
	@echo $(CC) $(CFLAGSld) -o $@ $(CDFCMPobjs) $(LIBs1)
	      $(CC) $(CFLAGSld) -o $@ $(CDFCMPobjs) $(LIBs1)

cdfdump: $(CDFDUMPobjs) $(LIBCDFa)
	@echo $(CC) $(CFLAGSld) -o $@ $(CDFDUMPobjs) $(LIBs1)
	      $(CC) $(CFLAGSld) -o $@ $(CDFDUMPobjs) $(LIBs1)

cdfleapsecondsinfo: $(CDFLEAPSECONDSINFOobjs) $(LIBCDFa)
	@echo $(CC) $(CFLAGSld) -o $@ $(CDFLEAPSECONDSINFOobjs) $(LIBs1)
	      $(CC) $(CFLAGSld) -o $@ $(CDFLEAPSECONDSINFOobjs) $(LIBs1)

cdfirsdump: $(CDFIRsDUMPobjs) $(LIBCDFa)
	@echo $(CC) $(CFLAGSld) -o $@ $(CDFIRsDUMPobjs) $(LIBs1)
	      $(CC) $(CFLAGSld) -o $@ $(CDFIRsDUMPobjs) $(LIBs1)

cdfmerge: $(CDFMERGEobjs) $(LIBCDFa)
	@echo $(CC) $(CFLAGSld) -o $@ $(CDFMERGEobjs) $(LIBs1)
	      $(CC) $(CFLAGSld) -o $@ $(CDFMERGEobjs) $(LIBs1)

cdfvalidate: $(CDFVALIDATEobjs) $(LIBCDFa)
	@echo $(CC) $(CFLAGSld) -o $@ $(CDFVALIDATEobjs) $(LIBs1)
	      $(CC) $(CFLAGSld) -o $@ $(CDFVALIDATEobjs) $(LIBs1)

cdfedit: $(CDFEDITobjs) $(LIBCDFa)
	@echo $(CC) $(CFLAGSld) -o $@ $(CDFEDITobjs) $(LIBs2)
	      $(CC) $(CFLAGSld) -o $@ $(CDFEDITobjs) $(LIBs2)

cdfxp: $(CDFXPobjs) $(LIBCDFa)
	@echo $(CC) $(CFLAGSld) -o $@ $(CDFXPobjs) $(LIBs2)
	      $(CC) $(CFLAGSld) -o $@ $(CDFXPobjs) $(LIBs2)

.c.o:
	@echo $(CC) $(CFLAGS) -c $<
	      $(CC) $(CFLAGS) -c $<

cdf2skt.o:	cdf2skt.c $(CDF2SKTh)
cdfcmp.o:	cdfcmp.c $(CDFCMPh)
cdfcvt.o:	cdfcvt.c $(CDFCVTh)
cdfdump.o:	cdfdump.c $(CDFDUMPh)
cdfirsdump.o:	cdfirsdump.c $(CDFIRsDUMPh)
cdfmerge.o:	cdfmerge.c $(CDFMERGEh)
cdfedit.o:	cdfedit.c $(CDFEDITh)
cdfedit1.o:	cdfedit1.c $(CDFEDITh)
cdfedit2.o:	cdfedit2.c $(CDFEDITh)
cdfedit3.o:	cdfedit3.c $(CDFEDITh)
cdfinq.o:	cdfinq.c $(CDFINQh)
cdfstats.o:	cdfstats.c $(CDFSTATSh)
cdfstat1.o:	cdfstat1.c $(CDFSTATSh)
cdfstat2.o:	cdfstat2.c $(CDFSTATSh)
cdfxp.o:	cdfxp.c $(CDFXPh)
cdfxp1.o:	cdfxp1.c $(CDFXPh)
cdfxp2.o:	cdfxp2.c $(CDFXPh)
skt2cdf.o:	skt2cdf.c $(SKT2CDFh)
cdfvalidate.o:	cdfvalidate.c $(CDFVALIDATEh)
cdfleapsecondsinfo.o:	cdfleapsecondsinfo.c $(CDFLEAPSECONDSINFOh)
toolbox1.o:	toolbox1.c $(CDFTOOLSh)
toolbox3.o:	toolbox3.c $(CDFTOOLSh)
widgets.o:	widgets.c $(WIDGETSh)
windoz.o:	windoz.c $(WINDOZh)

#------------------------------------------------------------------------------
# Clean/purge.
#------------------------------------------------------------------------------

clean:
	@-rm -f *.o
	@-rm -f *.exe
	@-rm -f cdfedit
	@-rm -f cdfxp
	@-rm -f cdfcvt
	@-rm -f skt2cdf
	@-rm -f cdf2skt
	@-rm -f cdfinq
	@-rm -f cdfstats
	@-rm -f cdfcmp
	@-rm -f cdfdump
	@-rm -f cdfirsdump
	@-rm -f cdfmerge
	@-rm -f cdfvalidate
	@-rm -f cdfleapsecondsinfo
	@-rm -f core

purge:
	@-rm -f *~
	@-rm -f *#
