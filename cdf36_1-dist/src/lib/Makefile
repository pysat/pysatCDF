#------------------------------------------------------------------------------
# Copyright 1996-2013 United States Government as represented by the
# Administrator of the National Aeronautics and Space Administration.
# All Rights Reserved.
#------------------------------------------------------------------------------
#
#  NSSDC/CDF						CDF library makefile.
#
#  Version 1.5b, 3-Apr-97, Hughes STX.
#
#  Modification history:
#
#   V1.0  15-Dec-93, J Love	Original version.
#   V1.0a 23-Feb-94, J Love	Added `CC='.
#   V1.0b  6-Apr-94, J Love	Finished `CC='.
#   V1.1  14-Nov-94, J Love	CDF V2.5.
#   V1.2  30-Dec-94, J Love	Corrected dependencies.
#   V1.2a 19-Jan-95, J Love	IRIX 6.x.
#   V1.3  16-Mar-95, J Love	Multi-compiler support, etc.
#   V1.4   3-Apr-95, J Love	POSIX.
#   V1.4a  4-May-95, J Love	Added `cdfmem.c'.
#   V1.5  13-Jun-96, J Love	CDF V2.6.
#   V1.5a 21-Feb-97, J Love	Removed RICE.
#   V1.5b  3-Apr-97, J Love	Removed mysterious `-M' option.
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

SHARED=
PIC=
CCx=
LDx=
COPTIONS=
SYSLIBS=
SHAREDEXT=
LDOPTIONS=
AROPTIONS=
RANLIB=

INCLUDEcdf=../include
ZlibDIR=.

CC=$(CCx)
LD=$(LDx)
AR=ar
RANLIBcmd=ranlib
MACos=Darwin

CFLAGS=$(COPTIONS) $(PIC) -I$(INCLUDEcdf) -I$(ZlibDIR) -DZ_PREFIX
LDFLAGS=$(LDOPTIONS)
ARFLAGS=$(AROPTIONS)
ZLIB=zlib

OBJs= \
cdflib.o cdfcre.o cdfope.o cdfclo.o cdfdel.o cdfsel.o cdfcon.o \
cdfget.o cdfmisc2.o cdfread.o cdfwrite.o cdfhyper.o epochuf.o dirutils.o \
cdfput1.o cdfput2.o cdfcsi.o cdffsi.o cdffii.o cdftext.o cdfed.o \
cdfstr.o cdfvalid.o cdfmem.o vstream.o epochu.o cdfmisc3.o cdfhuff.o \
cdfrle.o cdfmisc1.o cdfalloc.o cdfgzip.o cdfalloc64.o cdfdel64.o cdfmisc2_64.o \
cdfread64.o cdfclo64.o cdfget64.o cdfmisc3_64.o cdfsel64.o cdfcon64.o \
cdfput1_64.o cdfwrite64.o cdfcre64.o cdfmisc1_64.o cdfput2_64.o vstream64.o \
cdfhyper64.o cdfrle64.o cdfhuff64.o cdfgzip64.o cdffsi2.o cdffsi3.o cdfcks.o \
cdfmd5.o cdfsav.o cdfvalidator.o cdfvalidator64.o cdftt2000.o cdftt2000f.o \
$(ZLIB)/adler32.o $(ZLIB)/crc32.o $(ZLIB)/deflate.o $(ZLIB)/inffast.o \
$(ZLIB)/inflate.o $(ZLIB)/infback.o $(ZLIB)/inftrees.o $(ZLIB)/trees.o \
$(ZLIB)/zutil.o

CDFh= $(INCLUDEcdf)/cdf.h
CDFREVh= $(INCLUDEcdf)/cdfrev.h
CONFIGh= $(INCLUDEcdf)/cdfconfig.h
CDFDISTh= $(INCLUDEcdf)/cdfdist.h $(CDFh) $(CONFIGh)
CDFLIBh= $(INCLUDEcdf)/cdflib.h $(INCLUDEcdf)/cdflib64.h $(CDFDISTh)
CDFMD5h = $(CDFLIBh) $(INCLUDEcdf)/cdfmd5.h

#------------------------------------------------------------------------------
# Compile/link.
#------------------------------------------------------------------------------

all: libcdf.a shared.$(SHARED)

libcdf.a: $(OBJs)
	@if [ -f $@ ] ; then rm $@ ; else $(NULL) ; fi
	@echo $(AR) $(ARFLAGS) $@ $(OBJs)
	      $(AR) $(ARFLAGS) $@ $(OBJs)
	@make ranlib.$(RANLIB)

ranlib.yes:
	@echo $(RANLIBcmd) libcdf.a
	$(RANLIBcmd) libcdf.a
#	@cp .ranlib .ranlib.yes

shared.yes: libcdf.$(SHAREDEXT)

libcdf.so \
libcdf.dylib \
libcdf.dll \
libcdf.sl: $(OBJs)
	@if [ -f $@ ] ; then rm $@ ; else $(NULL) ; fi
	if [ $(MACos) =  `uname` ] ; then \
	  echo $(LD) $(LDFLAGS) -o $@ $(OBJs) $(SYSLIBS); \
	       $(LD) $(LDFLAGS) -o $@ $(OBJs) $(SYSLIBS); \
	else \
	  echo $(LD) $(LDFLAGS) -o $@ $(OBJs) $(SYSLIBS); \
	       $(LD) $(LDFLAGS) -o $@ $(OBJs) $(SYSLIBS); \
	fi

libcdf.o: $(OBJs)
	@echo $(CC) $(LDFLAGS) -o $@ $(OBJs) $(SYSLIBS)
	      $(CC) $(LDFLAGS) -o $@ $(OBJs) $(SYSLIBS)

.c.o:
	@echo $(CC) $(CFLAGS) -c $<
#	      $(CC) -ggdb $(CFLAGS) -c $<
	      $(CC) $(CFLAGS) -c $<

cdflib.o:	cdflib.c $(CDFLIBh)
cdfcre.o:	cdfcre.c $(CDFLIBh) $(CDFREVh)
cdfope.o:	cdfope.c $(CDFLIBh) $(CDFREVh)
cdfdel.o:	cdfdel.c $(CDFLIBh)
cdfclo.o:	cdfclo.c $(CDFLIBh)
cdfget.o:	cdfget.c $(CDFLIBh) $(CDFREVh)
cdfput1.o:	cdfput1.c $(CDFLIBh)
cdfput2.o:	cdfput2.c $(CDFLIBh)
cdfsel.o:	cdfsel.c $(CDFLIBh)
cdfcon.o:	cdfcon.c $(CDFLIBh)
cdfwrite.o:	cdfwrite.c $(CDFLIBh)
cdfread.o:	cdfread.c $(CDFLIBh)
cdfmisc1.o:	cdfmisc1.c $(CDFLIBh) $(CDFREVh)
cdfmisc2.o:	cdfmisc2.c $(CDFLIBh)
cdfmisc3.o:	cdfmisc3.c $(CDFLIBh)
cdfalloc.o:	cdfalloc.c $(CDFLIBh) $(CDFREVh)
cdftext.o:	cdftext.c $(CDFLIBh)
cdfcsi.o:	cdfcsi.c $(CDFLIBh)
cdffsi.o: 	cdffsi.c $(CDFLIBh)
cdffsi2.o:	cdffsi2.c $(CDFLIBh)
cdffsi3.o:	cdffsi3.c $(CDFLIBh)
cdffii.o: 	cdffii.c $(CDFLIBh)
cdfstr.o:	cdfstr.c $(CDFLIBh)
cdfed.o:  	cdfed.c $(CDFLIBh)
cdfvalid.o:  	cdfvalid.c $(CDFLIBh)
cdfhyper.o:	cdfhyper.c $(CDFLIBh)
cdfhyper64.o:	cdfhyper64.c $(CDFLIBh)
cdfmem.o:	cdfmem.c $(CDFLIBh)
cdfhuff.o:	cdfhuff.c $(CDFLIBh)
cdfrle.o:	cdfrle.c $(CDFLIBh)
cdfgzip.o:	cdfgzip.c $(CDFLIBh)
epochu.o:	epochu.c $(CDFLIBh)
epochuf.o:	epochuf.c $(CDFLIBh)
dirutils.o:	dirutils.c $(CDFLIBh)
vstream.o:	vstream.c $(CDFLIBh)
cdfalloc64.o:	cdfalloc64.c $(CDFLIBh) $(CDFREVh)
cdfclo64.o:	cdfclo64.c $(CDFLIBh)
cdfcon64.o:	cdfcon64.c $(CDFLIBh)
cdfcre64.o:	cdfcre64.c $(CDFLIBh) $(CDFREVh)
cdfdel64.o:	cdfdel64.c $(CDFLIBh)
cdfget64.o:	cdfget64.c $(CDFLIBh) $(CDFREVh)
cdfput1_64.o:	cdfput1_64.c $(CDFLIBh)
cdfput2_64.o:	cdfput2_64.c $(CDFLIBh)
cdfmisc1_64.o:	cdfmisc1_64.c $(CDFLIBh) $(CDFREVh)
cdfmisc2_64.o:	cdfmisc2_64.c $(CDFLIBh)
cdfmisc3_64.o:	cdfmisc3_64.c $(CDFLIBh)
cdfread64.o:	cdfread64.c $(CDFLIBh)
cdfsel64.o:	cdfsel64.c $(CDFLIBh)
cdfwrite64.o:	cdfwrite64.c $(CDFLIBh)
vstream64.o:	vstream64.c $(CDFLIBh)
cdfhuff64.o:	cdfhuff64.c $(CDFLIBh)
cdfrle64.o:	cdfrle64.c $(CDFLIBh)
cdfgzip64.o:	cdfgzip64.c $(CDFLIBh)
cdfcks.o:	cdfcks.c $(CDFLIBh) $(CDFMD5h)
cdfmd5.o:	cdfmd5.c $(CDFLIBh) $(CDFMD5h)
cdfsav.o:	cdfsav.c $(CDFLIBh)
cdfvalidator.o:	cdfvalidator.c $(CDFLIBh)
cdfvalidator64.o:	cdfvalidator64.c $(CDFLIBh)
cdftt2000.o:	cdftt2000.c $(CDFLIBh)
cdftt2000f.o:	cdftt2000f.c $(CDFLIBh)

ranlib.no:
shared.no:

#------------------------------------------------------------------------------
# Clean/purge.
#------------------------------------------------------------------------------

clean:
	@-rm -f *.o
	@-rm -f zlib/*.o
	@-rm -f core
	@-rm -f libcdf.a
	@-rm -f libcdf.so
	@-rm -f libcdf.sl
	@-rm -f libcdf.dylib
	@-rm -f libcdf.dll

purge:
	@-rm -f *~
	@-rm -f *#

