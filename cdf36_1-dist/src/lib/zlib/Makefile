#------------------------------------------------------------------------------
#
#  NSSDC/CDF						zlib makefile.
#
#  Version 1.0, 27-Mar-12, GSFC/SPDF.
#
#  Modification history:
#
#   V1.0  27-Mar-12, M Liu	Original version.
#------------------------------------------------------------------------------

.SILENT:

SHELL=/bin/sh
NULL=true

PIC=-fPIC
CCx=gcc
COPTIONS=-c

INCLUDEzlib=.

CC=$(CCx)

CFLAGS=$(COPTIONS) $(PIC) -I$(INCLUDEzlib) -DZ_PREFIX

OBJs= adler32.o inffast.o trees.o crc32.o inflate.o infback.o zutil.o \
deflate.o inftrees.o

ZLIB1h= zutil.h zlib.h zconf.h
ZLIB2h= inftrees.h inflate.h inffast.h

#------------------------------------------------------------------------------
# Compile/link.
#------------------------------------------------------------------------------

all:	$(OBJs)

.c.o:
	@echo $(CC) $(CFLAGS) -c $<
	      $(CC) $(CFLAGS) -c $<

adler32.o:	adler32.c $(ZLIB1h)
crc32.o:	crc32.c $(ZLIB1h) crc32.h
deflate.o:	deflate.c $(ZLIB1h) deflate.h
inffast.o:	inffast.c $(ZLIB1h) $(ZLIB2h)
inflate.o:	inflate.c $(ZLIB1h) $(ZLIB2h)
infback.o:	infback.c $(ZLIB1h) $(ZLIB2h)
inftrees.o:	inftrees.c $(ZLIB1h) inftrees.h
trees.o:	trees.c deflate.h trees.h $(ZLIB1h)
zutil.o:	zutil.c $(ZLIB1h)

#------------------------------------------------------------------------------
# Clean/purge.
#------------------------------------------------------------------------------

clean:
	@-rm -f *.o
	@-rm -f core

purge:
	@-rm -f *~
	@-rm -f *#
