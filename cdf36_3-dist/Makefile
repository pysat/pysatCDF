#------------------------------------------------------------------------------
# Copyright 1996-2016 United States Government as represented by the
# Administrator of the National Aeronautics and Space Administration.
# All Rights Reserved.
#------------------------------------------------------------------------------
#
#  NSSDC/CDF					Makefile for CDF distribution.
#
#  Version 1.0, 2-Feb-05, SPDF/GSFC/NASA.
#
#  Modification history:
#
#   V1.0  02-Feb-05, M Liu 	Original version.
#   V1.1  30-May-05, M Liu      Changed the Fortran compiler to g95 (from f77),
#                               an open source, if possible. 
#   V1.2  11-Jul-05, M Liu      Added MinGW port for PC.
#   V1.3  30-Jan-06, M Liu      Added FreeBSD for PC.
#   V1.4   1-Feb-06, M Liu      Added Intel C port for linux.
#   V1.5  19-Jun-06, M Liu      Added AIX and HP-UX ports.
#   V1.6  23-Aug-06, M Liu      Added cdfirdump and cdfmerge.
#   V1.7  12-Feb-07, D Berger   Added -ff2c to the linux, gnu fortran compiler
#                               options.
#   V1.8  04-May-07, D Berger   Added UCOPTIONS and removed -ff2c.            
#   V1.9  28-Apr-09, M Liu      Added linux/gnu64 combination.
#   V1.10 16-Dec-10, M Liu      Moodified to make it work for both Mac OS X
#                               10.5 and 10.6.
#   V1.11 21-Mar-11, M Liu      No longer support for g77. Use gfortran or g95.
#   V1.12 21-Jul-11, M Liu      Modified to support 64-bit Solaris on Intel 
#                               with Sunstudio and gnu compilers.
#   V1.13 18-Jun-12, M Liu      Modified to support shared library for
#                               Cygwin and Mingw.
#   V2.0  20-Feb-16, M Liu      Added version to dynamic library name for Mac.
#                               Mac OS to build both 32 and 64-bit universal
#                               binary for libraries and tools.
#------------------------------------------------------------------------------
#
# Notes:
#
# `make' on DECstations complains (prints a error message) if an if-then-else
# construct evaluates to FALSE but there is no `false' part.  To prevent this,
# the `NULL' command is executed as the `false' part.
#
#------------------------------------------------------------------------------

.SILENT:

SHELL=/bin/bash
NULL=true
PART=all
TARGET=
SOURCE=
DESTINATION=
VERSION=3.6.3

#------------------------------------------------------------------------------
# Directory locations.
#------------------------------------------------------------------------------

DEFSsrcDIR=src/definitions
INCsrcDIR=src/include
LIBsrcDIR=src/lib
TOOLSsrcDIR=src/tools
TESTSsrcDIR=src/tests
HELPsrcDIR=src/help
LIBsrcZlibDIR=src/lib/zlib

#------------------------------------------------------------------------------
# Macros specified on the `make' command line.
#------------------------------------------------------------------------------

FORTRAN=no
CURSES=yes
SHARED=yes
OS=
ENV=
INSTALLDIR=.
UCOPTIONS=

#------------------------------------------------------------------------------
# Other macros.
#------------------------------------------------------------------------------

MORE=$(shell uname -a | cut -f5 -d " ")
ifeq ("$(OS)","solaris")
  ifeq ("$(ENV)","x86")
    MAKE=gmake
  else
    ifeq ("$(ENV)","x64")
      MAKE=gmake
    else
      MAKE=make
    endif
  endif
else
  ifeq ("$(MORE)","i86pc")
    MAKE=gmake
  else
    MAKE=make
  endif
endif
RANLIB=ranlib

WHICHOS=$(shell uname)
MACVERSION=0
MACLIB=
ifeq ("$(WHICHOS)","Darwin")
  MACVERSION=$(shell uname -r | cut -f1 -d.)
  MACLIB=$(shell echo $$HOME)/lib
endif

#------------------------------------------------------------------------------
# Macros for Solaris.
#------------------------------------------------------------------------------

SHARED_solaris=yes
FOPTIONS_solaris=-w
FOPTIONSld_solaris=
SHAREDEXT_solaris=so
AROPTIONS_solaris=rc
RANLIB_solaris=no
FC_solaris=f90
EXEEXT_solaris=

CURSES_solaris_sparc=yes
CC_solaris_sparc=cc
LD_solaris_sparc=ld
LDOPTIONS_solaris_sparc=-G
PIC_solaris_sparc=-Kpic
COPTIONS_solaris_sparc=-DSOLARIS -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -D_LARGEFILE_SOURCE -xO2
COPTIONSZlib_solaris_sparc=-DHAVE_STDARG_H -DHAVE_UNISTD_H
COPTIONSld_solaris_sparc=
FOPTIONS_solaris_sparc=-ext_names=fsecond-underscore
FOPTIONSld_solaris_sparc=
CURSESLIB_solaris_sparc=-lcurses
SYSLIBSexe_solaris_sparc=-lm
SYSLIBSshr_solaris_sparc=

CURSES_solaris_sparc64=yes
CC_solaris_sparc64=cc
LD_solaris_sparc64=ld
LDOPTIONS_solaris_sparc64=-G
PIC_solaris_sparc64=-Kpic
COPTIONS_solaris_sparc64=-m64 -xarch=sparcvis -DSOLARIS -DSOLARIS64 -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -D_LARGEFILE_SOURCE -xO2
COPTIONSZlib_solaris_sparc64=-DHAVE_STDARG_H -DHAVE_UNISTD_H
COPTIONSld_solaris_sparc64=-m64 -xarch=sparcvis
FOPTIONS_solaris_sparc64=-m64 -ext_names=fsecond-underscore
FOPTIONSld_solaris_sparc64=-m64
CURSESLIB_solaris_sparc64=-lcurses
SYSLIBSexe_solaris_sparc64=-L/usr/local/lib/sparcv9 -lm
SYSLIBSshr_solaris_sparc64=

CURSES_solaris_gnu=yes
CC_solaris_gnu=gcc
LD_solaris_gnu=gcc
LDOPTIONS_solaris_gnu=-shared
PIC_solaris_gnu=-fpic
COPTIONS_solaris_gnu=-DSOLARIS -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -D_LARGEFILE_SOURCE -O2
COPTIONSZlib_solaris_gnu=-DHAVE_STDARG_H -DHAVE_UNISTD_H
COPTIONSld_solaris_gnu=
FOPTIONS_solaris_gnu=-ext_names=fsecond-underscore
FOPTIONSld_solaris_gnu=
CURSESLIB_solaris_gnu=-lcurses
SYSLIBSexe_solaris_gnu=-lm
SYSLIBSshr_solaris_gnu=

CURSES_solaris_gnu64=yes
CC_solaris_gnu64=gcc
LD_solaris_gnu64=gcc
LDOPTIONS_solaris_gnu64=-shared -m64
PIC_solaris_gnu64=-fpic
COPTIONS_solaris_gnu64=-DSOLARIS -DSOLARIS64 -m64 -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -D_LARGEFILE_SOURCE -O2
COPTIONSZlib_solaris_gnu64=-DHAVE_STDARG_H -DHAVE_UNISTD_H
COPTIONSld_solaris_gnu64=-m64
FOPTIONS_solaris_gnu64=-m64 -ext_names=fsecond-underscore
FOPTIONSld_solaris_gnu64=-m64
CURSESLIB_solaris_gnu64=-lcurses
SYSLIBSexe_solaris_gnu64=-L/usr/local/lib/sparcv9 -lm
SYSLIBSshr_solaris_gnu64=

CURSES_solaris_gnu64i=yes
CC_solaris_gnu64i=gcc
LD_solaris_gnu64i=gcc
LDOPTIONS_solaris_gnu64i=-shared -m64
PIC_solaris_gnu64i=-fpic
COPTIONS_solaris_gnu64i=-DSOLARIS -DSOLARIS64 -m64 -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -D_LARGEFILE_SOURCE -O2 -DX64
COPTIONSZlib_solaris_gnu64i=-DHAVE_STDARG_H -DHAVE_UNISTD_H
COPTIONSld_solaris_gnu64i=-m64
FOPTIONS_solaris_gnu64i=-m64
FOPTIONSld_solaris_gnu64i=-m64
CURSESLIB_solaris_gnu64i=-lcurses
SYSLIBSexe_solaris_gnu64i=-lm
SYSLIBSshr_solaris_gnu64i=

CURSES_solaris_x86=yes
CC_solaris_x86=cc
LD_solaris_x86=ld
LDOPTIONS_solaris_x86=-G
PIC_solaris_x86=-Kpic
COPTIONS_solaris_x86=-DSOLARIS -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -D_LARGEFILE_SOURCE -O
COPTIONSZlib_solaris_x86=-DHAVE_STDARG_H -DHAVE_UNISTD_H
COPTIONSld_solaris_x86=
FOPTIONS_solaris_x86=
FOPTIONSld_solaris_x86=
CURSESLIB_solaris_x86=-lcurses
SYSLIBSexe_solaris_x86=-lm
SYSLIBSshr_solaris_x86=

CURSES_solaris_x64=yes
CC_solaris_x64=cc
LD_solaris_x64=ld -64
LDOPTIONS_solaris_x64=-G
PIC_solaris_x64=-Kpic
COPTIONS_solaris_x64=-DSOLARIS -DSOLARIS64 -m64 -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -D_LARGEFILE_SOURCE -DX64 -O
COPTIONSZlib_solaris_x64=-DHAVE_STDARG_H -DHAVE_UNISTD_H
COPTIONSld_solaris_x64=-m64
FOPTIONS_solaris_x64=-m64
FOPTIONSld_solaris_x64=-m64
CURSESLIB_solaris_x64=-lcurses
SYSLIBSexe_solaris_x64=-lm
SYSLIBSshr_solaris_x64=

#------------------------------------------------------------------------------
# Macros for MacosX.
#------------------------------------------------------------------------------

SHARED_macosx=yes
FOPTIONS_macosx=
FOPTIONSld_macosx=
SHAREDEXT_macosx=dylib
AROPTIONS_macosx=rc
RANLIB_macosx=yes
FC_macosx=gfortran
EXEEXT_macosx=
MACOSxlink=
MACOSxFopt=
WHICHOS=$(shell uname)
ifeq ("$(WHICHOS)","Darwin")
  KERNELVERSION=$(shell uname -r | cut -f1 -d.)
  ifeq ("$(KERNELVERSION)","9")
    MACOSxlink=-L/usr/lib/gcc/i686-apple-darwin9/4.2.1 -lm -lc -lgcc
    MACOSxmin=-macosx_version_min 10.4
  else
    MACOSxlink=-lc -lm
    MACOSxFopt=-m64
    MACOSxmin=-macosx_version_min 10.6
  endif
endif

CURSES_macosx_gnu32=yes
CC_macosx_gnu32=gcc
LD_macosx_gnu32=libtool $(MACOSxmin)
LDOPTIONS_macosx_gnu32=-dynamic
PIC_macosx_gnu32=
COPTIONS_macosx_gnu32=-m32 -D__MACH__ -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -D_LARGEFILE_SOURCE -O2
COPTIONSZlib_macosx_gnu32=-DHAVE_STDARG_H -DHAVE_UNISTD_H
COPTIONSld_macosx_gnu32=
FOPTIONS_macosx_gnu32=-w -m32
FOPTIONSld_macosx_gnu32=-m32
CURSESLIB_macosx_gnu32=-lcurses
SYSLIBSexe_macosx_gnu32=$(MACOSxlink) -m32
SYSLIBSshr_macosx_gnu32=$(MACOSxlink) -m32

CURSES_macosx_gnu=yes
CC_macosx_gnu=gcc
LD_macosx_gnu=libtool $(MACOSxmin)
LDOPTIONS_macosx_gnu=-dynamic
PIC_macosx_gnu=
COPTIONS_macosx_gnu=-m64 -D__MACH__ -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -D_LARGEFILE_SOURCE -O2
COPTIONSZlib_macosx_gnu=-DHAVE_STDARG_H -DHAVE_UNISTD_H
COPTIONSld_macosx_gnu=
FOPTIONS_macosx_gnu=-w $(MACOSxFopt)
FOPTIONSld_macosx_gnu=$(MACOSxFopt)
CURSESLIB_macosx_gnu=-lcurses
SYSLIBSexe_macosx_gnu=$(MACOSxlink)
SYSLIBSshr_macosx_gnu=$(MACOSxlink)

CURSES_macosx_ppc=yes
CC_macosx_ppc=gcc
LD_macosx_ppc=libtool -syslibroot /Developer/SDKs/MacOSX10.5.sdk -macosx_version_min 10.4 -arch_only ppc
LDOPTIONS_macosx_ppc=-dynamic
PIC_macosx_ppc=
COPTIONS_macosx_ppc=-isysroot/Developer/SDKs/MacOSX10.5.sdk -arch ppc -D__ppc__ -D__MACH__ -D__APPLE__ -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -D_LARGEFILE_SOURCE -O2
COPTIONSZlib_macosx_ppc=-DHAVE_STDARG_H -DHAVE_UNISTD_H
COPTIONSld_macosx_ppc=-arch ppc -isysroot/Developer/SDKs/MacOSX10.5.sdk
FOPTIONS_macosx_ppc=-w
FOPTIONSld_macosx_ppc=
CURSESLIB_macosx_ppc=-lcurses
SYSLIBSexe_macosx_ppc=-L/Developer/SDKs/MacOSX10.5.sdk/usr/lib -L/usr/lib/gcc/powerpc-apple-darwin9/4.0.1 -lm -lc -lgcc
SYSLIBSshr_macosx_ppc=-L/Developer/SDKs/MacOSX10.5.sdk/usr/lib -L/usr/lib/gcc/powerpc-apple-darwin9/4.0.1 -lm -lc -lgcc

CURSES_macosx_ppc64=yes
CC_macosx_ppc64=gcc
LD_macosx_ppc64=libtool -syslibroot /Developer/SDKs/MacOSX10.5.sdk -macosx_version_min 10.4 -arch_only ppc64
LDOPTIONS_macosx_ppc64=-dynamic
PIC_macosx_ppc64=
COPTIONS_macosx_ppc64=-isysroot/Developer/SDKs/MacOSX10.5.sdk -arch ppc64 -m64 -DMACOSX64 -D__ppc__ -D__MACH__ -D__APPLE__ -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -D_LARGEFILE_SOURCE -O2
COPTIONSZlib_macosx_ppc64=-DHAVE_STDARG_H -DHAVE_UNISTD_H
COPTIONSld_macosx_ppc64=-arch ppc64 -isysroot/Developer/SDKs/MacOSX10.5.sdk
FOPTIONS_macosx_ppc64=-w
FOPTIONSld_macosx_ppc64=
CURSESLIB_macosx_ppc64=-lcurses
SYSLIBSexe_macosx_ppc64=-L/Developer/SDKs/MacOSX10.5.sdk/usr/lib -L/usr/lib/gcc/powerpc-apple-darwin9/4.0.1/ppc64 -lm -lc -lgcc
SYSLIBSshr_macosx_ppc64=-L/Developer/SDKs/MacOSX10.5.sdk/usr/lib -L/usr/lib/gcc/powerpc-apple-darwin9/4.0.1/ppc64 -lm -lc -lgcc

CURSES_macosx_i386=yes
CC_macosx_i386=clang
#LD_macosx_i386=libtool $(MACOSxmin) -arch_only i386
LD_macosx_i386=libtool $(MACOSxmin)
LDOPTIONS_macosx_i386=-dynamic
PIC_macosx_i386=
COPTIONS_macosx_i386=-mmacosx-version-min=10.4 -arch i386 -arch x86_64 -Di386 -D__MACH__ -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -D_LARGEFILE_SOURCE -O2
#COPTIONS_macosx_i386=-mmacosx-version-min=10.4 -m32 -Di386 -D__MACH__ -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -D_LARGEFILE_SOURCE -O2
COPTIONSZlib_macosx_i386=-DHAVE_STDARG_H -DHAVE_UNISTD_H
COPTIONSld_macosx_i386=-mmacosx-version-min=10.4 -arch i386 -arch x86_64
#COPTIONSld_macosx_i386=-mmacosx-version-min=10.4 -m32
FOPTIONS_macosx_i386=-w -m32
FOPTIONSld_macosx_i386=-m32
CURSESLIB_macosx_i386=-lcurses
# SYSLIBSexe_macosx_i386=-L/usr/lib/gcc/i686-apple-darwin10/4.2.1 -lm -lc -lgcc 
SYSLIBSexe_macosx_i386=-lc -lm
# SYSLIBSshr_macosx_i386=-L/usr/lib/gcc/i686-apple-darwin10/4.2.1 -lm -lc -lgcc
SYSLIBSshr_macosx_i386=-lc -lm

CURSES_macosx_x86_64=yes
CC_macosx_x86_64=clang
#LD_macosx_x86_64=libtool $(MACOSxmin) -arch_only x86_64 
LD_macosx_x86_64=libtool $(MACOSxmin)
LDOPTIONS_macosx_x86_64=-dynamic
PIC_macosx_x86_64=
#COPTIONS_macosx_x86_64=-mmacosx-version-min=10.4 -m64 -Di386 -D__MACH__ -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -D_LARGEFILE_SOURCE -O2
COPTIONS_macosx_x86_64=-mmacosx-version-min=10.4 -arch x86_64 -arch i386 -Di386 -D__MACH__ -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -D_LARGEFILE_SOURCE -O2
COPTIONSZlib_macosx_x86_64=-DHAVE_STDARG_H -DHAVE_UNISTD_H
COPTIONSld_macosx_x86_64=-mmacosx-version-min=10.4 -arch x86_64 -arch i386  
FOPTIONS_macosx_x86_64=-w -m64
FOPTIONSld_macosx_x86_64=-m64
CURSESLIB_macosx_x86_64=-lcurses
SYSLIBSexe_macosx_x86_64=-lc -lm
SYSLIBSshr_macosx_x86_64=-lc -lm

#------------------------------------------------------------------------------
# Macros for OSF (Digital UNIX).
#------------------------------------------------------------------------------

SHARED_osf=yes
FOPTIONS_osf=-warn declarations -warn nounused
FOPTIONSld_osf=
SHAREDEXT_osf=so
AROPTIONS_osf=rc
RANLIB_osf=yes
FC_osf=f77
EXEEXT_osf=

CURSES_osf_dec=yes
CC_osf_dec=cc
LD_osf_dec=ld
LDOPTIONS_osf_dec=-shared -expect_unresolved '*'
PIC_osf_dec=
COPTIONS_osf_dec=-std1 -Dunix -ieee_with_inexact -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -D_LARGEFILE_SOURCE
COPTIONSZlib_osf_dec=-DHAVE_STDARG_H -DHAVE_UNISTD_H
COPTIONSld_osf_dec=
FOPTIONS_osf_dec=
FOPTIONSld_osf_dec=
CURSESLIB_osf_dec=-lcurses
SYSLIBSexe_osf_dec=-lm -lc
SYSLIBSshr_osf_dec=

CURSES_osf_gnu=yes
CC_osf_gnu=gcc
LD_osf_gnu=gcc
LDOPTIONS_osf_gnu=-shared -expect_unresolved '*'
PIC_osf_gnu=-fpic
COPTIONS_osf_gnu=-D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -D_LARGEFILE_SOURCE -O2
COPTIONSZlib_osf_gnu=-DHAVE_STDARG_H -DHAVE_UNISTD_H
COPTIONSld_osf_gnu=
FOPTIONS_osf_gnu=
FOPTIONSld_osf_gnu=
CURSESLIB_osf_gnu=-lcurses
SYSLIBSexe_osf_gnu=-lm -lc
SYSLIBSshr_osf_gnu=

#------------------------------------------------------------------------------
# Macros for IRIX 6.x.
#------------------------------------------------------------------------------

SHARED_irix6=yes
FOPTIONS_irix6=-u
FOPTIONSld_irix6=
SHAREDEXT_irix6=so
AROPTIONS_irix6=rc
RANLIB_irix6=no
FC_irix6=f77

CURSES_irix6_sgin32=yes
CC_irix6_sgin32=cc
LD_irix6_sgin32=ld
LDOPTIONS_irix6_sgi32=-shared
PIC_irix6_sgin32=-KPIC
COPTIONS_irix6_sgin32=-n32 -woffall -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -D_LARGEFILE_SOURCE
COPTIONSZlib_irix6_sgin32=-DHAVE_STDARG_H -DHAVE_UNISTD_H
COPTIONSld_irix6_sgin32=-n32 -L/usr/lib32
FOPTIONS_irix6_sgin32=-n32
FOPTIONSld_irix6_sgin32=-n32 -L/usr/lib32
CURSESLIB_irix6_sgin32=-lcurses
SYSLIBSexe_irix6_sgin32=-lm -lc
SYSLIBSshr_irix6_sgin32=-lm -lc

CURSES_irix6_sgi64=yes
CC_irix6_sgi64=cc
LD_irix6_sgi64=ld
LDOPTIONS_irix6_sgi64=-shared
PIC_irix6_sgi64=-KPIC
COPTIONS_irix6_sgi64=-64 -woffall -DIRIX64bit -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -D_LARGEFILE_SOURCE
COPTIONSZlib_irix6_sgin64=-DHAVE_STDARG_H -DHAVE_UNISTD_H
COPTIONSld_irix6_sgi64=-64 -DIRIX64bit -L/usr/lib64
FOPTIONS_irix6_sgi64=-64
FOPTIONSld_irix6_sgi64=-64 -L/usr/lib64
CURSESLIB_irix6_sgi64=-lcurses
SYSLIBSexe_irix6_sgi64=-lm -lc
SYSLIBSshr_irix6_sgi64=-lm -lc

CURSES_irix6_gnu=yes
CC_irix6_gnu=gcc
LD_irix6_gnu=gcc
LDOPTIONS_irix6_gnu=-shared
PIC_irix6_gnu=-fpic
COPTIONS_irix6_gnu=-D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -D_LARGEFILE_SOURCE -O2
COPTIONSZlib_irix6_gnu=-DHAVE_STDARG_H -DHAVE_UNISTD_H
COPTIONSld_irix6_gnu=
FOPTIONS_irix6_gnu=
FOPTIONSld_irix6_gnu=
CURSESLIB_irix6_gnu=-lcurses
SYSLIBSexe_irix6_gnu=-lm -lc
SYSLIBSshr_irix6_gnu=

#------------------------------------------------------------------------------
# Macros for Linux.
#------------------------------------------------------------------------------

SHARED_linux=yes
FOPTIONS_linux=-w
FOPTIONSld_linux=
SHAREDEXT_linux=so
AROPTIONS_linux=rc
RANLIB_linux=yes
FC_linux=gfortran
EXEEXT_linux=

CURSES_linux_gnu=yes
CC_linux_gnu=gcc
LD_linux_gnu=gcc
LDOPTIONS_linux_gnu=-shared
PIC_linux_gnu=-fPIC
COPTIONS_linux_gnu=-I/usr/include/ncurses -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -D_LARGEFILE_SOURCE -DMALLOC_CHECK_=0 -O2
COPTIONSZlib_linux_gnu=-DHAVE_STDARG_H -DHAVE_UNISTD_H
COPTIONSld_linux_gnu=
FOPTIONS_linux_gnu=-fsecond-underscore
FOPTIONSld_linux_gnu=
CURSESLIB_linux_gnu=-lcurses
SYSLIBSexe_linux_gnu=-lm -lc
SYSLIBSshr_linux_gnu=-lm -lc

CURSES_linux_gnu32=yes
CC_linux_gnu32=gcc
LD_linux_gnu32=gcc 
LDOPTIONS_linux_gnu32=-shared -m32
PIC_linux_gnu32=-fPIC
COPTIONS_linux_gnu32=-m32 -I/usr/include/ncurses -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -D_LARGEFILE_SOURCE -DMALLOC_CHECK_=0 -O2
COPTIONSZlib_linux_gnu32=-DHAVE_STDARG_H -DHAVE_UNISTD_H
COPTIONSld_linux_gnu32=-m32
FOPTIONS_linux_gnu32=-m32 -fsecond-underscore
FOPTIONSld_linux_gnu32=-m32
CURSESLIB_linux_gnu32=-lcurses
SYSLIBSexe_linux_gnu32=-lm -lc
SYSLIBSshr_linux_gnu32=-lm -lc

CURSES_linux_gnu64=yes
CC_linux_gnu64=gcc
LD_linux_gnu64=gcc
LDOPTIONS_linux_gnu64=-shared -m64
PIC_linux_gnu64=-fPIC
COPTIONS_linux_gnu64=-m64 -I/usr/include/ncurses -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -D_LARGEFILE_SOURCE -DMALLOC_CHECK_=0 -O2
COPTIONSZlib_linux_gnu64=-DHAVE_STDARG_H -DHAVE_UNISTD_H
COPTIONSld_linux_gnu64=-m64
FOPTIONS_linux_gnu64=-m64 -fsecond-underscore
FOPTIONSld_linux_gnu64=-m64
CURSESLIB_linux_gnu64=-lcurses
SYSLIBSexe_linux_gnu64=-lm -lc
SYSLIBSshr_linux_gnu64=-lm -lc

CURSES_linux_intel=yes
CC_linux_intel=icc
LD_linux_intel=ld
LDOPTIONS_linux_intel=-shared
PIC_linux_intel=-fPIC
COPTIONS_linux_intel=-I/usr/include/ncurses -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -D_LARGEFILE_SOURCE -DMALLOC_CHECK_=0
COPTIONSZlib_linux_intel=-DHAVE_STDARG_H -DHAVE_UNISTD_H
COPTIONSld_linux_intel=
FOPTIONS_linux_intel=
FOPTIONSld_linux_intel=
CURSESLIB_linux_intel=-lcurses
SYSLIBSexe_linux_intel=-lm -lc
SYSLIBSshr_linux_intel=-lm -lc

#------------------------------------------------------------------------------
# Macros for AIX.
#------------------------------------------------------------------------------

SHARED_aix=yes
FOPTIONS_aix=-u -qcharlen=256
FOPTIONSld_aix=-L../lib
SHAREDEXT_aix=o
AROPTIONS_aix=rc
AROPTIONS_aix64=-r -c -X 64
RANLIB_aix=yes
FC_aix=xlf

CURSES_aix_ibm=yes
CC_aix_ibm=cc
LD_aix_ibm=ld
LDOPTIONS_aix_ibm=-bnoentry -bM:SRE -bE:libcdf.exp
PIC_aix_ibm=
COPTIONS_aix_ibm=-DIBMRS -DAIX -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -D_LARGEFILE_SOURCE 
COPTIONSZlib_aix_ibm=-DHAVE_STDARG_H -DHAVE_UNISTD_H
COPTIONSld_aix_ibm=-L../lib
FOPTIONS_aix_ibm=
FOPTIONSld_aix_ibm=-L../lib
CURSESLIB_aix_ibm=-lcurses
SYSLIBSexe_aix_ibm=-lm -lc
SYSLIBSshr_aix_ibm=-lm -lc

CURSES_aix_ibm64=yes
CC_aix_ibm64=cc
LD_aix_ibm64=ld
LDOPTIONS_aix_ibm64=-b64 -bnoentry -bM:SRE -bE:libcdf.exp
PIC_aix_ibm64=
COPTIONS_aix_ibm64=-q64 -DIBMRS -DAIX -DAIX64 -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -D_LARGEFILE_SOURCE
COPTIONSZlib_aix_ibm64=-DHAVE_STDARG_H -DHAVE_UNISTD_H
COPTIONSld_aix_ibm64=-q64 -L../lib
FOPTIONS_aix_ibm64=
FOPTIONSld_aix_ibm64=-L../lib
CURSESLIB_aix_ibm64=-lcurses
SYSLIBSexe_aix_ibm64=-lm -lc
SYSLIBSshr_aix_ibm64=-lm -lc

CURSES_aix_gnu=yes
CC_aix_gnu=gcc
LD_aix_gnu=gcc
LDOPTIONS_aix_gnu=-shared
PIC_aix_gnu=-fpic
COPTIONS_aix_gnu=-DIBMRS -DAIX -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -D_LARGEFILE_SOURCE -O2
COPTIONSZlib_aix_gnu=-DHAVE_STDARG_H -DHAVE_UNISTD_H
COPTIONSld_aix_gnu=-L../lib
FOPTIONS_aix_gnu=
FOPTIONSld_aix_gnu=-L../lib
CURSESLIB_aix_gnu=-lcurses
SYSLIBSexe_aix_gnu=-lm -lc
SYSLIBSshr_aix_gnu=-lm -lc

CURSES_aix_gnu64=yes
CC_aix_gnu64=gcc
LD_aix_gnu64=gcc
LDOPTIONS_aix_gnu64=-shared -maix64
PIC_aix_gnu64=-fpic
COPTIONS_aix_gnu64=-maix64 -DIBMRS -DAIX -DAIX64 -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -D_LARGEFILE_SOURCE -O2
COPTIONSZlib_aix_gnu64=-DHAVE_STDARG_H -DHAVE_UNISTD_H
COPTIONSld_aix_gnu64=-maix64 -L../lib
FOPTIONS_aix_gnu64=
FOPTIONSld_aix_gnu64=-L../lib
CURSESLIB_aix_gnu64=-lcurses
SYSLIBSexe_aix_gnu64=-lm -lc
SYSLIBSshr_aix_gnu64=-lm -lc

#------------------------------------------------------------------------------
# Macros for HP-UX.
# +DD64 for 64-bit mode (--LP64__ turned on)
# none  for 32-bit 
#------------------------------------------------------------------------------

SHARED_hpux=yes
FOPTIONS_hpux=-u
FOPTIONSld_hpux=
SHAREDEXT_hpux=sl
AROPTIONS_hpux=rc
RANLIB_hpux=no
FC_hpux=f77

CURSES_hpux_std=yes
CC_hpux_std=cc
LD_hpux_std=ld
LDOPTIONS_hpux_std=-b
PIC_hpux_std=
COPTIONS_hpux_std=-DHP -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -D_LARGEFILE_SOURCE 
COPTIONSZlib_hpux_std=-DHAVE_STDARG_H -DHAVE_UNISTD_H
COPTIONSld_hpux_std=
FOPTIONS_hpux_std=
FOPTIONSld_hpux_std=
CURSESLIB_hpux_std=-lcurses
SYSLIBSexe_hpux_std=-lm -lc
SYSLIBSshr_hpux_std=

CURSES_hpux_opt=no
CC_hpux_opt=cc
LD_hpux_opt=ld
LDOPTIONS_hpux_opt=-b
PIC_hpux_opt=+z
COPTIONS_hpux_opt=-w -DHP -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -D_LARGEFILE_SOURCE 
COPTIONSZlib_hpux_opt=-DHAVE_STDARG_H -DHAVE_UNISTD_H
COPTIONSld_hpux_opt=
FOPTIONS_hpux_opt=
FOPTIONSld_hpux_opt=
CURSESLIB_hpux_opt=-lcurses
SYSLIBSexe_hpux_opt=-lm -lc
SYSLIBSshr_hpux_opt=

CURSES_hpux_opt64=no
CC_hpux_opt64=cc
LD_hpux_opt64=ld
LDOPTIONS_hpux_opt64=-b
PIC_hpux_opt64=+z
COPTIONS_hpux_opt64=-w -DHP -DHP64 +DD64 -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -D_LARGEFILE_SOURCE
COPTIONSZlib_hpux_opt64=-DHAVE_STDARG_H -DHAVE_UNISTD_H
COPTIONSld_hpux_opt64=+DD64
FOPTIONS_hpux_opt64=
FOPTIONSld_hpux_opt64=
CURSESLIB_hpux_opt64=-lcurses
SYSLIBSexe_hpux_opt64=-lm -lc
SYSLIBSshr_hpux_opt64=

CURSES_hpux_posix=yes
CC_hpux_posix=c89
LD_hpux_posix=ld
LDOPTIONS_hpux_posix=-b
PIC_hpux_posix=+z
COPTIONS_hpux_posix=-DHPUXposix -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -D_LARGEFILE_SOURCE 
COPTIONSZlib_hpux_posix=-DHAVE_STDARG_H -DHAVE_UNISTD_H
COPTIONSld_hpux_posix=
FOPTIONS_hpux_posix=
FOPTIONSld_hpux_posix=
CURSESLIB_hpux_posix=-lcurses
SYSLIBSexe_hpux_posix=-lm -lc
SYSLIBSshr_hpux_posix=

CURSES_hpux_gnu=no
CC_hpux_gnu=gcc
LD_hpux_gnu=gcc
LDOPTIONS_hpux_gnu=-shared
PIC_hpux_gnu=-fpic
COPTIONS_hpux_gnu=-DHP -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -D_LARGEFILE_SOURCE -O2
COPTIONSZlib_hpux_gnu=-DHAVE_STDARG_H -DHAVE_UNISTD_H
COPTIONSld_hpux_gnu=
FOPTIONS_hpux_gnu=
FOPTIONSld_hpux_gnu=
CURSESLIB_hpux_gnu=-lcurses
SYSLIBSexe_hpux_gnu=-lm -lc
SYSLIBSshr_hpux_gnu=

CURSES_hpux_gnu64=no
CC_hpux_gnu64=gcc
LD_hpux_gnu64=gcc
LDOPTIONS_hpux_gnu64=-shared -mlp64
PIC_hpux_gnu64=-fpic
COPTIONS_hpux_gnu64=-DHP -DHP64 -mlp64 -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -D_LARGEFILE_SOURCE -O2
COPTIONSZlib_hpux_gnu64=-DHAVE_STDARG_H -DHAVE_UNISTD_H
COPTIONSld_hpux_gnu64=-mlp64
FOPTIONS_hpux_gnu64=
FOPTIONSld_hpux_gnu64=
CURSESLIB_hpux_gnu64=-lcurses
SYSLIBSexe_hpux_gnu64=-lm -lc
SYSLIBSshr_hpux_gnu64=

#------------------------------------------------------------------------------
# Macros for Cygwin.
#------------------------------------------------------------------------------

SHARED_cygwin=yes
FOPTIONS_cygwin=-w
FOPTIONSld_cygwin=
SHAREDEXT_cygwin=dll
AROPTIONS_cygwin=rc
RANLIB_cygwin=yes
FC_cygwin=gfortran
EXEEXT_cygwin=.exe

CURSES_cygwin_gnu=yes
CC_cygwin_gnu=gcc
LD_cygwin_gnu=gcc
LDOPTIONS_cygwin_gnu=-shared
PIC_cygwin_gnu=
COPTIONS_cygwin_gnu=-I/usr/include/ncurses -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -D_LARGEFILE_SOURCE -O2
COPTIONSZlib_cygwin_gnu=-DHAVE_STDARG_H -DHAVE_UNISTD_H
COPTIONSld_cygwin_gnu=
FOPTIONS_cygwin_gnu=-fsecond-underscore
FOPTIONSld_cygwin_gnu=
CURSESLIB_cygwin_gnu=-lncurses
SYSLIBSexe_cygwin_gnu=-lgcc
SYSLIBSshr_cygwin_gnu=-lgcc

#------------------------------------------------------------------------------
# Macros for MinGW.
#------------------------------------------------------------------------------

SHARED_mingw=yes
FOPTIONS_mingw=-w
FOPTIONSld_mingw=
SHAREDEXT_mingw=dll
AROPTIONS_mingw=rc
RANLIB_mingw=yes
FC_mingw=gfortran
EXEEXT_mingw=.exe

CURSES_mingw_gnu=yes
CC_mingw_gnu=gcc
LD_mingw_gnu=gcc
LDOPTIONS_mingw_gnu=-shared
PIC_mingw_gnu=
COPTIONS_mingw_gnu=-D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -D_LARGEFILE_SOURCE -O2 -posix
COPTIONSZlib_mingw_gnu=-DHAVE_STDARG_H -DHAVE_UNISTD_H
COPTIONSld_mingw_gnu=
FOPTIONS_mingw_gnu=-fsecond-underscore
FOPTIONSld_mingw_gnu=
CURSESLIB_mingw_gnu=-lpdcurses
SYSLIBSexe_mingw_gnu=-lgcc
SYSLIBSshr_mingw_gnu=-lgcc

#------------------------------------------------------------------------------
# Macros for FreeBSD.
#------------------------------------------------------------------------------

SHARED_freebsd=yes
FOPTIONS_freebsd=-w 
FOPTIONSld_freebsd=
SHAREDEXT_freebsd=so
AROPTIONS_freebsd=rc
RANLIB_freebsd=yes
FC_freebsd=f77

CURSES_freebsd_gnu=yes
CC_freebsd_gnu=gcc
LD_freebsd_gnu=gcc
LDOPTIONS_freebsd_gnu=-shared
PIC_freebsd_gnu=-fPIC
COPTIONS_freebsd_gnu=-D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -D_LARGEFILE_SOURCE -DMALLOC_CHECK_=0
COPTIONSZlib_freebsd_gnu=-DHAVE_STDARG_H -DHAVE_UNISTD_H
COPTIONSld_freebsd_gnu=
FOPTIONS_freebsd_gnu=
FOPTIONSld_freebsd_gnu=
CURSESLIB_freebsd_gnu=-lncurses
SYSLIBSexe_freebsd_gnu=-lm -lc
SYSLIBSshr_freebsd_gnu=-lm -lc

#------------------------------------------------------------------------------
# Miscellaneous Macros.
#------------------------------------------------------------------------------

AND.yes.yes=yes
AND.yes.no=no
AND.no.yes=no
AND.no.no=no

SUPPORTED.yes=supported
SUPPORTED.no=not supported

#------------------------------------------------------------------------------
# Compile/link entire distribution.
#------------------------------------------------------------------------------

all.help:
	@if `type -p more > /dev/null 2>&1` ; then \
	   more Help.all ; \
	 else \
	   less Help.all ; \
	 fi

all: all.$(OS).$(ENV)

all..:
	echo "Missing OS and ENV variables."

all.linux.gnu: all.build
all.linux.gnu32: all.build
all.linux.gnu64: all.build
all.linux.intel: note1.intel all.build
all.solaris.sparc: note1.noCC all.build
all.solaris.sparc64: note1.noCC all.build
all.solaris.gnu: all.build
all.solaris.gnu64: all.build
all.solaris.gnu64i: all.build
all.solaris.x86: all.build
all.solaris.x64: all.build
all.osf.dec: all.build
all.osf.gnu: all.build
all.cygwin.gnu: all.build
all.mingw.gnu: all.build
all.freebsd.gnu: all.build
all.macosx.gnu32: all.build
all.macosx.gnu: all.build
all.macosx.ppc: all.build
all.macosx.ppc64: all.build
all.macosx.i386: all.build
all.macosx.x86_64: all.build

all.build:
	@cd $(LIBsrcZlibDIR); $(MAKE) MAKE=$(MAKE) \
"PIC=$(PIC_$(OS)_$(ENV))" \
"CCx=$(CC_$(OS)_$(ENV))" \
"MAKE=$(MAKE)" \
"COPTIONS=$(UCOPTIONS) $(COPTIONS_$(OS)_$(ENV)) $(COPTIONSZlib_$(OS)_$(ENV))" \
all
	@cd $(LIBsrcDIR); $(MAKE) MAKE=$(MAKE) \
"SHARED=$(AND.$(SHARED).$(SHARED_$(OS)))" \
"PIC=$(PIC_$(OS)_$(ENV))" \
"CCx=$(CC_$(OS)_$(ENV))" \
"MAKE=$(MAKE)" \
"LDx=$(LD_$(OS)_$(ENV))" \
"COPTIONS=$(UCOPTIONS) $(COPTIONS_$(OS)_$(ENV))" \
"SYSLIBS=$(SYSLIBSshr_$(OS)_$(ENV))" \
"SHAREDEXT=$(SHAREDEXT_$(OS))" \
"LDOPTIONS=$(LDOPTIONS_$(OS)_$(ENV))" \
"AROPTIONS=$(AROPTIONS_$(OS))" \
"RANLIB=$(RANLIB_$(OS))" \
"VERSION=$(VERSION)" \
all
	@cd $(TOOLSsrcDIR); $(MAKE) MAKE=$(MAKE) \
"CURSES=$(AND.$(CURSES).$(CURSES_$(OS)_$(ENV)))" \
"CCx=$(CC_$(OS)_$(ENV))" \
"MAKE=$(MAKE)" \
"COPTIONS=$(COPTIONS_$(OS)_$(ENV))" \
"COPTIONSld=$(COPTIONSld_$(OS)_$(ENV))" \
"SYSLIBS=$(SYSLIBSexe_$(OS)_$(ENV))" \
"CURSESLIB=$(CURSESLIB_$(OS)_$(ENV))" \
all
	@cd $(TESTSsrcDIR); $(MAKE) MAKE=$(MAKE) \
"FORTRAN=$(FORTRAN)" \
"FCx=$(FC_$(OS))" \
"MAKE=$(MAKE)" \
"CCx=$(CC_$(OS)_$(ENV))" \
"SHARED=$(AND.$(SHARED).$(SHARED_$(OS)))" \
"FOPTIONS=$(FOPTIONS_$(OS)) $(FOPTIONS_$(OS)_$(ENV))" \
"FOPTIONSld=$(FOPTIONSld_$(OS)) $(FOPTIONSld_$(OS)_$(ENV))" \
"COPTIONS=$(COPTIONS_$(OS)_$(ENV))" \
"COPTIONSld=$(COPTIONSld_$(OS)_$(ENV))" \
"SYSLIBS=$(SYSLIBSexe_$(OS)_$(ENV))" \
"SHAREDEXT=$(SHAREDEXT_$(OS))" \
all

#------------------------------------------------------------------------------
# Test distribution.
#------------------------------------------------------------------------------

test.help:
	@if `type -p more > /dev/null 2>&1` ; then \
	   more Help.test ; \
	 else \
	   less Help.test ; \
	 fi

test:
	@cd $(TESTSsrcDIR); $(MAKE) MAKE=$(MAKE) test 

#------------------------------------------------------------------------------
# Install files.
#------------------------------------------------------------------------------

install.help:
	@if `type -p more > /dev/null 2>&1` ; then \
	   more Help.install ; \
	 else \
	   less Help.install ; \
	 fi

install: install.$(PART)

install.all: install.definitions install.include install.lib \
	     install.tools install.help_ notify.user copy.leapseconds

install.definitions: create.bin copy.definitions change.definitions
install.include: create.include copy.include
install.tools: create.bin copy.tools
install.help_: create.help copy.help

install.lib: create.lib copy.lib.a
	@if [ -f $(LIBsrcDIR)/libcdf.so ] ; then \
	   $(MAKE) MAKE=$(MAKE) "INSTALLDIR=$(INSTALLDIR)" install.lib.so ; \
	 else \
	   $(NULL) ; \
	 fi
	@if [ -f $(LIBsrcDIR)/libcdf.sl ] ; then \
	   $(MAKE) MAKE=$(MAKE) "INSTALLDIR=$(INSTALLDIR)" install.lib.sl ; \
	 else \
	   $(NULL) ; \
	 fi
	@if [ -f $(LIBsrcDIR)/libcdf.dylib ] ; then \
	   $(MAKE) MAKE=$(MAKE) "INSTALLDIR=$(INSTALLDIR)" install.lib.dylib ; \
	 else \
	   $(NULL) ; \
	 fi
	@if [ -f $(LIBsrcDIR)/libcdf.o ] ; then \
	   $(MAKE) MAKE=$(MAKE) "INSTALLDIR=$(INSTALLDIR)" install.lib.o ; \
	 else \
	   $(NULL) ; \
	 fi
	@if [ -f $(LIBsrcDIR)/libcdf.dll ] ; then \
	   $(MAKE) MAKE=$(MAKE) "INSTALLDIR=$(INSTALLDIR)" install.lib.dll ; \
	 else \
	   $(NULL) ; \
	 fi

install.lib.so: create.lib copy.lib.so
install.lib.sl: create.lib copy.lib.sl
install.lib.dylib: create.lib create.maclib copy.lib.dylib
install.lib.o: create.lib copy.lib.o
install.lib.dll: create.lib copy.lib.dll

create.include:
	@if [ ! -d $(INSTALLDIR)/include ] ; then \
	   $(MAKE) MAKE=$(MAKE) "TARGET=$(INSTALLDIR)/include" create.dir ; \
	 else \
	   $(NULL) ; \
	 fi

create.bin:
	@if [ ! -d $(INSTALLDIR)/bin ] ; then \
	   $(MAKE) MAKE=$(MAKE) "TARGET=$(INSTALLDIR)/bin" create.dir ; \
	 else \
	   $(NULL) ; \
	 fi

create.lib:
	@if [ ! -d $(INSTALLDIR)/lib ] ; then \
	   $(MAKE) MAKE=$(MAKE) "TARGET=$(INSTALLDIR)/lib" create.dir ; \
	 else \
	   $(NULL) ; \
	 fi

create.maclib:
	@if [ $(MACVERSION) -gt 14 ] ; then \
	  if [ ! -d $(MACLIB) ] ; then \
	     $(MAKE) MAKE=$(MAKE) "TARGET=$(MACLIB)" create.dir ; \
	  else \
	     $(NULL) ; \
	  fi \
	fi

create.help:
	@if [ ! -d $(INSTALLDIR)/lib ] ; then \
	   $(MAKE) MAKE=$(MAKE) "TARGET=$(INSTALLDIR)/lib" create.dir ; \
	 else \
	   $(NULL) ; \
	 fi
	@if [ ! -d $(INSTALLDIR)/lib/cdf ] ; then \
	   $(MAKE) MAKE=$(MAKE) "TARGET=$(INSTALLDIR)/lib/cdf" create.dir ; \
	 else \
	   $(NULL) ; \
	 fi
	@if [ ! -d $(INSTALLDIR)/lib/cdf/help ] ; then \
	   $(MAKE) MAKE=$(MAKE) "TARGET=$(INSTALLDIR)/lib/cdf/help" create.dir ; \
	 else \
	   $(NULL) ; \
	 fi

change.definitions:
	@if [ $(INSTALLDIR) = "." ] ; then \
	   $(SHELL) ./modify_definition_files.sh `pwd` ; \
	else \
	   $(SHELL) ./modify_definition_files.sh $(INSTALLDIR) ; \
	fi

copy.leapseconds:
	@if [ $(INSTALLDIR) != "." ] ; then \
	      cp ./CDFLeapSeconds.txt $(INSTALLDIR) ; \
	fi

copy.definitions:
	@echo cp $(DEFSsrcDIR)/definitions.C $(INSTALLDIR)/bin
	      cp $(DEFSsrcDIR)/definitions.C $(INSTALLDIR)/bin
	@echo cp $(DEFSsrcDIR)/definitions.K $(INSTALLDIR)/bin
	      cp $(DEFSsrcDIR)/definitions.K $(INSTALLDIR)/bin
	@echo cp $(DEFSsrcDIR)/definitions.B $(INSTALLDIR)/bin
	      cp $(DEFSsrcDIR)/definitions.B $(INSTALLDIR)/bin

copy.include:
	@echo cp $(INCsrcDIR)/cdf.h $(INSTALLDIR)/include
	      cp $(INCsrcDIR)/cdf.h $(INSTALLDIR)/include
	@echo cp $(INCsrcDIR)/cdf.inc $(INSTALLDIR)/include
	      cp $(INCsrcDIR)/cdf.inc $(INSTALLDIR)/include
	@echo cp $(INCsrcDIR)/cdflib.h $(INSTALLDIR)/include
	      cp $(INCsrcDIR)/cdflib.h $(INSTALLDIR)/include
	@echo cp $(INCsrcDIR)/cdflib64.h $(INSTALLDIR)/include
	      cp $(INCsrcDIR)/cdflib64.h $(INSTALLDIR)/include
	@echo cp $(INCsrcDIR)/cdfdist.h $(INSTALLDIR)/include
	      cp $(INCsrcDIR)/cdfdist.h $(INSTALLDIR)/include
	@echo cp $(INCsrcDIR)/cdfconfig.h $(INSTALLDIR)/include
	      cp $(INCsrcDIR)/cdfconfig.h $(INSTALLDIR)/include

copy.lib.so:
	@echo cp $(LIBsrcDIR)/libcdf.so $(INSTALLDIR)/lib
	      cp $(LIBsrcDIR)/libcdf.so $(INSTALLDIR)/lib/libcdf.$(VERSION).so
	      rm -f $(INSTALLDIR)/lib/libcdf.so
	      cd $(INSTALLDIR)/lib && ln -s libcdf.$(VERSION).so libcdf.so

copy.lib.sl:
	@echo cp $(LIBsrcDIR)/libcdf.sl $(INSTALLDIR)/lib
	      cp $(LIBsrcDIR)/libcdf.sl $(INSTALLDIR)/lib
	      cp $(LIBsrcDIR)/libcdf.sl $(INSTALLDIR)/lib/libcdf.$(VERSION).sl
	      rm -f $(INSTALLDIR)/lib/libcdf.sl
	      cd $(INSTALLDIR)/lib && ln -s libcdf.$(VERSION).sl libcdf.sl

copy.lib.dylib:
	@echo cp $(LIBsrcDIR)/libcdf.dylib $(INSTALLDIR)/lib
	      cp $(LIBsrcDIR)/libcdf.dylib $(INSTALLDIR)/lib
	      cp $(LIBsrcDIR)/libcdf.dylib $(INSTALLDIR)/lib/libcdf.$(VERSION).dylib
	      rm -f $(INSTALLDIR)/lib/libcdf.dylib
	      cd $(INSTALLDIR)/lib && ln -s libcdf.$(VERSION).dylib libcdf.dylib
	@if [ $(MACVERSION) -gt 14 ] ; then \
	  cp $(LIBsrcDIR)/libcdf.dylib $(MACLIB)/libcdf.$(VERSION).dylib ; \
	  rm -f $(MACLIB)/libcdf.dylib ; \
	  cd $(MACLIB) && ln -s libcdf.$(VERSION).dylib libcdf.dylib ; \
	fi
	

copy.lib.o:
	@echo cp $(LIBsrcDIR)/libcdf.o $(INSTALLDIR)/lib
	      cp $(LIBsrcDIR)/libcdf.o $(INSTALLDIR)/lib

copy.lib.dll:
	@echo cp $(LIBsrcDIR)/libcdf.dll $(INSTALLDIR)/lib
	      cp $(LIBsrcDIR)/libcdf.dll $(INSTALLDIR)/lib
	      cp $(LIBsrcDIR)/libcdf.dll $(INSTALLDIR)/lib/libcdf.$(VERSION).dll
	      rm -f $(INSTALLDIR)/lib/libcdf.dll
	      cd $(INSTALLDIR)/lib && ln -s libcdf.$(VERSION).dll libcdf.dll

copy.lib.a:
	@echo cp -p $(LIBsrcDIR)/libcdf.a $(INSTALLDIR)/lib
	      cp -p $(LIBsrcDIR)/libcdf.a $(INSTALLDIR)/lib

copy.tools:
	@if [ -f $(TOOLSsrcDIR)/cdfedit.exe ] ; then \
	   $(MAKE) MAKE=$(MAKE) \
		"SOURCE=$(TOOLSsrcDIR)/cdfedit.exe" \
		"DESTINATION=$(INSTALLDIR)/bin/cdfedit.exe" \
		copy.file ; \
	elif [ -f $(TOOLSsrcDIR)/cdfedit ] ; then \
	   $(MAKE) MAKE=$(MAKE) \
	        "SOURCE=$(TOOLSsrcDIR)/cdfedit" \
	        "DESTINATION=$(INSTALLDIR)/bin/cdfedit" \
	        copy.file ; \
	else \
	   $(NULL) ; \
	fi
	@if [ -f $(TOOLSsrcDIR)/cdfxp.exe ] ; then \
	   $(MAKE) MAKE=$(MAKE) \
		"SOURCE=$(TOOLSsrcDIR)/cdfxp.exe" \
		"DESTINATION=$(INSTALLDIR)/bin/cdfexport.exe" \
		copy.file ; \
	elif [ -f $(TOOLSsrcDIR)/cdfxp ] ; then \
	   $(MAKE) MAKE=$(MAKE) \
	        "SOURCE=$(TOOLSsrcDIR)/cdfxp" \
	        "DESTINATION=$(INSTALLDIR)/bin/cdfexport" \
	        copy.file ; \
	else \
	   $(NULL) ; \
	fi
	@if [ -f $(TOOLSsrcDIR)/cdfcvt.exe ] ; then \
	   $(MAKE) MAKE=$(MAKE) \
		"SOURCE=$(TOOLSsrcDIR)/cdfcvt.exe" \
		"DESTINATION=$(INSTALLDIR)/bin/cdfconvert.exe" \
		copy.file ; \
	elif [ -f $(TOOLSsrcDIR)/cdfcvt ] ; then \
	   $(MAKE) MAKE=$(MAKE) \
	        "SOURCE=$(TOOLSsrcDIR)/cdfcvt" \
	        "DESTINATION=$(INSTALLDIR)/bin/cdfconvert" \
	        copy.file ; \
	else \
	   $(NULL) ; \
	fi
	@if [ -f $(TOOLSsrcDIR)/skt2cdf.exe ] ; then \
	   $(MAKE) MAKE=$(MAKE) \
		"SOURCE=$(TOOLSsrcDIR)/skt2cdf.exe" \
		"DESTINATION=$(INSTALLDIR)/bin/skeletoncdf.exe" \
		copy.file ; \
	elif [ -f $(TOOLSsrcDIR)/skt2cdf ] ; then \
	   $(MAKE) MAKE=$(MAKE) \
	        "SOURCE=$(TOOLSsrcDIR)/skt2cdf" \
	        "DESTINATION=$(INSTALLDIR)/bin/skeletoncdf" \
	        copy.file ; \
	else \
	   $(NULL) ; \
	fi
	@if [ -f $(TOOLSsrcDIR)/cdf2skt.exe ] ; then \
	   $(MAKE) MAKE=$(MAKE) \
		"SOURCE=$(TOOLSsrcDIR)/cdf2skt.exe" \
		"DESTINATION=$(INSTALLDIR)/bin/skeletontable.exe" \
		copy.file ; \
	elif [ -f $(TOOLSsrcDIR)/cdf2skt ] ; then \
	   $(MAKE) MAKE=$(MAKE) \
	        "SOURCE=$(TOOLSsrcDIR)/cdf2skt" \
	        "DESTINATION=$(INSTALLDIR)/bin/skeletontable" \
	        copy.file ; \
	else \
	   $(NULL) ; \
	fi
	@if [ -f $(TOOLSsrcDIR)/cdfinq.exe ] ; then \
	   $(MAKE) MAKE=$(MAKE) \
		"SOURCE=$(TOOLSsrcDIR)/cdfinq.exe" \
		"DESTINATION=$(INSTALLDIR)/bin/cdfinquire.exe" \
		copy.file ; \
	elif [ -f $(TOOLSsrcDIR)/cdfinq ] ; then \
	   $(MAKE) MAKE=$(MAKE) \
	        "SOURCE=$(TOOLSsrcDIR)/cdfinq" \
	        "DESTINATION=$(INSTALLDIR)/bin/cdfinquire" \
	        copy.file ; \
	else \
	   $(NULL) ; \
	fi
	@if [ -f $(TOOLSsrcDIR)/cdfstats.exe ] ; then \
	   $(MAKE) MAKE=$(MAKE) \
		"SOURCE=$(TOOLSsrcDIR)/cdfstats.exe" \
		"DESTINATION=$(INSTALLDIR)/bin/cdfstats.exe" \
		copy.file ; \
	elif [ -f $(TOOLSsrcDIR)/cdfstats ] ; then \
	   $(MAKE) MAKE=$(MAKE) \
	        "SOURCE=$(TOOLSsrcDIR)/cdfstats" \
	        "DESTINATION=$(INSTALLDIR)/bin/cdfstats" \
	        copy.file ; \
	else \
	   $(NULL) ; \
	fi
	@if [ -f $(TOOLSsrcDIR)/cdfcmp.exe ] ; then \
	   $(MAKE) MAKE=$(MAKE) \
		"SOURCE=$(TOOLSsrcDIR)/cdfcmp.exe" \
		"DESTINATION=$(INSTALLDIR)/bin/cdfcompare.exe" \
		copy.file ; \
	elif [ -f $(TOOLSsrcDIR)/cdfcmp ] ; then \
	   $(MAKE) MAKE=$(MAKE) \
	        "SOURCE=$(TOOLSsrcDIR)/cdfcmp" \
	        "DESTINATION=$(INSTALLDIR)/bin/cdfcompare" \
	        copy.file ; \
	else \
	   $(NULL) ; \
	fi
	@if [ -f $(TOOLSsrcDIR)/cdfdump.exe ] ; then \
	   $(MAKE) MAKE=$(MAKE) \
		"SOURCE=$(TOOLSsrcDIR)/cdfdump.exe" \
		"DESTINATION=$(INSTALLDIR)/bin/cdfdump.exe" \
		copy.file ; \
	elif [ -f $(TOOLSsrcDIR)/cdfdump ] ; then \
	   $(MAKE) MAKE=$(MAKE) \
	        "SOURCE=$(TOOLSsrcDIR)/cdfdump" \
	        "DESTINATION=$(INSTALLDIR)/bin/cdfdump" \
	        copy.file ; \
	else \
	   $(NULL) ; \
	fi
	@if [ -f $(TOOLSsrcDIR)/cdfirsdump.exe ] ; then \
	   $(MAKE) MAKE=$(MAKE) \
		"SOURCE=$(TOOLSsrcDIR)/cdfirsdump.exe" \
		"DESTINATION=$(INSTALLDIR)/bin/cdfirsdump.exe" \
		copy.file ; \
	elif [ -f $(TOOLSsrcDIR)/cdfirsdump ] ; then \
	   $(MAKE) MAKE=$(MAKE) \
	        "SOURCE=$(TOOLSsrcDIR)/cdfirsdump" \
	        "DESTINATION=$(INSTALLDIR)/bin/cdfirsdump" \
	        copy.file ; \
	else \
	   $(NULL) ; \
	fi
	@if [ -f $(TOOLSsrcDIR)/cdfmerge.exe ] ; then \
	   $(MAKE) MAKE=$(MAKE) \
		"SOURCE=$(TOOLSsrcDIR)/cdfmerge.exe" \
		"DESTINATION=$(INSTALLDIR)/bin/cdfmerge.exe" \
		copy.file ; \
	elif [ -f $(TOOLSsrcDIR)/cdfmerge ] ; then \
	   $(MAKE) MAKE=$(MAKE) \
	        "SOURCE=$(TOOLSsrcDIR)/cdfmerge" \
	        "DESTINATION=$(INSTALLDIR)/bin/cdfmerge" \
	        copy.file ; \
	else \
	   $(NULL) ; \
	fi
	@if [ -f $(TOOLSsrcDIR)/cdfvalidate.exe ] ; then \
	   $(MAKE) MAKE=$(MAKE) \
		"SOURCE=$(TOOLSsrcDIR)/cdfvalidate.exe" \
		"DESTINATION=$(INSTALLDIR)/bin/cdfvalidate.exe" \
		copy.file ; \
	elif [ -f $(TOOLSsrcDIR)/cdfvalidate ] ; then \
	   $(MAKE) MAKE=$(MAKE) \
	        "SOURCE=$(TOOLSsrcDIR)/cdfvalidate" \
	        "DESTINATION=$(INSTALLDIR)/bin/cdfvalidate" \
	        copy.file ; \
	else \
	   $(NULL) ; \
	fi
	@if [ -f $(TOOLSsrcDIR)/cdfleapsecondsinfo.exe ] ; then \
	   $(MAKE) MAKE=$(MAKE) \
		"SOURCE=$(TOOLSsrcDIR)/cdfleapsecondsinfo.exe" \
		"DESTINATION=$(INSTALLDIR)/bin/cdfleapsecondsinfo.exe" \
		copy.file ; \
	elif [ -f $(TOOLSsrcDIR)/cdfleapsecondsinfo ] ; then \
	   $(MAKE) MAKE=$(MAKE) \
	        "SOURCE=$(TOOLSsrcDIR)/cdfleapsecondsinfo" \
	        "DESTINATION=$(INSTALLDIR)/bin/cdfleapsecondsinfo" \
	        copy.file ; \
	else \
	   $(NULL) ; \
	fi
	@echo cp $(TOOLSsrcDIR)/cdfdir.unix $(INSTALLDIR)/bin/cdfdir
	      cp $(TOOLSsrcDIR)/cdfdir.unix $(INSTALLDIR)/bin/cdfdir

copy.help:
	@echo cp $(HELPsrcDIR)/cdfedit.olh $(INSTALLDIR)/lib/cdf/help
	      cp $(HELPsrcDIR)/cdfedit.olh $(INSTALLDIR)/lib/cdf/help
	@echo cp $(HELPsrcDIR)/cdfeditj.olh $(INSTALLDIR)/lib/cdf/help
	      cp $(HELPsrcDIR)/cdfeditj.olh $(INSTALLDIR)/lib/cdf/help
	@echo cp $(HELPsrcDIR)/cdfedit.ilh $(INSTALLDIR)/lib/cdf/help
	      cp $(HELPsrcDIR)/cdfedit.ilh $(INSTALLDIR)/lib/cdf/help
	@echo cp $(HELPsrcDIR)/cdfbrow.ilh $(INSTALLDIR)/lib/cdf/help
	      cp $(HELPsrcDIR)/cdfbrow.ilh $(INSTALLDIR)/lib/cdf/help
	@echo cp $(HELPsrcDIR)/cdfxp.ilh $(INSTALLDIR)/lib/cdf/help
	      cp $(HELPsrcDIR)/cdfxp.ilh $(INSTALLDIR)/lib/cdf/help
	@echo cp $(HELPsrcDIR)/cdfcvt.olh $(INSTALLDIR)/lib/cdf/help
	      cp $(HELPsrcDIR)/cdfcvt.olh $(INSTALLDIR)/lib/cdf/help
	@echo cp $(HELPsrcDIR)/cdfcvtj.olh $(INSTALLDIR)/lib/cdf/help
	      cp $(HELPsrcDIR)/cdfcvtj.olh $(INSTALLDIR)/lib/cdf/help
	@echo cp $(HELPsrcDIR)/cdfcmp.olh $(INSTALLDIR)/lib/cdf/help
	      cp $(HELPsrcDIR)/cdfcmp.olh $(INSTALLDIR)/lib/cdf/help
	@echo cp $(HELPsrcDIR)/cdfcmpj.olh $(INSTALLDIR)/lib/cdf/help
	      cp $(HELPsrcDIR)/cdfcmpj.olh $(INSTALLDIR)/lib/cdf/help
	@echo cp $(HELPsrcDIR)/cdf2skt.olh $(INSTALLDIR)/lib/cdf/help
	      cp $(HELPsrcDIR)/cdf2skt.olh $(INSTALLDIR)/lib/cdf/help
	@echo cp $(HELPsrcDIR)/cdf2sktj.olh $(INSTALLDIR)/lib/cdf/help
	      cp $(HELPsrcDIR)/cdf2sktj.olh $(INSTALLDIR)/lib/cdf/help
	@echo cp $(HELPsrcDIR)/skt2cdf.olh $(INSTALLDIR)/lib/cdf/help
	      cp $(HELPsrcDIR)/skt2cdf.olh $(INSTALLDIR)/lib/cdf/help
	@echo cp $(HELPsrcDIR)/skt2cdfj.olh $(INSTALLDIR)/lib/cdf/help
	      cp $(HELPsrcDIR)/skt2cdfj.olh $(INSTALLDIR)/lib/cdf/help
	@echo cp $(HELPsrcDIR)/cdfstats.olh $(INSTALLDIR)/lib/cdf/help
	      cp $(HELPsrcDIR)/cdfstats.olh $(INSTALLDIR)/lib/cdf/help
	@echo cp $(HELPsrcDIR)/cdfstatsj.olh $(INSTALLDIR)/lib/cdf/help
	      cp $(HELPsrcDIR)/cdfstatsj.olh $(INSTALLDIR)/lib/cdf/help
	@echo cp $(HELPsrcDIR)/cdfdump.olh $(INSTALLDIR)/lib/cdf/help
	      cp $(HELPsrcDIR)/cdfdump.olh $(INSTALLDIR)/lib/cdf/help
	@echo cp $(HELPsrcDIR)/cdfdumpj.olh $(INSTALLDIR)/lib/cdf/help
	      cp $(HELPsrcDIR)/cdfdumpj.olh $(INSTALLDIR)/lib/cdf/help
	@echo cp $(HELPsrcDIR)/cdfirsdump.olh $(INSTALLDIR)/lib/cdf/help
	      cp $(HELPsrcDIR)/cdfirsdump.olh $(INSTALLDIR)/lib/cdf/help
	@echo cp $(HELPsrcDIR)/cdfirsdumpj.olh $(INSTALLDIR)/lib/cdf/help
	      cp $(HELPsrcDIR)/cdfirsdumpj.olh $(INSTALLDIR)/lib/cdf/help
	@echo cp $(HELPsrcDIR)/cdfinq.olh $(INSTALLDIR)/lib/cdf/help
	      cp $(HELPsrcDIR)/cdfinq.olh $(INSTALLDIR)/lib/cdf/help
	@echo cp $(HELPsrcDIR)/cdfinqj.olh $(INSTALLDIR)/lib/cdf/help
	      cp $(HELPsrcDIR)/cdfinqj.olh $(INSTALLDIR)/lib/cdf/help
	@echo cp $(HELPsrcDIR)/cdfdirj.olh $(INSTALLDIR)/lib/cdf/help
	      cp $(HELPsrcDIR)/cdfdirj.olh $(INSTALLDIR)/lib/cdf/help
	@echo cp $(HELPsrcDIR)/cdfmerge.olh $(INSTALLDIR)/lib/cdf/help
	      cp $(HELPsrcDIR)/cdfmerge.olh $(INSTALLDIR)/lib/cdf/help
	@echo cp $(HELPsrcDIR)/cdfmergej.olh $(INSTALLDIR)/lib/cdf/help
	      cp $(HELPsrcDIR)/cdfmergej.olh $(INSTALLDIR)/lib/cdf/help
	@echo cp $(HELPsrcDIR)/cdfvalidate.olh $(INSTALLDIR)/lib/cdf/help
	      cp $(HELPsrcDIR)/cdfvalidate.olh $(INSTALLDIR)/lib/cdf/help
	@echo cp $(HELPsrcDIR)/cdfvalidatej.olh $(INSTALLDIR)/lib/cdf/help
	      cp $(HELPsrcDIR)/cdfvalidatej.olh $(INSTALLDIR)/lib/cdf/help
	@echo cp $(HELPsrcDIR)/cdfleaptableinfo.olh $(INSTALLDIR)/lib/cdf/help
	      cp $(HELPsrcDIR)/cdfleaptableinfo.olh $(INSTALLDIR)/lib/cdf/help
	@echo cp $(HELPsrcDIR)/cdfleaptableinfoj.olh $(INSTALLDIR)/lib/cdf/help
	      cp $(HELPsrcDIR)/cdfleaptableinfoj.olh $(INSTALLDIR)/lib/cdf/help

create.dir:
	@echo mkdir -p $(TARGET)
	      mkdir -p $(TARGET)

copy.file:
	@echo cp $(SOURCE) $(DESTINATION)
	      cp $(SOURCE) $(DESTINATION)

notify.user:
	@echo
	@echo Installation completed!
	@echo
	@echo "**********"
	@echo "*  NOTE  *"
	@echo "**********"
	@echo "If you want to use any of the CDF command-line utilitites (e.g. cdfedit, "
	@echo "cdfexport, etc.), we strongly encourage you to set the CDF environment "
	@echo "variables defined in the CDF definition files.  Once the environment variables"
	@echo "defined, you can invoke the CDF utility of interest just by typing the utility"
	@echo "name. Otherwise, you'll have to specify the full path of the utility."
	@echo
	@echo If you use TCSH or CSH, run the following command:
	@echo
	@if [ $(INSTALLDIR) = "." ] ; then \
	   echo "    source `pwd`/bin/definitions.C" ; \
	else \
	   echo "    source $(INSTALLDIR)/bin/definitions.C" ; \
	fi
	@echo
	@echo
	@echo If you use BASH or BSH, run the following command: 
	@echo
	@if [ $(INSTALLDIR) = "." ] ; then \
	   echo "    . `pwd`/bin/definitions.B" ; \
	else \
	   echo "    . $(INSTALLDIR)/bin/definitions.B" ; \
	fi
	@echo
	@echo
	@echo If you use KSH, run the following command: 
	@echo
	@if [ $(INSTALLDIR) = "." ] ; then \
	   echo "    . `pwd`/bin/definitions.K" ; \
	else \
	   echo "    . $(INSTALLDIR)/bin/definitions.K" ; \
	fi
	@echo

ranlib.file:
	@echo $(RANLIB) $(TARGET)
	      $(RANLIB) $(TARGET)

#------------------------------------------------------------------------------
# Clean/purge.
#------------------------------------------------------------------------------

clean:
	@-rm -f core
	@-rm -f samples/core
	@cd $(LIBsrcZlibDIR); $(MAKE) MAKE=$(MAKE) clean
	@cd $(LIBsrcDIR); $(MAKE) MAKE=$(MAKE) clean
	@cd $(TOOLSsrcDIR); $(MAKE) MAKE=$(MAKE) clean
	@cd $(TESTSsrcDIR); $(MAKE) MAKE=$(MAKE) clean

purge:
	@-rm -f *~
	@-rm -f *#
	@-rm -f samples/*~
	@-rm -f samples/*#
	@-rm -f $(HELPsrcDIR)/*~
	@-rm -f $(HELPsrcDIR)/*#
	@-rm -f $(DEFSsrcDIR)/*~
	@-rm -f $(DEFSsrcDIR)/*#
	@-rm -f $(INCsrcDIR)/*~
	@-rm -f $(INCsrcDIR)/*#
	@cd $(LIBsrcZlibDIR); $(MAKE) MAKE=$(MAKE) purge
	@cd $(LIBsrcDIR); $(MAKE) MAKE=$(MAKE) purge
	@cd $(TOOLSsrcDIR); $(MAKE) MAKE=$(MAKE) purge
	@cd $(TESTSsrcDIR); $(MAKE) MAKE=$(MAKE) purge

#------------------------------------------------------------------------------
# Ultrix.
#------------------------------------------------------------------------------

all.ultrix.risc: note1.all.ultrix.risc.build
all.ultrix.gnu: note1.1st all.ultrix.gnu.build

all.ultrix.risc.build:
	@cd $(LIBsrcZlibDIR); $(MAKE) MAKE=$(MAKE) \
"PIC=$(PIC_ultrix_risc)" \
"CCx=$(CC_ultrix_risc)" \
"COPTIONS=$(COPTIONS_ultrix_risc)" \
all
	@cd $(LIBsrcDIR); $(MAKE) MAKE=$(MAKE) \
"SHARED=$(SHARED_ultrix)" \
"PIC=$(PIC_ultrix_risc)" \
"CCx=$(CC_ultrix_risc)" \
"LDx=$(LD_ultrix_risc)" \
"COPTIONS=$(COPTIONS_ultrix_risc)" \
"SYSLIBS=$(SYSLIBSshr_ultrix_risc)" \
"SHAREDEXT=$(SHAREDEXT_ultrix)" \
"LDOPTIONS=$(LDOPTIONS_ultrix_risc)" \
"AROPTIONS=$(AROPTIONS_ultrix)" \
"RANLIB=$(RANLIB_ultrix)" \
all
	@cd $(TOOLSsrcDIR); $(MAKE) MAKE=$(MAKE) \
"CURSES=$(AND.$(CURSES).$(CURSES_$(OS)_$(ENV)))" \
"CCx=$(CC_ultrix_risc)" \
"COPTIONS=$(COPTIONS_ultrix_risc)" \
"COPTIONSld=$(COPTIONSld_ultrix_risc)" \
"SYSLIBS=$(SYSLIBSexe_ultrix_risc)" \
"CURSESLIB=$(CURSESLIB_ultrix_risc)" \
all
	@cd $(TESTSsrcDIR); $(MAKE) MAKE=$(MAKE) \
"FORTRAN=$(FORTRAN)" \
"FCx=$(FC_ultrix)" \
"CCx=$(CC_ultrix_risc)" \
"SHARED=$(SHARED_ultrix)" \
"FOPTIONS=$(FOPTIONS_ultrix)" \
"FOPTIONSld=$(FOPTIONSld_ultrix)" \
"COPTIONS=$(COPTIONS_ultrix_risc)" \
"COPTIONSld=$(COPTIONSld_ultrix_risc)" \
"SYSLIBS=$(SYSLIBSexe_ultrix_risc)" \
"SHAREDEXT=$(SHAREDEXT_ultrix)" \
all

all.ultrix.gnu.build:
	@cd $(LIBsrcZlibDIR); $(MAKE) MAKE=$(MAKE) \
"PIC=$(PIC_ultrix_gnu)" \
"CCx=$(CC_ultrix_gnu)" \
"COPTIONS=$(COPTIONS_ultrix_gnu)" \
all
	@cd $(LIBsrcDIR); $(MAKE) MAKE=$(MAKE) \
"SHARED=$(SHARED_ultrix)" \
"PIC=$(PIC_ultrix_gnu)" \
"CCx=$(CC_ultrix_gnu)" \
"LDx=$(LD_ultrix_gnu)" \
"COPTIONS=$(COPTIONS_ultrix_gnu)" \
"SYSLIBS=$(SYSLIBSshr_ultrix_gnu)" \
"SHAREDEXT=$(SHAREDEXT_ultrix)" \
"LDOPTIONS=$(LDOPTIONS_ultrix_gnu)" \
"AROPTIONS=$(AROPTIONS_ultrix)" \
"RANLIB=$(RANLIB_ultrix)" \
all
	@cd $(TOOLSsrcDIR); $(MAKE) MAKE=$(MAKE) \
"CURSES=$(AND.$(CURSES).$(CURSES_$(OS)_$(ENV)))" \
"CCx=$(CC_ultrix_gnu)" \
"COPTIONS=$(COPTIONS_ultrix_gnu)" \
"COPTIONSld=$(COPTIONSld_ultrix_gnu)" \
"SYSLIBS=$(SYSLIBSexe_ultrix_gnu)" \
"CURSESLIB=$(CURSESLIB_ultrix_gnu)" \
all
	@cd $(TESTSsrcDIR); $(MAKE) MAKE=$(MAKE) \
"FORTRAN=$(FORTRAN)" \
"FCx=$(FC_ultrix)" \
"CCx=$(CC_ultrix_gnu)" \
"SHARED=$(SHARED_ultrix)" \
"FOPTIONS=$(FOPTIONS_ultrix)" \
"FOPTIONSld=$(FOPTIONSld_ultrix)" \
"COPTIONS=$(COPTIONS_ultrix_gnu)" \
"COPTIONSld=$(COPTIONSld_ultrix_gnu)" \
"SYSLIBS=$(SYSLIBSexe_ultrix_gnu)" \
"SHAREDEXT=$(SHAREDEXT_ultrix)" \
all

#------------------------------------------------------------------------------
# HP-UX.
#------------------------------------------------------------------------------

all.hpux.gnu: all.hpux.gnu.build
all.hpux.opt: all.hpux.opt.build
all.hpux.gnu64: all.hpux.gnu64.build
all.hpux.opt64: all.hpux.opt64.build

all.hpux.std.build:
	@cd $(LIBsrcZlibDIR); $(MAKE) MAKE=$(MAKE) \
"PIC=$(PIC_hpux_std)" \
"CCx=$(CC_hpux_std)" \
"COPTIONS=$(COPTIONS_hpux_std) $(COPTIONSZlib_hpux_std)" \
all
	@cd $(LIBsrcDIR); $(MAKE) MAKE=$(MAKE) \
"SHARED=$(SHARED_hpux_std)" \
"PIC=$(PIC_hpux_std)" \
"CCx=$(CC_hpux_std)" \
"LDx=$(LD_hpux_std)" \
"COPTIONS=$(COPTIONS_hpux_std)" \
"SYSLIBS=$(SYSLIBSshr_hpux_std)" \
"SHAREDEXT=$(SHAREDEXT_hpux)" \
"LDOPTIONS=$(LDOPTIONS_hpux_std)" \
"AROPTIONS=$(AROPTIONS_hpux)" \
"RANLIB=$(RANLIB_hpux)" \
all
	@cd $(TOOLSsrcDIR); $(MAKE) MAKE=$(MAKE) \
"CURSES=$(AND.$(CURSES).$(CURSES_$(OS)_$(ENV)))" \
"CCx=$(CC_hpux_std)" \
"COPTIONS=$(COPTIONS_hpux_std)" \
"COPTIONSld=$(COPTIONSld_hpux_std)" \
"SYSLIBS=$(SYSLIBSexe_hpux_std)" \
"CURSESLIB=$(CURSESLIB_hpux_std)" \
all
	@cd $(TESTSsrcDIR); $(MAKE) MAKE=$(MAKE) \
"FORTRAN=$(FORTRAN)" \
"FCx=$(FC_hpux)" \
"CCx=$(CC_hpux_std)" \
"SHARED=$(SHARED_hpux_std)" \
"FOPTIONS=$(FOPTIONS_hpux)" \
"FOPTIONSld=$(FOPTIONSld_hpux)" \
"COPTIONS=$(COPTIONS_hpux_std)" \
"COPTIONSld=$(COPTIONSld_hpux_std)" \
"SYSLIBS=$(SYSLIBSexe_hpux_std)" \
"SHAREDEXT=$(SHAREDEXT_hpux)" \
all

all.hpux.opt.build:
	@cd $(LIBsrcZlibDIR); $(MAKE) MAKE=$(MAKE) \
"PIC=$(PIC_hpux_opt)" \
"CCx=$(CC_hpux_opt)" \
"COPTIONS=$(COPTIONS_hpux_opt) $(COPTIONSZlib_hpux_opt)" \
all
	@cd $(LIBsrcDIR); $(MAKE) MAKE=$(MAKE) \
"SHARED=$(SHARED)" \
"PIC=$(PIC_hpux_opt)" \
"CCx=$(CC_hpux_opt)" \
"LDx=$(LD_hpux_opt)" \
"COPTIONS=$(COPTIONS_hpux_opt)" \
"SYSLIBS=$(SYSLIBSshr_hpux_opt)" \
"SHAREDEXT=$(SHAREDEXT_hpux)" \
"LDOPTIONS=$(LDOPTIONS_hpux_opt)" \
"AROPTIONS=$(AROPTIONS_hpux)" \
"RANLIB=$(RANLIB_hpux)" \
all
	@cd $(TOOLSsrcDIR); $(MAKE) MAKE=$(MAKE) \
"CURSES=$(AND.$(CURSES).$(CURSES_$(OS)_$(ENV)))" \
"CCx=$(CC_hpux_opt)" \
"COPTIONS=$(COPTIONS_hpux_opt)" \
"COPTIONSld=$(COPTIONSld_hpux_opt)" \
"SYSLIBS=$(SYSLIBSexe_hpux_opt)" \
"CURSESLIB=$(CURSESLIB_hpux_opt)" \
all
	@cd $(TESTSsrcDIR); $(MAKE) MAKE=$(MAKE) \
"FORTRAN=$(FORTRAN)" \
"FCx=$(FC_hpux)" \
"CCx=$(CC_hpux_opt)" \
"SHARED=$(SHARED)" \
"FOPTIONS=$(FOPTIONS_hpux)" \
"FOPTIONSld=$(FOPTIONSld_hpux)" \
"COPTIONS=$(COPTIONS_hpux_opt)" \
"COPTIONSld=$(COPTIONSld_hpux_opt)" \
"SYSLIBS=$(SYSLIBSexe_hpux_opt)" \
"SHAREDEXT=$(SHAREDEXT_hpux)" \
all

all.hpux.opt64.build:
	@cd $(LIBsrcZlibDIR); $(MAKE) MAKE=$(MAKE) \
"PIC=$(PIC_hpux_opt64)" \
"CCx=$(CC_hpux_opt64)" \
"COPTIONS=$(COPTIONS_hpux_opt64) $(COPTIONSZlib_hpux_opt64)" \
all
	@cd $(LIBsrcDIR); $(MAKE) MAKE=$(MAKE) \
"SHARED=$(SHARED)" \
"PIC=$(PIC_hpux_opt64)" \
"CCx=$(CC_hpux_opt64)" \
"LDx=$(LD_hpux_opt64)" \
"COPTIONS=$(COPTIONS_hpux_opt64)" \
"SYSLIBS=$(SYSLIBSshr_hpux_opt64)" \
"SHAREDEXT=$(SHAREDEXT_hpux)" \
"LDOPTIONS=$(LDOPTIONS_hpux_opt64)" \
"AROPTIONS=$(AROPTIONS_hpux)" \
"RANLIB=$(RANLIB_hpux)" \
all
	@cd $(TOOLSsrcDIR); $(MAKE) MAKE=$(MAKE) \
"CURSES=$(AND.$(CURSES).$(CURSES_$(OS)_$(ENV)))" \
"CCx=$(CC_hpux_opt64)" \
"COPTIONS=$(COPTIONS_hpux_opt64)" \
"COPTIONSld=$(COPTIONSld_hpux_opt64)" \
"SYSLIBS=$(SYSLIBSexe_hpux_opt64)" \
"CURSESLIB=$(CURSESLIB_hpux_opt64)" \
all
	@cd $(TESTSsrcDIR); $(MAKE) MAKE=$(MAKE) \
"FORTRAN=$(FORTRAN)" \
"FCx=$(FC_hpux)" \
"CCx=$(CC_hpux_opt64)" \
"SHARED=$(SHARED)" \
"FOPTIONS=$(FOPTIONS_hpux)" \
"FOPTIONSld=$(FOPTIONSld_hpux)" \
"COPTIONS=$(COPTIONS_hpux_opt64)" \
"COPTIONSld=$(COPTIONSld_hpux_opt64)" \
"SYSLIBS=$(SYSLIBSexe_hpux_opt64)" \
"SHAREDEXT=$(SHAREDEXT_hpux)" \
all

all.hpux.posix.build:
	@cd $(LIBsrcZlibDIR); $(MAKE) MAKE=$(MAKE) \
"PIC=$(PIC_hpux_posix)" \
"CCx=$(CC_hpux_posix)" \
"COPTIONS=$(COPTIONS_hpux_posix) $(COPTIONSZlib_hpux_posix)" \
all
	@cd $(LIBsrcDIR); $(MAKE) MAKE=$(MAKE) \
"SHARED=$(SHARED)" \
"PIC=$(PIC_hpux_posix)" \
"CCx=$(CC_hpux_posix)" \
"LDx=$(LD_hpux_posix)" \
"COPTIONS=$(COPTIONS_hpux_posix)" \
"SYSLIBS=$(SYSLIBSshr_hpux_posix)" \
"SHAREDEXT=$(SHAREDEXT_hpux)" \
"LDOPTIONS=$(LDOPTIONS_hpux_posix)" \
"AROPTIONS=$(AROPTIONS_hpux)" \
"RANLIB=$(RANLIB_hpux)" \
all
	@cd $(TOOLSsrcDIR); $(MAKE) MAKE=$(MAKE) \
"CURSES=$(AND.$(CURSES).$(CURSES_$(OS)_$(ENV)))" \
"CCx=$(CC_hpux_posix)" \
"COPTIONS=$(COPTIONS_hpux_posix)" \
"COPTIONSld=$(COPTIONSld_hpux_posix)" \
"SYSLIBS=$(SYSLIBSexe_hpux_posix)" \
"CURSESLIB=$(CURSESLIB_hpux_posix)" \
all
	@cd $(TESTSsrcDIR); $(MAKE) MAKE=$(MAKE) \
"FORTRAN=$(FORTRAN)" \
"FCx=$(FC_hpux)" \
"CCx=$(CC_hpux_posix)" \
"SHARED=$(SHARED)" \
"FOPTIONS=$(FOPTIONS_hpux)" \
"FOPTIONSld=$(FOPTIONSld_hpux)" \
"COPTIONS=$(COPTIONS_hpux_posix)" \
"COPTIONSld=$(COPTIONSld_hpux_posix)" \
"SYSLIBS=$(SYSLIBSexe_hpux_posix)" \
"SHAREDEXT=$(SHAREDEXT_hpux)" \
all

all.hpux.gnu.build:
	@cd $(LIBsrcZlibDIR); $(MAKE) MAKE=$(MAKE) \
"PIC=$(PIC_hpux_gnu)" \
"CCx=$(CC_hpux_gnu)" \
"COPTIONS=$(COPTIONS_hpux_gnu) $(COPTIONSZlib_hpux_gnu)" \
all
	@cd $(LIBsrcDIR); $(MAKE) MAKE=$(MAKE) \
"SHARED=$(SHARED)" \
"PIC=$(PIC_hpux_gnu)" \
"CCx=$(CC_hpux_gnu)" \
"LDx=$(LD_hpux_gnu)" \
"COPTIONS=$(COPTIONS_hpux_gnu)" \
"SYSLIBS=$(SYSLIBSshr_hpux_gnu)" \
"SHAREDEXT=$(SHAREDEXT_hpux)" \
"LDOPTIONS=$(LDOPTIONS_hpux_gnu)" \
"AROPTIONS=$(AROPTIONS_hpux)" \
"RANLIB=$(RANLIB_hpux)" \
all
	@cd $(TOOLSsrcDIR); $(MAKE) MAKE=$(MAKE) \
"CURSES=$(AND.$(CURSES).$(CURSES_$(OS)_$(ENV)))" \
"CCx=$(CC_hpux_gnu)" \
"COPTIONS=$(COPTIONS_hpux_gnu)" \
"COPTIONSld=$(COPTIONSld_hpux_gnu)" \
"SYSLIBS=$(SYSLIBSexe_hpux_gnu)" \
"CURSESLIB=$(CURSESLIB_hpux_gnu)" \
all
	@cd $(TESTSsrcDIR); $(MAKE) MAKE=$(MAKE) \
"FORTRAN=$(FORTRAN)" \
"FCx=$(FC_hpux)" \
"CCx=$(CC_hpux_gnu)" \
"SHARED=$(SHARED)" \
"FOPTIONS=$(FOPTIONS_hpux)" \
"FOPTIONSld=$(FOPTIONSld_hpux)" \
"COPTIONS=$(COPTIONS_hpux_gnu)" \
"COPTIONSld=$(COPTIONSld_hpux_gnu)" \
"SYSLIBS=$(SYSLIBSexe_hpux_gnu)" \
"SHAREDEXT=$(SHAREDEXT_hpux)" \
all

all.hpux.gnu64.build:
	@cd $(LIBsrcZlibDIR); $(MAKE) MAKE=$(MAKE) \
"PIC=$(PIC_hpux_gnu64)" \
"CCx=$(CC_hpux_gnu64)" \
"COPTIONS=$(COPTIONS_hpux_gnu64) $(COPTIONSZlib_hpux_gnu64)" \
all
	@cd $(LIBsrcDIR); $(MAKE) MAKE=$(MAKE) \
"SHARED=$(SHARED)" \
"PIC=$(PIC_hpux_gnu64)" \
"CCx=$(CC_hpux_gnu64)" \
"LDx=$(LD_hpux_gnu64)" \
"COPTIONS=$(COPTIONS_hpux_gnu64)" \
"SYSLIBS=$(SYSLIBSshr_hpux_gnu64)" \
"SHAREDEXT=$(SHAREDEXT_hpux)" \
"LDOPTIONS=$(LDOPTIONS_hpux_gnu64)" \
"AROPTIONS=$(AROPTIONS_hpux)" \
"RANLIB=$(RANLIB_hpux)" \
all
	@cd $(TOOLSsrcDIR); $(MAKE) MAKE=$(MAKE) \
"CURSES=$(AND.$(CURSES).$(CURSES_$(OS)_$(ENV)))" \
"CCx=$(CC_hpux_gnu64)" \
"COPTIONS=$(COPTIONS_hpux_gnu64)" \
"COPTIONSld=$(COPTIONSld_hpux_gnu64)" \
"SYSLIBS=$(SYSLIBSexe_hpux_gnu64)" \
"CURSESLIB=$(CURSESLIB_hpux_gnu64)" \
all
	@cd $(TESTSsrcDIR); $(MAKE) MAKE=$(MAKE) \
"FORTRAN=$(FORTRAN)" \
"FCx=$(FC_hpux)" \
"CCx=$(CC_hpux_gnu64)" \
"SHARED=$(SHARED)" \
"FOPTIONS=$(FOPTIONS_hpux)" \
"FOPTIONSld=$(FOPTIONSld_hpux)" \
"COPTIONS=$(COPTIONS_hpux_gnu64)" \
"COPTIONSld=$(COPTIONSld_hpux_gnu64)" \
"SYSLIBS=$(SYSLIBSexe_hpux_gnu64)" \
"SHAREDEXT=$(SHAREDEXT_hpux)" \
all
#------------------------------------------------------------------------------
# IRIX 6.x.
#------------------------------------------------------------------------------

all.irix6.sgin32: all.irix6.sgin32.build
all.irix6.sgi64: all.irix6.sgi64.build
all.irix6.gnu: note1.1st all.irix6.gnu.build

all.irix6.sgin32.build:
	@cd $(LIBsrcZlibDIR); $(MAKE) MAKE=$(MAKE) \
"PIC=$(PIC_irix6_sgin32)" \
"CCx=$(CC_irix6_sgin32)" \
"COPTIONS=$(COPTIONS_irix6_sgin32) $(COPTIONSZlib_irix6_sgin32)" \
all
	@cd $(LIBsrcDIR); $(MAKE) MAKE=$(MAKE) \
"SHARED=$(SHARED)" \
"PIC=$(PIC_irix6_sgin32)" \
"CCx=$(CC_irix6_sgin32)" \
"LDx=$(LD_irix6_sgin32)" \
"COPTIONS=$(COPTIONS_irix6_sgin32)" \
"SYSLIBS=$(SYSLIBSshr_irix6_sgin32)" \
"SHAREDEXT=$(SHAREDEXT_irix6)" \
"LDOPTIONS=$(LDOPTIONS_irix6_sgin32) -n32" \
"AROPTIONS=$(AROPTIONS_irix6)" \
"RANLIB=$(RANLIB_irix6)" \
all
	@cd $(TOOLSsrcDIR); $(MAKE) MAKE=$(MAKE) \
"CURSES=$(AND.$(CURSES).$(CURSES_$(OS)_$(ENV)))" \
"CCx=$(CC_irix6_sgin32)" \
"COPTIONS=$(COPTIONS_irix6_sgin32)" \
"COPTIONSld=$(COPTIONSld_irix6_sgin32)" \
"SYSLIBS=$(SYSLIBSexe_irix6_sgin32)" \
"CURSESLIB=$(CURSESLIB_irix6_sgin32)" \
all
	@cd $(TESTSsrcDIR); $(MAKE) MAKE=$(MAKE) \
"FORTRAN=$(FORTRAN)" \
"FCx=$(FC_irix6)" \
"CCx=$(CC_irix6_sgin32)" \
"SHARED=$(SHARED)" \
"FOPTIONS=$(FOPTIONS_irix6)" \
"FOPTIONSld=$(FOPTIONSld_irix6)" \
"COPTIONS=$(COPTIONS_irix6_sgin32)" \
"COPTIONSld=$(COPTIONSld_irix6_sgin32)" \
"SYSLIBS=$(SYSLIBSexe_irix6_sgin32)" \
"SHAREDEXT=$(SHAREDEXT_irix6)" \
all

all.irix6.sgi64.build:
	@cd $(LIBsrcZlibDIR); $(MAKE) MAKE=$(MAKE) \
"PIC=$(PIC_irix6_sgi64)" \
"CCx=$(CC_irix6_sgi64)" \
"COPTIONS=$(COPTIONS_irix6_sgi64) $(COPTIONSZlib_irix6_sgi64)" \
all
	@cd $(LIBsrcDIR); $(MAKE) MAKE=$(MAKE) \
"SHARED=$(SHARED)" \
"PIC=$(PIC_irix6_sgi64)" \
"CCx=$(CC_irix6_sgi64)" \
"LDx=$(LD_irix6_sgi64)" \
"COPTIONS=$(COPTIONS_irix6_sgi64)" \
"SYSLIBS=$(SYSLIBSshr_irix6_sgi64)" \
"SHAREDEXT=$(SHAREDEXT_irix6)" \
"LDOPTIONS=$(LDOPTIONS_irix6_sgin64) -64" \
"AROPTIONS=$(AROPTIONS_irix6)" \
"RANLIB=$(RANLIB_irix6)" \
all
	@cd $(TOOLSsrcDIR); $(MAKE) MAKE=$(MAKE) \
"CURSES=$(AND.$(CURSES).$(CURSES_$(OS)_$(ENV)))" \
"CCx=$(CC_irix6_sgi64)" \
"COPTIONS=$(COPTIONS_irix6_sgi64)" \
"COPTIONSld=$(COPTIONSld_irix6_sgi64)" \
"SYSLIBS=$(SYSLIBSexe_irix6_sgi64)" \
"CURSESLIB=$(CURSESLIB_irix6_sgi64)" \
all
	@cd $(TESTSsrcDIR); $(MAKE) MAKE=$(MAKE) \
"FORTRAN=$(FORTRAN)" \
"FCx=$(FC_irix6)" \
"CCx=$(CC_irix6_sgi64)" \
"SHARED=$(SHARED)" \
"FOPTIONS=$(FOPTIONS_irix6)" \
"FOPTIONSld=$(FOPTIONSld_irix6)" \
"COPTIONS=$(COPTIONS_irix6_sgi64)" \
"COPTIONSld=$(COPTIONSld_irix6_sgi64)" \
"SYSLIBS=$(SYSLIBSexe_irix6_sgi64)" \
"SHAREDEXT=$(SHAREDEXT_irix6)" \
all

all.irix6.gnu.build:
	@cd $(LIBsrcZlibDIR); $(MAKE) MAKE=$(MAKE) \
"PIC=$(PIC_irix6_gnu)" \
"CCx=$(CC_irix6_gnu)" \
"COPTIONS=$(COPTIONS_irix6_gnu) $(COPTIONSZlib_irix6_gnu)" \
all
	@cd $(LIBsrcDIR); $(MAKE) MAKE=$(MAKE) \
"SHARED=$(SHARED)" \
"PIC=$(PIC_irix6_gnu)" \
"CCx=$(CC_irix6_gnu)" \
"LDx=$(LD_irix6_gnu)" \
"COPTIONS=$(COPTIONS_irix6_gnu)" \
"SYSLIBS=$(SYSLIBSshr_irix6_gnu)" \
"SHAREDEXT=$(SHAREDEXT_irix6)" \
"LDOPTIONS=$(LDOPTIONS_irix6_gnu) -n32" \
"AROPTIONS=$(AROPTIONS_irix6)" \
"RANLIB=$(RANLIB_irix6)" \
all
	@cd $(TOOLSsrcDIR); $(MAKE) MAKE=$(MAKE) \
"CURSES=$(AND.$(CURSES).$(CURSES_$(OS)_$(ENV)))" \
"CCx=$(CC_irix6_gnu)" \
"COPTIONS=$(COPTIONS_irix6_gnu)" \
"COPTIONSld=$(COPTIONSld_irix6_gnu)" \
"SYSLIBS=$(SYSLIBSexe_irix6_gnu)" \
"CURSESLIB=$(CURSESLIB_irix6_gnu)" \
all
	@cd $(TESTSsrcDIR); $(MAKE) MAKE=$(MAKE) \
"FORTRAN=$(FORTRAN)" \
"FCx=$(FC_irix6)" \
"CCx=$(CC_irix6_gnu)" \
"SHARED=$(SHARED)" \
"FOPTIONS=$(FOPTIONS_irix6)" \
"FOPTIONSld=$(FOPTIONSld_irix6)" \
"COPTIONS=$(COPTIONS_irix6_gnu)" \
"COPTIONSld=$(COPTIONSld_irix6_gnu)" \
"SYSLIBS=$(SYSLIBSexe_irix6_gnu)" \
"SHAREDEXT=$(SHAREDEXT_irix6)" \
all

#------------------------------------------------------------------------------
# AIX.
#------------------------------------------------------------------------------

all.aix.ibm: all.aix.ibm.build
all.aix.gnu: all.aix.gnu.build
all.aix.ibm64: all.aix.ibm64.build
all.aix.gnu64: all.aix.gnu64.build

all.aix.ibm.build:
	@cd $(LIBsrcZlibDIR); $(MAKE) MAKE=$(MAKE) \
"PIC=$(PIC_aix_ibm)" \
"CCx=$(CC_aix_ibm)" \
"COPTIONS=$(COPTIONS_aix_ibm) $(COPTIONSZlib_aix_ibm)" \
all
	@cd $(LIBsrcDIR); $(MAKE) MAKE=$(MAKE) \
"SHARED=$(SHARED)" \
"PIC=$(PIC_aix_ibm)" \
"CCx=$(CC_aix_ibm)" \
"LDx=$(LD_aix_ibm)" \
"COPTIONS=$(COPTIONS_aix_ibm)" \
"SYSLIBS=$(SYSLIBSshr_aix_ibm)" \
"SHAREDEXT=$(SHAREDEXT_aix)" \
"LDOPTIONS=$(LDOPTIONS_aix_ibm)" \
"AROPTIONS=$(AROPTIONS_aix)" \
"RANLIB=$(RANLIB_aix)" \
all
	@cd $(TOOLSsrcDIR); $(MAKE) MAKE=$(MAKE) \
"CURSES=$(AND.$(CURSES).$(CURSES_$(OS)_$(ENV)))" \
"CCx=$(CC_aix_ibm)" \
"COPTIONS=$(COPTIONS_aix_ibm)" \
"COPTIONSld=$(COPTIONSld_aix_ibm)" \
"SYSLIBS=$(SYSLIBSexe_aix_ibm)" \
"CURSESLIB=$(CURSESLIB_aix_ibm)" \
all
	@cd $(TESTSsrcDIR); $(MAKE) MAKE=$(MAKE) \
"FORTRAN=$(FORTRAN)" \
"FCx=$(FC_aix)" \
"CCx=$(CC_aix_ibm)" \
"SHARED=$(SHARED)" \
"FOPTIONS=$(FOPTIONS_aix)" \
"FOPTIONSld=$(FOPTIONSld_aix)" \
"COPTIONS=$(COPTIONS_aix_ibm)" \
"COPTIONSld=$(COPTIONSld_aix_ibm)" \
"SYSLIBS=$(SYSLIBSexe_aix_ibm)" \
"SHAREDEXT=$(SHAREDEXT_aix)" \
all

all.aix.gnu.build:
	@cd $(LIBsrcZlibDIR); $(MAKE) MAKE=$(MAKE) \
"PIC=$(PIC_aix_gnu)" \
"CCx=$(CC_aix_gnu)" \
"COPTIONS=$(COPTIONS_aix_gnu)  $(COPTIONSZlib_aix_gnu)" \
all
	@cd $(LIBsrcDIR); $(MAKE) MAKE=$(MAKE) \
"SHARED=$(SHARED)" \
"PIC=$(PIC_aix_gnu)" \
"CCx=$(CC_aix_gnu)" \
"LDx=$(LD_aix_gnu)" \
"COPTIONS=$(COPTIONS_aix_gnu)" \
"SYSLIBS=$(SYSLIBSshr_aix_gnu)" \
"SHAREDEXT=$(SHAREDEXT_aix)" \
"LDOPTIONS=$(LDOPTIONS_aix_gnu)" \
"AROPTIONS=$(AROPTIONS_aix)" \
"RANLIB=$(RANLIB_aix)" \
all
	@cd $(TOOLSsrcDIR); $(MAKE) MAKE=$(MAKE) \
"CURSES=$(AND.$(CURSES).$(CURSES_$(OS)_$(ENV)))" \
"CCx=$(CC_aix_gnu)" \
"COPTIONS=$(COPTIONS_aix_gnu)" \
"COPTIONSld=$(COPTIONSld_aix_gnu)" \
"SYSLIBS=$(SYSLIBSexe_aix_gnu)" \
"CURSESLIB=$(CURSESLIB_aix_gnu)" \
all
	@cd $(TESTSsrcDIR); $(MAKE) MAKE=$(MAKE) \
"FORTRAN=$(FORTRAN)" \
"FCx=$(FC_aix)" \
"CCx=$(CC_aix_gnu)" \
"SHARED=$(SHARED)" \
"FOPTIONS=$(FOPTIONS_aix)" \
"FOPTIONSld=$(FOPTIONSld_aix)" \
"COPTIONS=$(COPTIONS_aix_gnu)" \
"COPTIONSld=$(COPTIONSld_aix_gnu)" \
"SYSLIBS=$(SYSLIBSexe_aix_gnu)" \
"SHAREDEXT=$(SHAREDEXT_aix)" \
all

all.aix.ibm64.build:
	@cd $(LIBsrcZlibDIR); $(MAKE) MAKE=$(MAKE) \
"PIC=$(PIC_aix_ibm64)" \
"CCx=$(CC_aix_ibm64)" \
"COPTIONS=$(COPTIONS_aix_ibm64) $(COPTIONSZlib_aix_ibm64)" \
all
	@cd $(LIBsrcDIR); $(MAKE) MAKE=$(MAKE) \
"SHARED=$(SHARED)" \
"PIC=$(PIC_aix_ibm64)" \
"CCx=$(CC_aix_ibm64)" \
"LDx=$(LD_aix_ibm64)" \
"COPTIONS=$(COPTIONS_aix_ibm64)" \
"SYSLIBS=$(SYSLIBSshr_aix_ibm64)" \
"SHAREDEXT=$(SHAREDEXT_aix)" \
"LDOPTIONS=$(LDOPTIONS_aix_ibm64)" \
"AROPTIONS=$(AROPTIONS_aix64)" \
"RANLIB=$(RANLIB_aix)" \
all
	@cd $(TOOLSsrcDIR); $(MAKE) MAKE=$(MAKE) \
"CURSES=$(AND.$(CURSES).$(CURSES_$(OS)_$(ENV)))" \
"CCx=$(CC_aix_ibm64)" \
"COPTIONS=$(COPTIONS_aix_ibm64)" \
"COPTIONSld=$(COPTIONSld_aix_ibm64)" \
"SYSLIBS=$(SYSLIBSexe_aix_ibm64)" \
"CURSESLIB=$(CURSESLIB_aix_ibm64)" \
all
	@cd $(TESTSsrcDIR); $(MAKE) MAKE=$(MAKE) \
"FORTRAN=$(FORTRAN)" \
"FCx=$(FC_aix)" \
"CCx=$(CC_aix_ibm64)" \
"SHARED=$(SHARED)" \
"FOPTIONS=$(FOPTIONS_aix)" \
"FOPTIONSld=$(FOPTIONSld_aix)" \
"COPTIONS=$(COPTIONS_aix_ibm64)" \
"COPTIONSld=$(COPTIONSld_aix_ibm64)" \
"SYSLIBS=$(SYSLIBSexe_aix_ibm64)" \
"SHAREDEXT=$(SHAREDEXT_aix)" \
all

all.aix.gnu64.build:
	@cd $(LIBsrcZlibDIR); $(MAKE) MAKE=$(MAKE) \
"PIC=$(PIC_aix_gnu64)" \
"CCx=$(CC_aix_gnu64)" \
"COPTIONS=$(COPTIONS_aix_gnu64) $(COPTIONSZlib_aix_gnu64)" \
all
	@cd $(LIBsrcDIR); $(MAKE) MAKE=$(MAKE) \
"SHARED=$(SHARED)" \
"PIC=$(PIC_aix_gnu64)" \
"CCx=$(CC_aix_gnu64)" \
"LDx=$(LD_aix_gnu64)" \
"COPTIONS=$(COPTIONS_aix_gnu64)" \
"SYSLIBS=$(SYSLIBSshr_aix_gnu64)" \
"SHAREDEXT=$(SHAREDEXT_aix)" \
"LDOPTIONS=$(LDOPTIONS_aix_gnu64)" \
"AROPTIONS=$(AROPTIONS_aix64)" \
"RANLIB=$(RANLIB_aix)" \
all
	@cd $(TOOLSsrcDIR); $(MAKE) MAKE=$(MAKE) \
"CURSES=$(AND.$(CURSES).$(CURSES_$(OS)_$(ENV)))" \
"CCx=$(CC_aix_gnu64)" \
"COPTIONS=$(COPTIONS_aix_gnu64)" \
"COPTIONSld=$(COPTIONSld_aix_gnu64)" \
"SYSLIBS=$(SYSLIBSexe_aix_gnu64)" \
"CURSESLIB=$(CURSESLIB_aix_gnu64)" \
all
	@cd $(TESTSsrcDIR); $(MAKE) MAKE=$(MAKE) \
"FORTRAN=$(FORTRAN)" \
"FCx=$(FC_aix)" \
"CCx=$(CC_aix_gnu64)" \
"SHARED=$(SHARED)" \
"FOPTIONS=$(FOPTIONS_aix)" \
"FOPTIONSld=$(FOPTIONSld_aix)" \
"COPTIONS=$(COPTIONS_aix_gnu64)" \
"COPTIONSld=$(COPTIONSld_aix_gnu64)" \
"SYSLIBS=$(SYSLIBSexe_aix_gnu64)" \
"SHAREDEXT=$(SHAREDEXT_aix)" \
all

#------------------------------------------------------------------------------
# Mach.
#------------------------------------------------------------------------------

all.mach.next: note1.1st all.mach.next.build
all.mach.macosx: all.mach.macosx.build 
all.mach.gnu: note1.1st all.mach.gnu.build

all.mach.next.build:
	@cd $(LIBsrcZlibDIR); $(MAKE) MAKE=$(MAKE) \
"PIC=$(PIC_mach_next)" \
"CCx=$(CC_mach_next)" \
"COPTIONS=$(COPTIONS_mach_next)" \
all
	@cd $(LIBsrcDIR); $(MAKE) MAKE=$(MAKE) \
"SHARED=$(SHARED_mach)" \
"PIC=$(PIC_mach_next)" \
"CCx=$(CC_mach_next)" \
"LDx=$(LD_mach_next)" \
"COPTIONS=$(COPTIONS_mach_next)" \
"SYSLIBS=$(SYSLIBSshr_mach_next)" \
"SHAREDEXT=$(SHAREDEXT_mach)" \
"LDOPTIONS=$(LDOPTIONS_mach_next)" \
"AROPTIONS=$(AROPTIONS_mach)" \
"RANLIB=$(RANLIB_mach)" \
all
	@cd $(TOOLSsrcDIR); $(MAKE) MAKE=$(MAKE) \
"CURSES=$(CURSES_mach_next)" \
"CCx=$(CC_mach_next)" \
"COPTIONS=$(COPTIONS_mach_next)" \
"COPTIONSld=$(COPTIONSld_mach_next)" \
"SYSLIBS=$(SYSLIBSexe_mach_next)" \
"CURSESLIB=$(CURSESLIB_mach_next)" \
all
	@cd $(TESTSsrcDIR); $(MAKE) MAKE=$(MAKE) \
"FORTRAN=$(FORTRAN)" \
"FCx=$(FC_mach)" \
"CCx=$(CC_mach_next)" \
"SHARED=$(SHARED_mach)" \
"FOPTIONS=$(FOPTIONS_mach)" \
"FOPTIONSld=$(FOPTIONSld_mach)" \
"COPTIONS=$(COPTIONS_mach_next)" \
"COPTIONSld=$(COPTIONSld_mach_next)" \
"SYSLIBS=$(SYSLIBSexe_mach_next)" \
"SHAREDEXT=$(SHAREDEXT_mach)" \
all

all.mach.macosx.build:
	@cd $(LIBsrcZlibDIR); $(MAKE) MAKE=$(MAKE) \
"PIC=$(PIC_mach_macosx)" \
"CCx=$(CC_mach_macosx)" \
"COPTIONS=$(COPTIONS_mach_macosx)" \
all
	@cd $(LIBsrcDIR); $(MAKE) MAKE=$(MAKE) \
"SHARED=$(SHARED_mach)" \
"PIC=$(PIC_mach_macosx)" \
"CCx=$(CC_mach_macosx)" \
"LDx=$(LD_mach_macosx)" \
"COPTIONS=$(COPTIONS_mach_macosx)" \
"SYSLIBS=$(SYSLIBSshr_mach_macosx)" \
"SHAREDEXT=$(SHAREDEXT_mach)" \
"LDOPTIONS=$(LDOPTIONS_mach_macosx)" \
"AROPTIONS=$(AROPTIONS_mach)" \
"RANLIB=$(RANLIB_mach)" \
all
	@cd $(TOOLSsrcDIR); $(MAKE) MAKE=$(MAKE) \
"CURSES=$(CURSES_mach_macosx)" \
"CCx=$(CC_mach_macosx)" \
"COPTIONS=$(COPTIONS_mach_macosx)" \
"COPTIONSld=$(COPTIONSld_mach_macosx)" \
"SYSLIBS=$(SYSLIBSexe_mach_macosx)" \
"CURSESLIB=$(CURSESLIB_mach_macosx)" \
all
	@cd $(TESTSsrcDIR); $(MAKE) MAKE=$(MAKE) \
"FORTRAN=$(FORTRAN)" \
"FCx=$(FC_mach)" \
"CCx=$(CC_mach_macosx)" \
"SHARED=$(SHARED_mach)" \
"FOPTIONS=$(FOPTIONS_mach)" \
"FOPTIONSld=$(FOPTIONSld_mach)" \
"COPTIONS=$(COPTIONS_mach_macosx)" \
"COPTIONSld=$(COPTIONSld_mach_macosx)" \
"SYSLIBS=$(SYSLIBSexe_mach_macosx)" \
"SHAREDEXT=$(SHAREDEXT_mach)" \
all

all.mach.gnu.build:
	@cd $(LIBsrcZlibDIR); $(MAKE) MAKE=$(MAKE) \
"PIC=$(PIC_mach_gnu)" \
"CCx=$(CC_mach_gnu)" \
"COPTIONS=$(COPTIONS_mach_gnu)" \
all
	@cd $(LIBsrcDIR); $(MAKE) MAKE=$(MAKE) \
"SHARED=$(SHARED_mach)" \
"PIC=$(PIC_mach_gnu)" \
"CCx=$(CC_mach_gnu)" \
"LDx=$(LD_mach_gnu)" \
"COPTIONS=$(COPTIONS_mach_gnu)" \
"SYSLIBS=$(SYSLIBSshr_mach_gnu)" \
"SHAREDEXT=$(SHAREDEXT_mach)" \
"LDOPTIONS=$(LDOPTIONS_mach_gnu)" \
"AROPTIONS=$(AROPTIONS_mach)" \
"RANLIB=$(RANLIB_mach)" \
all
	@cd $(TOOLSsrcDIR); $(MAKE) MAKE=$(MAKE) \
"CURSES=$(CURSES_mach_gnu)" \
"CCx=$(CC_mach_gnu)" \
"COPTIONS=$(COPTIONS_mach_gnu)" \
"COPTIONSld=$(COPTIONSld_mach_gnu)" \
"SYSLIBS=$(SYSLIBSexe_mach_gnu)" \
"CURSESLIB=$(CURSESLIB_mach_gnu)" \
all
	@cd $(TESTSsrcDIR); $(MAKE) MAKE=$(MAKE) \
"FORTRAN=$(FORTRAN)" \
"FCx=$(FC_mach)" \
"CCx=$(CC_mach_gnu)" \
"SHARED=$(SHARED_mach)" \
"FOPTIONS=$(FOPTIONS_mach)" \
"FOPTIONSld=$(FOPTIONSld_mach)" \
"COPTIONS=$(COPTIONS_mach_gnu)" \
"COPTIONSld=$(COPTIONSld_mach_gnu)" \
"SYSLIBS=$(SYSLIBSexe_mach_gnu)" \
"SHAREDEXT=$(SHAREDEXT_mach)" \
all

#------------------------------------------------------------------------------
# QNX.
#------------------------------------------------------------------------------

all.qnx.ccwat: note1.1st all.qnx.ccwat.build

all.qnx.ccwat.build:
	@cd $(LIBsrcZlibDIR); $(MAKE) MAKE=$(MAKE) \
"PIC=$(PIC_qnx_ccwat)" \
"CCx=$(CC_qnx_ccwat)" \
"COPTIONS=$(COPTIONS_qnx_ccwat)" \
all
	@cd $(LIBsrcDIR); $(MAKE) MAKE=$(MAKE) \
"SHARED=$(SHARED_qnx)" \
"PIC=$(PIC_qnx_ccwat)" \
"CCx=$(CC_qnx_ccwat)" \
"LDx=$(LD_qnx_ccwat)" \
"COPTIONS=$(COPTIONS_qnx_ccwat)" \
"SYSLIBS=$(SYSLIBSshr_qnx_ccwat)" \
"SHAREDEXT=$(SHAREDEXT_qnx)" \
"LDOPTIONS=$(LDOPTIONS_qnx_ccwat)" \
"AROPTIONS=$(AROPTIONS_qnx)" \
"RANLIB=$(RANLIB_qnx)" \
all
	@cd $(TOOLSsrcDIR); $(MAKE) MAKE=$(MAKE) \
"CURSES=$(CURSES_qnx_ccwat)" \
"CCx=$(CC_qnx_ccwat)" \
"COPTIONS=$(COPTIONS_qnx_ccwat)" \
"COPTIONSld=$(COPTIONSld_qnx_ccwat)" \
"SYSLIBS=$(SYSLIBSexe_qnx_ccwat)" \
"CURSESLIB=$(CURSESLIB_qnx_ccwat)" \
all
	@cd $(TESTSsrcDIR); $(MAKE) MAKE=$(MAKE) \
"FORTRAN=$(FORTRAN)" \
"FCx=$(FC_qnx)" \
"CCx=$(CC_qnx_ccwat)" \
"SHARED=$(SHARED_qnx)" \
"FOPTIONS=$(FOPTIONS_qnx)" \
"FOPTIONSld=$(FOPTIONSld_qnx)" \
"COPTIONS=$(COPTIONS_qnx_ccwat)" \
"COPTIONSld=$(COPTIONSld_qnx_ccwat)" \
"SYSLIBS=$(SYSLIBSexe_qnx_ccwat)" \
"SHAREDEXT=$(SHAREDEXT_qnx)" \
all

#------------------------------------------------------------------------------
# Show settings.
#------------------------------------------------------------------------------

show: show.$(OS).$(ENV)

show.sunos.bsd: show.supported
show.sunos.bsd5: show.supported
show.sunos.sysV: show.supported
show.sunos.gnu: show.supported
show.sunos.gnu5: show.supported
show.solaris.bsd: show.supported
show.solaris.bsd5: show.supported
show.solaris.sparc: show.supported
show.solaris.sparc64: show.supported
show.solaris.sysV: show.supported
show.solaris.gnu: show.supported
show.solaris.gnu64: show.supported
show.solaris.gnu64i: show.supported
show.solaris.gnu5: show.supported
show.solaris.gnu5_64: show.supported
show.solaris.gnu-64: show.supported
show.ultrix.risc: show.supported
show.ultrix.gnu: show.supported
show.mach.next: show.supported
show.mach.macosx: show.supported
show.mach.gnu: show.supported
show.hpux.std: show.supported
show.hpux.opt: show.supported
show.hpux.posix: show.supported
show.hpux.gnu: show.supported
show.aix.ibm: show.supported
show.aix.gnu: show.supported
show.osf.dec: show.supported
show.osf.gnu: show.supported
show.osf.dec64: show.supported
show.osf.gnu64: show.supported
show.irix34.sgi: show.supported
show.irix34.gnu: show.supported
show.irix5.sgi: show.supported
show.irix5.gnu: show.supported
show.irix6.sgi32: show.supported
show.irix6.sgin32: show.supported
show.irix6.sgi64: show.supported
show.irix6.gnu: show.supported
show.qnx.gnu: show.notsupported
show.linux.gnu: show.supported
show.linux.gnu32: show.supported
show.linux.gnu64: show.supported
show.linux.intel: show.supported
show.breebsd.gnu: show.supported
show.cygwin.gnu: show.supported
show.mingw.gnu: show.supported
show.freebsd.gnu: show.supported
show.posix.vax: show.supported
show.posix.alphaD: show.supported
show.posix.alphaG: show.supported
show.posix.alphaI: show.supported

show.supported:
	@echo SHARED is $(SUPPORTED.$(SHARED_$(OS)))
	@echo CURSES is $(SUPPORTED.$(CURSES_$(OS)_$(ENV)))
	@echo FOPTIONS=$(FOPTIONS_$(OS))
	@echo FOPTIONSld=$(FOPTIONSld_$(OS))
	@echo SHAREDEXT=$(SHAREDEXT_$(OS))
	@echo LDOPTIONSlibcdf=$(LDOPTIONS_$(OS)_$(ENV))
	@echo RANLIB=$(RANLIB_$(OS))
	@echo FCx=$(FC_$(OS))
	@echo CCx=$(CC_$(OS)_$(ENV))
	@echo PIC=$(PIC_$(OS)_$(ENV))
	@echo COPTIONS=$(COPTIONS_$(OS)_$(ENV))
	@echo COPTIONSld=$(COPTIONSld_$(OS)_$(ENV))
	@echo CURSESLIB=$(CURSESLIB_$(OS)_$(ENV))
	@echo SYSLIBSexe=$(SYSLIBSexe_$(OS)_$(ENV))
	@echo SYSLIBSshr=$(SYSLIBSshr_$(OS)_$(ENV))

show.notsupported:
	@echo Sorry, \`make show\' is not available on this machine.

#------------------------------------------------------------------------------
# Warning messages.
#------------------------------------------------------------------------------

note1.1st:
	@cat Note.1st

note1.noCC:
	@cat Note.noCC

note1.solaris:
	@cat Note.solaris

note1.intel:
	@cat Note.intel

note1.macx.no:
	@cat Note.MacX

note1.macx.yes:
	@$(NOOP)
