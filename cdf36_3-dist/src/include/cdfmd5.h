/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
#ifndef MD5_H
#define MD5_H

#include "cdflib.h"

/*  The following tests optimise behaviour on little-endian
    machines, where there is no need to reverse the byte order
    of 32 bit words in the MD5 computation.  By default,
    HIGHFIRST is defined, which indicates we're running on a
    big-endian (most significant byte first) machine, on which
    the byteReverse function in md5.c must be invoked. However,
    byteReverse is coded in such a way that it is an identity
    function when run on a little-endian machine, so calling it
    on such a platform causes no harm apart from wasting time. 
    If the platform is known to be little-endian, we speed
    things up by undefining HIGHFIRST, which defines
    byteReverse as a null macro.  Doing things in this manner
    insures we work on new platforms regardless of their byte
    order.  */

#undef HIGHFIRST

#if defined(sun) || defined(MIPSEB) || defined(IBMRS) || \
    defined(HP)  || defined(NeXT)   || defined(mac)   || \
    defined(PPC) || defined(macosXppc) || defined(__PPC__)
#  define HIGHFIRST
#endif

/*  On machines where "long" is 64 bits, we need to declare
    uint32 as something guaranteed to be 32 bits.  */

#if defined(alphaosf) || defined(IRIX64bit) || defined(_LP64) || \
    defined(__amd64) || defined(__x86_64__) || defined(__ia64__) || \
    defined(__PPC64__) || defined(__ppc64__)
typedef unsigned int uint32;
#else
typedef unsigned long uint32;
#endif

struct MD5Context {
        uint32 buf[4];
        uint32 bits[2];
        unsigned char in[64];
};

void byteReverse PROTOARGs((
  unsigned char *buf, unsigned longs
));
VISIBLE_PREFIX void MD5Init PROTOARGs((
  struct MD5Context *ctx
));
VISIBLE_PREFIX void MD5Update PROTOARGs((
  struct MD5Context *ctx, unsigned char *buf, unsigned len
));
VISIBLE_PREFIX void MD5FinalZ PROTOARGs((
  unsigned char *digest, struct MD5Context *ctx
));
VISIBLE_PREFIX void MD5Transform PROTOARGs((
  uint32 *buf, uint32 *in
));

/*
 * This is needed to make RSAREF happy on some MS-DOS compilers.
 */
typedef struct MD5Context MD5_CTX;

/*  Define CHECK_HARDWARE_PROPERTIES to have main,c verify
    byte order and uint32 settings.  */
#define CHECK_HARDWARE_PROPERTIES

#endif /* CDFMD5_H */
