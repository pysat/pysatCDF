/* $Id: cdfjava.h,v 1.1.1.1 2016/10/12 19:21:25 liu Exp $
 *
 */
/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/

#include <cdflib.h>
#include <cdflib64.h>
#include "cdfNativeLibrary.h"

/****************************************/
/* Max # of elements CmdVector can hold */
/****************************************/
#define MAX_VECTOR_SIZE 100              

/******************************************
 *    CDFObject Types
 ******************************************/
#define CDFOBJECT_CLASS  0L
#define CDF_CLASS        1L
#define FILE_CLASS       2L
#define VAR_CLASS        3L
#define ENTRY_CLASS      4L
#define DATA_CLASS       5L
#define ATTR_CLASS       6L

/******************************************
 *    Should this be added to cdf.h?
 ******************************************/
/* #define SAVE_            1009 */
/* #define BACKWARD_        1010 */
/* #define GETCDFFILEBACKWARD_        1011 */

/***********************************************
 *    Offset for big/little-endian.
 *    If NETWORKbyteORDERcpu is defined in
 *    cdflib.h, then it is a big-endian machine.
 ***********************************************/

#if defined(NETWORKbyteORDERcpu)
#define	LONGOFFSET	4
#define INTOFFSET	2
#define SHORTOFFSET	1
#else
#define LONGOFFSET	0
#define INTOFFSET	0
#define SHORTOFFSET	0
#endif 


/*
 *  Macro definitions:
 */
#define CheckStatus(msg,flag) ErrorHandler(msg, status, env, theCDF, flag)

#define isItem(obj) ((*env)->IsInstanceOf(env, obj, \
                     (*env)->FindClass(env, "java/util/Vector"))) == JNI_FALSE

#define CHECKNULL(a,v) \
    if ( v == NULL ) { \
        char message[1024]; \
        sprintf( message, "%s is NULL!", a ); \
        (*env)->ThrowNew( env, (*env)->FindClass( env, "java/lang/NullPointerException" ), \
                       message );\
        return (BAD_FNC_OR_ITEM); \
      }

#define MYDEBUG(f,m) \
   if (mydebug) { printf("%-15s: %s\n", f, m); }
 
/*******************************************************************
 *    Structure that is used to hold information about argument 
 *    (e.g. id, name, etc.) 
 ******************************************************************/
typedef struct argStruct {
    jobject     myObject;    /* The object that the instance var is a member of */
    jfieldID    id;          /* The field ID of the instance var */
    jobject     nameObject;  /* The name string object. Needed for memory management */
    const char *name;        /* The name of the argument as a C string */
    jobject     sigObject;   /* The sig string object. Needed for memory management */
    const char *sig;         /* The signature of the arg as a C string */
    void       *ptr;         /* A pointer to the arg in C */
    jstring     tempString;  /* Temp string object needed to handle memory for instance
				vars that are Strings */
    jboolean    isArray;     /* JNI_FALSE = not an array, JNI_TRUE = is an array */
    jboolean    isEntryorVariableData;     /* Is arg an Entry/Variable data */
    jobject     dsObject;    /* If entryData then this is the actual data signature 
				Since in the "data" instance var is a generic 
				java/lang/Object we need 2 signatures.  The actual signature
				"Ljava/lang/Object;" to get the data and this one to build
				and get the data out. */
    const char *datasig;     /* The signature of the entry "data" as a C string */
    jsize       length;      /* = numElements */
    jsize       numDims;
    long        dimSizes[CDF_MAX_DIMS];
    char	Csig[25];
} argument;

/************************************************************************
 *    Structure that is used to hold information about the CDF file. 
 *    (e.g. id, name, etc.) 
 ************************************************************************/
typedef struct cdfIDStruct {
    CDFid  id;                  /* The CDFid from open or create function    */
    jlong   jid;                /* Where it is in the link list structure    */
    int    nCDFs;               /* Reseved for future use                    */
    jboolean open;		/* flag indicating whether CDF is still open */
    struct cdfIDStruct *next;   /* A pointer to the next item in the list    */
} cdfIDList;
