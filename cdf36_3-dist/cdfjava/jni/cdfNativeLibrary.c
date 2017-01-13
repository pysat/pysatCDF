/*
 * $Id: cdfNativeLibrary.c,v 1.1.1.1 2016/10/12 19:21:25 liu Exp $
 */
/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/*
 * cdfjava.c
 *
 * This file contains all the JNI C wrappers for the CDF, Attribute, etc
 * classes and their children and instances.
 *
 * C Functions:
 *    ErrorHandler  - Handles all errors and throws appropriate CDFExceptions
 *    cdfObjectType - Determines the CDF Object's class 
 *    getFuncOrItem - Gets a java/lang/Long out of the command vector and 
 *                    returns the CDF function or item.
 *    getJavaField  - Gets the value out of the intance var that the argument
 *                    describes and places it in arg.ptr
 *    setJavaField  - Sets the instance var to the value pointed to by arg.ptr
 *    argAlloc      - Allocated space to arg.ptr to hold the value
 *    cleanArgument - Cleans up the memory and UTF strings
 *    ErrHandle     - Cleans up the memory and UTF strings before returning an
 *                    error status after a CDFlib call
 *    cleanGlobalRefs - Cleans the global references
 *    getItem       - Retrieves the elements in a vector that is passed in
 *    addCDFid      - Adds a CDF id to the list of maintained CDF ids
 *    getCDFid      - Returns the CDF id that corresponds to the jid
 *    removeCDFid   - Mark the CDF id as closed 
 *    cleanGlobalRefs  - Clear the CDFid link list and free the global 
 *                       references if there is none open CDF
 *    JAVAcdflib    - The main method for parsing and executing the command
 *                    vector 
 *
 * JNI methods:
 *   Java_gsfc_nssdc_cdf_CDFNativeLibrary_cdfNativeLib
 *      - Main wrapper for the native implementation of the CDF Java APIs
 *
 *   Java_gsfc_nssdc_cdf_util_Epoch_compute
 *   Java_gsfc_nssdc_cdf_util_Epoch_breakdown
 *   Java_gsfc_nssdc_cdf_util_Epoch_encode
 *   Java_gsfc_nssdc_cdf_util_Epoch_encode1
 *   Java_gsfc_nssdc_cdf_util_Epoch_encode2
 *   Java_gsfc_nssdc_cdf_util_Epoch_encode3
 *   Java_gsfc_nssdc_cdf_util_Epoch_encode4
 *   Java_gsfc_nssdc_cdf_util_Epoch_encodex
 *
 * @version 2.0 03/18/05  Removed the code for selection of current CDF, attribute, 
 *                        variable or entry when just passed an "NULL_" item.
 *                        It caused a problem under multi-thread environment as the
 *                        selection might not be followed immediately by the intended
 *                        operation of the same thread, thus unexpected results would
 *                        happen. Selections are done as part of operations passed 
 *                        to JNI. JNI call is synchronized so only one process is
 *                        allowed in a JVM. 
 * @version 2.1 06/18/09  Added call ROWtoCOL when hyper-put and COLtoROW
 *                        when hyper-get for multi-dimensional variables
 *                        in a COLUMN-major CDF as the data passed from/to
 *                        Java is ROW-based.
 * @version 2.2 05/28/13  Added ErrHandle to free up space(s) before an error
 *                        status is returnedi to avoid memory leak(s).
 *
 * Author: Mike Liu, RSTX  
 *         Phil Williams. QSS/NASA/GSFC
 *
 */

#include "cdfjava.h"

#if defined(sun) && !defined(SOLARIS) && !defined(__GCC_NEW_VARARGS__)
#  define VARARGS
#else
#  define STDARG
#endif

#if defined(hpux)
#  define VA_START(ap) { \
va_list _apT_; \
va_start (_apT_); \
ap = _apT_; \
}
#else
#  define VA_START(ap) va_start(ap)
#endif

/*********************************************
 **                                         **
 **              Global References          **
 **                                         **
 *********************************************/

static char  msg[2048];         /* Debug message buffer   */
int   mydebug = 0;         /* 0 = off, non-zero = on */
jlong jmax = (jlong) 1 << 32;
jlong jsignmax = ((jlong) 1 << 31) - 1;
jint  imax = (jint) 1 << 16;
jint  isignmax = (jint) ( 1 << 15) - 1;
jshort smax = (jshort) 1 << 8;
jshort ssignmax = (jshort) (1 << 7) -1;

static jclass	cdfClass = 0, varClass, entryClass, attrClass, dataClass,
		excClass;
static jclass byteClass, shortClass, intClass, longClass, floatClass,
	      doubleClass, strClass, objClass, vecClass; 
static jmethodID BvalID, SvalID, IvalID, LvalID, FvalID, DvalID;
static jfieldID  cdfStatusID, infoWarnID;
/*static jfieldID  backwardEnvVarID; */
static long envVar;
static long lib_ver;
static char *leapTable;

/* 
   A list of currently available CDFs.  This list allows multiple cdfs to be 
   opened at one time.  The cdfIDs are maintained in this list and the only
   information that is passed back to Java objects is the position in the
   list of the current CDF
*/
cdfIDList *listOfCDFs = NULL;

/*********************************************
 **                                         **
 **          C Utility Functions            **
 **                                         **
 *********************************************/

/* Function: ErrorHandler
 *
 * Purpose:  Throw exceptions back to Java to handle CDF errors
 *           Depending on the status returned a CDFException will be thrown
 *
 * Params:   
 *    in:  where  - string describing where error occured in calling routine.
 *    in:  status - CDF status code.
 *    in:  env    - JNI runtime environment
 * Returns:
 *    1 if no fatal errors had occured
 *    0 if a fatal error occured
 */
int ErrorHandler (char *where, long status, JNIEnv *env, jobject myCDF, 
                  int flag) {

    jmethodID  cdfExceptionInit;
    jthrowable cdfException;
    char    text[CDF_STATUSTEXT_LEN+128];

    if (flag == 1) (*env)->SetLongField(env, myCDF, cdfStatusID, (jlong)status);
    if (status == CDF_OK) return(1);    /* ignore OK */

    /* Get the message and throw appropriate exception */
    CDFlib (SELECT_, CDF_STATUS_, status,
	             GET_, STATUS_TEXT_, text,
	    NULL_);
    
    if ((status > CDF_OK) || (status == CHECKSUM_ERROR) ||
	(CDF_OK > status && status >= CDF_WARN)) {
	jlong infowarn;
	infowarn = (long) (*env)->GetLongField(env, myCDF, infoWarnID);
        /* Print warnings or information to the stdout */
	strcat(text, "\n  "); 
	strcat(text, where); 
	MYDEBUG("ErrorHandler", text);
	if (infowarn == 1) printf("%s\n", text);
	return(1);
    } else { /* Throw the rest */
        strcpy(msg, where); 
        strcat(msg, " exception occured: ");
        strcat(msg, text);
        MYDEBUG("ErrorHandler", msg);

        /* if excClass not found then simply return.
           This should never happen. */
        if (excClass == 0) return(0);
        /* Get the method ID for the constructor CDFException(long) */
        cdfExceptionInit = (*env)->GetMethodID(env, excClass,
                                               "<init>",
                                               "(JLjava/lang/String;)V");

        /* Build a new cdfException object */
        cdfException = (*env)->NewObject(env, excClass, cdfExceptionInit,
                                         (jlong)status,
                                         (*env)->NewStringUTF(env, where));

        MYDEBUG("ErrorHandler", "cdfException object created");

        /* Throw the newly created exception */
        (*env)->Throw(env, cdfException);

        MYDEBUG("ErrorHandler", "cdfException thrown");

        return(0);
    }
} /* end ErrorHandler */


/* Function: cdfObjectType
 *
 * Purpose: Determine the CDFObject's class and return the
 *          appropriate flag
 *
 * Parameters:
 *    in:  env - the Java runtime environment
 *    in:  obj - the Object whose type will be determined
 * Returns:
 *    A "long" integer representing the object type (see cdfjava.h for 
 *    definitions.
 */
long cdfObjectType(JNIEnv *env, jobject obj) {

    if (obj == NULL) {
	(*env)->ThrowNew(env, excClass, "cdfObject is null");
        return -1L;
    }

    if ((*env)->IsInstanceOf(env, obj, cdfClass))
	return CDF_CLASS;

    else if ((*env)->IsInstanceOf(env, obj, varClass))
	return VAR_CLASS;

    else if ((*env)->IsInstanceOf(env, obj, dataClass))
	return DATA_CLASS;

    else if ((*env)->IsInstanceOf(env, obj, entryClass))
	return ENTRY_CLASS;

    else if ((*env)->IsInstanceOf(env, obj, attrClass))
	return ATTR_CLASS;

    else 
	return -1L;        /* Unknown class */
}
    
/* Function:  getFuncOrItem
 *
 * Purpose:   parse the next element in the command vector and return 
 *            the CDF function or item at that location.
 * Parameters:
 *    in:   env - the Java runtime environment
 *    in:   obj - a Long object whose value should correspond to a CDF function
 *                or CDF item
 *    out:  funcOrItem - the CDF function or item that the object represents
 * Returns:
 *    CDFstatus
 */
CDFstatus getFuncOrItem(JNIEnv *env, jobject obj, long *funcOrItem) {

    if ((*env)->IsInstanceOf(env, obj, longClass) == JNI_FALSE){
	return(BAD_FNC_OR_ITEM);
    }

    *funcOrItem = (long)((*env)->CallLongMethod(env, obj, LvalID));

    return(CDF_OK);
}

/* Function: getJavaField
 *
 * Purpose:  Move the value from the Java's instance variable
 *           to arg.ptr for use in C
 * Parameters:
 *   env - Java VM pointer
 *   arg - A pointer to an argument structure
 * Returns:
 *    none (should be CDFstatus)
 */
void getJavaField(JNIEnv *env, argument *arg) {

    jthrowable  exc;
    jobject     tempObject;
    jarray      arrayObject;
    jbyte       *bbody;
    jshort      *sbody;
    jint        *ibody;
    jlong       *jbody;
    jfloat      *fbody;
    jdouble     *dbody;
    char        dummySig[50];
    char	dummyCSig[50];

    if (arg->isEntryorVariableData) {
	if (arg->isArray) {          
	    strcpy(dummySig, arg->datasig+1);     /* Skip "[" */
	    strcpy(dummyCSig, arg->Csig+1);
	} else {
	    strcpy(dummySig, arg->datasig);
	    strcpy(dummyCSig, arg->Csig);
	}

    } else {
	if (arg->isArray)
	    strcpy(dummySig, arg->sig+1);         /* Skip "[" */
	else
	    strcpy(dummySig, arg->sig);
    }
    sprintf(msg,"dummySig = %s", dummySig);
    MYDEBUG("getJavaField", msg);
    switch (dummySig[0]) {
    case 'Z':                    /* boolean */
	break;

    case 'B':                    /* byte */
	if (arg->isArray) {
	    arrayObject = (*env)->GetObjectField(env, arg->myObject, arg->id);
	    bbody = (*env)->GetByteArrayElements(env,  (jarray)arrayObject, 0);
	    memcpy (arg->ptr, bbody, sizeof(char)*arg->length);
	    (*env)->ReleaseByteArrayElements(env, 
					     (jarray)arrayObject, bbody, 0);
	} else {
	    *(char *)arg->ptr  
	       = (*env)->GetByteField(env, arg->myObject, arg->id);
	}
	break;

    case 'C':                    /* char */
	break;

    case 'S':                    /* short */
	if (arg->isArray) {
	    arrayObject = (*env)->GetObjectField(env, arg->myObject, arg->id);
	    sbody = (*env)->GetShortArrayElements(env, (jarray)arrayObject, 0);
	    if (strncmp((char *)dummyCSig, "S", 1) == 0) 
		memcpy (arg->ptr, sbody, sizeof(short)*arg->length);
	    else { /* Data type is CDF_UINT1 */
		int i; 
		for (i=0; i< arg->length; i++) 
		   if (sbody[i] <= ssignmax)
		     *((jbyte *) arg->ptr + i) = (jbyte) sbody[i];
		   else
		     *((jbyte *) arg->ptr + i) = (jbyte) (sbody[i] - smax);
	    }
	    (*env)->ReleaseShortArrayElements(env, 
					      (jarray)arrayObject, sbody, 0);
	} else {
	    if (strncmp((char *)dummyCSig, "S", 1) == 0)
		*(short *)arg->ptr
			= (*env)->GetShortField(env, arg->myObject, arg->id);
	    else
		*(jbyte *)arg->ptr
			= (jbyte) (*env)->GetShortField(env, arg->myObject, arg->id);
	}
	break;

    case 'I':                    /* integer */
	if (arg->isArray) {
	    arrayObject = (*env)->GetObjectField(env, arg->myObject, arg->id);
	    ibody = (*env)->GetIntArrayElements(env, (jarray)arrayObject, 0);
	    if (strncmp((char *)dummyCSig, "I", 1) == 0)
		memcpy (arg->ptr, ibody, sizeof(jint)*arg->length);
	    else { /* Data type is CDF_UINT2 */
 		int i; 
		for (i=0; i< arg->length; i++)
		   if (ibody[i] <= isignmax)
		     *((jshort *) arg->ptr + i) = (jshort) ibody[i];
		   else
		     *((jshort *) arg->ptr + i) = (jshort) (ibody[i] - imax);
	    }
	    (*env)->ReleaseIntArrayElements(env, 
					    (jarray)arrayObject, ibody, 0);
	} else {
	    if (strncmp((char *)dummyCSig, "I", 1) == 0)
		*(jint *)arg->ptr
			= (*env)->GetIntField(env, arg->myObject, arg->id);
	    else /* Data type is CDF_UINT2 */
		*(jshort *)arg->ptr
			= (jshort) (*env)->GetIntField(env, arg->myObject, arg->id);
	}
	break;

    case 'J':                    /* long */
	if (arg->isArray) {
	    int i;
	    arrayObject = (*env)->GetObjectField(env, arg->myObject, arg->id);
	    jbody = (*env)->GetLongArrayElements(env, (jarray)arrayObject, 0);
            if (!arg->isEntryorVariableData) {
                for (i=0; i< arg->length; i++)
                   *((long *) arg->ptr + i) = (long) jbody[i];
            } else {
               if (strncmp((char *)dummyCSig, "J", 1) == 0) { /* CDF_INT8 */
                 memcpy (arg->ptr, jbody, sizeof(jlong)*arg->length);;
	       } else { /* Data type is CDF_UINT4 */
	 	 for (i=0; i< arg->length; i++)
		   if (jbody[i] <= jsignmax)
		     *((jint *) arg->ptr + i) = (jint) jbody[i];
		   else
		     *((jint *) arg->ptr + i) = (jint) (jbody[i] - jmax);
	       }
            }
	    (*env)->ReleaseLongArrayElements(env, 
					     (jarray)arrayObject, jbody, 0);
	} else {
	    if (!arg->isEntryorVariableData) 
		*(long *)arg->ptr  
		      = (long) (*env)->GetLongField(env, arg->myObject, arg->id);
	    else {
              if (strncmp((char *)dummyCSig, "J", 1) == 0)
                *(jlong *)arg->ptr 
                      = (jlong) (*env)->GetLongField(env, arg->myObject, arg->id);
              else
		*(jint *)arg->ptr
			= (jint) (*env)->GetLongField(env, arg->myObject, arg->id);			
	    }
	}
	sprintf (msg, "field = %s value(long) = %ld ",
	 	 arg->name,*(long *)arg->ptr);
	MYDEBUG("getJavaField", msg);
	break;

    case 'F':                    /* float */
	if (arg->isArray) {
	    arrayObject = (*env)->GetObjectField(env, arg->myObject, arg->id);
	    fbody = (*env)->GetFloatArrayElements(env, (jarray)arrayObject, 0);
	    memcpy (arg->ptr, fbody, sizeof(float)*arg->length);
	    (*env)->ReleaseFloatArrayElements(env,
					      (jarray)arrayObject, fbody, 0);
	} else {
	    *(float *)arg->ptr
	       = (*env)->GetFloatField(env, arg->myObject, arg->id);
	}
	break;

    case 'D':                    /* double */
	if (arg->isArray) {
	    arrayObject = (*env)->GetObjectField(env, arg->myObject, arg->id);
	    dbody = (*env)->GetDoubleArrayElements(env, (jarray)arrayObject, 0);
	    memcpy (arg->ptr, dbody, sizeof(double)*arg->length);
	    (*env)->ReleaseDoubleArrayElements(env, 
                                               (jarray)arrayObject, dbody, 0);
	} else {
	    *(double *)arg->ptr
	       = (*env)->GetDoubleField(env, arg->myObject, arg->id);
	}
	break;

    case 'L':                    /* object */
	if (strcmp(dummySig,"Ljava/lang/String;") == 0) { 
	  if (arg->isArray) { /* N/A */
	  } else {
	    arg->tempString  = 
		(*env)->GetObjectField(env, arg->myObject, arg->id);
	    if (arg->ptr != NULL) free (arg->ptr);
	    arg->ptr = (void *)
		(*env)->GetStringUTFChars(env, arg->tempString, 0);
/* Have to comment out the following line as it causes problems if the combined
   string is bigger than 2048, the size of msg buffer. */
/*	    sprintf(msg, "field = %s value(String) = %s",
		    arg->name,(char *)arg->ptr); */
	    MYDEBUG("getJavaField", msg);
	  }

	} else if (strcmp(dummySig, "Ljava/lang/Byte;") == 0) {
	    tempObject = (*env)->GetObjectField(env, arg->myObject, arg->id);
	    *(char *)arg->ptr 
		= (*env)->CallByteMethod(env, tempObject, BvalID);

	} else if (strcmp(dummySig, "Ljava/lang/Short;") == 0) {
	    tempObject = (*env)->GetObjectField(env, arg->myObject, arg->id);
	    if (strncmp((char *)dummyCSig, "S", 1) == 0) /* CDF_INT2 */
		*(short *)arg->ptr
			= (*env)->CallShortMethod(env, tempObject, SvalID);
	    else /* CDF_UINT1 */
		*(char *)arg->ptr
			= (jbyte) (*env)->CallShortMethod(env, tempObject, SvalID);

	} else if (strcmp(dummySig, "Ljava/lang/Integer;") == 0) {
	    tempObject = (*env)->GetObjectField(env, arg->myObject, arg->id);
	    if (strncmp((char *)dummyCSig, "I", 1) == 0) /* CDF_INT4 */
		*(jint *)arg->ptr
			= (jint) (*env)->CallIntMethod(env, tempObject, IvalID);
	    else /* CDF_UINT2 */
		*(short *)arg->ptr
			= (short) (*env)->CallIntMethod(env, tempObject, IvalID);

	} else if (strcmp(dummySig, "Ljava/lang/Long;") == 0) {
	    tempObject = (*env)->GetObjectField(env, arg->myObject, arg->id);
	    if (!arg->isEntryorVariableData) 
              *(long *)arg->ptr
                        = (long) (*env)->CallLongMethod(env, tempObject, LvalID);
            else {
              if (strncmp((char *)dummyCSig, "J", 1) == 0) /* CDF_INT8 */
		*(jlong *)arg->ptr  
			= (jlong) (*env)->CallLongMethod(env, tempObject, LvalID);
	      else /* CDF_UINT4 */
		*(jint *)arg->ptr
			= (jint) (*env)->CallLongMethod(env, tempObject, LvalID);
	    }
	} else if (strcmp(dummySig, "Ljava/lang/Float;") == 0) {
	    tempObject = (*env)->GetObjectField(env, arg->myObject, arg->id);
	    *(float *)arg->ptr
		= (*env)->CallFloatMethod(env, tempObject, FvalID);

	} else if (strcmp(dummySig, "Ljava/lang/Double;") == 0) {
	    tempObject = (*env)->GetObjectField(env, arg->myObject, arg->id);
	    *(double *)arg->ptr
		= (*env)->CallDoubleMethod(env, tempObject, DvalID);
	}
	break;

    case '[':	                 /* array */
	break;

    default:
	MYDEBUG("getJavaField","unknown type");
    }

    exc = (*env)->ExceptionOccurred(env);
    if (exc) {
	printf("Exception in getJavaField\n");
	(*env)->ExceptionDescribe(env);
	(*env)->ExceptionClear(env);
    }
}             /* getJavaField */

/* Function:  setJavaField
 *
 * Purpose:   Move the value out of the arg.ptr to the
 *            Java's instance variable.
 *
 * Parameters:
 *   env - Java VM pointer
 *   arg - An argument structure
 * Returns:
 *   none (Should be CDFStatus)
 *
 */
void setJavaField(JNIEnv *env, argument arg) {

    jthrowable     exc;
    jobject        tempObject;
    jmethodID      tempMethod;
    jlong          *jbody;
    jint           *ibody;
    jshort         *sbody;
    jbyte          *bbody;
    jfloat         *fbody;
    jdouble        *dbody;

    int    i;
    char   dummySig[50];
    char   dummyCSig[50];

    if (arg.isArray) MYDEBUG("setJavaField", "it is an array");
    if (arg.isEntryorVariableData) MYDEBUG("setJavaField", "it is an entry/variable data");

    if (arg.isArray)
	if (arg.isEntryorVariableData) {
	    strcpy(dummySig, arg.datasig+1);         /* Skip "[" */
	    strcpy(dummyCSig, arg.Csig+1);
	} else
	    strcpy(dummySig, arg.sig+1);	     /* Skip "[" */
    else
	if (arg.isEntryorVariableData) {
	    strcpy(dummySig, arg.datasig);
	    strcpy(dummyCSig, arg.Csig);
	} else
	    strcpy(dummySig, arg.sig);
    
    sprintf(msg, "dummySig = %s", dummySig);
    MYDEBUG("setJavaField", msg);
    switch (dummySig[0]) {
    case 'Z':                    /* boolean */
	break;

    case 'B':                    /* byte */
	if (arg.isArray) {
	    sprintf(msg,"setting byte array");
	    tempObject = (*env)->GetObjectField(env, arg.myObject, arg.id);
	    bbody =
		(*env)->GetByteArrayElements(env, (jbyteArray)tempObject, 0);
	    memcpy (bbody, (jbyte *) arg.ptr, arg.length);
	    (*env)->ReleaseByteArrayElements(env, tempObject, bbody, 0);
	} else {
	    sprintf(msg,"setting byte var");
	    (*env)->SetByteField(env, arg.myObject, arg.id, *(char *) arg.ptr);
	}
	break;

    case 'C':                    /* char */
	break;

    case 'S':                    /* short */
	if (arg.isArray) {
	    sprintf(msg,"setting short array");
	    tempObject = (*env)->GetObjectField(env, arg.myObject, arg.id);
	    sbody = (*env)->GetShortArrayElements(
                                  env, (jshortArray)tempObject, 0);
	    if (strncmp((char *)dummyCSig, "S", 1) == 0) {
		memcpy (sbody, (jbyte *)arg.ptr, sizeof(short)*arg.length);
	    } else {
		for (i=0; i< arg.length; i++) {
		   if (*((jbyte *) arg.ptr + i) >= 0)
		     sbody[i] = (jshort) *((jbyte *) arg.ptr + i);
		   else
		     sbody[i] = smax + (jshort) *((jbyte *) arg.ptr + i);
		}
	    }
	    (*env)->ReleaseShortArrayElements(env, tempObject, sbody, 0);
	} else {
	    sprintf(msg,"setting short var");
	    if (strncmp((char *)dummyCSig, "S", 1) == 0)
		(*env)->SetShortField (env, arg.myObject, arg.id,
					*(short *)arg.ptr);
	    else {
		jshort ss;
		if (*(jbyte *) arg.ptr >= 0)
		  ss = (jshort) *(jbyte *) arg.ptr;
		else
		  ss = smax + (jshort) *(jbyte *) arg.ptr;
	 	(*env)->SetShortField (env, arg.myObject, arg.id, ss);
	    }
	}
	break;

    case 'I':                    /* integer */
	if (arg.isArray) {
	    sprintf(msg,"setting int array");
	    tempObject = (*env)->GetObjectField(env, arg.myObject, arg.id);
	    ibody = (*env)->GetIntArrayElements(env, (jintArray)tempObject, 0);
	    if (strncmp((char *)dummyCSig, "I", 1) == 0) {
	      for (i=0; i< arg.length; i++) 
		ibody[i] = *((jint *) arg.ptr + i);
	    } else {
		for (i=0; i< arg.length; i++) {
		   if (*((jshort *) arg.ptr + i) >= 0)
		     ibody[i] = (jint) *((jshort *) arg.ptr + i);
		   else 
		     ibody[i] = imax + (jint) *((jshort *) arg.ptr + i);
		}
	    }
	    (*env)->ReleaseIntArrayElements(env, tempObject, ibody, 0);
	} else {
	    if (strncmp((char *)dummyCSig, "I", 1) == 0)
		(*env)->SetIntField (env, arg.myObject, arg.id,
				     *(jint *) arg.ptr);
	    else {
		jint ii = 0;
		if (*(jshort *) arg.ptr >= 0) 
		  ii = (jint) *(jshort *) arg.ptr;
		else
		  ii = imax + (jint) *(jshort *) arg.ptr;
		 (*env)->SetIntField (env, arg.myObject, arg.id, ii);
	    }
	}
	break;

    case 'J':                    /* long */
	if (arg.isArray) {
	    tempObject = (*env)->GetObjectField(env, arg.myObject, arg.id);
	    jbody = (*env)->GetLongArrayElements(env, (jlongArray)tempObject, 0);
            if (!arg.isEntryorVariableData) {
              for (i=0; i<arg.length; i++) {
                jbody[i] = *((long *) arg.ptr + i);
              }
	    } else {
                if (strncmp((char *)dummyCSig, "J", 1) == 0) {
                  for (i=0; i< arg.length; i++)
                    jbody[i] = *((jlong *) arg.ptr + i);
                } else {
		  for (i=0; i< arg.length; i++) {
		    if (*((jint *) arg.ptr + i) >= 0) 
			jbody[i] = (jlong) *((jint *) arg.ptr + i);
		    else 
			jbody[i] = jmax + (jlong) *((jint *) arg.ptr + i);
		  }
		}
	    }
	    (*env)->ReleaseLongArrayElements(env, tempObject, jbody, 0);
	} else {
	    sprintf(msg,"name = %s; sig  = %s; val  = %ld",
		    arg.name, arg.sig, *(long *) arg.ptr);
	    if (!arg.isEntryorVariableData) {
	 	(*env)->SetLongField (env, arg.myObject, arg.id, 
				      (jlong) (*(long *) arg.ptr));
	    } else {
                if (strncmp((char *)dummyCSig, "J", 1) == 0)
                  (*env)->SetLongField (env, arg.myObject, arg.id,
                                      (jlong) (*(jlong *) arg.ptr));
                else {
		  jlong jj = 0;
		  if (*(jint *) arg.ptr >= 0)
		    jj = (jlong) *(jint *) arg.ptr;
		  else
		    jj = jmax + (jlong) *(jint *) arg.ptr;
		  (*env)->SetLongField (env, arg.myObject, arg.id, jj);
		}
	    }
	}
	break;

    case 'F':                    /* float */
	if (arg.isArray) {
	    sprintf(msg,"setting float array");
	    tempObject = (*env)->GetObjectField(env, arg.myObject, arg.id);
	    fbody = (*env)->GetFloatArrayElements (env, 
                                                   (jfloatArray)tempObject, 0);
	    for (i=0; i<arg.length; i++) 
		fbody[i] = *((float *)arg.ptr + i);
	    (*env)->ReleaseFloatArrayElements(env, tempObject, fbody, 0);
	} else {
	    sprintf(msg,"setting float var");
	    (*env)->SetFloatField (env, arg.myObject, arg.id, 
				   (jfloat) *(float *) arg.ptr);
	}
	break;

    case 'D':                    /* double */
	if (arg.isArray) {
	    sprintf(msg,"setting double array");
	    tempObject = (*env)->GetObjectField(env, arg.myObject, arg.id);
	    dbody = (*env)->GetDoubleArrayElements(env, 
                                                   (jdoubleArray)tempObject, 0);
	    for (i=0; i<arg.length; i++) 
		dbody[i] = *((double *)arg.ptr + i);
	    (*env)->ReleaseDoubleArrayElements(env, tempObject, dbody, 0);
	} else {
	    sprintf(msg,"setting double var");
	    (*env)->SetDoubleField (env, arg.myObject, arg.id, 
				    (jdouble) *(double *) arg.ptr);
	}
	break;

    case 'L':                    /* object */
	if (strcmp(dummySig,"Ljava/lang/String;") == 0) {
          if (arg.ptr != NULL) {
	    ((char *)arg.ptr)[arg.length - 1] = '\0';
/* Have to comment out the following line as it causes problems if the combined 
   string is bigger than 2048, the size of msg buffer. */
/*	    sprintf(msg,"name = %s; sig = %s val = %s\n", 
		    arg.name, arg.sig, ((char *) arg.ptr)); */
	    (*env)->SetObjectField(env, arg.myObject, arg.id,
				   (*env)->NewStringUTF(env, (char *) arg.ptr));
          }
	} else if (strcmp(dummySig, "Ljava/lang/Byte;") == 0) {
	    sprintf(msg,"building byte object");
	    tempMethod = (*env)->GetMethodID(env, byteClass, "<init>", "(B)V");
	    tempObject = (*env)->NewObject(env, byteClass, tempMethod,
					   (jbyte) *(char *) arg.ptr);
	    (*env)->SetObjectField(env, arg.myObject, arg.id, tempObject);

	} else if (strcmp(dummySig, "Ljava/lang/Short;") == 0) {
	    sprintf(msg,"building short object");
	    tempMethod = (*env)->GetMethodID(env, shortClass, "<init>", "(S)V");
	    if (!arg.isEntryorVariableData || 
		strncmp((char *)dummyCSig, "S", 1) == 0) {
		tempObject = (*env)->NewObject(env, shortClass, tempMethod,
					       *(short *) arg.ptr);
	    } else {
		jshort ss = 0;
		if (*(jbyte *)arg.ptr >= 0)
		  ss = (jshort) *(jbyte *) arg.ptr;
		else
		  ss = smax + (jshort) *(jbyte *) arg.ptr;
		tempObject = (*env)->NewObject(env, shortClass, tempMethod, ss);
	    }
	    (*env)->SetObjectField(env, arg.myObject, arg.id, tempObject);

	} else if (strcmp(dummySig, "Ljava/lang/Integer;") == 0) {
	    sprintf(msg,"building int object");
	    tempMethod = (*env)->GetMethodID(env, intClass, "<init>", "(I)V");
	    if (!arg.isEntryorVariableData || 
		strncmp((char *)dummyCSig, "I", 1) == 0) {
		tempObject = (*env)->NewObject (env, intClass, tempMethod,
						*(jint *) arg.ptr);
	    } else {
		jint ii = 0;
		if (*(jshort *) arg.ptr >= 0)
		  ii = (jint) *(jshort *) arg.ptr;
		else
		  ii = imax + (jint) *(jshort *) arg.ptr;
		tempObject = (*env)->NewObject (env, intClass, tempMethod, ii);
	    }
	    (*env)->SetObjectField(env, arg.myObject, arg.id, tempObject);

	} else if (strcmp(dummySig, "Ljava/lang/Long;") == 0) {
	    sprintf(msg,"building long object");
	    tempMethod = (*env)->GetMethodID(env, longClass, "<init>", "(J)V");
	    if (!arg.isEntryorVariableData || 
		strncmp((char *)dummyCSig, "J", 1) == 0) {
		tempObject = (*env)->NewObject (env, longClass, tempMethod,
						(jlong) (*(jlong *) arg.ptr));
	    } else {
		jlong jj;
		if (*(jint*)arg.ptr >= 0) 
		   jj = (jlong) *(jint*)arg.ptr;
		else {
		   jj = jmax + (jlong) *(jint*)arg.ptr;
		}
		tempObject = (*env)->NewObject (env, longClass, tempMethod, jj);
	    }
	    (*env)->SetObjectField(env, arg.myObject, arg.id, tempObject);

	} else if (strcmp(dummySig, "Ljava/lang/Float;") == 0) {
	    sprintf(msg,"building float object");
	    tempMethod = (*env)->GetMethodID(env, floatClass, "<init>", "(F)V");
	    tempObject = (*env)->NewObject(env, floatClass, tempMethod,
					   (jfloat) *(float *) arg.ptr);
	    (*env)->SetObjectField(env, arg.myObject, arg.id, tempObject);

	} else if (strcmp(dummySig, "Ljava/lang/Double;") == 0) {
	    sprintf(msg,"building double object");
	    tempMethod = (*env)->GetMethodID(env, doubleClass, "<init>", "(D)V");
	    tempObject = (*env)->NewObject(env, doubleClass, tempMethod,
				           (jdouble) *(double *) arg.ptr);
	    (*env)->SetObjectField(env, arg.myObject, arg.id, tempObject);
	}
	break;

    case '[':	                 /* array */
	break;

    default:
	sprintf(msg, "Unknown type; sig =  %s",arg.sig);
    }
    MYDEBUG("setJavaField",msg);

    exc = (*env)->ExceptionOccurred(env);
    if (exc) {
	printf("Exception in setJavaField\n");
	(*env)->ExceptionDescribe(env);
	(*env)->ExceptionClear(env);
    }
}            /* setJavaField */


/* Function:  argAlloc
 *
 * Purpose: determines the size needed for the pointer and mallocs
 *          the appropriate space.
 *
 * Parameters:
 *   arg - the argument to malloc space for the variable
 * Returns:
 *   CDFStatus - success or failure
 */
CDFstatus argAlloc(JNIEnv *env, const char *signature, argument *arg) {

    int i;
    CDFstatus status = CDF_OK;

    if (arg->length == -1) arg->length = 1; 

    switch (signature[0]) {
    case 'Z':                    /* boolean */
	break;

    case 'B':                    /* byte */
	sprintf(msg,"allocated space for %d byte variable(s)", (int) arg->length);
	MYDEBUG("argAlloc",msg);
	arg->ptr = (char *) malloc (sizeof(char) * arg->length);
	for (i=0;i<arg->length;i++)
	    *((char *)arg->ptr + i) = 0;
	break;

    case 'C':                    /* char */
        sprintf(msg,"allocated space for %d char variable(s)", (int) arg->length);
        MYDEBUG("argAlloc",msg);
        arg->ptr = (char *) malloc (sizeof(char) * arg->length);
	break;

    case 'S':                    /* short */
	sprintf(msg,"allocated space for %d short variable(s)", (int) arg->length);
	MYDEBUG("argAlloc",msg);
	arg->ptr = (short *) malloc (sizeof(short) * arg->length);
	for (i=0; i < arg->length; i++)
	    *((short *)arg->ptr + i) = (short)0;
	break;

    case 'I':                    /* integer */
	sprintf(msg,"allocated space for %d int variable(s)", (int) arg->length);
	MYDEBUG("argAlloc",msg);
	arg->ptr = (jint *) malloc (sizeof(jint) * arg->length);
	for (i=0; i < arg->length; i++)
	    *((jint *)arg->ptr + i) = (jint) 0;
	break;

    case 'J':                    /* long */
	sprintf(msg,"allocated space for %d long variable(s)", (int) arg->length);
	MYDEBUG("argAlloc",msg);
        if (!arg->isEntryorVariableData) {
	  arg->ptr = (long *) malloc (sizeof(long) * arg->length);
	  for (i=0; i < arg->length; i++)
	    *((long *)arg->ptr + i) = (long) 0;
        } else {
          arg->ptr = (jlong *) malloc (sizeof(jlong) * arg->length);
          for (i=0; i < arg->length; i++)
            *((jlong *)arg->ptr + i) = (jlong) 0;
        }
	break;

    case 'F':                    /* float */
	sprintf(msg,"allocated space for %d float variable(s)", (int) arg->length);
	MYDEBUG("argAlloc",msg);
	arg->ptr = (float *) malloc (sizeof(float) * arg->length);
	for (i=0; i < arg->length; i++)
	    *((float *)arg->ptr + i) = 0.0;
	break;

    case 'D':                    /* double */
	sprintf(msg,"allocated space for %d double variable(s)", (int) arg->length);
	MYDEBUG("argAlloc",msg);
	arg->ptr = (double *) malloc (sizeof(double) * arg->length);
	for (i=0; i < arg->length; i++)
	    *((double *)arg->ptr + i) = 0;
	break;

    case 'L':                    /* object */
	if (strcmp(signature,"Ljava/lang/String;") == 0) {
	    sprintf(msg,"allocated space for a %d long string", (int) arg->length);
	    MYDEBUG("argAlloc",msg);
	    arg->ptr = (void *) malloc (sizeof(char) * (arg->length));

	} else if (strcmp(signature,"Ljava/lang/Byte;") == 0) {
	    sprintf(msg,"allocated space for %d byte object", (int) arg->length);
	    MYDEBUG("argAlloc",msg);
	    arg->ptr = (char *) malloc (sizeof(char) * arg->length);
	    for (i=0; i < arg->length; i++)
		*((char *)arg->ptr + i) = 0;

	} else if (strcmp(signature,"Ljava/lang/Short;") == 0) {
	    sprintf(msg,"allocated space for %d short object", (int) arg->length);
	    MYDEBUG("argAlloc",msg);
	    arg->ptr = (short *) malloc (sizeof(short) * arg->length);
	    for (i=0; i < arg->length; i++)
		*((short *)arg->ptr + i) = 0;

	} else if (strcmp(signature,"Ljava/lang/Integer;") == 0) {
	    sprintf(msg,"allocated space for %d int object", (int) arg->length);
	    MYDEBUG("argAlloc",msg);
	    arg->ptr = (jint *) malloc (sizeof(jint) * arg->length);
	    for (i=0; i < arg->length; i++)
		*((jint *)arg->ptr + i) = 0;

	} else if (strcmp(signature,"Ljava/lang/Long;") == 0) {
	    sprintf(msg,"allocated space for %d long object", (int) arg->length);
	    MYDEBUG("argAlloc",msg);
            if (!arg->isEntryorVariableData) {
	      arg->ptr = (long *) malloc (sizeof(long) * arg->length);
	      for (i=0; i < arg->length; i++)
		*((long *)arg->ptr + i) = (long) 0;
            } else {
	      arg->ptr = (jlong *) malloc (sizeof(jlong) * arg->length);
	      for (i=0; i < arg->length; i++)
		*((jlong *)arg->ptr + i) = (jlong) 0;
	    }

	} else if (strcmp(signature,"Ljava/lang/Float;") == 0) {
	    sprintf(msg,"allocated space for %d float object", (int) arg->length);
	    MYDEBUG("argAlloc",msg);
	    arg->ptr = (float *) malloc (sizeof(float) * arg->length);
	    for (i=0; i < arg->length; i++)
		*((float *)arg->ptr + i) = 0.0;

	} else if (strcmp(signature,"Ljava/lang/Double;") == 0) {
	    sprintf(msg,"allocated space for %d double object", (int) arg->length);
	    MYDEBUG("argAlloc",msg);
	    arg->ptr = (double *) malloc (sizeof(double) * arg->length);
	    for (i=0; i < arg->length; i++)
		*((double *)arg->ptr + i) = 0;
	}
	break;

    case '[':	                 /* array */
	arg->isArray = JNI_TRUE;
	arg->length = (*env)->GetArrayLength(env, 
			      (jarray)(*env)->GetObjectField(env,
							     arg->myObject,
							     arg->id));
	MYDEBUG("argAlloc", "calling self");
	status = argAlloc(env, signature+1, arg);       /* Recursive call */
	break;

    default:
	MYDEBUG("argAlloc","unknown type");
    }

    if (arg->ptr == NULL) {
	MYDEBUG("argAlloc","malloc failed");
	return(BAD_MALLOC);
    }
    
    return(status);
}

/* Function: cleanArgument
 *
 * Purpose:  clean up the memory and UTF strings used by the argument
 *
 * Parameters:
 *   env - The Java runtime environment
 *   arg - The argument to clean
 * Returns:
 *   none
 */
void cleanArgument(JNIEnv *env, argument *arg) {
  /* 
   * tempString is only used for certain String arguments.  If it was used
   * free it.
   */

    if ((!strcmp(arg->sig,"Ljava/lang/String;")) && arg->tempString != NULL) 
	/* a Java string variable like name or path */
	(*env)->ReleaseStringUTFChars(env, arg->tempString, (char *)arg->ptr); 
    else if (arg->datasig != NULL &&
		((!strcmp(arg->datasig,"Ljava/lang/String;")) && arg->tempString != NULL)) 
  /* a Java data which contains a string value */ 
	(*env)->ReleaseStringUTFChars(env, arg->tempString, (char *)arg->ptr);
	else /* other data type: a non-string Java variable or a non-string data object */
	free (arg->ptr); 

    /* Free the name */
    (*env)->ReleaseStringUTFChars(env, arg->nameObject, arg->name);
    (*env)->ReleaseStringUTFChars(env, arg->sigObject, arg->sig);
    if (arg->isEntryorVariableData) 
		(*env)->ReleaseStringUTFChars(env, arg->dsObject, arg->datasig);
    /* reinitialize to allow reuse */
    arg->ptr        = NULL;
    arg->isArray    = JNI_FALSE;
    arg->isEntryorVariableData    = JNI_FALSE;
    arg->length     = -1;
    arg->tempString = NULL;

}
/* Function: ErrHandle
 *
 * Purpose:  clean up the memory used by the argument(s) when
 *           an error that forces the function to return
 *
 * Parameters:
 *   status - The return status
 *   env - The Java runtime environment
 *   arg - The variable list of argument(s) to clean
 *   NULL - The end of variable argument list
 * Returns:
 *   status
 */
#if defined(STDARG)
static CDFstatus ErrHandle (CDFstatus status, JNIEnv *env, ...)
#else
static CDFstatus ErrHandle (CDFstatus status, JNIEnv *env, va_alist)
va_dcl
#endif
{
  va_list ap;
#if defined(STDARG)
  va_start (ap, env);
#else
  VA_START (ap);
#endif
  for (;;) {
     argument *arg = va_arg (ap, argument *);
     if (arg == NULL) {
       va_end (ap);
       return status;
     } else
       cleanArgument(env, arg);
  }
}


/* Function:  getItem
 *
 * Purpose:   Setup the argument structure for the obj
 *
 * Parameters:
 *    env - the JNI runtime environment
 *    obj - the object for which the argument is setup.
 *    parent - the parent of the obj
 *    arg - a pointer to a blank argument structure
 *
 * Returns:
 *    CDFstatus - success or failure
 */
CDFstatus getItem(JNIEnv *env, jobject theCDF, jobject itemObj, jobject parent, argument *arg) {

    jobject   temp1, temp2, temp3;
    jclass  parentClass = (*env)->GetObjectClass(env, parent);
    jmethodID 
	vElemID = (*env)->GetMethodID(env, vecClass, "elementAt",
				      "(I)Ljava/lang/Object;");
    jthrowable exc;
    CDFstatus  status;
    long       dataType;

    /********************/
    /*    Initialize    */ 
    /********************/
    arg->isArray    = JNI_FALSE;
    arg->tempString = NULL;
    arg->myObject   = parent;
    arg->datasig    = NULL;

    MYDEBUG("getItem", "completed initialization.");

    /*******************************************/
    /* Make sure that the argument is a Vector */
    /*******************************************/
    if (((*env)->IsInstanceOf(env, itemObj, vecClass)) == JNI_FALSE) {
	MYDEBUG("getItem","itemObj not a vector");
	return(BAD_FNC_OR_ITEM);
    }
    /**********************************************************/
    /* Get the name of the instance var out of the arg object */
    /**********************************************************/
    temp1 = (*env)->CallObjectMethod(env, itemObj, vElemID, 0);
    exc = (*env)->ExceptionOccurred(env);
    if (exc) {
	printf("Exception in getItem\n");
	(*env)->ExceptionDescribe(env);
	(*env)->ExceptionClear(env);
	return(BAD_FNC_OR_ITEM);
    }
    arg->nameObject  = temp1;
    arg->name = (*env)->GetStringUTFChars(env, arg->nameObject, 0);
    /*******************************************/
    /* Get the signature out of the arg object */
    /*******************************************/
    temp2 = (*env)->CallObjectMethod(env, itemObj, vElemID, 1);
    exc = (*env)->ExceptionOccurred(env);
    if (exc) {
	printf("Exception in getItem2\n");
	(*env)->ExceptionDescribe(env);
	(*env)->ExceptionClear(env);
	return(BAD_FNC_OR_ITEM);
    }
    arg->sigObject = temp2;
    arg->sig = (*env)->GetStringUTFChars(env, arg->sigObject, 0);
    sprintf(msg,"(1) name = %s sig = %s", arg->name, arg->sig);
    MYDEBUG("getItem", msg);

    /*********************************************/
    /* Get the field id of the instance variable */
    /*********************************************/
    arg->id   = (*env)->GetFieldID(env, parentClass, arg->name, arg->sig);
    CHECKNULL("field ID", arg->id);
    MYDEBUG("getItem", "got Field ID");

    arg->isEntryorVariableData = 
		(((*env)->IsAssignableFrom(env, dataClass, parentClass) &&
		  (strcmp(arg->name, "_data") == 0)) ||
		 ((*env)->IsAssignableFrom(env, entryClass, parentClass) &&
		  (strcmp(arg->name, "data") == 0)) ||
		 ((*env)->IsAssignableFrom(env, varClass, parentClass) &&
		  (strcmp(arg->name, "padValue") == 0)));
    if (arg->isEntryorVariableData) {
	MYDEBUG("getItem", "got entry/variable data/pad value");
	temp3 = 
	    (*env)->GetObjectField(env, parent,
				   (*env)->GetFieldID(env, parentClass,
						      "dataSignature",
						      "Ljava/lang/String;"));
	exc = (*env)->ExceptionOccurred(env);
	if (exc) {
	    printf("Exception in getItem 3\n");
	    (*env)->ExceptionDescribe(env);
	    (*env)->ExceptionClear(env);
	    return(BAD_FNC_OR_ITEM);
	}
	arg->dsObject = temp3;
	arg->datasig = (*env)->GetStringUTFChars(env, arg->dsObject, 0);
	sprintf(msg,"datasig = %s", arg->datasig);
	dataType = (long) (*env)->GetLongField(env, parent,
						(*env)->GetFieldID(env,
								parentClass,
								"dataType",
								"J"));
	if (dataType == CDF_CHAR || dataType == CDF_UCHAR) 
	  if (strncmp(arg->datasig, "[", 1) == 0) 
		strcpy(arg->Csig, "[B"); 
	  else 
		strcpy(arg->Csig, "Ljava/lang/String;");

	else if (dataType == CDF_BYTE || dataType == CDF_INT1 || 
		 dataType == CDF_UINT1)
	  if (strncmp(arg->datasig, "[", 1) == 0) 
		strcpy(arg->Csig, "[B");
	  else 
		strcpy(arg->Csig, "B");

	else if (dataType == CDF_INT2 || dataType == CDF_UINT2) 
	  if (strncmp(arg->datasig, "[", 1) == 0) 
		strcpy(arg->Csig, "[S");
	  else 
		strcpy(arg->Csig, "S");

	else if (dataType == CDF_INT4 || dataType == CDF_UINT4)
	  if (strncmp(arg->datasig, "[", 1) == 0) 
		strcpy(arg->Csig, "[I");
	  else 
		strcpy(arg->Csig, "I");

	else if (dataType == CDF_INT8 || dataType == CDF_TIME_TT2000)
	  if (strncmp(arg->datasig, "[", 1) == 0) 
		strcpy(arg->Csig, "[J");
	  else 
		strcpy(arg->Csig, "J");

	else if (dataType == CDF_REAL4 || dataType == CDF_FLOAT)
	  if (strncmp(arg->datasig, "[", 1) == 0) 
		strcpy(arg->Csig, "[F");
	  else 
		strcpy(arg->Csig, "F");

	else if (dataType == CDF_REAL8 || dataType == CDF_DOUBLE || 
		 dataType == CDF_EPOCH)
	  if (strncmp(arg->datasig, "[", 1) == 0) 
		strcpy(arg->Csig, "[D");
	  else 
		strcpy(arg->Csig, "D");

        else if (dataType == CDF_EPOCH16) {
          if (strncmp(arg->datasig, "[", 1) == 0)
                strcpy(arg->Csig, "[D");
          else {
                strcpy(arg->Csig, "D");
		arg->length = 2;
	  }
	}

	MYDEBUG("getItem", msg);
	if (strcmp(arg->datasig, "Ljava/lang/String;") == 0)
	  arg->length = 
             (jsize)(*env)->GetLongField(env, parent, 
                                         (*env)->GetFieldID(env, 
							    parentClass,
							    "numElements", 
							    "J")) 
             + 1;
	status = argAlloc(env, arg->Csig, arg);

	if (!CheckStatus("getItem", 0)) return(status);
        (*env)->ReleaseStringUTFChars(env, (jstring) temp3, 0);
	return (CDF_OK);
    }

    /**************************************************************/
    /*  Allocate the appropriate amount of space for the variable */
    /**************************************************************/

    if (strcmp(arg->name, "name") == 0 || strcmp(arg->name, "path") == 0) {
      if (arg->length == -1) { /* Have to use the length of the passed string */ 
	if (strcmp(arg->name, "name") == 0) 
	  temp3 = 
		(*env)->GetObjectField(env, parent,
                	                   (*env)->GetFieldID(env, parentClass,
                                                      "name",
                                                      "Ljava/lang/String;"));
        else
          temp3 =
                (*env)->GetObjectField(env, parent,
                                           (*env)->GetFieldID(env, parentClass,
                                                      "path",
                                                      "Ljava/lang/String;"));
        arg->length =
		(jsize)(*env)->GetStringUTFLength(env, (jstring) temp3);
	
	(*env)->ReleaseStringUTFChars(env, (jstring) temp3, 0);
      }
    }
    status = argAlloc(env, arg->sig, arg);
    if (!CheckStatus("getItem", 0)) return(status);
    
    return (CDF_OK);
}                          /* getItem */


/* Function:  addCDFid
 *
 * Purpose:   addCDFid adds a CDF id to the list of maintained CDF ids.
 *
 * Parameters:    
 *    id - the C CDFid to add to the list
 * Returns:   
 *    jid - the position in the CDF list where the id was added
 *
 * Note that items from the list may not be removed since they may cause the id of the
 * CDF object to become out of sync with the native list.
 *
 * If this is done, then all the CDF objects will have to be synced, which may be
 * difficult.
 *
 * Note that they may also be needed by the finalize method.
 */
jlong addCDFid(CDFid id) {
    
    cdfIDList *myListOfCDFs;
    jlong     i = 0;

    myListOfCDFs = listOfCDFs;

    if (listOfCDFs == NULL) {  /* It's the first one */
	myListOfCDFs = (cdfIDList *) malloc (sizeof(cdfIDList));
	listOfCDFs = myListOfCDFs;

    } else {                  /* Find the end of the list and then add it */
	while (myListOfCDFs->next != NULL) {
	    i++;
	    myListOfCDFs = myListOfCDFs->next;
	}
	i++;
	myListOfCDFs->next = (cdfIDList *) malloc (sizeof(cdfIDList));
	myListOfCDFs = myListOfCDFs->next;
    }
    myListOfCDFs->jid   = i;
    myListOfCDFs->nCDFs = (int)i+1;
    myListOfCDFs->id    = id;
    myListOfCDFs->open  = TRUE;
    myListOfCDFs->next  = NULL;

    return i;
}                  /* addCDFid */

/* Function: getCDFid
 *
 * Purpose:  Get the jid-th CDFid out of the list
 *
 * Parameters:
 *    jid - position id of the CDF link list 
 *
 * Returns:  
 *    id  - the C CDFid
 */
CDFid getCDFid (jlong jid) {

    cdfIDList  *myListOfCDFs;
    int        i = 0;

    myListOfCDFs = listOfCDFs;
    for (i=0; i < (int)jid; i++)
	myListOfCDFs = myListOfCDFs->next;
    return myListOfCDFs->id;     /* Acutal CDF id, not jid */
}                 /* getCDFid */

/* Function: removeCDFid
 *
 * Purpose:  Mark the jid-th CDFid on the list as closed when the CDF is closed
 *
 * Parameters:
 *    jid - position id of the CDF link list
 *
 */
void removeCDFid (jlong jid) {

    cdfIDList  *myListOfCDFs;
    int i;

    myListOfCDFs = listOfCDFs;
    for (i=0; i < (int)jid; i++)
        myListOfCDFs = myListOfCDFs->next;

    myListOfCDFs->open = FALSE;     /* Mark it as closed */
}                  /* removeCDFid */

/* Function: flipDataElements
 *
 * Purpose:  Shuffle data elements in records from row to column or column 
 *           to row for a multi-dimensional variable in a column-major CDF. 
 *
 * Parameters:
 *    buffer - buffer pointer for record data
 *    direction - indicator, 1 for column-to-row, 2 for row-to-column
 *
 */
CDFstatus flipDataElements (char * buffer, jint direction) {

    long majority, numdims, dimSizes[CDF_MAX_DIMS], reccount;
    long dimVarys[CDF_MAX_DIMS];
    long dimcounts[CDF_MAX_DIMS];
    int ix;
    int toflip;
    CDFstatus statuz;
    long numdimsx;
    statuz = CDFlib (GET_, CDF_MAJORITY_, &majority,
                           zVAR_NUMDIMS_, &numdims,
                           zVAR_DIMSIZES_, dimSizes,
                           zVAR_DIMVARYS_, dimVarys,
                     CONFIRM_, zVAR_RECCOUNT_, &reccount,
                               zVAR_DIMCOUNTS_, dimcounts,
                     NULL_);
    if (statuz != CDF_OK) return(statuz);
    numdimsx = 0;
    for (ix = 0; ix < (int) numdims; ix++) {
      if (dimVarys[ix]) {
        dimSizes[numdimsx] = dimSizes[ix];
        ++numdimsx;
      }
    }
    toflip = 1;
    if (majority == ROW_MAJOR) toflip = 0;
    else if (numdimsx < 2) toflip = 0;
    else if (reccount < 1) toflip = 0;
    else {
      for (ix = 0; ix < (int) numdimsx; ix++) { 
        if (dimSizes[ix] != dimcounts[ix]) {
          toflip = 0;
          break;
        }
      }
    } 
    if (toflip == 1) {
      long NvalueBytes, numelems, datatype;
      long nValuesPerRecord = 1L, nBytesPerRecord;
      char *tmpbuf;
      statuz = CDFlib (GET_, zVAR_DATATYPE_, &datatype,
                             zVAR_NUMELEMS_, &numelems,
                       NULL_);
      if (statuz != CDF_OK) return(statuz);

      for (ix = 0; ix < (int) numdims; ix++) {
        if (dimVarys[ix] && dimcounts[ix] > 0)
          nValuesPerRecord *= dimcounts[ix];
      }
      NvalueBytes = numelems * CDFelemSize(datatype);
      nBytesPerRecord = nValuesPerRecord * NvalueBytes;
      tmpbuf = (char *) malloc ((size_t) nBytesPerRecord *
                                reccount);
      for (ix = 0; ix < (int) reccount; ix++) {
         size_t offset = (size_t) (ix * nBytesPerRecord);
         if (direction == 1) 
           COLtoROW (buffer + offset, tmpbuf + offset,
                     numdimsx, dimcounts, NvalueBytes);
         else
           ROWtoCOL (buffer + offset, tmpbuf + offset,
                     numdimsx, dimcounts, NvalueBytes);

      }
      memmove (buffer, tmpbuf,
               (size_t) reccount * nBytesPerRecord);
      free (tmpbuf);
    }
    return (CDF_OK);
}                  /* flipDataElements */

/* Function: cleanGlobalRefs
 *
 * Purpose:  Clear the global references if no CDFs are still open. In this 
 *           case, the CDFid link list are also cleared.
 *
 * Parameters:
 *    env - the Java runtime environment 
 *
 */
void cleanGlobalRefs (JNIEnv *env) {

    cdfIDList  *myListOfCDFs;

    if (listOfCDFs == NULL) return;
    /* Check if there is still at least one open CDF. If so, then return. */ 
    if (listOfCDFs->open) return;
    myListOfCDFs = listOfCDFs;
    while (myListOfCDFs->next !=  NULL) {
	myListOfCDFs = myListOfCDFs->next;
	if (myListOfCDFs->open) return;
    }
   
    /* All CDFs on the link list are closed. So  */ 
    /* clear the link list and global references */

    while (listOfCDFs->next !=  NULL) {
	myListOfCDFs = listOfCDFs->next;
	free (listOfCDFs);
	listOfCDFs = myListOfCDFs;
    }
    free (listOfCDFs);
    listOfCDFs = NULL;
    (*env)->DeleteGlobalRef(env, cdfClass);
    (*env)->DeleteGlobalRef(env, varClass);
    (*env)->DeleteGlobalRef(env, entryClass);
    (*env)->DeleteGlobalRef(env, attrClass);
    (*env)->DeleteGlobalRef(env, dataClass);
    (*env)->DeleteGlobalRef(env, excClass);

    (*env)->DeleteGlobalRef(env, byteClass);
    (*env)->DeleteGlobalRef(env, shortClass);
    (*env)->DeleteGlobalRef(env, intClass);
    (*env)->DeleteGlobalRef(env, longClass);
    (*env)->DeleteGlobalRef(env, floatClass);
    (*env)->DeleteGlobalRef(env, doubleClass);
    (*env)->DeleteGlobalRef(env, strClass);
    (*env)->DeleteGlobalRef(env, objClass);
    (*env)->DeleteGlobalRef(env, vecClass);

    cdfClass = 0;
    MYDEBUG("cleanGlobalRefs", "Global Refs deleted.");
    return;
}                 /* cleanGlobalRefs */

/*  Function:  JAVAcdflib
 *  Purpose:   JAVAcdflib is the main wrapper to CDFlib calls
 *
 *  Parameters:
 *     env - the JNI runtime environment
 *     obj - the CDFObject on which the command is to be executed.
 *     cmdVector - the CDF cmd wrapped in a java.long.Vector
 *     jid - the java CDF id which contains the CDFObject
 *  Returns:
 *     status - the return status of the CDFlib call
 */
CDFstatus JAVAcdflib (JNIEnv *env,
		      jobject theCDF, 
		      jobject obj, 
                      jobject cmdVector,      /* Command vector to process */
                      long jid,               /* CDF id in the link list   */
                      long function) {        /* Function to be processed  */

    jclass
	objClass   = (*env)->GetObjectClass(env, obj);
    jmethodID
	vSizeID  = (*env)->GetMethodID(env, vecClass, "size", "()I"),
	vElemID  = (*env)->GetMethodID(env, vecClass, "elementAt", "(I)Ljava/lang/Object;");

    jobject     nextItem;
    jthrowable  exc;
    CDFid       id;                     /* Only need for the close method.  */
    CDFid       oid;
    CDFstatus   status, status2;
    
    int      i;
    char     *CDFname;
    argument arg, arg2, arg3, arg4, arg5, arg6, arg7, arg8;
    long     item,                  /* Item (e.g. CDF_, ATTR_, etc.)       */ 
             func;                  /* Function (e.g. CREATE_, OPEN_, etc. */
    jlong    newID;                 /* Position id in the link list        */

    /****************************************/
    /* Get the value of the passed function */
    /****************************************/
    func = function;

    MYDEBUG("cdflib",">>>>>>>>>>>>>>>>>>>>>>");
    strcpy(msg, "vecClass is");
    if (vecClass == NULL)
	strcat(msg, " null!?!");
    else
	strcat(msg, " ok!");
    MYDEBUG("cdflib", msg);
    
    /**********************************************************************/
    /* Since the first item is processed by the calling routine, skip the */
    /* command and start processing item and its arguments                */
    /**********************************************************************/
    i = 1;     

    /***********************************************************************/
    /*  Process until there are no more items to process in the cmdVector  */ 
    /***********************************************************************/
 
    arg.length = -1; 

    while (func != NULL_) {
	switch (func) {

	case OPEN_:             /* Open CDF */
	    nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
	    if (isItem(nextItem)) {

		status = getFuncOrItem(env, nextItem, &item);
  		if (!CheckStatus("Java getFuncOrItem", 0)) return(status);
		sprintf(msg,"func = %ld item = %ld status = %ld",func,item,status);
		MYDEBUG("cdflib",msg);

                /************************/
		/* Get the CDF filename */
                /************************/
		nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
		arg.length = -1; /* Actual path length is determined in getItem */
		status = getItem(env, theCDF, nextItem, obj, &arg);
		if (!CheckStatus("Java getItem", 0)) return(status);
		getJavaField (env, &arg);       
		sprintf(msg,"filename = %s", ((char *) arg.ptr));
		MYDEBUG("cdflib", msg);

                /**************/
		/* Get the id */
                /**************/
		arg2.length = 1;
		nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
		status = getItem(env, theCDF, nextItem, obj, &arg2);
		if (!CheckStatus("Java getItem", 0)) return(status);
		
                /**************************************************/
		/* Perform the OPEN_ and place the CDF in zMODE/2 */
                /**************************************************/
		status = CDFlib(OPEN_, item, (char *)arg.ptr, &oid,
				NULL_);
		if (!CheckStatus("cdflib (OPEN_, CDF_)", 1))
		  return (ErrHandle(status, env, &arg, &arg2, NULL));

                /********************************************************/
		/* Set the CDF id returned by CDFlib (i.e. oid) and add */
                /* the position id of the link list (i.e. newID).       */
                /********************************************************/
		newID = addCDFid(oid);
		(*env)->SetLongField(env, obj, arg2.id, newID);

		cleanArgument(env, &arg);
		cleanArgument(env, &arg2);

		MYDEBUG("cdflib", "Getting nextItem");
		nextItem   = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
		exc = (*env)->ExceptionOccurred(env);
		if (exc) {
		    printf("Exception in cdflib\n");
		    (*env)->ExceptionDescribe(env);
		    (*env)->ExceptionClear(env);
		}
		CHECKNULL("nextItem",nextItem);
		status2 = getFuncOrItem(env, nextItem, &item);
	    } else {
		(*env)->ThrowNew(env, excClass, "Bad function or item");
	    }
	    break;           /* end OPEN_ */
	    
	case CLOSE_:                     /* Special case: only close CDFs */
	    MYDEBUG("cdflib","closing the file");
            /********************************************/
	    /* NEW Get the jid-th CDFid out of the list */
            /********************************************/
	    id = getCDFid(jid);
	    nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);

	    status = CDFlib(SELECT_, CDF_, id, 
			    CLOSE_, CDF_,
			    NULL_);

	    if (!CheckStatus("cdflib (CLOSE_, CDF_)", 1)) return(status);
	    
	    removeCDFid(jid);		
	    MYDEBUG("cdflib", "Getting nextItem");
	    nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
	    exc = (*env)->ExceptionOccurred(env);
	    if (exc) {
		printf("Exception in cdflib\n");
		(*env)->ExceptionDescribe(env);
		(*env)->ExceptionClear(env);
	    }
	    CHECKNULL("nextItem",nextItem);
	    status2 = getFuncOrItem(env, nextItem, &item);
	    cleanGlobalRefs(env);
	    break;      /* end CLOSE_ */

	case CONFIRM_:
	    nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
	    if (isItem(nextItem)) {
		status2 = getFuncOrItem(env, nextItem, &item);

		while (item < NULL_) {
		    sprintf (msg,"func = %ld item = %ld status = %ld",
                             func,item,status2);
		    MYDEBUG("cdflib",msg);

		    switch(item) {

                    case CDF_READONLY_MODE_:
                    case CDF_DECODING_:
                    case CDF_CACHESIZE_:
                    case CDF_NEGtoPOSfp0_MODE_:
                    case CDF_zMODE_:
                    case COMPRESS_CACHESIZE_:
                    case STAGE_CACHESIZE_:
                    case gENTRY_:
                    case zENTRY_:
                    case ATTR_:
                    case zVAR_RECNUMBER_:
                    case zVAR_RECCOUNT_:
                    case zVAR_RECINTERVAL_:
                    case zVAR_CACHESIZE_:
		    case zVAR_RESERVEPERCENT_:

                        nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
                        status = getItem(env, theCDF, nextItem, obj, &arg);
                        if (!CheckStatus("cdflib (CONFIRM_ 1)", 0)) return(status);

                        getJavaField(env, &arg);

                        sprintf (msg,"confirming item %ld, id = %ld",
                                 item,*(long *)arg.ptr);
                        MYDEBUG("cdflib",msg);

                        status = CDFlib(CONFIRM_, item, (long *)arg.ptr,
                                        NULL_);
                        if (!CheckStatus("cdflib (CONFIRM_ 2)", 1))
			  return(ErrHandle(status, env, &arg, NULL));

			setJavaField(env, arg);
                        cleanArgument(env, &arg);
                        break;

		    case zENTRY_EXISTENCE_:

			nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
			status = getItem(env, theCDF, nextItem, obj, &arg);
			if (!CheckStatus("cdflib (CONFIRM_)", 0)) return(status);

			getJavaField(env, &arg);
			
			sprintf (msg,"confirming item %ld, id = %ld",
                                 item,*(long *)arg.ptr);
			MYDEBUG("cdflib",msg);

			status = CDFlib(CONFIRM_, item, *(long *)arg.ptr, 
                                        NULL_);
			if (!CheckStatus("cdflib (CONFIRM_)", 1))
			  return(ErrHandle(status, env, &arg, NULL));

			setJavaField(env, arg);
			cleanArgument(env, &arg);
			break;

		    case ATTR_EXISTENCE_:
			nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
			status = getItem(env, theCDF, nextItem, obj, &arg);
			if (!CheckStatus("cdflib (CONFIRM_)", 0)) return(status);

			getJavaField(env, &arg);     /* Attribute name */
			
			sprintf (msg,"confirming item %ld, id = %ld",
                                 item,*(long *)arg.ptr);
			MYDEBUG("cdflib",msg);
			status = CDFlib(CONFIRM_, item, (char *)arg.ptr, 
                                        NULL_);
			if (!CheckStatus("cdflib (CONFIRM_)", 1))
			  return(ErrHandle(status, env, &arg, NULL));

			setJavaField(env, arg);
			cleanArgument(env, &arg);
			break;

                    case zVAR_PADVALUE_:
		    case CDF_CHECKSUM_:
                        
                        status = CDFlib(CONFIRM_, item, 
                                        NULL_);

			if (!CheckStatus("cdflib (CONFIRM_)", 1)) return(status);
                        break;

		    default:
			MYDEBUG("cdflib","Can't deal with it yet");
			break;
		    } /* switch(item) */

		    MYDEBUG("cdflib", "Getting nextItem");
		    nextItem   = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
		    exc = (*env)->ExceptionOccurred(env);
		    if (exc) {
			printf("Exception in cdflib2\n");
			(*env)->ExceptionDescribe(env);
			(*env)->ExceptionClear(env);
		    }
		    CHECKNULL("nextItem",nextItem);
		    status2 = getFuncOrItem(env, nextItem, &item);
		}
	    } else {
		(*env)->ThrowNew(env, excClass, "Bad function or item");
	    }
	    break; /* end CONFIRM_ */

	case CREATE_:

	    nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
	    if (isItem(nextItem)) {
		status2 = getFuncOrItem(env, nextItem, &item);
		while (item < NULL_) {
		    sprintf (msg,"func = %ld item = %ld status = %ld",
                             func,item,status2);
		    MYDEBUG("cdflib",msg);

		    switch(item) {

		    case ATTR_:
			arg.length = -1; /* Actual name length is determined in getItem */  
			arg2.length = arg3.length = -1;

			nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
			status = getItem(env, theCDF, nextItem, obj, &arg);
			if (!CheckStatus("cdflib (CREATE_,ATTR_) 1", 0)) return(status);
			nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
			status = getItem(env, theCDF, nextItem, obj, &arg2);
			if (!CheckStatus("cdflib (CREATE_,ATTR_) 2", 0)) return(status);
			nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
			status = getItem(env, theCDF, nextItem, obj, &arg3);
			if (!CheckStatus("cdflib (CREATE_,ATTR_) 3", 0)) return(status);

			getJavaField(env, &arg);       /* Attr name  */
			getJavaField(env, &arg2);      /* Attr scope */ 

			status = CDFlib (CREATE_, ATTR_, (char *)arg.ptr, 
					                 *(long *)arg2.ptr,
					                 arg3.ptr, 
					 NULL_);
			if (!CheckStatus("cdflib (CREATE_,ATTR_) 4", 1))
			  return (ErrHandle(1, env, &arg, &arg2, &arg3, NULL));

			setJavaField (env,arg3);     /* Set the value of attrNum */

			cleanArgument(env, &arg);
			cleanArgument(env, &arg2);
			cleanArgument(env, &arg3);
			break;

		    case CDF_:
			arg.length = -1; /* Actual path length is determined in getItem */
			arg2.length = arg3.length = arg4.length = -1;

			nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
			status = getItem(env, theCDF, nextItem, obj, &arg);
			if (!CheckStatus("cdflib (CREATE_,CDF_) 1", 0)) return(status);
			nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
			status = getItem(env, theCDF, nextItem, obj, &arg2);
			if (!CheckStatus("cdflib (CREATE_,CDF_) 2", 0)) return(status);
			nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
			status = getItem(env, theCDF, nextItem, obj, &arg3);
			if (!CheckStatus("cdflib (CREATE_,CDF_) 3", 0)) return(status);
			nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
			status = getItem(env, theCDF, nextItem, obj, &arg4);
			if (!CheckStatus("cdflib (CREATE_,CDF_) 4", 0)) return(status);
			getJavaField(env, &arg);      /* char *CDFname   */
			getJavaField(env, &arg2);     /* long numDims    */
			getJavaField(env, &arg3);     /* long dimSizes[] */

			/****************************************************
			 * When creating CDF files, the name should not have 
                         * the ".cdf" or ".CDF" extension at the end.  Check
			 * the name (in arg.ptr) to see if ".cdf" or ".CDF" 
			 * is present and remove it if it is.
			 ***************************************************/

			if (strcmp((char *)arg.ptr+arg.length-4,".cdf") == 0 ||
			    strcmp((char *)arg.ptr+arg.length-4,".CDF") == 0) {
			    CDFname = malloc(arg.length - 4 + 1);
			    if (CDFname == NULL) return(BAD_MALLOC);
			    memmove(CDFname, arg.ptr, arg.length-4);
			    CDFname[arg.length-4] = NUL;
			} else {
			    CDFname = malloc(arg.length+1);
			    if (CDFname == NULL) return(BAD_MALLOC);
			    strcpy(CDFname, arg.ptr);
			}

			status = CDFlib(CREATE_, CDF_, CDFname, 
					               *(long *)arg2.ptr,
					               arg3.ptr, 
					               &oid,    /* out-CDFid */
					NULL_);
			free(CDFname);
			if (!CheckStatus("cdflib (CREATE_,CDF_) 5", 1))
			  return (ErrHandle(status, env, &arg, &arg2, &arg3,
					    &arg4, NULL));

            /****************************************************/
            /* Add the oid (CDF id) returned by the CDFlib call */ 
            /* to the link list.                                */
            /****************************************************/
			newID = addCDFid(oid);

                        /********************************/
                        /* Set the CDF id field in Java */
                        /********************************/
			*(long *)arg4.ptr = (long) newID;

			setJavaField(env,arg4);       

			cleanArgument(env, &arg);
			cleanArgument(env, &arg2);
			cleanArgument(env, &arg3);
			cleanArgument(env, &arg4);
			break;

		    case zVAR_:
			arg.length = -1; /* Actual name length is determined in getItem */
			arg2.length = arg3.length = arg4.length = arg5.length = 
			    arg6.length = arg7.length = arg8.length = -1;

			nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
			status = getItem(env, theCDF, nextItem, obj, &arg);
			if (!CheckStatus("cdflib (CREATE_,VAR_) 1", 0)) return(status);
			nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
			status = getItem(env, theCDF, nextItem, obj, &arg2);
			if (!CheckStatus("cdflib (CREATE_,VAR_) 2", 0)) return(status);
			nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
			status = getItem(env, theCDF, nextItem, obj, &arg3);
			if (!CheckStatus("cdflib (CREATE_,VAR_) 3", 0)) return(status);
			nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
			status = getItem(env, theCDF, nextItem, obj, &arg4);
			if (!CheckStatus("cdflib (CREATE_,VAR_) 4", 0)) return(status);
			nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
			status = getItem(env, theCDF, nextItem, obj, &arg5);
			if (!CheckStatus("cdflib (CREATE_,VAR_) 5", 0)) return(status);
			nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
			status = getItem(env, theCDF, nextItem, obj, &arg6);
			if (!CheckStatus("cdflib (CREATE_,VAR_) 6", 0)) return(status);
			nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
			status = getItem(env, theCDF, nextItem, obj, &arg7);
			if (!CheckStatus("cdflib (CREATE_,VAR_) 7", 0)) return(status);
			nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
			status = getItem(env, theCDF, nextItem, obj, &arg8);
			if (!CheckStatus("cdflib (CREATE_,VAR_) 8", 0)) return(status);

			getJavaField(env, &arg);      /* char *varName     */
			getJavaField(env, &arg2);     /* jlong dataType    */
			getJavaField(env, &arg3);     /* jlong numElements */
			getJavaField(env, &arg4);     /* jlong numDims     */
			getJavaField(env, &arg5);     /* jlong dimSizes[]  */
			getJavaField(env, &arg6);     /* jlong recVary     */
			getJavaField(env, &arg7);     /* jlong dimVary[]   */
			status = CDFlib(CREATE_, zVAR_, (char *)arg.ptr, 
					                *(long *)arg2.ptr,
					                *(long *)arg3.ptr,
					                *(long *)arg4.ptr, 
					                (long *) arg5.ptr, 
					                *(long *)arg6.ptr,
					                (long *) arg7.ptr, 
					                arg8.ptr,  /* varNum */
					NULL_);
			if (!CheckStatus("cdflib (CREATE_,VAR_) 9", 1))
			  return (ErrHandle(1, env, &arg, &arg2, &arg3,
					    &arg4, &arg5, &arg6, &arg7, &arg8,
					    NULL));

			setJavaField(env,arg8);    /* Set the variable number */
			cleanArgument(env, &arg);
			cleanArgument(env, &arg2);
			cleanArgument(env, &arg3);
			cleanArgument(env, &arg4);
			cleanArgument(env, &arg5);
			cleanArgument(env, &arg6);
			cleanArgument(env, &arg7);
			cleanArgument(env, &arg8);
			break;

		    default:
			(*env)->ThrowNew(env, excClass, "Bad function or item");
			break;
		    } /* switch(item) */
		    
		    MYDEBUG("cdflib", "Getting nextItem");
		    nextItem   = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
		    exc = (*env)->ExceptionOccurred(env);
		    if (exc) {
			printf("Exception in cdflib2\n");
			(*env)->ExceptionDescribe(env);
			(*env)->ExceptionClear(env);
		    }
		    CHECKNULL("nextItem",nextItem);
		    status2 = getFuncOrItem(env, nextItem, &item);
		}
	    } else {
		(*env)->ThrowNew(env, excClass, "Bad function or item");
	    }
	    break;          /* end CREATE_ */

	case DELETE_:
	    nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
	    if (isItem(nextItem)) {
		status2 = getFuncOrItem(env, nextItem, &item);

		while (item < NULL_) {
		    sprintf(msg,"func = %ld item = %ld status = %ld",func,item,
                            status2);
		    MYDEBUG("cdflib",msg);

		    switch(item) {

		    case ATTR_:
		    case CDF_:
		    case gENTRY_:
		    case zENTRY_:
		    case zVAR_:
			status = CDFlib(DELETE_, item, NULL_);
			if (!CheckStatus("cdflib (DELETE_) 1", 1)) return(status);
			if (item == CDF_) removeCDFid(jid);
			break;

                    case zVAR_RECORDS_:
                    case zVAR_RECORDS_RENUMBER_:
                        arg.length = arg2.length = -1;
                        nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
                        status = getItem(env, theCDF, nextItem, obj, &arg);
                        if (!CheckStatus("cdflib (DELETE_, VAR_RECORDS_) 1", 0))
                            return(status);
                        nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
                        status = getItem(env, theCDF, nextItem, obj, &arg2);
                        if (!CheckStatus("cdflib (DELETE_,VAR_RECORDS_) 2", 0))
                            return(status);

                        getJavaField(env, &arg);     /* first record number */
			getJavaField(env, &arg2);    /* last record number */

                        status = CDFlib(DELETE_, item, *(long *)arg.ptr,
                                                       *(long *)arg2.ptr,
                                        NULL_);
                        if (!CheckStatus("cdflib (DELETE_, VAR_RECORDS_) 3", 1))
                            return(ErrHandle(status, env, &arg, &arg2, NULL));

                        cleanArgument(env, &arg);
			cleanArgument(env, &arg2);
			break;
		    default:
			(*env)->ThrowNew(env, excClass, "Function not implemented yet. Contact the CDFJava API developer for requests/questions");
			break;
		    } /* switch(item) */

		    MYDEBUG("cdflib", "Getting nextItem");
		    nextItem   = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
		    exc = (*env)->ExceptionOccurred(env);
		    if (exc) {
			printf("Exception in cdflib2\n");
			(*env)->ExceptionDescribe(env);
			(*env)->ExceptionClear(env);
		    }
		    CHECKNULL("nextItem",nextItem);
		    status2 = getFuncOrItem(env, nextItem, &item);
		}
	    } else {
		(*env)->ThrowNew(env, excClass, "Bad function or item");
	    }
	    break; /* end DELETE_ */

	case SAVE_:
	    nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
	    if (isItem(nextItem)) {
		status2 = getFuncOrItem(env, nextItem, &item);

		while (item < NULL_) {
		    sprintf(msg,"func = %ld item = %ld status = %ld",func,item,
                            status2);
		    MYDEBUG("cdflib",msg);

		    switch(item) {

		      case CDF_:
			status = CDFlib(SAVE_, item, NULL_);
			if (!CheckStatus("cdflib (SAVE_) 1", 1)) return(status);
			break;
		      default:
			(*env)->ThrowNew(env, excClass, "Function not implemented yet. Contact the CDFJava API developer for requests/questions");
			break;
		    } /* switch(item) */
                    MYDEBUG("cdflib", "Getting nextItem");
                    nextItem   = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
                    exc = (*env)->ExceptionOccurred(env);
                    if (exc) {
                        printf("Exception in cdflib2\n");
                        (*env)->ExceptionDescribe(env);
                        (*env)->ExceptionClear(env);
                    }
                    CHECKNULL("nextItem",nextItem);
                    status2 = getFuncOrItem(env, nextItem, &item);
		}
	    } else {
		(*env)->ThrowNew(env, excClass, "Bad function or item");
	    }
	    break; /* end SAVE_ */

	case GET_:
	    nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
	    if (isItem(nextItem)) {
		status2 = getFuncOrItem(env, nextItem, &item);

		while (item < NULL_) {

		    sprintf(msg,"func = %ld item = %ld status = %ld",func,item,
                            status2);
		    MYDEBUG("cdflib",msg);

		  switch(item) {

		    case ATTR_NAME_:
		    case ATTR_SCOPE_:
		    case ATTR_MAXgENTRY_:
		    case ATTR_MAXzENTRY_:
		    case ATTR_NUMgENTRIES_:
		    case ATTR_NUMzENTRIES_:
		    case CDF_COPYRIGHT_:
		    case CDF_ENCODING_:
		    case CDF_FORMAT_:
		    case CDF_INCREMENT_:
		    case CDF_MAJORITY_:
		    case CDF_NUMATTRS_:
		    case CDF_NUMgATTRS_:
		    case CDF_NUMvATTRS_:
		    case CDF_NUMrVARS_:
		    case CDF_NUMzVARS_:
		    case CDF_VERSION_:
		    case CDF_RELEASE_:
		    case CDF_CHECKSUM_:
		    case CDF_LEAPSECONDLASTUPDATED_:
		    case gENTRY_DATATYPE_:
		    case gENTRY_NUMELEMS_:
		    case zENTRY_DATATYPE_:
		    case zENTRY_NUMELEMS_:
		    case zVAR_NAME_:
		    case zVAR_NUMELEMS_:
		    case zVAR_DATATYPE_:
		    case zVAR_NUMDIMS_:
		    case zVAR_RECVARY_:
		    case zVAR_DIMVARYS_:
		    case zVAR_DIMSIZES_:
		    case zVAR_MAXREC_:
		    case zVAR_BLOCKINGFACTOR_:
		    case zVAR_MAXallocREC_:
		    case zVAR_NUMallocRECS_:
		    case zVAR_NUMRECS_:
		    case zVAR_SPARSERECORDS_:
		    case zVARs_MAXREC_:
		    case LIB_COPYRIGHT_:
		    case LIB_INCREMENT_:
		    case LIB_RELEASE_:
		    case LIB_VERSION_:
		    case LIB_subINCREMENT_:
			if (item == CDF_COPYRIGHT_ || item == LIB_COPYRIGHT_)
			    arg.length = CDF_COPYRIGHT_LEN + 1;
			if (item == ATTR_NAME_) {
			    if (lib_ver == 2 || CDFgetFileBackward())
			      arg.length = CDF_ATTR_NAME_LEN + 1;
			    else
			      arg.length = CDF_ATTR_NAME_LEN256 + 1;
			}
			if (item == zVAR_NAME_) {
			    if (lib_ver == 2 || CDFgetFileBackward())
			      arg.length = CDF_VAR_NAME_LEN + 1;
			    else
			      arg.length = CDF_VAR_NAME_LEN256 + 1;
                        }
			if (item == LIB_subINCREMENT_)
                            arg.length = 2;

			nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);

			status = getItem(env, theCDF, nextItem, obj, &arg);
			if (!CheckStatus("cdflib (GET_) 1", 0)) return(status);

			status = CDFlib(GET_, item, arg.ptr, NULL_);
			if (!CheckStatus("cdflib (GET_) 2", 1))
			  return(ErrHandle(status, env, &arg, NULL));

			setJavaField(env,arg);
			cleanArgument(env, &arg);

			break;

		    case ATTR_NUMBER_:
			if (lib_ver == 2 || CDFgetFileBackward())
			  arg.length = CDF_ATTR_NAME_LEN256+1;
			else
			  arg.length = CDF_ATTR_NAME_LEN+1;
			arg2.length = -1;
			nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
			status = getItem(env, theCDF, nextItem, obj, &arg);
			if (!CheckStatus("cdflib (GET_, ATTR_NUMBER_) 1", 0)) 
			    return(status);
			nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
			status = getItem(env, theCDF, nextItem, obj, &arg2);
			if (!CheckStatus("cdflib (GET_,ATTR_NUMBER_) 2", 0)) 
			    return(status);

			getJavaField(env, &arg);     /* Attribute name */

			status = CDFlib(GET_, ATTR_NUMBER_, (char *)arg.ptr, 
					                    arg2.ptr,
					NULL_);
			if (!CheckStatus("cdflib (GET_, ATTR_NUMBER_) 3", 1)) 
			    return(ErrHandle(status, env, &arg, &arg2, NULL));

			setJavaField(env,arg2);   /* Set the attribute number */
			cleanArgument(env, &arg);
			cleanArgument(env, &arg2);
			break;

                    case zVAR_ALLOCATEDFROM_:
		    case zVAR_ALLOCATEDTO_:
                        arg.length = arg2.length = -1;
                        nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
                        status = getItem(env, theCDF, nextItem, obj, &arg);
                        if (!CheckStatus("cdflib (GET_, ALLOCATEDFROM_/TO_) 1", 0))
                            return(status);
                        nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
                        status = getItem(env, theCDF, nextItem, obj, &arg2);
                        if (!CheckStatus("cdflib (GET_,ALLOCATEDFROM_/TO_) 2", 0))
                            return(status);

                        getJavaField(env, &arg);     /* start record number */

                        status = CDFlib(GET_, item, *(long *)arg.ptr,
                                                    arg2.ptr,
                                        NULL_);
                        if (!CheckStatus("cdflib (GET_, ALLOCATEDFROM_/TO_) 3", 1))
                            return(ErrHandle(status, env, &arg, &arg2, NULL));

                        setJavaField(env,arg2);   /* Set the last record number */
                        cleanArgument(env, &arg);
                        cleanArgument(env, &arg2);
                        break;

		    case gENTRY_DATA_:
		    case zENTRY_DATA_:
		    case zVAR_DATA_:
		    case zVAR_HYPERDATA_:
		    case zVAR_PADVALUE_:
			nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
			status = getItem(env, theCDF, nextItem, obj, &arg);
			if (!CheckStatus("cdflib (GET_, ENTRY/VARIABLE_DATA_) 1", 0))
			    return(status);

			status = CDFlib (GET_, item, (void *) arg.ptr, 
					 NULL_);

			if (!CheckStatus("cdflib (GET_,ENTRY/VARIABLE_DATA_) 2", 1))
			    return(ErrHandle(status, env, &arg, NULL));

                        if (item == zVAR_HYPERDATA_) {
                          status2 = flipDataElements((char *) arg.ptr, 1);
                        }
			setJavaField(env,arg);  /* Set the attribute entry data */
			cleanArgument(env, &arg);
			break;

		    case CDF_COMPRESSION_:
		    case rVAR_COMPRESSION_:
		    case zVAR_COMPRESSION_:
			arg.length = arg2.length = arg3.length = -1;
			nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
			status = getItem(env, theCDF, nextItem, obj, &arg);
			if (!CheckStatus("cdflib (GET_, COMPRESSION_) 1", 0))
			    return(status);

			nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
			status = getItem(env, theCDF, nextItem, obj, &arg2);
			if (!CheckStatus("cdflib (GET_, COMPRESSION_) 2", 0))
			    return(status);

			nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
			status = getItem(env, theCDF, nextItem, obj, &arg3);
			if (!CheckStatus("cdflib (GET_, COMPRESSION_) 3", 0))
			    return(status);

			status = CDFlib(GET_, item, arg.ptr, 
                                                    arg2.ptr, 
                                                    arg3.ptr,
					NULL_);
			if (!CheckStatus("cdflib (GET_, COMPRESSION_) 4", 1))
			    return (ErrHandle(status, env, &arg, &arg2, &arg3,
					      NULL));

			setJavaField(env,arg);     /* long *cType */
			setJavaField(env,arg2);    /* long cParms[CDF_MAX_PARMS] */
			setJavaField(env,arg3);    /* long *cPct */
			cleanArgument(env, &arg);
			cleanArgument(env, &arg2);
			cleanArgument(env, &arg3);
			break;

		    default:
			MYDEBUG("cdflib","Can't deal with it yet");
			break;
		    } /* switch(item) */

		    MYDEBUG("cdflib", "Getting nextItem");
		    nextItem   = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
		    exc = (*env)->ExceptionOccurred(env);
		    if (exc) {
			printf("Exception in cdflib2\n");
			(*env)->ExceptionDescribe(env);
			(*env)->ExceptionClear(env);
		    }
		    CHECKNULL("nextItem",nextItem);
		    status2 = getFuncOrItem(env, nextItem, &item);
		}
	    } else {
		(*env)->ThrowNew(env, excClass, "Bad function or item");
	    }
	    break; /* end GET_ */

	case PUT_:
	    nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
	    if (isItem(nextItem)) {
		status2 = getFuncOrItem(env, nextItem, &item);

		while (item < NULL_) {
		  sprintf (msg,"func = %ld item = %ld status = %ld",
                           func,item,status2);
		  MYDEBUG("cdflib",msg);
		  switch(item) {

		    case ATTR_NAME_:
		    case zVAR_NAME_: /* for 1 pointer variable input */
			arg.length = -1; /* Actual name length is determined in getItemm */
			nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
			status = getItem(env, theCDF, nextItem, obj, &arg);
			if (!CheckStatus("cdflib (PUT_) 1", 0)) return(status);

			getJavaField(env, &arg);

			status = CDFlib(PUT_, item, (char *)arg.ptr, NULL_);
			if (!CheckStatus("cdflib (PUT_) 2", 1))
			  return(ErrHandle(status, env, &arg, NULL));

			cleanArgument(env, &arg);
			break;
			
		    case ATTR_SCOPE_:
		    case CDF_ENCODING_:
		    case CDF_MAJORITY_:
		    case CDF_FORMAT_:
		    case CDF_CHECKSUM_:
		    case CDF_LEAPSECONDLASTUPDATED_:
		    case zVAR_ALLOCATERECS_:
		    case zVAR_BLOCKINGFACTOR_:
		    case zVAR_RECVARY_:
		    case zVAR_SPARSERECORDS_:
		    case zVAR_INITIALRECS_: /* for 1 long variable as input */

			arg.length = -1;
			nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
			status = getItem(env, theCDF, nextItem, obj, &arg);
			if (!CheckStatus("cdflib (PUT_,CDF_ENC/MAJ_/FOR_/zVAR_ALLOC_/BLOCK_/INIT_/SPARSE_/RECVARY_) 1", 0))
			    return(status);
			getJavaField(env, &arg);
			status = CDFlib(PUT_, item,  *(long *)arg.ptr,
					NULL_);
			if (!CheckStatus("cdflib (PUT_,CDF_ENC/MAJ_/FOR_/zVAR_ALLOC_/BLOCK_/INIT_/SPARSE_/RECVARY_) 2", 1))
			    return (ErrHandle(status, env, &arg, NULL));

			cleanArgument(env, &arg);
			break;

		    case zENTRY_DATA_:
		    case gENTRY_DATA_: /* for 2 long and 1 pointer variables input */
			arg.length  = arg2.length = arg3.length = -1;
			nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
			status = getItem(env, theCDF, nextItem, obj, &arg);
			if (!CheckStatus("cdflib (PUT_,ENTRY_DATA_) 1", 0))
			    return(status);

			nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
			status = getItem(env, theCDF, nextItem, obj, &arg2);
			if (!CheckStatus("cdflib (PUT_,ENTRY_DATA_) 2", 0)) 
			    return(status);

			nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
			status = getItem(env, theCDF, nextItem, obj, &arg3);
			if (!CheckStatus("cdflib (PUT_,ENTRY_DATA_) 3", 0)) 
			    return(status);

			getJavaField(env, &arg);      /* long dataType    */

			getJavaField(env, &arg2);     /* long numElements */

			getJavaField(env, &arg3);     /* void *data       */

			status = CDFlib(PUT_, item, *(long *)arg.ptr, 
					            *(long *)arg2.ptr,
					            arg3.ptr,
					NULL_);
			if (!CheckStatus("cdflib (PUT_,ENTRY_DATA_) 4", 1))
			    return (ErrHandle(status, env, &arg, &arg2, &arg3,
			 		      NULL));

			cleanArgument(env, &arg);
			cleanArgument(env, &arg2);
			cleanArgument(env, &arg3);
			break;

                    case gENTRY_DATASPEC_: 
		    case zENTRY_DATASPEC_:
		    case zVAR_DATASPEC_:
		    case zVAR_ALLOCATEBLOCK_: /* for 2 long variables input */
                        arg.length  = arg2.length = -1;

                        nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
                        status = getItem(env, theCDF, nextItem, obj, &arg);

                        if (!CheckStatus("cdflib (PUT_,g/zENTRY_DATASPEC_|zVAR_DATASPEC_|zVAR_ALLOCATEBLOCK_) 1", 0))
                            return(status);

                        nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
                        status = getItem(env, theCDF, nextItem, obj, &arg2);
                        if (!CheckStatus("cdflib (PUT_,g/zENTRY_DATASPEC_|zVAR_DATASPEC_|zVAR_ALLOCATEBLOCK_) 2", 0))
                            return(status);

                        getJavaField(env, &arg);      /* long firstRecord */
                        getJavaField(env, &arg2);     /* long lastRecord */

                        status = CDFlib(PUT_, item, *(long *)arg.ptr,
                                                    *(long *)arg2.ptr,
                                        NULL_);
                        if (!CheckStatus("cdflib (PUT_,g/zENTRY_DATASPEC_|zVAR_DATASPEC_|zVAR_ALLOCATEBLOCK_) 3", 1))
                            return (ErrHandle(status, env, &arg, &arg2, NULL));

                        cleanArgument(env, &arg);
                        cleanArgument(env, &arg2);
                        break;

                    case zVAR_PADVALUE_:
		    case zVAR_DATA_:
                    case zVAR_HYPERDATA_: /* for 1 pointer variable for input */

                        arg.length = -1;
                        nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
                        status = getItem(env, theCDF, nextItem, obj, &arg);
                        if (!CheckStatus("cdflib (PUT_,zVAR_PADVALUE_/_DATA_/_HYPERDATA_) 1", 0))
                            return(status);

                        getJavaField(env, &arg);

                        if (item == zVAR_HYPERDATA_) {
                          status2 = flipDataElements((char *) arg.ptr, 2);
                        }

                        status = CDFlib(PUT_, item,  (void *)arg.ptr,
                                        NULL_);
                        if (!CheckStatus("cdflib (PUT_,zVAR_PADVALUE_/_DATA_/_HYPERDATA_) 2", 1))
                            return (ErrHandle(status, env, &arg, NULL));

                        cleanArgument(env, &arg);
                        break;

                    case CDF_COMPRESSION_:
		    case zVAR_COMPRESSION_: /* for 1 long and 1 pointer variables input */
                        
			arg.length = arg2.length = -1;
                        nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
                        status = getItem(env, theCDF, nextItem, obj, &arg);
                        if (!CheckStatus("cdflib (PUT_,CDF_/zVAR_COMPRESSION_) 1", 0))
                            return(status);

                        nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
                        status = getItem(env, theCDF, nextItem, obj, &arg2);
                        if (!CheckStatus("cdflib (PUT_,CDF_/zVAR_COMPRESSION_) 2", 0))
                            return(status);

                        getJavaField(env, &arg);

			getJavaField(env, &arg2);

                        status = CDFlib(PUT_, item,  *(long *)arg.ptr, arg2.ptr,
                                        NULL_);
                        if (!CheckStatus("cdflib (PUT_,CDF_/zVAR_COMPRESSION_) 3", 1))
                            return (ErrHandle(status, env, &arg, &arg2, NULL));

                        cleanArgument(env, &arg);
			cleanArgument(env, &arg2);
                        break;

		    case zVAR_DIMVARYS_: /* for 1 pointer variable input */

                        arg.length = -1;
                        nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
                        status = getItem(env, theCDF, nextItem, obj, &arg);
                        if (!CheckStatus("cdflib (PUT_,zVAR_DIMVARYS_) 1", 0))
                            return(status);

                        getJavaField(env, &arg);

                        status = CDFlib(PUT_, item,  arg.ptr,
                                        NULL_);
                        if (!CheckStatus("cdflib (PUT_,zVAR_DIMVARYS_) 2", 1))
                            return (ErrHandle(status, env, &arg, NULL));

                        cleanArgument(env, &arg);
                        break;

 		    default:
			(*env)->ThrowNew(env, excClass, "Bad function or item");
			break;
		    } /* switch(item) */
		    
		    MYDEBUG("cdflib", "Getting nextItem");
		    nextItem   = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
		    exc = (*env)->ExceptionOccurred(env);
		    if (exc) {
			printf("Exception in cdflib2\n");
			(*env)->ExceptionDescribe(env);
			(*env)->ExceptionClear(env);
		    }
		    CHECKNULL("nextItem",nextItem);
		    status2 = getFuncOrItem(env, nextItem, &item);
		}
	    } else {
		(*env)->ThrowNew(env, excClass, "Bad function or item");
	    }
	    break;          /* end PUT_ */

        /******************************************************************/
        /*  SELECT_                                                       */
        /******************************************************************/
	case SELECT_:
	    nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
	    if (isItem(nextItem)) {
		status2 = getFuncOrItem(env, nextItem, &item);

		while (item < NULL_) {
		  sprintf (msg,"func = %ld item = %ld status = %ld",
                           func,item,status2);
		  MYDEBUG("cdflib",msg);

		  switch(item) {

		    case CDF_READONLY_MODE_:
		    case CDF_DECODING_:
		    case CDF_CACHESIZE_:
		    case CDF_NEGtoPOSfp0_MODE_:
		    case CDF_zMODE_:
		    case COMPRESS_CACHESIZE_:
		    case STAGE_CACHESIZE_:
		    case gENTRY_:
		    case zENTRY_:
		    case ATTR_:
		    case zVAR_RECNUMBER_:
		    case zVAR_RECCOUNT_:
		    case zVAR_RECINTERVAL_:
		    case zVAR_CACHESIZE_:
		    case zVAR_RESERVEPERCENT_:
		    case CDF_:
		    case zVAR_:
			nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
			status = getItem(env, theCDF, nextItem, obj, &arg);
			getJavaField(env, &arg);
			if (!CheckStatus("cdflib (SELECT_)", 0))
			    return(status);
			
			if (item == CDF_) {
			  id = getCDFid(jid);
			  status = CDFlib(SELECT_, CDF_, id, NULL_);
			} else {
			  status = CDFlib(SELECT_, item, *(long *)arg.ptr, 
                                          NULL_);
			}
			if (!CheckStatus("cdflib (SELECT_)", 1))
			  return(ErrHandle(status, env, &arg, NULL));

			cleanArgument(env, &arg);
			break;

                    case zVAR_DIMINDICES_:
		    case zVAR_DIMCOUNTS_:
		    case zVAR_DIMINTERVALS_:
                        nextItem = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
                        status = getItem(env, theCDF, nextItem, obj, &arg);
                        getJavaField(env, &arg);
                        if (!CheckStatus("cdflib (SELECT_)", 0))
                            return(status);

                        status = CDFlib(SELECT_, item, (long *)arg.ptr,
                                        NULL_);
                        if (!CheckStatus("cdflib (SELECT_)", 1))
			  return(ErrHandle(status, env, &arg, NULL));

                        cleanArgument(env, &arg);
                        break;

		    default:
			MYDEBUG("cdflib","Can't deal with it yet");
			break;
		    } /* switch(item) */

		    MYDEBUG("cdflib", "Getting nextItem");
		    nextItem   = (*env)->CallObjectMethod(env, cmdVector, vElemID, i++);
		    exc = (*env)->ExceptionOccurred(env);
		    if (exc) {
			printf("Exception in cdflib2\n");
			(*env)->ExceptionDescribe(env);
			(*env)->ExceptionClear(env);
		    }
		    CHECKNULL("nextItem",nextItem);
		    status2 = getFuncOrItem(env, nextItem, &item);
		}
	    } else {
		(*env)->ThrowNew(env, excClass, "Bad function or item");
	    }
	    break; /* end SELECT_ */
	    
	default: 
	    (*env)->ThrowNew(env, excClass, "Bad function or item");
	    break;
	} /* switch(func) */
	func = item;
	sprintf(msg, "The next func = %ld",func);
	MYDEBUG("cdflib", msg);
	
	exc = (*env)->ExceptionOccurred(env);
	if (exc) {
	    printf("Exception in cdflib3\n");
	    (*env)->ExceptionDescribe(env);
	    (*env)->ExceptionClear(env);
	    return(BAD_FNC_OR_ITEM);
	}
	
    } /* while(func!=NULL_) */
    MYDEBUG("cdflib","<<<<<<<<<<<<<<<<<<<<");
    return(status);

}         /* end JAVAcdflib */

/*********************************************
 **                                         **
 **              JNI Methods                **
 **                                         **
 *********************************************/

/* Function:  cdfNativeLib
 * Class:     CDFNativeLibrary
 * Package:   gsfc.nssdc.cdf
 *
 * Purpose:   This is the main wrapper for the native implementation of the
 *            CDFJava API
 *
 * Parameters:
 *    in:   env -  The Java environment pointer
 *    in:   nativeLibraryObject - The instance of CDFNativeLibrary making
 *             the call
 *    in:   theCDF -  The CDF to which the object belongs
 *    in:   obj -  The instance of a CDFObject on which the cmds should be
 *             executed
 *    in:   cmdVector -  A java.util.Vector that contains the CDF command
 *             to perform
 * Returns:
 *    void
 */
JNIEXPORT void JNICALL 
Java_gsfc_nssdc_cdf_CDFNativeLibrary_cdfNativeLib(JNIEnv *env, 
						  jobject nativeLibraryObject,
						  jobject theCDF,
						  jobject obj,
						  jobject cmdVector) {
    
    jclass
	myobjClass   = (*env)->GetObjectClass(env, obj),
	cmdClass     = (*env)->GetObjectClass(env, cmdVector);

    jfieldID   cdfidID, idID;
    jmethodID  vElemID;
    jobject    firstCmd, firstArg;

    long       cdfid,       /* CDF id assigned by the CDFlib routine        */ 
               objType,     /* CDF object type                              */
               func,        /* CDF function (e.g. CREATE_, GET_, etc)       */
               newArg;      /* Argument after function                      */
    

    /* Needed for save operation without closing a CDF file */
    struct    CDFstruct *cdfstruct;
    CDFid     id0;
    CDFstatus status;

    /* For the first call to here, set up all global references */
    if (cdfClass == 0) {

        jclass cdfClass1, varClass1, entryClass1, attrClass1, dataClass1,
	       excClass1;
        jclass byteClass1, shortClass1, intClass1, longClass1, floatClass1,
               doubleClass1, strClass1, vecClass1, objClass1;

        cdfClass1   = (*env)->FindClass(env, "gsfc/nssdc/cdf/CDF"),
        varClass1   = (*env)->FindClass(env, "gsfc/nssdc/cdf/Variable"),
        entryClass1 = (*env)->FindClass(env, "gsfc/nssdc/cdf/Entry"),
        attrClass1  = (*env)->FindClass(env, "gsfc/nssdc/cdf/Attribute"),
        dataClass1  = (*env)->FindClass(env, "gsfc/nssdc/cdf/CDFData"),
        excClass1   = (*env)->FindClass(env, "gsfc/nssdc/cdf/CDFException"),

        vecClass1    = (*env)->FindClass(env, "java/util/Vector"),
        byteClass1   = (*env)->FindClass(env, "java/lang/Byte"),
        shortClass1  = (*env)->FindClass(env, "java/lang/Short"),
        intClass1    = (*env)->FindClass(env, "java/lang/Integer"),
        longClass1   = (*env)->FindClass(env, "java/lang/Long"),
        floatClass1  = (*env)->FindClass(env, "java/lang/Float"),
        doubleClass1 = (*env)->FindClass(env, "java/lang/Double"),
        strClass1    = (*env)->FindClass(env, "java/lang/String"),
        objClass1    = (*env)->FindClass(env, "java/lang/Object");

        cdfClass    = (*env)->NewGlobalRef(env, cdfClass1);
        varClass    = (*env)->NewGlobalRef(env, varClass1);
        entryClass  = (*env)->NewGlobalRef(env, entryClass1);
        attrClass   = (*env)->NewGlobalRef(env, attrClass1);
	dataClass   = (*env)->NewGlobalRef(env, dataClass1);
	excClass    = (*env)->NewGlobalRef(env, excClass1);

        vecClass    = (*env)->NewGlobalRef(env, vecClass1);
        byteClass   = (*env)->NewGlobalRef(env, byteClass1);
        shortClass  = (*env)->NewGlobalRef(env, shortClass1);
        intClass    = (*env)->NewGlobalRef(env, intClass1);
        longClass   = (*env)->NewGlobalRef(env, longClass1);
        floatClass  = (*env)->NewGlobalRef(env, floatClass1);
        doubleClass = (*env)->NewGlobalRef(env, doubleClass1);
        strClass    = (*env)->NewGlobalRef(env, strClass1);
        objClass    = (*env)->NewGlobalRef(env, objClass1);

	BvalID = (*env)->GetMethodID(env, byteClass, "byteValue", "()B");
	SvalID = (*env)->GetMethodID(env, shortClass, "shortValue", "()S");
	IvalID = (*env)->GetMethodID(env, intClass, "intValue", "()I");
	LvalID = (*env)->GetMethodID(env, longClass, "longValue", "()J");
	FvalID = (*env)->GetMethodID(env, floatClass, "floatValue", "()F");
	DvalID = (*env)->GetMethodID(env, doubleClass, "doubleValue", "()D");

	cdfStatusID = (*env)->GetFieldID(env, cdfClass, "cdfStatus", "J");
	infoWarnID  = (*env)->GetFieldID(env, cdfClass, "infoWarning", "J");

        status = CDFlib(GET_, LIB_VERSION_, &lib_ver,
                        NULL_);

    }

    if (cdfClass == NULL) MYDEBUG("cdfNativeLib", "cdfClass is null");
    if (objClass == NULL) MYDEBUG("cdfNativeLib", "objClass is null");
    if (cmdClass == NULL) MYDEBUG("cdfNativeLib", "cmdClass is null");
    if (vecClass == NULL) MYDEBUG("cdfNativeLib", "vecClass is null???");

    cdfidID = (*env)->GetFieldID(env, cdfClass, "id", "J");
    MYDEBUG("cdfNativeLib","got cdfidID");

    idID    = (*env)->GetFieldID(env, myobjClass, "id", "J");
    MYDEBUG("cdfNativeLib","got idID");

    vElemID=(*env)->GetMethodID(env, cmdClass, "elementAt", 
				"(I)Ljava/lang/Object;");
    MYDEBUG("cdfNativeLib","got jmethodIDs");
    /******************************************************/
    /* Get the real CDF id assigned by the CDFlib routine */
    /******************************************************/
    cdfid = (long)(*env)->GetLongField(env, theCDF, cdfidID);
    MYDEBUG("cdfNativeLib","got fields");
    MYDEBUG("cdfNativeLib","getting first command");

    /***************************************************/
    /* Get the first command and make sure that is one */
    /***************************************************/
    firstCmd  = (*env)->CallObjectMethod(env, cmdVector, vElemID, 0);
    status    = getFuncOrItem(env, firstCmd, &func);
    if (!CheckStatus("Java getFuncOrItem", 0)) return;
    sprintf(msg,"1st function = %ld", func);
    MYDEBUG("cdfNativeLib", msg);
    if (func < NULL_) {
	(*env)->ThrowNew(env, excClass, "Illegal function detected.");
        return;
    }
    /*****************************************************/
    /* ok to continue.  Determine what type of CDFObject */
    /*****************************************************/

    objType = cdfObjectType(env, obj);
    switch (objType) {

    case CDF_CLASS:
	if (func == SAVE_) {  /* Should SAVE_ be added to the main CDFlib?? */
	    id0 = getCDFid(cdfid);
	    cdfstruct = (struct CDFstruct *)id0;
	    if (cdfstruct->largeFile) { 
	      /* UpdateDotCDF returns 0 for success nonzero indicates failure */
	      if (UpdateDotCDF64(cdfstruct) != 0)
		(*env)->ThrowNew(env, excClass, "Save Failed 1.");
	      /* V_flush returns 0 for success nonzero indicates failure */
	      if (V_flush64(cdfstruct->fp) != 0)
		(*env)->ThrowNew(env, excClass, "Save Failed 2.");
	    } else {
              /* UpdateDotCDF returns 0 for success nonzero indicates failure */
              if (UpdateDotCDF(cdfstruct) != 0)
                (*env)->ThrowNew(env, excClass, "Save Failed 1.");
              if (V_flush(cdfstruct->fp) != 0)
                (*env)->ThrowNew(env, excClass, "Save Failed 2.");
	    }
        } else if (func == BACKWARD_) {  
/*	         if (func == BACKWARD_) { */
            firstArg  = (*env)->CallObjectMethod(env, cmdVector, vElemID, 1);
            status    = getFuncOrItem(env, firstArg, &newArg);
            if (!CheckStatus("Java getFuncOrItem", 0)) return;
            CDFsetFileBackward((int)newArg);
        } else if (func == CHECKSUM_) {
            firstArg  = (*env)->CallObjectMethod(env, cmdVector, vElemID, 1);
            status    = getFuncOrItem(env, firstArg, &newArg);
            if (!CheckStatus("Java getFuncOrItem", 0)) return;
            CDFsetChecksumMode(newArg);
        } else if (func == VALIDATE_) {
            firstArg  = (*env)->CallObjectMethod(env, cmdVector, vElemID, 1);
            status    = getFuncOrItem(env, firstArg, &newArg);
            if (!CheckStatus("Java getFuncOrItem", 0)) return;
            CDFsetValidate(newArg);
        } else if (func == GETCDFFILEBACKWARD_ || func == GETCDFCHECKSUM_ ||
                   func == GETCDFVALIDATE_) {
	    argument arg;
	    arg.length = -1;
	    if (func == GETCDFFILEBACKWARD_)
              envVar = (long) CDFgetFileBackwardEnvVar();
	    else if (func == GETCDFCHECKSUM_)
	      envVar = (long) CDFgetChecksumEnvVar();
	    else
	      envVar = (long) CDFgetValidate();
            firstArg  = (*env)->CallObjectMethod(env, cmdVector, vElemID, 1);
            status    = getItem(env, theCDF, firstArg, obj, &arg);
	    *(long *)arg.ptr = envVar;
	    setJavaField(env, arg);
            cleanArgument(env, &arg);
        } else if (func == GETLEAPSECONDSENVVAR_) {
	    argument arg;
            char *envvar;
	    arg.length = CDF_PATHNAME_LEN + 1;
            firstArg  = (*env)->CallObjectMethod(env, cmdVector, vElemID, 1);
            status    = getItem(env, theCDF, firstArg, obj, &arg);
	    envvar = (char *) CDFgetLeapSecondsTableEnvVar();
            if (envvar != NULL) 
              strncpy (arg.ptr, envvar, CDF_PATHNAME_LEN);
            else
              arg.ptr = NULL;
	    setJavaField(env, arg);
            cleanArgument(env, &arg);
	} else if (func == NULL_) { /* just to select this CDF */
	    id0 = getCDFid(cdfid);
	    status = CDFlib(SELECT_, CDF_, id0,
			    NULL_);
	    if (!CheckStatus("cdflib (CDF)", 1)) return;
	    if (func == NULL_) return;        /* If func == SELECT, return */
	} else {
	    status = JAVAcdflib (env, theCDF, obj, cmdVector, cdfid, func);
	    if (status != CDF_OK) return;
	}
	if (func == CLOSE_ || func == DELETE_) cleanGlobalRefs(env);
	break;

    case VAR_CLASS:
/*	id0 = getCDFid(cdfid); */
/*	if (func == CREATE_) { */
/*	    status = CDFlib(SELECT_, CDF_, id0,  */
/*                           NULL_); */
/*	} else { */
	    /* Get the id of the calling var */
/*	    id = (long) (*env)->GetLongField(env, obj, idID); */
/* */
/*	    sprintf(msg,"Selecting variable #%ld",id); */
/*	    MYDEBUG("cdflib (variable)",msg); */
/*		 */
/*	    status = CDFlib(SELECT_, CDF_, id0,  */
/*                                     zVAR_, id,  */
/*                            NULL_); */
/*	} */
/*	if (!CheckStatus("cdflib (Variable)")) return; */
/*        if (func == NULL_) return; */       /* If func = SELECT, return */

	status = JAVAcdflib(env, theCDF, obj, cmdVector, cdfid, func);
        if (status != CDF_OK) return;
	break;

    case ENTRY_CLASS:
/*	if (func == NULL_) return; */
	/* Get the id of the calling entry */
/*	id = (long) (*env)->GetLongField(env, obj, idID); */
/*	id0 = getCDFid(cdfid); */
/*	entryScopeID = (*env)->GetFieldID(env, entryClass, "scope", "J"); */
/*	scope = (long)(*env)->GetLongField(env, obj, entryScopeID); */
/*	if (scope == GLOBAL_SCOPE) */
/*	    status = CDFlib(SELECT_, CDF_, id0, gENTRY_, id, */
/*			    NULL_); */
/*	else */
/*	    status = CDFlib(SELECT_, CDF_, id0,  */
/*			             zENTRY_, id, */
/*			    NULL_); */
/* */
/*	if (!CheckStatus("cdflib (Entry)")) return; */
/*        if (func == NULL_) return; */        /* If func = SELECT, return */
	
	status = JAVAcdflib(env, theCDF, obj, cmdVector, cdfid, func);
        if (status != CDF_OK) return;
	break;

    case DATA_CLASS:
/*	id0 = getCDFid(cdfid); */
/*	status = CDFlib(SELECT_, CDF_, id0, */
/*			NULL_); */
/*	if (!CheckStatus("cdflib (DATA_CLASS)")) return; */
	status = JAVAcdflib(env, theCDF, obj, cmdVector, cdfid, func);
        if (status != CDF_OK) return; 
	break;

    case ATTR_CLASS:
/*	id0 = getCDFid(cdfid); */
/*	if (func == CREATE_) { */
/*	    MYDEBUG("_Attribute_ 2", "CREATE_, ATTR_"); */
/*	    status = CDFlib(SELECT_, CDF_, id0, */
/*			    NULL_); */
/*	} else { */
	    /* Get the id of the calling attr */
/*	    id = (long) (*env)->GetLongField(env, obj, idID);  */
/*	    sprintf(msg,"Selecting attr #%ld",id); */
/*	    MYDEBUG("_Attribute_ 2",msg); */
/*	    status = CDFlib(SELECT_, CDF_, id0,  */
/*			             ATTR_, id, */
/*			    NULL_); */
/*	} */
/*	if (!CheckStatus("cdflib (Attribute)")) return; */
/*      if (func == NULL_) return; */       /* If func = SELECT, return */

	status = JAVAcdflib(env, theCDF, obj, cmdVector, cdfid, func);
        if (status != CDF_OK) return;
        break;

    default:         /* bad class.  Throw an error */
	(*env)->ThrowNew(env, excClass, "Bad CDFObject passed to cdflib\n");
	break;
    }	/* switch(objtype) */
/*    cleanGlobalRefs(env); */


}          /* Java_gsfc_nssdc_cdf_CDFNativeLibrary_cdfNativeLib */

/*********************************************
 **                                         **
 **         Epoch Utility Wrappers          **
 **                                         **
 **  Note: These wrappers are now           **
 **        obsolete and are present for     **
 **        testing and comparison           **
 **                                         **
 *********************************************/

JNIEXPORT jdouble JNICALL 
Java_gsfc_nssdc_cdf_util_EpochNative_compute(JNIEnv *env, 
					jobject obj,
					jlong   year,
					jlong   month,
					jlong   day,
					jlong   hour,
					jlong   minute,
					jlong   second, 
					jlong   msec) {

    double epoch = computeEPOCH((long)year, (long)month, (long)day, (long)hour,
				(long)minute, (long)second, (long)msec);

    return((jdouble)epoch);
}

JNIEXPORT jlongArray JNICALL 
Java_gsfc_nssdc_cdf_util_EpochNative_breakdown(JNIEnv *env, jobject obj, 
					  jdouble epoch) {

    long year, month, day, hour, minute, second, msec;
    jlongArray jarray;
    jlong *jbody;

    jarray = (*env)->NewLongArray(env, 7);
    EPOCHbreakdown(epoch, &year, &month, &day, &hour, &minute, &second, &msec);
    jbody = (*env)->GetLongArrayElements(env, jarray, 0);
    jbody[0] = (jlong)year;
    jbody[1] = (jlong)month;
    jbody[2] = (jlong)day;
    jbody[3] = (jlong)hour;
    jbody[4] = (jlong)minute;
    jbody[5] = (jlong)second;
    jbody[6] = (jlong)msec;

    (*env)->ReleaseLongArrayElements(env, jarray, jbody, 0);
    
    return (jarray);
}

JNIEXPORT jstring JNICALL 
Java_gsfc_nssdc_cdf_util_EpochNative_encode(JNIEnv *env, 
				       jobject obj, 
				       jdouble epoch) {

    char epString[EPOCH_STRING_LEN+1];

    encodeEPOCH(epoch, epString);
    return (*env)->NewStringUTF(env, epString);
}

JNIEXPORT jstring JNICALL 
Java_gsfc_nssdc_cdf_util_EpochNative_encode1(JNIEnv *env, 
					jobject obj, 
					jdouble epoch) {

    char epString[EPOCH1_STRING_LEN+1];

    encodeEPOCH1(epoch, epString);
    return (*env)->NewStringUTF(env, epString);
}

JNIEXPORT jstring JNICALL 
Java_gsfc_nssdc_cdf_util_EpochNative_encode2(JNIEnv *env, 
					jobject obj, 
					jdouble epoch) {

    char epString[EPOCH2_STRING_LEN+1];

    encodeEPOCH2(epoch, epString);
    return (*env)->NewStringUTF(env, epString);
}

JNIEXPORT jstring JNICALL 
Java_gsfc_nssdc_cdf_util_EpochNative_encode3(JNIEnv *env, 
					jobject obj, 
					jdouble epoch) {

    char epString[EPOCH3_STRING_LEN+1];

    encodeEPOCH3(epoch, epString);
    return (*env)->NewStringUTF(env, epString);
}

JNIEXPORT jstring JNICALL
Java_gsfc_nssdc_cdf_util_EpochNative_encode4(JNIEnv *env,
                                        jobject obj,
                                        jdouble epoch) {

    char epString[EPOCH4_STRING_LEN+1];

    encodeEPOCH4(epoch, epString);
    return (*env)->NewStringUTF(env, epString);
}

JNIEXPORT jstring JNICALL 
Java_gsfc_nssdc_cdf_util_EpochNative_encodex(JNIEnv *env, 
					jobject obj, 
					jdouble epoch,
					jstring format) {

    char  epString[EPOCH3_STRING_LEN+1];
    const char *formatString = (*env)->GetStringUTFChars(env, format, 0);

    encodeEPOCHx (epoch, (char *)formatString, epString);
    (*env)->ReleaseStringUTFChars(env, format, formatString);
    return (*env)->NewStringUTF(env, epString);
}

JNIEXPORT jdouble JNICALL
Java_gsfc_nssdc_cdf_util_EpochNative_parse(JNIEnv *env, 
				     jclass obj,
				     jstring sEpoch) {

    jdouble millis;
    const char *epochString  = (*env)->GetStringUTFChars(env, sEpoch, 0);
    millis = parseEPOCH((char *)epochString);
    (*env)->ReleaseStringUTFChars(env, sEpoch, epochString);

    return millis;
}

JNIEXPORT jdouble JNICALL
Java_gsfc_nssdc_cdf_util_EpochNative_parse1(JNIEnv *env, 
				      jclass obj,
				      jstring sEpoch) {

    jdouble millis;
    const char *epochString  = (*env)->GetStringUTFChars(env, sEpoch, 0);
    millis = parseEPOCH1((char *)epochString);
    (*env)->ReleaseStringUTFChars(env, sEpoch, epochString);

    return millis;
}

JNIEXPORT jdouble JNICALL
Java_gsfc_nssdc_cdf_util_EpochNative_parse2(JNIEnv *env, 
				      jclass obj,
				      jstring sEpoch) {

    jdouble millis;
    const char *epochString  = (*env)->GetStringUTFChars(env, sEpoch, 0);
    millis = parseEPOCH2((char *)epochString);
    (*env)->ReleaseStringUTFChars(env, sEpoch, epochString);

    return millis;
}

JNIEXPORT jdouble JNICALL
Java_gsfc_nssdc_cdf_util_EpochNative_parse3(JNIEnv *env, 
				      jclass obj,
				      jstring sEpoch) {

    jdouble millis;
    const char *epochString  = (*env)->GetStringUTFChars(env, sEpoch, 0);
    millis = parseEPOCH3((char *)epochString);
    (*env)->ReleaseStringUTFChars(env, sEpoch, epochString);

    return millis;
}
