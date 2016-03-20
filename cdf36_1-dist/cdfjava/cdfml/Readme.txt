***************************************************************************
*   Please make sure that you have done the following before proceeding   *
*   with the CDF Markup Language (CDFML) utilities:                       * 
*                                                                         *
*     - installed JDK 1.5. Programs may fail running under an earleir JVM.*
*     - installed/built the CDF distribution package.                     *
*     - executed the definitions.* file located under the bin directory   *
*       of CDF installation (e.g. `source definitions.C' for C shell) to  *
*       define the environment variables (e.g. CDF_BASE, CDF_LIB, etc).   *
*     - installed the CDF Java APIs and built the CDF-Java JNI.           *
*       (Download the latest CDF Java APIs if you already have the        *
*        CDF-Java JNI.)                                                   *
*     - optionally, install Sun's Multi-Schema XML Validator.             *
***************************************************************************

Instructions for using the CDF's CDFML programs 
-----------------------------------------------

1. Download and un-tar the cdfml.tar archive file from the directory that
   contains cdf35_0-dist. 

2. Set the CLASSPATH and LD_LIBRARY_PATH (DYLD_LIBRARY_PATH for Mac OSX)
   environment variables.

   1) CLASSPATH should point to the following information:
      - the location (full path name) of the cdfml.jar file that was
        extracted in step 1.
      - the location (full path name) of the cdfjava.jar file.  

      Examples: 

      - using Sun's V1.5

         setenv CLASSPATH ${CLASSPATH}:.: \
                          /home/cdf/cdf35_0-dist/cdfjava/classes/cdfjava.jar: \
                          /home/cdf/cdf35_0-dist/cdfjava/cdfml/cdfml.jar

      Note:
          Absolute/full path name (not relative path) MUST be used when adding
          the location of a program to the CLASSPATH environment variable.  

         
   2) LD_LIBRARY_PATH should point to the following information:
      - the location (full path name) of the CDF-Java JNI 
        libcdfNativeLibrary.[so|sl|jnilib] (it should be under cdfjava/lib 
        from the distribution) 

      - the location (full path name) of the CDF shared library that is 
        defined by the environment variable CDF_LIB.  This variable is 
        defined when the CDF library is installed.

      Example: 
         setenv LD_LIBRARY_PATH .:/home/cdf/cdf35_0-dist/cdfjava/lib:${CDF_LIB} 

         OR for Mac OSX,
         setenv DYLD_LIBRARY_PATH .:/home/cdf/cdf35_0-dist/cdfjava/lib:${CDF_LIB}


3. Use CDF2CDFML Java program to create a CDFML document from a CDF file. 
   To get the instructions and valid arguments/options, enter:

       java CDF2CDFML

4. Use CDFML2CDF Java program to convert a CDFML document to a CDF file. To 
   get the instructions and valid arguments/options, enter:

       java CDFML2CDF

5. Use Sun's Multi-Schema Validator, a Java tool, to validate a CDFML document 
   conforming to cdf.dtd or cdf.xsd, if needed. 

   Run: java -jar msv.jar cdf.dtd test.xml or
        java -jar msv.jar cdf.xsd testxsd.xml


Note: 
   If one creates a CDFML file from an existing CDF file and creates a new
   CDF file from that CDFML file, the contents of the new file is the same 
   as the original CDF file.  But the ordering of some of the variable 
   attributes in the new file could be different from the original file 
   that should be transparent to the users.
 
   Please direct any comments or questions to:
   gsfc-cdf-support@lists.nasa.gov  
