***************************************************************************
*   Please make sure that you have done the following before proceeding   *
*   with the CDF Json utilities:                                          * 
*                                                                         *
*     - Installed JDK 1.7 or later.                                       *
*     - Installed/built the CDF distribution package.                     *
*     - On Windows, executed the setenv.bat located in the top            *
*       distribution to set up proper environment variables. On other     *
*       platforms, run the similar script in bin directory once the       *
*       distribution is built/installed.                                  *
***************************************************************************

Instructions for using the CDF's Json programs 
-----------------------------------------------

1. Make sure that CDF distribution is installed. If downloading the source,
   the CDF-JNI library needs to be built.

2. Set the CLASSPATH environment variable to the directory that contains
   cdfjava.jsr, cdfjson.jar and other jar files that comes from the CDF
   distribution.

3. CDF2Json: convert a CDF file into a CDF-specific Json form.

   To get the help information and valid arguments/options, enter:

       java CDF2Json 

   Use the proper options to run the converter.

4. Json2CDF: convert a CDF-specific Json file into a CDF file.

   To get the instructions and valid arguments/options, enter:

       java Json2CDF

   Use the proper options to run the converter.

5. CDFJsonReader: display the data contents from a CDF-specific Json file

   To get the instructions and valid arguments/options, enter:

       java CDFJsonReader

   Use the proper options to run the reader.
 
Please send any comments or questions to:

   NASA-CDF-Support@nasa.onmicrosoft.com
 
