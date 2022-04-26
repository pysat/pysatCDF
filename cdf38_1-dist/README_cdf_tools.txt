CDF Utilities
=============
Below is a list of the CDF tools that are included as part of the standard CDF 
distribution package and their descriptions. 

There are two different types of the CDF tools are distributed in the CDF
package: command line-based and GUI-based. 

GUI CDF Tools
-------------
  1) CDFToolsDriver.jar - contains the GUI version (Java) of the CDF tools

       This file contains a complete suite of the CDF tools.

       See HOWTO.run-Java-tools.txt in the <cdf_dir> directory on 
       how to run/invoke the GUI tools.

  2) CDFso.exe (Windows only)

       This program contains MS Foundation Class-based CDFstats, CDFconvert, 
                 CDFcompare, CDFinquire, CDFdump, SkeletonCDF, and 
                 SkeletonTable

       Double-click the program and select the program you want to invoke 
       from the 'File' menu item.

  3) CDFfsi.exe (Windows only)

       This program contains MS Foundation Class-based CDFedit and CDFexport 

       Double-click the program and select the program you want to invoke 
       from the 'File' menu item.
 

Command-line CDF Tools
----------------------
The following programs can be found under the <cdf_install_dir>/bin directory:

  1) cdfcompare
  2) cdfconvert
  3) cdfdump
  4) cdfedit
  5) cdfexport
  6) cdfinquire
  7) cdfirsdump
  8) cdfleapsecondsinfo
  9) cdfmerge
 10) cdfstats
 11) cdfvalidate
 12) skeletoncdf
 13) skeletontable

Enterring the program name alone will display the help message.


Description of CDF Utilities
============================
CDFcompare 
     This utility is used to display a directory listing of a CDF's files. 
     The .cdf file is displayed first followed by the rVariable files and 
     then the zVariable files (if either exist in a multi-file CDF) in 
     numerical order. 

CDFconvert 
     This program is used to convert various properties of a CDF. In all 
     cases new CDFs are created (existing CDFs are not modified.) Any 
     combination of the following properties may be changed when converting 
     a CDF. 

       - A CDF created with an earlier release of the CDF library (e.g., CDF 
         V2.5) may be converted to the current library release. 
       - The format of the CDF may be changed. 
       - The data encoding of the CDF may be changed. 
       - The variable majority of the CDF may be changed. 

CDFdump 
     This program dumps the data contents from a CDF file, including data
     and metadata as well as variable information (e.g. data type, 
     dimensionality, number of written records, blocking factor, etc.).

CDFedit 
     This program allows the display and/or modification of practically all 
     of the contents of a CDF by way of a full-screen interface. It is also 
     possible to run CDFedit in a browse-only mode if that's desired. 

CDFexport 
     This program allows the entire contents or a portion of a CDF file to be 
     exported to the terminal screen, a text file, or another CDF. The 
     variables to be exported can be selected along with a filter range for 
     each variable which allows a subset of the CDF to be generated. 
     When exporting to another CDF, a new compression and sparseness can be 
     specified for each variable. When exporting to the terminal screen or 
     a text file, the format of the output can be tailored as necessary. 

CDFinquire 
     This program displays the version of the CDF distribution being used and 
     the default toolkit qualifiers. 

CDFirsdump 
     This program dumps the Inernal Records (IRs) in a CDF file.

CDFleapsecondsinfo 
     This program shows how the leap seconds is referenced by the CDF library
     and optioanlly displays the table contents.

CDFmerge 
     This program merges multiple CDFs into a single one.

CDFstats 
     This program produces a statistical report on a CDF's variable data. 
     Both rVariables and zVariables are analyzed. For each variable it 
     determines the actual minimum and maximum values (in all of the variable 
     records), the minimum and maximum values within the valid range (if the 
     VALIDMIN and VALIDMAX vAttributes and corresponding entries are present 
     in the CDF), and the monotonicity. An option exists to allow fill 
     values (specified by the FILLVAL vAttribute) to be ignored when 
     collecting statistics. 

CDFvalidate 
     This program validates if a given file is a valid CDF file. If it's a valid
     CDF file, it check's the integrity of CDF internal data structures.

SkeletonTable 
     This program is used to create an ASCII text file called a skeleton table 
     containing information about a given CDF (SkeletonTable can also be 
     instructed to output the skeleton table to the terminal screen.) It reads 
     a CDF file and writes into the skeleton table the following information. 

        1. Format (single or multi file), data encoding, variable majority. 
        2. Number of dimensions and dimension sizes for the rVariables. 
        3. gAttribute definitions (and gEntry values). 
        4. rVariable and zVariable definitions and vAttribute definitions 
           (with rEntry/zEntry values). 
        5. Data values for all or a subset of the CDF's variables. 
           Traditionally, only NRV variable values are written to a skeleton 
           table. RV variable values may now also be written. The above 
           information is written in a format that can be "understood" by the 
           SkeletonCDF program. SkeletonCDF reads a skeleton table and creates 
           a new CDF (called a skeleton CDF). 

SkeletonCDF 
     This program is used to make a fully structured CDF, called a skeleton 
     CDF, by reading a text file called a skeleton table. The skeleton table 
     contains the information necessary to create a CDF that is complete in 
     all respects except for record-variant (RV) var iable values. (RV 
     variables vary from record to record.) RV values are then written to the 
     CDF by the execution of an application program. The SkeletonCDF program 
     allows a CDF to be created with the following. 

        1. The necessary header information - the number of dimensions and 
           dimension sizes for the rVariables, format, data encoding, and 
           variable majority. 
        2. The gAttribute definitions and any number of gEntries for each. 
        3. The rVariable and zVariable definitions. 
        4. The vAttribute definitions and the entries corresponding to each 
           variable. 
        5. The data values for those variables that are non-record-variant 
           (NRV). NRV variables do not vary from record to record.

CDF2CDFML
     This program extracts the contents of a CDF file into a XML file that 
     conforms to the CDF DTD or schema (a.k.a CDF Markup Language).
 
CDFML2CDF: 
     This program creates a CDF file from a XML/CDFML file that conforms to 
     the CDF DTD or schema.   
 
     The XML parser, SAX, is required to run this program.  Note that Sun's 
     JDK 1.4 or a later version includes a XML parser so you don't need to
     download a XML parser. Java Virtual Machines (JVMs) from some vendors 
     don't include a XML parser.  If that's the case, please download a XML 
     parser of your choice. 

