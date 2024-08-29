/*****************************************************************************
* Copyright 1996-2013 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/

import java.io.*;
import java.text.*;
import java.util.*;
import java.lang.*;
import java.lang.reflect.*;
import gsfc.nssdc.cdf.*;
import gsfc.nssdc.cdf.util.*;
import javax.json.*;

/****************************************************************************
  Description:
    This program exports the contents of a CDF file(s) into a Json file
 
    For single CDF file, the file is specified at the last of the auguments. 
    The name of the input CDF file is used as the default output file name.
    Alternatively, use "fileNames" option to specify the CDf file(s) for
    conversions.

  Usage: java CDF2Json [Options] [cdfFileName] 
 
  Options: 
 
     -fileNames:fileNames
         fileNames: specifies a text file that contains the names of CDF files
                    for converting into a single Json file.
		    If this option is used, there is no need for entering
		    cdfFileName
 
     -globalAttrs:[yes | no]   -globalAttrs:yes is the default.
         -globalAttrs:yes extracts all global attributes
         -globalAttrs:no doesn't extract global attributes
 
     -varAttrs:[yes | no]   -varAttrs:yes is the default.
         -varAttrs:yes extracts each variable's attributes
         -varAttrs:no doesn't extract each variable's attributes
 
     -vars:var1,var2,... 
         -vars:var1,var2,... extracts only the specified variables with
                             variable names separated by a single comma
 
     -varData:[yes | no]   -varData:yes is the default.
         -varData:yes extracts each variable's data
         -varData:no doesn't extract each record-varying variable's data
 
     -output:[outputFileName | STDOUT]
         outputFileName: The output Json file name. An extension of ".json"
                         is appended if it is not specified. If this option is
                         not specified, the input cdfFileName for a single file
                         or fileNmaes for a group of files is used for the
                         output Json file  name.
         -output:STDOUT displays the Json output on the screen.
 
     -format:[yes | no | prefer]   -format:prefer is the default.
         -format:yes uses the variable's FORMAT attribute to encode the data
         -format:prefer uses the variable's FORMAT attribute to encode the data
                        However, if the FORMAT is not properly defined, it is
                        not used.
         -format:no  uses the general Java data encoding form

     -encodeepoch:[yes | no]   -encodeepoch:yes is the default.
         -encodeepoch:yes encodes the CDF epoch data
         -encodeepoch:no  shows the CDF epoch data in its original form

     -[showProgress | Progress]
         This option displays the processing progress on the screen.  
         It shows how many CDF variables are there to process and which
         variable is being processed.
 
     -withZ
         This option works with CDF epoch data by adding an extra "Z" to
         the end of date/time of ISO 8601 form.

     -delete
         Over-ride the existing Json file.

     -debug
         Display the specifications to run the application. 

     -nofullpath
         Specifies to use only the file name portion of the entered path name 
         for each CDF in the Json file.

     -showfile
         Specifies to display the CDF file name(s) being processed. 

 The Json file, produced from CDF2Json class, has the following form. It 
 differs slightly from the origial converter, which only handled a single
 CDF. The CDFname is a part in CDFFileInfo attribute. 

 <PRE> 
 CDFname1:    <== CDF file name key
   CDFFileInfo:
     FileVersion:.....
     Format:.....
     Majority:...
     Encoding:.....
   CDFglobalAttributes:
     Gattr_name1:
       entry#:value
       ...
       ...
     Gattr_name2:
       entry#:value
       ...
       ...
   ...
   ...
   CDFVariables:
     VarName1:  <== Variable name key
       VarDescription:
         DataType:....
         ...
         ...
       VarAttributes:
         VALIDMIN:....
         VALIDMAX:....
         ...
         ...
       VarData:
       ...
     VarName2:
       VarDescription:
         DataType:....
         ...
         ...
       VarAttributes:
         VALIDMIN:....
         VALIDMAX:....
         ...
         ...
       VarData:
         ...
   EmptyAttributes:  <== Exists if there are defined attributes but no values
     GlobalAttributes: [...]
     VariableAttributes: [...]

 CDFname2:
   ...
   ...
 ... 
 ...
 </PRE> 

 This class is based on JNI-based cdfjava.jar and javax.json.jar.
 
 HISTORY:
      Jan 30, 2022     Mike Liu         Initial version.
 *****************************************************************************/

public class CDF2Json implements CDFConstants {

    private static CDF cdf = null;
    private static boolean  debug = false;

    private static String  inFile = null,      /* Input file name            */
                           outFile = null,     /* Output file name           */
                           varData = "yes",    /* Extract variable data      */
                           globalAttrs = "yes",/* Extract global attributes  */
                           format = "prefer",  /* Use FORMAT attribute       */
                           encodeEpoch = "yes",/* Encode epoch data */
                           varAttrs = "yes";   /* Extract variable attributes */

    private static boolean showProgress = false;
    private static boolean withz = false;
    private static boolean delete = false;
    private static boolean fullpath = true;
    private static boolean encodeEp = true;
    private static boolean showfile = false;
    private static JsonObjectBuilder CDFJsonObject0 = Json.createObjectBuilder();
    private static JsonObjectBuilder CDFJsonObject = null;
    private static int varCount = -1;
    private static String[] varNames;
    private static String[] fileNames = null;
    private static int numFiles = 0;
    private static String item0 = "CDF";
    private static String item1 = "CDFFileInfo";
    private static String item2 = "CDFglobalAttributes";
    private static String item3 = "CDFVariables";
    private static String item4 = "VarDescription";
    private static String item5 = "VarAttributes";
    private static String item6 = "VarData";
    private static String item7 = "EmptyAttributes";
    private static String item8 = "GlobalAttributes";
    private static String item9 = "VariableAttributes";
    private static String separator = System.getProperty("file.separator");
private static long numDimsN;
    public static void main(String[] args) throws Exception {

        try {
            parseCmdLine(args);

            if (outFile == null) 
                outFile = getDefaultOutputFileName();

            if (!delete) {
                if (new File(outFile).exists()) {
                    System.out.println("ERROR... output file:"+outFile+
                                       " already exists. ");
                    System.exit(1);
                }
            }
            int ix;
            for (ix = 0; ix < numFiles; ++ix) {
              String originalInputFilename = fileNames[ix];
              if (showfile) System.out.println(fileNames[ix]);
              cdf = CDF.open(originalInputFilename, READONLYoff);

              CDFJsonObject = Json.createObjectBuilder();
              JsonArrayBuilder two = printCDFFileInfo(originalInputFilename);
              if (two != null)
                CDFJsonObject.add (item1, two);

              if (globalAttrs.equalsIgnoreCase("yes")) {
                JsonArrayBuilder three = printGlobalAttributes();

              if (three != null)
                CDFJsonObject.add (item2, three);
              }

              JsonArrayBuilder four = printVariables();
              if (four != null)
                CDFJsonObject.add (item3, four);

              JsonArrayBuilder five = printEmptyAttributes();
              if (five != null)
                CDFJsonObject.add (item7, five);

              cdf.close();

              String cdfnameKey = null;
              if (fullpath)
                cdfnameKey = (!originalInputFilename.endsWith(".cdf") &&
                              !originalInputFilename.endsWith(".CDF")) ?
                              originalInputFilename+".cdf" :
                              originalInputFilename;
              else {
                int loc = originalInputFilename.lastIndexOf(separator);
                if (loc == -1) 
                  cdfnameKey = (!originalInputFilename.endsWith(".cdf") &&
                              !originalInputFilename.endsWith(".CDF")) ?
                              originalInputFilename+".cdf" :
                              originalInputFilename;
                else {
                  String filenamex = originalInputFilename.substring(loc+1);
                  cdfnameKey = (!filenamex.endsWith(".cdf") &&
                                !filenamex.endsWith(".CDF")) ?
                                filenamex+".cdf" : filenamex;
                }
              } 
              CDFJsonObject0.add (cdfnameKey, CDFJsonObject);
            }
            OutputStream os = new FileOutputStream(outFile);
            JsonWriter jsonWriter = Json.createWriter(os);
            jsonWriter.writeObject(CDFJsonObject0.build());
            jsonWriter.close();

        } catch (Exception e) {
            if (cdf != null) cdf.close();
            System.out.println ("Exception occurred in main.\n"+e);
        }
    }


    /*********************************************************/
    /*  Parse the command line input that is in the form of  */
    /*     java CDF2Json [Options] cdfFileName               */
    /*                                                       */
    /*  See the top of this file for a detailed description  */
    /*  of the Options.                                      */
    /*********************************************************/
    private static void parseCmdLine (String[] args) {
        String numSpaces = null;
        int which = 0;
        String[] tmpArr = new String[100];

        if (args.length == 0)        // No input is given (i.e. java CDF2Json)
            exit("");

        else {                  // Process options
            for (int i=0; i < args.length; i++) {
                 if (args[i].indexOf(":") == -1 && 
                     args[i].charAt(0) != '-') {
                     if (which == 0) which = 1;
                     if (which == 1) {
                       tmpArr[numFiles] = args[i];
                       ++numFiles;
                       if (!CDFUtils.cdfFileExists(args[i])) { 
                         System.out.println ("** Error: file '"+args[i]+
                                             "' does not exist **");
                         System.exit (1); 
		       }
                       if (numFiles == 1) inFile = args[i];
		       continue;
                     } else {
                       System.out.println("** Error: File(s) already specified"+
                                          " in option \"fileNames\"...");
		       System.exit(1);
                     }
                 }
                 int loc = args[i].indexOf(":");
                 if (args[i].toLowerCase().startsWith("-filenames:")) {
                       if (which == 0) which = 2;
                       if (which == 2) {
			 String fileTxt = args[i].substring(loc+1);
                         if (fileTxt.toLowerCase().endsWith(".txt"))
                           inFile = fileTxt.substring(0, fileTxt.length()-4);
                         else if (fileTxt.toLowerCase().endsWith(".text"))
                           inFile = fileTxt.substring(0, fileTxt.length()-5);
                         else
                           inFile = fileTxt;
			 readFileNames (fileTxt);
                       } else {
                         System.out.println("** Error: File(s) already "+
                                            "specified in the command line...");
		         System.exit(1);
                       }
                 }
                 else if (args[i].toLowerCase().startsWith("-vardata:")) {
                         varData = args[i].substring(loc+1);
                         if (!varData.equalsIgnoreCase("yes") && 
                             !varData.equalsIgnoreCase("no")) 
                            exit("** Error: Invalid -varData entered **"); 
                 }
                 else if (args[i].toLowerCase().startsWith("-globalattrs:")) {
                         globalAttrs = args[i].substring(loc+1);
                         if (!globalAttrs.equalsIgnoreCase("yes") &&
                             !globalAttrs.equalsIgnoreCase("no"))
                            exit("** Error: Invalid -globalattrs entered **");
                 }
                 else if (args[i].toLowerCase().startsWith("-varattrs:")) {
                         varAttrs = args[i].substring(loc+1);
                         if (!varAttrs.equalsIgnoreCase("yes") &&
                             !varAttrs.equalsIgnoreCase("no"))
                            exit("** Error: Invalid -varattrs entered **");
                 }
                 else if (args[i].toLowerCase().startsWith("-format:")) {
                         format = args[i].substring(loc+1);
                         if (!format.equalsIgnoreCase("yes") &&
                             !format.equalsIgnoreCase("no") &&
                             !format.equalsIgnoreCase("prefer"))
                            exit("** Error: Invalid -format entered **");
                 }
                 else if (args[i].toLowerCase().startsWith("-encodeepoch:")) {
                         encodeEpoch = args[i].substring(loc+1);
                         if (!encodeEpoch.equalsIgnoreCase("yes") &&
                             !encodeEpoch.equalsIgnoreCase("no"))
                            exit("** Error: Invalid -encodeepoch entered **");
                         else {
                           if (encodeEpoch.equalsIgnoreCase("no"))
                             encodeEp = false;
                         }
                 }
                 else if (args[i].toLowerCase().startsWith("-vars:")) {
                         String names = args[i].substring(6);
                         int e;
                         varCount = 1;
                         for (e = 0; e < names.length(); e++) {
                           if (names.charAt(e) == ',') varCount++;
                         }
                         varNames = new String[varCount];
                         StringBuffer tmp = new StringBuffer(names);
                         for (e = 0; e < varCount; ++e) {
                            int dstr = tmp.indexOf(",");
                            if (dstr != -1) {
                              varNames[e] = tmp.substring(0, dstr).toString();
                              tmp = new StringBuffer(tmp.substring(dstr+1));
                            } else {
                              varNames[e] = new String(tmp);
                            }
                         }
                 }
                 else if (args[i].toLowerCase().startsWith("-output:") ||
                          args[i].toLowerCase().startsWith("-json:")) {
                         outFile = args[i].substring(loc+1);
                         if (outFile.equalsIgnoreCase("stdout"))
                             outFile = "STDOUT";
                         else { 
                             if (!outFile.toLowerCase().endsWith(".json"))
                                 outFile = outFile + ".json";
                         }
                 }
                 else if (args[i].equalsIgnoreCase("-showprogress") ||
                              args[i].equalsIgnoreCase("-progress")) {
                         showProgress = true;
                 }
                 else if (args[i].equalsIgnoreCase("-withz")) {
                         withz = true;
                 }
                 else if (args[i].toLowerCase().startsWith("-debug")) {
                         debug = true;
                 }
                 else if (args[i].equalsIgnoreCase("-delete")) {
                         delete = true;
                 }
                 else if (args[i].equalsIgnoreCase("-nofullpath")) {
                         fullpath = false;
                 }
                 else if (args[i].equalsIgnoreCase("-showfile")) {
                         showfile = true;
                 }
                 else {
                         exit ("** Error: Invalid option entered **");
                 }
            }
        }
        if (debug) {
            for (int i=0; i < args.length; i++)
                 System.out.println ("args["+i+"] = "+args[i]);
            System.out.println ("inFile="+inFile+", outputFile="+outFile+
                                ", globalattrs= "+globalAttrs+ 
                                ", varAttrs= "+varAttrs+ 
                                ", vars= all"+
                                ", vardata= "+varData+ 
                                ", showProgress="+showProgress+
                                ", withz="+withz+
                                ", showfile="+showfile+
                                ", delete="+delete);
        }

        if (inFile == null) exit ("** Error: CDF file name not specified **");

        if (which == 1) {
          fileNames = new String[numFiles];
          System.arraycopy(tmpArr, 0, fileNames, 0, numFiles);
        } 
        
    }


    /************************************************************************
     *  If a compression is specified, print the compression type and its
     *  compression level.  The compression level is printed only if
     *  the compression method is GZIP.
     ************************************************************************/
    private static String printCompression (long cType,
                                            long[] compressionLevel) {
        if (cType != CDFConstants.NO_COMPRESSION) {
          String compression = CDFUtils.getStringCompressionType(cType);
          if (cType != CDFConstants.GZIP_COMPRESSION)
            return compression;
          else
            return compression+"."+compressionLevel[0];
        } else
          return null;
    }


    /************************************/
    /*  Print the CDF file information  */
    /************************************/
    private static JsonArrayBuilder printCDFFileInfo (String theFile) {
        try {
          String version = cdf.getVersion();
          String fileFormat = CDFUtils.getStringFormat(cdf);
          String majority = CDFUtils.getStringMajority(cdf);
          String encoding = CDFUtils.getStringEncoding(cdf);
          String checksum = CDFUtils.getStringChecksum(cdf);
          long lastUpdated = CDFUtils.getLeapSecondLastUpdated(cdf);

          JsonArrayBuilder infos = Json.createArrayBuilder();

          JsonObjectBuilder versionO = Json.createObjectBuilder().
                                                  add("FileVersion", version);
          infos.add(versionO.build());
          
          JsonObjectBuilder formatx = Json.createObjectBuilder().add("Format",
                                                                    fileFormat);
          infos.add(formatx.build());

          long   cType = cdf.getCompressionType();
          long[] compressionLevel = cdf.getCompressionParms();
          String comp = printCompression(cType, compressionLevel);
          if (comp != null) {
            JsonObjectBuilder compress = Json.createObjectBuilder().
                                                    add("Compression", comp);
            infos.add(compress.build());
          }

          JsonObjectBuilder majorityO = Json.createObjectBuilder().
                                                   add("Majority", majority);
          infos.add(majorityO.build());

          JsonObjectBuilder encodingO = Json.createObjectBuilder().
                                                   add("Encoding", encoding);
          infos.add(encodingO.build());
/*          
          long negToPosFp0 = cdf.confirmNegtoPosfp0();
          String enable;
          if (negToPosFp0 == CDFConstants.NEGtoPOSfp0on)
              enable = "ENABLE";
          else
              enable = "DISABLE";
          JsonObjectBuilder negToPosFp = Json.createObjectBuilder().
                                                    add("NegToPosFp0", enable);
          infos.add(negToPosFp.build());

          if (!checksum.equalsIgnoreCase("none") &&
              !checksum.equalsIgnoreCase("other")) {
              JsonObjectBuilder chksum = Json.createObjectBuilder().
                                                    add("checksum", checksum);
              infos.add(chksum.build());
          }

          if (lastUpdated > -1) {
              long yy, mm, dd;
              yy = (long) (lastUpdated / 10000);
              mm = (long) ((lastUpdated - yy * 10000) / 100);
              dd = lastUpdated - yy * 10000 - mm * 100;
              JsonObjectBuilder leap = Json.createObjectBuilder().add(
                                       "LeapsecondLastUpdated",yy+"-"+
                                       (mm<10?"0":"")+mm+"-"+(dd<10?"0":"")+dd);
              infos.add(leap.build());
          }
*/
          return infos;

      } catch (Exception e) {
          System.out.println ("** Error occurred in printCDFFileInfo");
          System.out.println (e);
          System.exit (1);
      }
      return null;
    }


    /*************************************/
    /*  Print the CDF global attributes  */
    /*************************************/
    private static JsonArrayBuilder printGlobalAttributes () {
        try {
            Attribute a = null;
            String    attrName = null;
            int       i;
            long      numAttrs = cdf.getNumGattrs();
            Vector    ga = cdf.getGlobalAttributes();

            if (numAttrs > 0) {
                JsonArrayBuilder entries = Json.createArrayBuilder();
                JsonArrayBuilder globals = Json.createArrayBuilder();
                for (Enumeration e = ga.elements() ; e.hasMoreElements() ;) {
                    a = (Attribute) e.nextElement();
                    attrName = a.getName();
                    long n   = a.getNumEntries();
                    if (n == 0) continue;
                    Vector ent = a.getEntries();
                    int iflag;
                    for (Enumeration e1=ent.elements(); e1.hasMoreElements();) {
                       Entry entry = (Entry) e1.nextElement();
                       if (entry != null) {
                         long eDataType = entry.getDataType();
                         long eNumElems = entry.getNumElements();
                         Object data = entry.getData();
			 if (eDataType == CDF_EPOCH) iflag = 1; 
			 else if (eDataType == CDF_EPOCH16) iflag = 2;
                         else if (eDataType == CDF_TIME_TT2000) iflag = 3;
                         else iflag = 0;
                         String entryID = String.valueOf(entry.getID());
                         JsonObjectBuilder entryO = Json.createObjectBuilder(); 
                         entryO = (JsonObjectBuilder) printSingleData (
                                         (Object) entryO, entryID, data,
                                         eDataType, eNumElems, -1,
                                         (withz?"Z":""), null, null, false);
                         entries.add(entryO.build());
                       }
                    }
                    JsonObjectBuilder global1 = Json.createObjectBuilder().
                                                     add(attrName, entries);
                    globals.add(global1.build());
                }
                return globals;
            }

        } catch (Exception e) {
            System.out.println ("** Error occurred in printGlobalAttributes");
            System.out.println (e);
            System.exit (1);
        }
        return null;
    }


    /*************************************************************************  
     *  Print the following information for each variable in a CDF file:
     *    - variable information (e.g. dimensionality, dimesnion sizes, etc.)
     *    - variable attributes
     *    - variable data
     ************************************************************************/
    private static JsonArrayBuilder printVariables () {

        long numVars = 0;
        Variable v = null;

        try {
            int i = 1;
            numVars = cdf.getNumVars();

            if (numVars > 0) {
               JsonArrayBuilder variables = Json.createArrayBuilder();

               Vector  vars = cdf.getVariables();
               String cFormat = null;

               for (Enumeration e = vars.elements() ; e.hasMoreElements() ;) {
                   v = (Variable) e.nextElement();
                   String varName = v.getName();
                   Object fillVal = null;
                   if (!format.toLowerCase().equals("no")) {
                     String formatA;
                     try {
                       String format0 = (String) v.getEntryData("FORMAT");
                       if (format0.trim().length() == 0) formatA = null;
                       else formatA = format0.trim();
                     } catch (CDFException xx) {
                       formatA = null;
                     }
                     if (formatA != null) cFormat = toCformat(formatA);
                   }
                   try {
                     fillVal = v.getEntryData("FILLVAL");
                   } catch (CDFException xx) {
                     try {
                       fillVal = v.getPadValue();
                     } catch (CDFException xy) {
                       fillVal = null;
                     }
                   }
                   boolean toPrint = true;
                   if (varCount > 0) {
                     toPrint = false;
                     for (int jj = 0; jj < varCount; ++jj) {
                       if (varName.equals(varNames[jj])) {
                         toPrint = true;
                         break;
                       }
                     }
                   }
                   if (toPrint) {
                     if (showProgress) {
                       long maxRec = v.getMaxWrittenRecord() + 1;
                       System.out.print ("Variable "+i+": "+varName+" of "+
                                         (varCount>0?varCount:numVars)+
                                         " ("+(maxRec<10?" ":"")+maxRec+
                                         " records): ");
                       i++;
                     }
                     JsonArrayBuilder variable = Json.createArrayBuilder();
                     JsonObjectBuilder variableO = Json.createObjectBuilder();
                     JsonArrayBuilder one = printVarInfo(v, cFormat);
                     if (one != null)
                       variable.add(Json.createObjectBuilder().
                                           add(item4, one).build());
                     if (varAttrs.equalsIgnoreCase("yes")) {
                       JsonArrayBuilder two = printVarAttributes(v, cFormat,
                                                                 fillVal);
                       if (two != null)
                         variable.add(Json.createObjectBuilder().
                                             add(item5, two).build());
                     }
                     if (varData.equalsIgnoreCase("yes") ||
                         !(v.getRecVariance())) {
                       JsonArrayBuilder three = printVarData(v, cFormat,
                                                             fillVal);
                       if (three != null)
                         variable.add(Json.createObjectBuilder().
                                             add(item6, three).build());
                     }
//                     variableO.add("Variable", variable);
                     variableO.add(varName, variable);
                     variables.add(variableO);
                   }
               }
               return variables;
            }

        } catch (Exception e) {
            System.out.println ("** Error occurred in printVariables");
            System.out.println ("** # of variables: "+numVars);
            System.out.println ("** variable: "+v.getName());
            System.out.println (e);
            System.exit (1);
        }
        return null;
    }


    /*************************************************************************
     *  Print the CDF variable information.
     ************************************************************************/
    private static JsonArrayBuilder printVarInfo (Variable v, String formatV) {
        try {
            String  recVariance, sparseRecord, variance;
            int     i;
            long    dataType, numDims, numElements, blockingFactor;
            long    numRecords, maxWrittenRec;
            long[]  dimSizes = {1L}, dimVariances;
            Object  padValue = null;
            String  varName, strDataType;

            JsonArrayBuilder infos = Json.createArrayBuilder();
/*
            varName         = v.getName();
            JsonObjectBuilder vn = Json.createObjectBuilder().
                                        add("VarName",varName);
            infos.add (vn.build());
            if (showProgress) {
              System.out.print (varName);
            }
*/
            dataType         = v.getDataType();
            strDataType = CDFUtils.getStringDataType(dataType);
            JsonObjectBuilder vd = Json.createObjectBuilder().
                                        add("DataType",strDataType);
            infos.add (vd.build());
            if (showProgress) {
              if (v.getName().length() < 9)
                System.out.print ("\t\t"+strDataType+"/");
              else
                System.out.print ("\t"+strDataType+"/");
            }

            numElements      = v.getNumElements();
            JsonObjectBuilder ne = Json.createObjectBuilder().
                                        add("NumElements",numElements);
            infos.add (ne.build());
            if (showProgress) {
                System.out.print (numElements);
                if (dataType == CDF_TIME_TT2000) System.out.print ("  ");
                else System.out.print ("\t");
            }

            numDims          = v.getNumDims();
            dimSizes         = v.getDimSizes();
            dimVariances     = v.getDimVariances();

//            long numDimsN;
            long[] dimSizesN = new long[(int)CDF_MAX_DIMS];
            numDimsN = 0;
            for (i = 0; i < numDims; i++) {
              if (dimVariances[i] == CDFConstants.VARY &&
                  dimSizes[i] > 1) {
                dimSizesN[(int)numDimsN] = dimSizes[i];
                ++numDimsN;
              }
            }
            JsonObjectBuilder nd = Json.createObjectBuilder().
                                        add("NumDims",numDimsN);
            infos.add (nd.build());
            if (showProgress) 
                System.out.print (numDimsN+":[");
            if (numDimsN > 0) {
              JsonObjectBuilder ds = Json.createObjectBuilder();
              JsonArrayBuilder dsO = Json.createArrayBuilder();
              for (i=0; i < numDimsN; i++) {
                 if (i > 0) {
                         if (showProgress)
                             System.out.print (",");
                  }    
                  dsO.add(dimSizesN[i]);
                  if (showProgress)
                      System.out.print (dimSizesN[i]);
              }
              ds.add("DimSizes",dsO);
              infos.add (ds.build());
            }
            if (showProgress)
                System.out.print ("]   "+(numDims==0?" ":""));

            recVariance      = v.getRecVariance() ? "VARY" : "NOVARY";
            JsonObjectBuilder rv = Json.createObjectBuilder().
                                        add("RecVariance",recVariance);
            infos.add (rv.build());
            if (showProgress) {
                if (recVariance.equalsIgnoreCase("vary"))
                    System.out.print ("T/");
                else
                    System.out.print ("F/");
            }

            if (numDimsN > 0) {
              JsonObjectBuilder dv = Json.createObjectBuilder();
              JsonArrayBuilder dvO = Json.createArrayBuilder();
              for (i=0; i < numDimsN; i++) {
                   dvO.add("VARY");
                   if (showProgress) {
                       System.out.print ("T");
                   }
              }
              dv.add("DimVariances",dvO);
              infos.add (dv.build());
            }

            if (showProgress) System.out.println ("");

            long   cType = v.getCompressionType();
            long[] compressionLevel = v.getCompressionParms();
            String comp = printCompression (cType, compressionLevel);
            if (comp != null) {
              JsonObjectBuilder compO = Json.createObjectBuilder().
                                             add("Compression",comp);
              infos.add (compO.build());
            }
 
            numRecords       = v.getNumWrittenRecords();
            sparseRecord     = CDFUtils.getStringSparseRecord(v);
            if (sparseRecord.equalsIgnoreCase("none")) {
                if (numRecords > 0) {		
                  JsonObjectBuilder nw = Json.createObjectBuilder().
                                              add("NumWrittenRecords",
                                                  numRecords);
                  infos.add (nw.build());
                }
            }
            else {
                maxWrittenRec   = v.getMaxWrittenRecord();
                JsonObjectBuilder sr = Json.createObjectBuilder().
                                            add("SparseRecords",sparseRecord);
                infos.add (sr.build());
                JsonObjectBuilder nw = Json.createObjectBuilder().
                                            add("NumWrittenRecords",numRecords);
                infos.add (nw.build());
                JsonObjectBuilder mw = Json.createObjectBuilder().
                                            add("MaxWrittenRec",maxWrittenRec);
                infos.add (mw.build());

            }

            if (v.checkPadValueExistence()) {    // Pad value has been defined
                JsonObjectBuilder pv = Json.createObjectBuilder();
                padValue         = v.getPadValue();
                pv = (JsonObjectBuilder) printSingleData ((Object)pv,
                                         "PadValue", padValue, dataType,
                                         numElements, -1, (withz?"Z":""),
                                         formatV, null, false);
                infos.add (pv.build());
            }

            blockingFactor   = v.getBlockingFactor();
            if (blockingFactor > 0) {
                JsonObjectBuilder bf = Json.createObjectBuilder().
                                           add("BlockingFactor",blockingFactor);
                infos.add (bf.build());
            }
            return infos;

        } catch (Exception e) {
            System.out.println ("** Error occurred in printVariables");
            System.out.println (e);
            System.exit (1);
        }
        return null;
    }


    /*************************************************************************
     *  Print the CDF variable attributes
     ************************************************************************/
    private static JsonArrayBuilder printVarAttributes (Variable v,
                                                        String formatT,
                                                        Object fillVal) {
        Attribute a = null;
        Entry entry = null;
        String    attrName = null;

        try {
            /**************************************************************
                Check and see if there are any variable attributes that
                are associated with the passed Variable.                   
             **************************************************************/
            Vector   va = v.getAttributes();
            if (va.size() == 0) return null;      
            long vDataType = v.getDataType();
            /*******************************************/
            /*  Process and print variable attributes  */
            /*******************************************/
            JsonArrayBuilder entries = Json.createArrayBuilder();
            for (Enumeration e=va.elements(); e.hasMoreElements();) {
                 a = (Attribute) e.nextElement();
                 attrName = a.getName();
                 try {
                   entry = a.getEntry(v);
                   if (entry != null) {
                      long eDataType = entry.getDataType();
                      long eNumElements = entry.getNumElements();
                      Object data = entry.getData();
                      JsonObjectBuilder entryO = Json.createObjectBuilder();
                      entryO = (JsonObjectBuilder) printSingleData (
                                             (Object)entryO, attrName, data,
                                             eDataType, eNumElements, vDataType,
                                             (withz?"Z":""), formatT, fillVal,
                                             false);           
                      entries.add (entryO.build());
                   }
                 } catch (CDFException ex) {
                 }
            }
            return entries;


        } catch (Exception e) {
            System.out.println ("** Error occurred in printVarAttributes");
            System.out.println ("attr= "+attrName+" entry ="+entry.getName());
            System.out.println (e);
            System.exit (1);
        }
        return null;
    }


    /*************************************************************************
     *  Print the empty attributes.
     ************************************************************************/
    private static JsonArrayBuilder printEmptyAttributes () {

        Vector gAttributes = cdf.getGlobalAttributes();
        Vector vAttributes = cdf.getVariableAttributes();
        JsonArrayBuilder eattrs = null;
        JsonArrayBuilder gattrs = null;
        JsonArrayBuilder vattrs = null;

        int ix;
        ix = 0;
        for (Enumeration e=gAttributes.elements(); e.hasMoreElements();) {
          Attribute a = (Attribute)e.nextElement();
          long num = a.getNumEntries();
          if (num != 0) continue;
          ++ix;
          if (ix == 1) {
            gattrs = Json.createArrayBuilder();
          }
//          JsonObjectBuilder eg = Json.createObjectBuilder().
//                                        add(a.getName(),null);
          gattrs.add(a.getName());
        }
        if (ix != 0) {
          eattrs = Json.createArrayBuilder();
          eattrs.add(Json.createObjectBuilder().add("GlobalAttributes",
                                                    gattrs).build());
        }
 
        ix = 0;
        for (Enumeration e=vAttributes.elements(); e.hasMoreElements();) {
          Attribute a = (Attribute)e.nextElement();
          long num = a.getNumEntries();
          if (num != 0) continue;
          ++ix;
          if (ix == 1) {
            vattrs = Json.createArrayBuilder();
          }
//          JsonObjectBuilder eg = Json.createObjectBuilder().
//                                        add(a.getName(),null);
          vattrs.add(a.getName());
        }
        if (ix != 0) {
          if (eattrs == null)
            eattrs = Json.createArrayBuilder();
          eattrs.add(Json.createObjectBuilder().add("VariableAttributes",
                                                    vattrs).build());
        }

        return eattrs;
    }

    /*************************************************************************
     *  Print the variable data.
     ************************************************************************/
    private static JsonArrayBuilder printVarData (Variable v, String formatT,
                                                  Object fillVal) {
        try {
            long    maxRec, numDims, numElements, numValuesToRead, dataType;
            long    recCount;
            boolean recVary;
            long[]  dimSizes = {1L}, dimVariances;
            long[]  dimIndices   = {0L};
            long[]  dimIntervals = {1L};
//            long numDimsN = 0;
            long[] dimSizesN = null;
            maxRec = v.getMaxWrittenRecord();

            /*******************************************************
               if varData = "no", do not extract variable data.
               if maxRec < 0, then the variable doesn't have data.
            ********************************************************/
            if (maxRec < 0) { 
                return null;
            }

            recVary      = v.getRecVariance();
            numDims      = v.getNumDims();
            dimSizes     = v.getDimSizes();
            numElements  = v.getNumElements();
            dataType     = v.getDataType();
            dimVariances = v.getDimVariances();
            numValuesToRead = 1;
/*
            if (numDims > 0) {
                dimIntervals = new long[dimSizes.length];
                dimIndices   = new long[dimSizes.length];
                dimSizesN    = new long[dimSizes.length];
                int j = 0;
                numDimsN = 0;
                for (int i=0; i < dimSizes.length; i++) {
                     if (dimVariances[i] == VARY) {
                       ++numDimsN;
                       dimSizesN[j] = dimSizes[i];
                       ++j;
                     }
                     dimIntervals[i] = 1;
                     dimIndices[i]   = 0;
                     numValuesToRead *= dimSizes[i];
                }
            }
*/
            if (numDimsN > 0) {
                dimIntervals = new long[(int)numDimsN];
                dimIndices   = new long[(int)numDimsN];
                dimSizesN    = new long[(int)numDimsN];
                int j = 0;
//                numDimsN = 0;
                for (int i=0; i < numDimsN; i++) {
                     if (dimVariances[i] == VARY) {
//                       ++numDimsN;
                       dimSizesN[j] = dimSizes[i];
                       ++j;
                     }
                     dimIntervals[i] = 1;
                     dimIndices[i]   = 0;
                     numValuesToRead *= dimSizes[i];
                }
            }

            JsonArrayBuilder records = Json.createArrayBuilder();

            /********************************************************/
            /*  maxRec represents the last record number for this   */
            /*  variable, not the number of records.                */
            /*                                                      */
            /*  NOTE: maxRec starts at 0, so if the value of maxRec */
            /*        is 2, the actual number of records is 3.      */
            /*        If there are no records exists, the value of  */
            /*        maxRec is -1.                                 */
            /********************************************************/
            long incrementCounter, numRecordsToRead, recNo;
            long sparseRecord = v.getSparseRecords();

            if (sparseRecord == NO_SPARSERECORDS) 
                incrementCounter = (numDimsN == 0 ? 1000 : 200);
            else
                incrementCounter = 1;
            CDFData cdfdata;
            for (int i=0; i <= maxRec; i+=incrementCounter) {
                 if ((i+incrementCounter) > maxRec)
                     numRecordsToRead = maxRec - i + 1;
                 else
                     numRecordsToRead = incrementCounter; 
                 if (numDimsN > 0) {
                     cdfdata = v.getHyperDataObject((long) i, 
                                                    numRecordsToRead, 1L,
                                                    dimIndices,
                                                    dimSizes,
                                                    dimIntervals);
                 } else {
                     cdfdata = v.getRecordsObject((long) i,
                                                   numRecordsToRead);
                 }
                 // Get the status of the last CDF call
                 long status = cdf.getStatus();
                 Object dataArray = cdfdata.getData();
                 for (int j=0; j < numRecordsToRead; j++) {
                      Object datum;
                      if (numRecordsToRead > 1) {
                          if (dataType != CDF_EPOCH16)
                            datum = Array.get(dataArray, j);
                          else {
                            double[] epoch16 = new double[2];
                            epoch16[0] = ((Double) Array.get(dataArray, 2*j)).
                                          doubleValue();
                            epoch16[1] = ((Double) Array.get(dataArray, 2*j+1)).
                                          doubleValue();
                            datum = (Object) epoch16;
                          }
                      } else 
                          datum = dataArray;

                      if (numDimsN > 0 && numDims != numDimsN) {
                        // Filled data for non-varying dimension(s)
                        long iz;
                        iz = numDims;
                        while (iz != numDimsN) {
                          datum = Array.get(datum, 0);   
                          --iz;
                        }
                      }

                      if (numDimsN < 2) {
                        boolean single = false;
                        if (!recVary || maxRec == 0) single = true;
                         records = (JsonArrayBuilder) printSingleData (
                                                    (Object)records, null,
                                                    datum, dataType, numDimsN,
                                                    -1, (withz?"Z":""), formatT,
                                                    fillVal, single);
                      } else {
                         JsonArrayBuilder oData = printMultipleData (datum,
                                                    dataType, (int)numDimsN,
                                                    dimSizes,
                                                    (withz?"Z":""), formatT,
                                                    fillVal);
                         if (!recVary || maxRec == 0) return oData;
                         else records.add (oData);
                      }
                 }   // end of for (int j=0; j < numRecordsToRead; j++)

            }     // end of "for (int i=0; i <= maxRec; i++)"
            return records;

        } catch (Exception e) {
            System.out.println ("** Error occurred in printVarData");
            System.out.println ("variable: "+v.getName());
            System.out.println (e);
            System.exit (1);
        }
        return null;
    }


    /*************************************************************************
     *  This method returns the output file name if one is not provided 
     *  through the -output option from the command line. 
     ************************************************************************/
    private static String getDefaultOutputFileName () {
        String fileName;
        int    loc1 = inFile.lastIndexOf(".cdf");
        int    loc2 = inFile.lastIndexOf(".CDF");
        boolean extCDF = inFile.endsWith(".cdf") ||
                         inFile.endsWith(".CDF");

        // Input file name given from the command line doesn't have
        // .cdf or .CDF extension in it.  
        if (!extCDF) 
            fileName = inFile + ".json";

        else { // Remove the .cdf or .CDF extension
            if (loc1 != -1) {
                fileName = inFile.substring(0,loc1);
                fileName = fileName + ".json";
            }
            else {
                fileName = inFile.substring(0,loc2);
                fileName = fileName + ".json";
            }
        }
        if (!fullpath) {
          int loc = fileName.lastIndexOf(separator);
          if (loc != -1)
            fileName = fileName.substring(loc+1);
        }
        return fileName;
    }


    /*************************************************************************
     *  This method removes the file path from the given file name and
     *  returns the file name.
     ************************************************************************/
    private static String removeFilePath (String fileName) {
        int loc = fileName.lastIndexOf(separator);
        fileName = fileName.substring(loc+1);

        return fileName;
    }


    /************************************************************************* 
     *  If an invalid option is entered from the command line, this routine 
     *  is invoked to print an appropriate message followed by its proper
     *  syntax.
     ************************************************************************/
    private static void exit (String msg) {
        if (!msg.equals("")) System.out.println (msg);
        usage();
        System.exit(1);
    }


    /************************************************************************* 
     *  Print the proper syntax for invoking this program. 
     ************************************************************************/
    private static void usage() {
       System.out.println ("\nDescription:");
       System.out.println ("    This program exports the contents of a CDF "+
                           "file(s) into a single Json file.");
       System.out.println ("    The Json file will have an \".json\" extension"+
                           " if it is not specified. ");
       System.out.println (" ");
       System.out.println ("Usage: java CDF2Json [Options] [cdfFile1 "+
                           "cdfFile2 ...]");
       System.out.println ("");
       System.out.println ("    cdfFile1 cdfFile2 .... ");
       System.out.println ("       Command line input CDF file(s) for "+
                           "conversion. A maximum of 100 ");
       System.out.println ("       files can be specified.");
       System.out.println ("       If the \"output\" option is not specified,"+
                           " the first file in the list is");
       System.out.println ("       used as the the output Json file name.");
       System.out.println ("       Alternatively, use \"fileNames\" option "+
                           "to specify a text file");
       System.out.println ("       that contains the CDF name(s) for "+
                           "conversion. Command line file(s) and");
       System.out.println ("       fileNames option are mutually exclusive.");
       System.out.println (" ");
       System.out.println ("Options:");
       System.out.println (" ");
       System.out.println ("   -fileNames:fileNames");
       System.out.println ("       fileNames is a text file that contains the "+
                           "input CDF file name(s)");
       System.out.println ("       for converting into a Json file. With this "+
                           "option, there is no need to");
       System.out.println ("       enter the file(s) in the command line."+
                           "If the output Json file is not");
       System.out.println ("       specified, this file is used as the name "+
                           "of the Json output.");
       System.out.println (" ");
       System.out.println ("   -globalAttrs:[yes | no].  -globalAttrs:yes is "+
                           "the default.");
       System.out.println ("      -globalAttrs:yes extracts all global "+
                           "attributes.");
       System.out.println ("      -globalAttrs:no doesn't extract global "+
                           "attributes.");
       System.out.println (" ");
       System.out.println ("   -varAttrs:[yes | no].  -varAttrs:yes is "+
                           "the default.");
       System.out.println ("      -varAttrs:yes extracts each variable's "+
                           "attributes.");
       System.out.println ("      -varAttrs:no doesn't extract variable's "+
                           "attributes.");
       System.out.println (" ");
       System.out.println ("   -vars:var1,var2,....");
       System.out.println ("      -vars:var1,var2,... extracts only the "+
                           "specified variables with");
       System.out.println ("                           variable names "+
                           "separated by a single ','.");
       System.out.println (" ");
       System.out.println ("   -varData:[yes | no].  -varData:yes is "+
                           "the default.");
       System.out.println ("      -varData:yes extracts each variable's data"); 
       System.out.println ("      -varData:no doesn't extract record-varying "+
                           "variable's data"); 
       System.out.println (" ");
       System.out.println ("   -format:[yes | no | prefer].  -format:prefer "+
                           "is the default.");
       System.out.println ("      -format:yes uses variable attribute FORMAT "+
                           "to encode data"); 
       System.out.println ("      -format:prefer uses variable attribute "+
                           "FORMAT to encode data."); 
       System.out.println ("                     However, if its FORMAT is not"+
                           " properly defined, it"); 
       System.out.println ("                     is treated as \"no\".");
       System.out.println ("      -format:no uses Java internal encoding to "+
                           "encode data");
       System.out.println ("      Note: The format is only relevant to "+
                           "floating-point values.");
       System.out.println (" ");
       System.out.println ("   -encodeepoch:[yes | no].  -encodeepoch:yes is "+
                           "the default.");
       System.out.println ("      -encodeepoch:yes encodes CDF epoch data "+
                           "values in time string");
       System.out.println ("      -encodeepoch:no  shows CDF epoch values in "+
                           "their original data form");
       System.out.println (" ");
       System.out.println ("   -[output | json]:outputFileName");
       System.out.println ("      The name of the input CDF file is used "+
                           "as the default output file");
       System.out.println ("      name.  Use this option to specify a "+
                           "different output file name.");
       System.out.println (" ");
       System.out.println ("   -[showProgress | Progress]");
       System.out.println ("      This option displays the processing "+
                           "progress on the screen.");
       System.out.println ("      It shows how many CDF variables are there "+
                           "to process and which");
       System.out.println ("      variable is being processed.");
       System.out.println (" ");
       System.out.println ("   -debug");
       System.out.println ("      Displays the specifications to "+
                           "run the application.");
       System.out.println (" ");
       System.out.println ("   -withZ");
       System.out.println ("      Adding \"Z\" to the encoded CDF epoch data "+
                           "of ISO 8601 form.");
       System.out.println ("");
       System.out.println ("   -delete");
       System.out.println ("      Over-ride the existing Json file.");
       System.out.println ("");
       System.out.println ("   -nofullpath");
       System.out.println ("      Only use the file name portion of the "+
                           "entered full path name as");
       System.out.println ("      the key to each CDF in the Json output. ");
       System.out.println ("");
       System.out.println ("   -showfile");
       System.out.println ("      Display the file name(s) being processed.");
       System.out.println ("");
       System.out.println ("NOTE:");
       System.out.println ("");
       System.out.println ("  1. Variable's non-varying dimension(s) is "+
                           "ignored. A varying dimension");
       System.out.println ("     with a size of only 1 is also considered a "+
                           "non-varying dimension.");
       System.out.println ("  2. NaN|-NaN and INF|-INF, valid floating-point "+
                           "values in CDF, but are not");
       System.out.println ("     allowed in Json. They are replaced by strings"+
                           " \"NaN\"|\"-NaN\" and ");
       System.out.println ("     \"INF\"|\"-INF\". Try to avoid using any of"+
                           " these as it might cause");
       System.out.println ("     unexpected results if such Json file is"+
                           " converted back to CDF.");
       System.out.println ("  3. Only value, without the data type, from "+
                           "meta-data is presented in the");
       System.out.println ("     Json output.");
       System.out.println ("  4. Use CDFJsonReader to display the Json file "+
                           "in a easy to read form.");
       System.out.println ("");
       System.out.println ("Examples: ");

       System.out.println ("   java CDF2Json test.cdf (same as "+
                           "java CDF2Json -json:test.json test.cdf)");
       System.out.println ("   java CDFJsonReader test.json ");
       System.out.println (" ");
       System.out.println ("The CDF's Json file has the following form: ");
       System.out.println (" ");
       System.out.println (" CDFname1:    <== CDF file name key\n"+
                           "   CDFFileInfo:\n"+
                           "     FileVersion:.....\n"+
                           "     Format:.....\n"+
                           "     Majority:...\n"+
                           "     Encoding:.....\n"+
                           "   CDFglobalAttributes:\n"+ 
                           "     Gattr_name1:\n"+
                           "       entry#:value\n"+
                           "       ...\n"+
                           "       ...\n"+
                           "     Gattr_name2:\n"+
                           "       entry#:value\n"+
                           "       ...\n"+
                           "       ...\n"+
                           "   ...\n"+
                           "   ...\n"+
                           "   CDFVariables:\n"+
                           "     VarName1:  <== Variable name key\n"+
                           "       VarDescription:\n"+
                           "         DataType:....\n"+
                           "         ...\n"+
                           "         ...\n"+
                           "       VarAttributes:\n"+
                           "         VALIDMIN:....\n"+
                           "         VALIDMAX:....\n"+
                           "         ...\n"+
                           "         ...\n"+
                           "       VarData:\n"+
                           "       ...\n"+
                           "     VarName2:\n"+
                           "       VarDescription:\n"+
                           "         DataType:....\n"+
                           "         ...\n"+
                           "         ...\n"+
                           "       VarAttributes:\n"+
                           "         VALIDMIN:....\n"+
                           "         VALIDMAX:....\n"+
                           "         ...\n"+
                           "         ...\n"+
                           "       VarData:\n"+
                           "         ...\n"+
                           "   EmptyAttributes:  <== Exists if there are "+
                                                 "attributes defined but no values\n"+
                           "     GlobalAttributes: [...]\n"+
                           "     VariableAttributes: [...]\n"+
                           " \n"+
                           " CDFname2:\n"+
                           "   ...\n"+
                           "   ...\n"+
                           " ...\n"+
                           " ...\n"+
                           " ");
    }

    /************************************************************************* 
     *  Output a record object with multi-dimensional data values to a
     *  JsonArray (called by printVarData). 
     ************************************************************************/

    private static JsonArrayBuilder printMultipleData (Object data,
                                                       long dataType,
                                                       int nDims,
                                                       long[] dimSizesL,
                                                       String addZ,
                                                       String formatT,
                                                       Object fillVal) {

        int[] dimSizes = new int[nDims];
        for (int i=0; i<(int)nDims;i++) {
           dimSizes[i] = (int) dimSizesL[i];
        }
        Object aRow = null;
        Object [] subArrays = new Object[(int)nDims-1];
        int [] cIndex   = new int[(int)nDims-1];
        long [] boundary = new long[(int)nDims-1];
        JsonArrayBuilder jArray = Json.createArrayBuilder();
        subArrays[0] = data;
        cIndex[0] = 0;
        boundary[0] = product(dimSizes,0,(int)nDims);
        for (int i=1; i<(int)nDims-1;i++) {
            subArrays[i] = Array.get(subArrays[i-1], 0);
            boundary[i] = product(dimSizes,i,(int)nDims);
            cIndex[i] = 0;
        }
        int n = 0; // The current element in the _data
        Object cObject = data;  // A temp object to hold a subarray
        boolean boundaryCrossed;
        while (n < boundary[0]) {
          // Get the correct 2D subarray to print
          if (n != 0) {
              for (int i = 0; i < (int)nDims-1; i++) {
                  boundaryCrossed = ((n % boundary[i]) == 0);
                  if (boundaryCrossed) {
                    // Get the next sub array
                    cIndex[i]+=1;
                    cObject = Array.get(cObject, cIndex[i]);
                    subArrays[i] = cObject;

                    // Get the first element of each
                    // subsequent subarray
                    for (int j=i+1;j<(int)nDims-1;j++) {
                        cIndex[j] = 0;
                        subArrays[j] =
                            Array.get(subArrays[j-1],cIndex[j]);
                    }
                    break;
                  } else {
                    cObject = subArrays[i];
                  }
              }
          }

          // Fill the correct elements of data
          for (int i=0;i<dimSizes[(int)nDims-2];i++) {
              switch ((int)dataType) {
              case (int)CDF_CHAR:
              case (int)CDF_UCHAR:
                  aRow = (String [])Array.get(subArrays[(int)nDims - 2], i);
                  for (int j=0; j<dimSizes[nDims - 1]; j++) {
                      jArray.add(((String [])aRow)[j]);
                  }
                  break;
              case (int)CDF_BYTE:
              case (int)CDF_INT1:
                  aRow = (byte [])Array.get(subArrays[(int)nDims - 2], i);
                  for (int j=0; j<dimSizes[nDims - 1]; j++) {
                      jArray.add(new Byte(((byte [])aRow)[j]).intValue());
                  }
                  break;
              case (int)CDF_INT2:
                  aRow = (short [])Array.get(subArrays[(int)nDims - 2], i);
                  for (int j=0; j<dimSizes[nDims - 1]; j++) {
                      jArray.add(new Short(((short [])aRow)[j]).intValue());
                  }
                  break;
              case (int)CDF_UINT1:
                  aRow = (short [])Array.get(subArrays[(int)nDims - 2], i);
                  for (int j=0; j<dimSizes[nDims - 1]; j++) {
                      if (((short [])aRow)[j] >=0)
                         jArray.add(new Short(((short [])aRow)[j]).intValue());
                      else {
                         short tmp = (short) (((short [])aRow)[j] + 256);
                         jArray.add((int)tmp);
                      }
                  }
                  break;
              case (int)CDF_INT4:
                  aRow = (int [])Array.get(subArrays[(int)nDims - 2], i);
                  for (int j=0; j<dimSizes[nDims - 1]; j++) {
                      jArray.add(((int [])aRow)[j]);
                  }
                  break;
              case (int)CDF_UINT2:
                  aRow = (int [])Array.get(subArrays[(int)nDims - 2], i);
                  for (int j=0; j<dimSizes[nDims - 1]; j++) {
                      if (((int [])aRow)[j] >=0)
                          jArray.add(((int [])aRow)[j]);
                      else {
                          int tmp = (int) (((int [])aRow)[j] + 65536);
                          jArray.add(tmp);
                      }
                  }
                  break;
              case (int)CDF_UINT4:
                  aRow = (long [])Array.get(subArrays[(int)nDims - 2], i);
                  for (int j=0; j<dimSizes[nDims - 1]; j++) {
                      if (((long [])aRow)[j] >=0)
                          jArray.add(((long [])aRow)[j]);
                      else {
                          long tmp = (long) (((long [])aRow)[j]+ 4294967296L);
                          jArray.add(tmp);
                      }
                  }
                  break;
              case (int)CDF_INT8:
              case (int)CDF_TIME_TT2000:
                  aRow = (long [])Array.get(subArrays[(int)nDims - 2], i);
                  for (int j=0; j<dimSizes[nDims - 1]; j++) {
                      if (dataType == CDF_INT8)
                        jArray.add(((long [])aRow)[j]);
                      else
                        jArray.add(CDFTT2000.encode(((long [])aRow)[j])+addZ);
                  }
                  break;
              case (int)CDF_REAL4:
              case (int)CDF_FLOAT:
                  aRow = (float [])Array.get(subArrays[(int)nDims - 2], i);
                  for (int j=0; j<dimSizes[nDims - 1]; j++) {
                    float ff = ((float [])aRow)[j];
                    if (formatT == null || ff == -10.E30f || ff == 1.0E30f)
                      jArray.add((double)ff);
                    else if (Float.isNaN(ff))
                      jArray.add ("NaN");
                    else if (Float.isNaN(-ff))
                      jArray.add ("-NaN");
                    else if (Float.isInfinite(ff))
                      jArray.add ("INF");
                    else if (Float.isInfinite(-ff))
                      jArray.add ("-INF");
                    else {
                      try {
                        String sd = String.format(formatT, ff);
                        jArray.add(new Double(sd).doubleValue());
                      } catch (Exception ec) {
                        jArray.add((double)ff);
                      }
                    }
                  }
                  break;
              case (int)CDF_REAL8:
              case (int)CDF_DOUBLE:
                  aRow = (double [])Array.get(subArrays[(int)nDims - 2], i);
                  for (int j=0; j<dimSizes[nDims - 1]; j++) {
                    double dd = ((double [])aRow)[j];
                    if (formatT == null || dd == -10.E30 || dd == 1.0E30)
                      jArray.add(dd);
                    else if (Double.isNaN(dd))
                      jArray.add ("NaN");
                    else if (Double.isNaN(-dd))
                      jArray.add ("-NaN");
                    else if (Double.isInfinite(dd))
                      jArray.add ("INF");
                    else if (Double.isInfinite(-dd))
                      jArray.add ("-INF");
                    else {
                      try {
                        String sd = String.format(formatT, dd);
                        jArray.add(new Double(sd).doubleValue());
                      } catch (Exception ec) {
                        jArray.add(dd);
                      }
                    }
                  }
                  break;
              case (int)CDF_EPOCH:
                  aRow = (double [])Array.get(subArrays[(int)nDims - 2], i);
                  for (int j=0; j<dimSizes[nDims - 1]; j++) {
                    if (encodeEp)
                      jArray.add(Epoch.encode4(((double [])aRow)[j])+addZ);
                    else
                      jArray.add(((double [])aRow)[j]);
                  }
                  break;
              case (int)CDF_EPOCH16:
                  double tmp[] = new double[2];
                  aRow = (double [])Array.get(subArrays[(int)nDims - 2], i);
                  for (int j=0; j<dimSizes[nDims - 1]; j++) {
                    tmp[0] = ((double [])aRow)[2*j];
                    tmp[1] = ((double [])aRow)[2*j+1];
                    if (encodeEp)
                      jArray.add(Epoch16.encode4(tmp)+addZ);
                    else {
                      JsonArrayBuilder epoch16 = Json.createArrayBuilder();
                      epoch16.add(tmp[0]);
                      epoch16.add(tmp[1]);
                      jArray.add(epoch16.build());
                    }
                  }
                  break;
              default:
                  break;
              } /* switch (dataType) */
              n += dimSizes[nDims - 1];
          }
        }
        return jArray;
    }

    // used to determine the boundaries
    private static long product (int [] array, int start, int stop) {

        long product = 1;
        for (int i=start; i<stop; i++)
        if (array[i] > 1) product *= array[i];

        return product;
    }

    private static boolean validORscaleORfill (String name) {

        if (name == null) return false;
        name = name.toLowerCase();
        if (name.equals("validmin") || name.equals("validmax") ||
            name.equals("scalemin") || name.equals("scalemax") ||
            name.equals("fillval")) return true;
        else return false;
    }

    private static boolean isEpochDataType (long dataType) {

        if (dataType == CDF_EPOCH || dataType == CDF_EPOCH ||
            dataType == CDF_TIME_TT2000) return true;
        else return false;
    }

    /************************************************************************* 
     *  Output an object with a single data value or an array of values to
     *  a JsonObject (called by printGlobalAttributes and printVarAttributes)
     *  or JsonArray (called by printVarData). 
     ************************************************************************/
    
    private static Object printSingleData (Object jsonObject, String name,
                                           Object data, long dataType,
                                           long numDims, long dataType2,
                                           String addZ, String formatT,
                                           Object fillVal, boolean single) {

        int iflag;
        boolean isArray = false;
        if (dataType == CDF_EPOCH) iflag = 1;
        else if (dataType == CDF_EPOCH16) iflag = 2;
        else if (dataType == CDF_TIME_TT2000) iflag = 3;
        else iflag = 0;
        int  i, arrayLength, jj = 1;
        if (iflag == 2) jj = 2;
        boolean multi = false;

        String signature = CDFUtils.getSignature(data);
        JsonObjectBuilder jObject = Json.createObjectBuilder();
        JsonArrayBuilder  jArray = Json.createArrayBuilder();
        if (jsonObject instanceof JsonObjectBuilder) 
          jObject = (JsonObjectBuilder) jsonObject;
        else
          jArray = (JsonArrayBuilder) jsonObject;
        if (signature.charAt(0) == '[') {
          arrayLength = Array.getLength(data);
          if (iflag == 2) arrayLength = arrayLength / 2;
          if (arrayLength > 1) multi = true;
          if (iflag != 2) isArray = true;
        }
        if (multi) {
            JsonArrayBuilder  jArray2 = Json.createArrayBuilder();
            arrayLength = Array.getLength(data);
            for (i=0; i < arrayLength; i=i+jj) {
                 if (signature.charAt(1) == 'B')
                     jArray2.add (Array.getByte(data,i));

                 else if (signature.charAt(1) == 'S')
                     jArray2.add (Array.getShort(data,i));

                 else if (signature.charAt(1) == 'I')
                     jArray2.add (Array.getInt(data,i));

                 else if (signature.charAt(1) == 'J')
                     if (iflag == 3 || (isEpochDataType(dataType2) &&
                                        validORscaleORfill(name))) {
                       if (encodeEp)
                         jArray2.add (CDFTT2000.encode(Array.getLong(data,i))+
                                      addZ);
                       else
                         jArray2.add (Array.getLong(data,i));
                     } else
                       jArray2.add (Array.getLong(data,i));

                 else if (signature.charAt(1) == 'F') {
                     float ff = Array.getFloat(data,i);
                     double dd;
                     if (Float.isNaN(ff))
                       jArray2.add ("NaN"); // jArray2.add (Float.NaN);
                     else if (Float.isNaN(-ff))
                       jArray2.add ("-NaN"); // jArray2.add (-Float.NaN);
                     else if (Float.isInfinite(ff))
                       jArray2.add ("INF"); // jArray2.add (Float.POSITIVE_INFINITY);
                     else if (Float.isInfinite(-ff))
                       jArray2.add ("-INF"); // jArray2.add (Float.NEGATIVE_INFINITY);
                     else {
                       if (ff == -1.0E30f) dd = -1.0E30;
                       else if (ff == -1.0E31f) dd = -1.0E31;
                       else if (ff == 1.0E30f) dd = 1.0E30;
                       else if (ff == 1.0E31f) dd = 1.0E31;
                       else dd = (double) ff;
                       if (formatT == null)
                         jArray2.add (dd);
                       else {
                         try {
                           String sd = String.format(formatT, dd);
                           jArray2.add (new Double(sd).doubleValue());
                         } catch (Exception ex) {
                           jArray2.add (dd);
                         }
                       }
                     }
                 } else if (signature.charAt(1) == 'D') {
                     double dd = Array.getDouble(data,i);
                     if (iflag == 1 || (isEpochDataType(dataType2) && 
                                        validORscaleORfill(name))) {
                       if (encodeEp)
                         jArray2.add (Epoch.encode4(dd)+addZ);
                       else
                         jArray2.add (dd);
                     } else if (iflag == 2 || (isEpochDataType(dataType2) &&
                                               validORscaleORfill(name))) {
                       double[] mmm = new double[2];
                       mmm[0] = Array.getDouble(data,i);
                       mmm[1] = Array.getDouble(data,i+1);
                       if (encodeEp)
                         jArray2.add (Epoch16.encode4(mmm)+addZ);
                       else {
                         JsonArrayBuilder epoch16 = Json.createArrayBuilder();
                         epoch16.add (mmm[0]);
                         epoch16.add (mmm[1]);
                         jArray2.add (epoch16.build());
                       }
                     } else {
                       if (Double.isNaN(dd))
                         jArray2.add ("NaN"); // jArray2.add (Double.NaN);
                       else if (Double.isNaN(-dd))
                         jArray2.add ("-NaN"); // jArray2.add (-Double.NaN);
                       else if (Double.isInfinite(dd))
                         jArray2.add ("INF"); // jArray2.add (Double.POSITIVE_INFINITY);
                       else if (Double.isInfinite(-dd))
                         jArray2.add ("-INF"); // jArray2.add (Double.NEGATIVE_INFINITY);
                       else {
                         if (formatT == null) {
                           jArray2.add (dd);
                         } else {
                           try {
                             String sd = String.format(formatT, dd);
                             jArray2.add (new Double(sd).doubleValue());
                           } catch (Exception ex) {
                             jArray2.add (dd);
                           }
                         }
                       }
                     }
                 }
                 else if (signature.indexOf("String") != -1)
                     jArray2.add (((String[])data)[i]);
            }
            if (name != null)
              return jObject.add(name, jArray2);
            else {
              if (single) return jArray2;
              else  return jArray.add(jArray2);
            }
        } else {
          if (signature.charAt(0) == '[' && iflag != 2)
            data = Array.get(data, 0);
          if (iflag == 1) {
            if (name != null) {
              if (encodeEp)
                return jObject.add(name,
                                   Epoch.encode4(((Double)data).doubleValue())+
                                   addZ);
              else
                return jObject.add(name, ((Double)data).doubleValue());
            } else {
              if (encodeEp)
                return jArray.add(Epoch.encode4(((Double)data).doubleValue())+
                                  addZ);
              else
                return jArray.add(((Double)data).doubleValue());
            }
          } else if (iflag == 2) {
            if (name != null) {
              if (encodeEp)
                return jObject.add(name, Epoch16.encode4((double[])data)+addZ);
              else {
                JsonArrayBuilder epoch16 = Json.createArrayBuilder();
                epoch16.add(((double[])data)[0]);
                epoch16.add(((double[])data)[1]);
                return jObject.add(name, epoch16.build());
              }
            } else {
              if (encodeEp)
                return jArray.add(Epoch16.encode4(((double[])data))+addZ);
              else {
                JsonArrayBuilder epoch16 = Json.createArrayBuilder();
                epoch16.add(((double[])data)[0]);
                epoch16.add(((double[])data)[1]);
                return jArray.add(epoch16.build());
              }
            }
          } else if (iflag == 3) {
            if (name != null) {
              if (encodeEp)
                return jObject.add(name,
                                   CDFTT2000.encode(((Long)data).longValue())+
                                   addZ);
              else
                return jObject.add(name, ((Long)data).longValue());
            } else {
              if (encodeEp)
                return jArray.add(CDFTT2000.encode(((Long)data).longValue())+
                                  addZ);
              else
                return jArray.add(((Long)data).longValue());
            }
          } else {
            if (signature.indexOf("Byte") != -1) {
              int value;
              value = ((Byte)data).intValue();
              if (name != null)
                return jObject.add(name, value);
              else
                return jArray.add(value);
            } else if (signature.indexOf("Short") != -1) {
              int value;
              value = ((Short)data).intValue();
              if (name != null)
                return jObject.add(name, value);
              else
                return jArray.add(value);
            } else if (signature.indexOf("Int") != -1) {
              int value;
              value = ((Integer)data).intValue();
              if (name != null)
                return jObject.add(name, value);
              else
                return jArray.add(value);
            } else if (signature.indexOf("Long") != -1) {
              long value;
              value = ((Long)data).longValue();
              if (name != null) {
                if (isEpochDataType(dataType2) && validORscaleORfill(name))
                  return jObject.add(name, CDFTT2000.encode(value)+addZ);
                else
                  return jObject.add(name, value);
              } else {
                if (isEpochDataType(dataType2) && validORscaleORfill(name))
                  return jArray.add(CDFTT2000.encode(value)+addZ);
                else
                  return jArray.add(value);
              }
            } else if (signature.indexOf("Float") != -1) {
              float ff;
              ff = ((Float)data).floatValue();
              if (Float.isNaN(ff)) {
                if (name != null)
                  return jObject.add (name, "NaN"); // jObject.add (name, Float.NaN);
                else
                  return jArray.add ("NaN"); // jArray.add (Float.NaN);
              } else if (Float.isNaN(-ff)) {
                if (name != null)
                  return jObject.add (name, "-NaN"); // jObject.add (name, -Float.NaN);
                else
                  return jArray.add ("-NaN"); // jArray.add (-Float.NaN);
              } else if (Float.isInfinite(ff)) {
                if (name != null)
                  return jObject.add (name, "INF"); // jObject.add (name, Float.POSITIVE_INFINITY);
                else
                  return jArray.add ("INF"); // jArray.add (Float.POSITIVE_INFINITY);
              } else if (Float.isInfinite(-ff)) {
                if (name != null) 
                  return jObject.add (name, "-INF"); // jObject.add (name, Float.NEGATIVE_INFINITY);
                else
                  return jArray.add ("-INF"); // jArray.add (Float.NEGATIVE_INFINITY);
              } else {
                double dd;
                if (ff == -1.0E30f) dd = -1.0E30;
                else if (ff == -1.0E31f) dd = -1.0E31;
                else if (ff == 1.0E30f) dd = 1.0E30;
                else if (ff == 1.0E31f) dd = 1.0E31;
                else dd = (double) ff;
                if (name != null) {
                  if (formatT == null) {
                    return jObject.add(name, dd);
                  } else {
                    try {
                      String sd = String.format(formatT, dd);
                      return jObject.add(name, new Double(sd).doubleValue());
                    } catch (Exception ex) {
                      return jObject.add (name, dd);
                    }
                  }
                } else {
                  if (formatT == null) {
                    return jArray.add(dd);
                  } else {
                    try {
                      String sd = String.format(formatT, dd);
                      return jArray.add(new Double(sd).doubleValue());
                    } catch (Exception ex) {
                      return jArray.add (dd);
                    }
                  }
                }
              }
            } else if (signature.indexOf("Double") != -1) {
              double dd;
              dd = ((Double)data).doubleValue();
              if (Double.isNaN(dd)) {
                if (name != null)
                  return jObject.add (name, "NaN"); // jObject.add (name, Double.NaN);
                else
                  return jArray.add ("NaN"); // jArray.add (Double.NaN);
              } else if (Double.isNaN(-dd)) {
                if (name != null)
                  return jObject.add (name, "NaN"); // jObject.add (name, -Double.NaN);
                else
                  return jArray.add ("-NaN"); // jArray.add (-Double.NaN);
              } else if (Double.isInfinite(dd)) {
                if (name != null)
                  return jObject.add (name, "INF"); // jObject.add (name, Double.POSITIVE_INFINITY);
                else
                  return jArray.add ("INF"); // jArray.add (Double.POSITIVE_INFINITY);
              } else if (Double.isInfinite(-dd)) {
                if (name != null)
                  return jObject.add (name, "-INF"); // jObject.add (name, Double.NEGATIVE_INFINITY);
                else
                  return jArray.add ("-INF"); // jArray.add (Double.NEGATIVE_INFINITY);
              } else {
                if (name != null) {
                  if (formatT == null) {
                    if (isEpochDataType(dataType2) && validORscaleORfill(name))
                      return jObject.add(name, Epoch.encode4(dd)+addZ);
                    else
                      return jObject.add(name, dd);
                  } else {
                    try {
                      String sd = String.format(formatT, dd);
                      if (isEpochDataType(dataType2) && validORscaleORfill(name))
                        return jObject.add(name, Epoch.encode4(
                                                 new Double(sd).doubleValue())+
                                                 addZ);
                      else
                        return jObject.add(name, new Double(sd).doubleValue());
                    } catch (Exception ec) {
                      if (isEpochDataType(dataType2) && validORscaleORfill(name))
                        return jObject.add(name, Epoch.encode4(dd)+addZ);
                      else
                        return jObject.add(name, dd);
                    } 
                  }
                } else {
                  if (formatT == null) { 
                    return jArray.add(dd);
                  } else {
                    try {
                      String sd = String.format(formatT, dd);
                      return jArray.add(new Double(sd).doubleValue());
                    } catch (Exception es) {
                      return jArray.add(dd);
                    }
                  }
                }
              }
            } else if (signature.indexOf("String") != -1) {
              if (name != null)
                return jObject.add(name, (String)data);
              else
                return jArray.add((String)data);
            }
          }
        }
        return null;
    }

     private static String toCformat (String oFormat) {
       if (oFormat.charAt(0) == '%') return oFormat;
       /***********************************************************************
        *      FORTRAN format form
        * Change `oformat' to point to first non-blank, non-digit, non-`('
        * character.  This will skip over a Fortran repeat count (eg. the `20'
        * in `20F8.4' or `20(F8.4)').
        ***********************************************************************/
       oFormat = oFormat.trim();
       if (oFormat == null) return null;
       int len = oFormat.length();
       String formatY = null;
       for (int i = 0; i < len; ++i) {
         char a = oFormat.charAt(i);
         if (Character.isDigit(a) || a == '(') continue;
         formatY = oFormat.substring(i);
         if (formatY.endsWith(")")) formatY = formatY.substring(0,
                                                             formatY.length()-1);
         break;
       }
       if (formatY == null) return null;
       String cformat;
       /***********************************************************************
        * Encode C format specification.
        ***********************************************************************/
       switch (formatY.charAt(0)) {
         /*********************************************************************
          * Integer/decimal.
	  *********************************************************************/
         case 'I':
         case 'i':
           cformat = formatY.substring(1);
           return "%"+cformat+"d";
         /*********************************************************************
          * Floating-point/non-scientific notation (which is called...
          *********************************************************************/
         case 'F':
         case 'f':
           cformat = formatY.substring(1);
           return "%"+cformat+"f";
         /*********************************************************************
          * Floating-point/scientific notation.
          *********************************************************************/
         case 'E':
         case 'e':
           cformat = formatY.substring(1);
           return "%"+cformat+"e";
       }
       return null;
     }

     private static void readFileNames (String fileTxt) {
       FileInputStream fIn = null;
       BufferedReader br = null;
       String line;
       int i;
       try {
           numFiles = 0;
           fIn = new FileInputStream(fileTxt);;
           br = new BufferedReader(new InputStreamReader(fIn));
           line = br.readLine();
           while (line != null) {
              if (CDFUtils.cdfFileExists(line)) ++numFiles;
              line = br.readLine();
           }
           if (numFiles == 0) {
             System.out.println("*** No CDF file(s) is found... ");
             System.exit(1);
           }
           fileNames = new String[numFiles];
           i = 0;
           fIn.getChannel().position(0);
           br = new BufferedReader(new InputStreamReader(fIn));
           line = br.readLine();
           while (line != null) {
              if (CDFUtils.cdfFileExists(line)) {
                 fileNames[i] = line;
                 ++i;
              }
              line = br.readLine();
           }
       } catch (Exception ex) {
         System.out.println("error: "+ex);
       } finally {
         try {
           if (fIn != null) fIn.close();
         } catch (Exception ey) {}
       }
     }
   
}
