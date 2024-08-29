/******************************************************************************
* Copyright 1996-2013 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/

import java.io.*;
import java.text.*;
import java.util.*;
import java.lang.reflect.*;
import gsfc.nssdc.cdf.*;
import gsfc.nssdc.cdf.util.*;

/**
 *  This program demonstrates how to read the contents of test.cdf created 
 *  by CreateCDF.java in this directory. It differs from ReadCDF by showing
 *  only metadata and all variables' specifications, no data. 
 */

public class ReadMetaCDF implements CDFConstants{

    public static void main(String[] args) {
      String  fileName = "test";
      int     maxVarNameLength = 22;
      int ii, jj;
      CDF cdf;

      jj = args.length;
      if (jj == 0) jj = 1;

      try {

        System.out.println("CDF Library version: "+CDF.getLibraryVersion()+"\n");
/*
        CDF.setValidate (VALIDATEFILEoff);
*/
      } catch (CDFException vax) {
        System.out.println("*** Error... "+vax);
        System.exit(1);
      }

      for (ii = 0; ii < jj; ++ii) {
	if (args.length != 0) fileName = args[ii];
        if (ii > 0) 
          System.out.println("");
        System.out.println("Reading " + fileName + "...:\n");
        try {
	  cdf = null;
          cdf = CDF.open(fileName, READONLYon);
          if (cdf.getStatus() != CDF_OK)
          {
            if (cdf.getStatus() == CHECKSUM_ERROR)
              System.out.println("*** Checksum failed... the cdf file is corrupted...\n");
              if (cdf != null) cdf.close();
          } else {

              /************************************************************/
              /* If a decoding method is not specified when a CDF file is */
              /* opened, the CDF libaray knows what encoding method was   */
              /* used to create the CDF file.                             */
              /*                                                          */
              /* Decoding method should be specified only if one needs    */
              /* to translate data from one platform to another.          */ 
              /************************************************************/
              // cdf.selectDecoding(NETWORK_DECODING);

              /**********************************/
              /* Print out the file information */
              /**********************************/
              System.out.println("File Info\n"+
                                 "=========================================");

              if (cdf.confirmReadOnlyMode() == READONLYon)
                System.out.println("CDF File:     "+cdf+" (READONLYon)");
              else {
                System.out.println("CDF File:     "+cdf+" (READONLYoff)");
              }

              System.out.println("Version:      "+cdf.getVersion());
              String cp = cdf.getCopyright();
              System.out.println("Copyright:    "+cp);
              System.out.println("Format:       "+CDFUtils.getStringFormat(cdf));
              System.out.println("Encoding:     "+
                                 CDFUtils.getStringEncoding(cdf));
              System.out.println("Decoding:     "+
                                 CDFUtils.getStringDecoding(cdf));
              System.out.println("Majority:     "+
                                 CDFUtils.getStringMajority(cdf));
              if (cdf.getMajority() == COLUMN_MAJOR)
                System.out.println("(********  Data dumps from multi-dimensional "+
                                   "variables are shown row-based.)"); 
              System.out.println("Checksum:     "+
                                 CDFUtils.getStringChecksum(cdf));
              System.out.println("numRvars:     "+cdf.getNumRvars());
              System.out.println("numZvars:     "+cdf.getNumZvars());
              System.out.println("numAttrs:     "+cdf.getNumAttrs()+
                                 " ("+cdf.getNumGattrs()+" global, "+
                                 cdf.getNumVattrs()+" variable)");
              System.out.println("Compression:  "+cdf.getCompression());
              if (cdf.getCompressionType() != 0)
                System.out.println("cPct:         "+cdf.getCompressionPct());
              if (new Integer(cdf.getVersion().substring(0,1)).intValue() > 3) {
                long lsLastUpdated = cdf.getLeapSecondLastUpdated();
                if (lsLastUpdated > 0)
		  System.out.println("LeapSecondLastUpdated:     "+lsLastUpdated);
              }
              System.out.println("Cache Size:   "+cdf.confirmCDFCacheSize());
  
              /**********************************************/
              /* Print out the Global Attribute information */
              /**********************************************/
              Attribute a;
              String    attrName = null;
              int       i;
              long      n = cdf.getNumGattrs();
              Vector    ga = cdf.getGlobalAttributes();
           
              System.out.println("\nGlobal Attributes ("+n+" attributes)\n"+
                                 "=========================================");
              i = 0;
              for (Enumeration e = ga.elements() ; e.hasMoreElements() ;) {
                a = (Attribute) e.nextElement();
                n = a.getNumEntries(); 
                if (i == 0) {
                   attrName = a.getName();
                   if (n <= 1)
                       System.out.println (attrName+" ("+n+" entry):");
                   else
                       System.out.println (attrName+" ("+n+" entries):");
                }
                else {
                   String currAttrName = a.getName();
                   if (currAttrName != attrName) {
                       if (n <= 1)
                          System.out.println (currAttrName+" ("+n+" entry):");
                       else
                          System.out.println (currAttrName+" ("+n+" entries):");
                   }
                } 
                i++;
                Vector ent = a.getEntries();
                for (Enumeration e1 =ent.elements() ; e1.hasMoreElements() ;) {
                   Entry entry = (Entry) e1.nextElement();
                   if (entry != null) {
                      long eDataType = entry.getDataType();
                      System.out.print ("\t"+entry.getID()+" ("+
                                         CDFUtils.getStringDataType(eDataType)+
                                         "/"+entry.getNumElements()+
                                         "): \t");
                      Object data = entry.getData();
                      if (eDataType == CDF_EPOCH) {
                         System.out.println(Epoch.toEncode(((Double) data).doubleValue()));
                      } else if (eDataType == CDF_EPOCH16) {
                         System.out.println(Epoch16.toEncode((double[])data));
                      } else if (eDataType == CDF_TIME_TT2000) {
                         System.out.println(CDFTT2000.toEncode(((Long)data).longValue()));
                      } else {
                         CDFUtils.printData (data);
                      }
//                       System.out.println (" ");
                    }
                }
                System.out.println (" ");
              }

              /************************************************/
              /* Print out the Variable Attribute information */
              /************************************************/
              attrName = null;
              n = cdf.getNumVattrs();
              Vector  va = cdf.getVariableAttributes();

              System.out.println("\nVariable Attributes ("+n+" attributes)\n"+
                               "=========================================");
              i = 0;
              for (Enumeration e = va.elements() ; e.hasMoreElements() ;) {
                a = (Attribute) e.nextElement();
                if (i == 0) {
                    attrName = a.getName();
                    System.out.println (attrName+":");
                }
                else {
                    String currAttrName = a.getName();;
                    if (currAttrName != attrName)
                        System.out.println (currAttrName+":");
                }
                i++;
                Vector ent = a.getEntries();
                for (Enumeration e1 =ent.elements() ; e1.hasMoreElements() ;) {
                    Entry entry = (Entry) e1.nextElement();
                    if (entry != null) {
                       long eDataType = entry.getDataType();
                       Variable v = cdf.getVariable(entry.getID());
                       System.out.print ("\t"+v.getName()+" ("+
                                        CDFUtils.getStringDataType(eDataType)+
                                        "/"+entry.getNumElements()+"): ");
                       Object data = entry.getData();
                       if (eDataType == CDF_EPOCH) {
                          System.out.println(Epoch.toEncode(((Double) data).doubleValue()));
                       } else if (eDataType == CDF_EPOCH16) {
                          System.out.println(Epoch16.toEncode((double[])data));
                       } else if (eDataType == CDF_TIME_TT2000) {
                          System.out.println(CDFTT2000.toEncode(((Long) data).longValue()));
                       } else {
                          if (entry.getScope() == VARIABLE_SCOPE) {
                            if (CDFUtils.isStringDataType(eDataType)) {
                              if (data instanceof String[]) {
                                long ix = ((String[])data).length;
                                System.out.println("\t("+ix+" string"+(ix>1?"s":"")+") ");
                                int iy;
                                for (iy=0; iy<ix; ++iy) {
                                  if (iy != (ix-1))
                                    System.out.println("\t\t\t\t\""+((String[])data)[iy]+"\""); 
                                  else
                                    System.out.print("\t\t\t\t\""+((String[])data)[iy]+"\"");
                                }
                              } else {
                                CDFUtils.printData (data);
                              }
                            } else {
                              String format;
                              try {
                                Attribute attr = cdf.getAttribute("FORMAT");
                                Entry formatEntry = attr.getEntry (entry.getID());
                                format = (String) formatEntry.getData();
                              } catch (CDFException ee) {
                                format = null;
                              }
                              CDFUtils.printDataWithFormat (data, format);
                            }
                          } else
                            CDFUtils.printData (data);
                       }
//                       System.out.println (" ");
                    }
                }
                System.out.println (" ");
              }
              /**************************************/
              /* Print out the Variable information */ 
              /**************************************/
              String  varName, dataType;
              int     noOfBlanks;
              long    dt, numDims, blockingFactor;
              Object  padValue = null;

              n = cdf.getNumVars();
              Vector  vars = cdf.getVariables();

              System.out.println("\nVariable Information ("+n+" variables)\n"+
                                 "===================================================");
              for (Enumeration e = vars.elements() ; e.hasMoreElements() ;) {
                Variable v = (Variable) e.nextElement();

                varName = v.getName();
                noOfBlanks = maxVarNameLength - varName.length();
                for (i=0; i < noOfBlanks; i++)
                    varName = varName + " ";

                long[] dimSizes = v.getDimSizes();
                dt = v.getDataType();
                dataType = CDFUtils.getStringDataType(dt);
                dataType = dataType + "/" + String.valueOf(v.getNumElements());
                noOfBlanks = 20 - dataType.length();
                for (i=0; i < noOfBlanks; i++)
                    dataType = dataType + " ";

                numDims = v.getNumDims();
                System.out.print (varName+dataType+ numDims+":[");
                for (i=0; i < numDims; i++) {
                    if (i > 0) System.out.print (",");
                    System.out.print (dimSizes[i]); 
                }
                System.out.print ("]  ");
                // if (numDims == 1) System.out.print ("\t");

                System.out.print((v.getRecVariance() ? "T" : "F")+"/");
                long[] dimVariances = v.getDimVariances();
                for (i=0; i < v.getNumDims(); i++)
                    System.out.print(
                        ((dimVariances[i] == CDFConstants.VARY) ? "T" : "F"));

                String sparseRecord = CDFUtils.getStringSparseRecord(v);
                if (!sparseRecord.equals("None"))
                     System.out.print (" sparseRecord=\""+sparseRecord+"\"");

                // Print the user-define pad value if one exists. 
		// if (v.checkPadValueExistence()) {
/*
                  padValue = v.getPadValue(); 
                  if (dt == CDF_EPOCH)
                    padValue = Epoch.toEncode(((Double) padValue).doubleValue());
                  else if (dt == CDF_EPOCH16)
                    padValue = Epoch16.toEncode((double[]) padValue);
                  else if (dt == CDF_TIME_TT2000)
                    padValue = CDFTT2000.toEncode(((Long) padValue).longValue());
                  else if (dt != CDF_REAL4 && dt != CDF_FLOAT &&
                           dt != CDF_REAL8 && dt != CDF_DOUBLE)
                    System.out.print (" PadValue="+((CDFUtils.isStringDataType(dt)||
                                                     CDFUtils.isEpochDataType(dt))?
                                                    "\"":"")+padValue+
                                                    ((CDFUtils.isStringDataType(dt)||
                                                     CDFUtils.isEpochDataType(dt))?"\"":""));
                  else {
                      String format;
                      try {
                        Attribute attr = cdf.getAttribute("FORMAT");
                        format = (String) attr.getEntry (v.getID()).getData();
                      } catch (CDFException ee) {
                        format = null;
                      }
                      System.out.print (" PadValue=");
                      CDFUtils.printDataWithFormat (padValue,format);
                  }
*/
                // }

                blockingFactor = v.getBlockingFactor();
                if (blockingFactor > 0)
                    System.out.print (" blockingFactor=\""+blockingFactor+"\"");

                System.out.println (" ");
              }

              /**********************************************************/
              /* Print out the Variable data (all variables in the CDF) */ 
              /**********************************************************/
              System.out.println("\n\nVariable Data ("+n+" variables)\n"+
                                 "=========================================");
              CDFData data = null;
              long   numRecs, maxRec;
              long[] dimIndices   = {0L};
              long[] dimIntervals = {1L};
              long[] dimSizes     = {1L};

              for (Enumeration e = vars.elements() ; e.hasMoreElements() ;) {
                  Variable v = (Variable) e.nextElement();

                if (v.getNumDims() > 0) {
                    dimSizes = v.getDimSizes();
                    dimIntervals = new long[dimSizes.length];
                    dimIndices   = new long[dimSizes.length];
                    for (i=0; i < dimSizes.length; i++) {
                        dimIntervals[i] = 1;
                        dimIndices[i]   = 0;
                    }
                }
                maxRec = v.getMaxWrittenRecord();
                numRecs = v.getNumWrittenRecords();
                varName = v.getName();
                System.out.println (varName);
                String format;
                try {
                  Attribute attr = cdf.getAttribute("FORMAT");
                  format = (String) attr.getEntry (v.getID()).getData();
                } catch (CDFException ee) {
                  format = null;
                }

                for (i=0; i < varName.length(); i++)
                     System.out.print ("-");
                System.out.println (" ");

                if (v.getCompressionType() == NO_COMPRESSION)
                    System.out.println ("Compression:      None");
                else
                    System.out.println ("Compression:      "+
                                        v.getCompression()+" ("+
                                        v.getCompressionPct()+"%)");
		if (v.checkPadValueExistence()) {
		  long myDT = v.getDataType();
                  Object pv = v.getPadValue();
                  if (pv != null) {
		    if (myDT == CDF_EPOCH)
		      System.out.println ("Pad value:        "+
                         Epoch.toEncode(((Double)pv).doubleValue()));
		    else if (myDT == CDF_EPOCH16)
		      System.out.println ("Pad value:        "+
                         Epoch16.toEncode((double[])pv));
                    else if (myDT == CDF_TIME_TT2000)
                      System.out.println ("Pad value:        "+
                         CDFTT2000.toUTCstring(((Long)pv).longValue()));
                    else if (CDFUtils.isStringDataType(myDT))
                      System.out.println ("Pad value:        \""+ pv+"\"");
                    else {
                      System.out.print ("Pad value:        ");
                      CDFUtils.printDataWithFormat (pv, format);
                    }
		  }
		}
                System.out.println ("Records:          "+
                                    numRecs+"n/"+maxRec+"x");
                System.out.println ("Allocated:        "+
                                    v.getNumAllocatedRecords()+"n/"+
                                    v.getMaxAllocatedRecord()+"x");
                System.out.println ("Blocking Factor:  "+
                                    v.getBlockingFactor());
                System.out.println ("Sparseness:       "+
                                    CDFUtils.getStringSparseRecord(v));
                System.out.println (" ");

                /********************************************************/
                /*  maxRec represents the last record number for this   */
                /*  variable, not the number of records.                */
                /*                                                      */
                /*  NOTE: maxRec starts at 0, so if the value of maxRec */
                /*        is 2, the actual number of records is 3.      */
                /*        If there are no records exists, the value of  */
                /*        maxRec is -1.                                 */
                /********************************************************/
/*
                long status;
                for (i=0; i <= maxRec; i++) {
                     data = v.getHyperDataObject(i, 1, 1,
                                                 dimIndices, 
                                                 dimSizes, 
                                                 dimIntervals);
                     status = cdf.getStatus();
                     if (status == CDF_OK || status != VIRTUAL_RECORD_DATA) {
                       System.out.println ("Record # "+i+":");
                       data.dumpDataWithFormat(format);
                     }
                }
*/
              }
            cdf.close();
          }
        } catch (Exception e) {
           System.out.println (e);
        }
      }
    }
}
