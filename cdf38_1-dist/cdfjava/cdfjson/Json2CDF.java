import java.io.*;
import java.text.*;
import java.util.*;
import java.lang.reflect.*;
import java.lang.*;
import javax.json.*;
import gsfc.nssdc.cdf.*;
import gsfc.nssdc.cdf.util.*;

/**
 * The Json2CDF class is the converter to convert a Json file back to a CDF.
 * The Json file is produced from CDF2Json class. The Json file has the
 * following form:
 *<PRE>
 *CDFname1:
 *  CDFFileInfo:
 *    FileVersion:.....
 *    Format:.....
 *    Majority:...
 *    Encoding:.....
 *  CDFglobalAttributes:
 *    Gattr_name1:
 *     entry#:value
 *       ...
 *       ...
 *    Gattr_name2:
 *     entry#:value
 *       ...
 *       ...
 *    ...
 *    ...
 *  CDFVariables:
 *    VarName1:
 *     VarDescription:
 *       DataType:....
 *       ...
 *       ...
 *     VarAttributes:
 *       VALIDMIN:....
 *       VALIDMAX:....
 *       ...
 *       ...
 *     VarData:
 *       ...
 *    VarName2:
 *     VarDescription:
 *       DataType:....
 *       ...
 *       ...
 *     VarAttributes:
 *       VALIDMIN:....
 *       VALIDMAX:....
 *       ...
 *       ...
 *     VarData:
 *       ...
 *  EmptyAttributes:        <== If attribute(s) defined but not assign value(s)
 *     GlobalAttributes:    <== If global attribute(s) is not assigned value
 *     VariableAttributes:  <== If variable attribute(s) is not assigned value
 *
 *CDFname2:
 *  CDFFileInfo:
 *   .....
 *   .....
 *  CDFglobalAttributes:
 *    Gattr_name1:
 *       ...
 *       ...
 *    Gattr_name2:
 *       ...
 *       ...
 *    ...
 *    ...
 *  CDFVariables:
 *    VarName1:
 *     VarDescription:
 *       ....
 *...
 *...
 *</PRE> 
 *
 * 
 * This class is based on JNI-based cdfjava.jar and javax.json.jar.
 *
 * @version 2.0 02/20/2022  
 * @author Mike Liu,      ADNET Systems
 */

public class Json2CDF implements CDFConstants {

   private static boolean progress = false,
                          delete = false,
                          showfile = false,
                          toCompress = true,
                          checksum = false;
   private static String inFile = null,
                         assignedFile = null,
                         outDir = null;
   private static int numFiles = 0;
   private static long dataType, numElements;
   private static boolean varData = true;
   private static long varDataType, varNumElems, varNumDims;
   private static long varSparseRec = NO_SPARSERECORDS;
   private static long varNumWritten = 0, varMaxWritten = -1;
   private static long[] varDimSizes = new long[(int)CDF_MAX_DIMS];
   private static long[] varDimVariances = new long[(int)CDF_MAX_DIMS];
   private static String varName = null;
   private static Object varPadValue = null;
   private static Object varFillVal = null;
   private static long varRecVary = VARY;
   private static String separator = System.getProperty("file.separator");
   private static String item1 = "CDFFileInfo";
   private static String item2 = "CDFglobalAttributes";
   private static String item3 = "CDFVariables";
   private static String item4 = "VarDescription";
   private static String item5 = "VarAttributes";
   private static String item6 = "VarData";
   private static String item7 = "EmptyAttributes";

   public static void main(String[] args) throws IOException {
     try {
       parseCmdLine(args);
       if (showfile) System.out.println(inFile);
       InputStream fis = new FileInputStream(inFile);
       JsonReader jsonReader = Json.createReader(fis);
       JsonObject jsonObject = jsonReader.readObject();
       jsonReader.close();
       fis.close();

       numFiles = jsonObject.keySet().size();

       for (String cdfName: jsonObject.keySet()) {
           outPutJsonObject(cdfName, 
                            (JsonObject)jsonObject.getJsonObject(cdfName));
       }

     } catch (Exception ep) {
       System.out.println("Exception... "+ep);
     }
   }

   private static void outPutJsonObject (String cdfName, 
                                         JsonObject jsonObject1) {
     long format = -1, majority = -1, encoding = -1;
     CDF cdf = null;
     Variable va = null;
     String createdCDF = null;

     try {
       JsonArray fileInfoArray = jsonObject1.getJsonArray(item1);
       if (fileInfoArray != null) {
         if (progress) System.out.println(item1+":");
         for (JsonValue fileInfo : fileInfoArray) {
           for (String key: ((JsonObject)fileInfo).keySet()) {
             String value = ((JsonObject)fileInfo).getString(key);
             if (progress) System.out.println("  "+key+":"+value);
             if (key.equalsIgnoreCase("filename")) {
               cdfName = value;
             } else if (key.equalsIgnoreCase("format")) {
               format = CDFUtils.getLongFormat (value);
             } else if (key.equalsIgnoreCase("majority")) {
               majority = CDFUtils.getLongMajority (value);
             } else if (key.equalsIgnoreCase("encoding")) {
               encoding = CDFUtils.getLongEncoding (value);
             } 
           }
         }
         String toDelete = null;
         if (delete) {
           File cdfFile;
           if (outDir == null) {
              if (numFiles == 1 && assignedFile != null)
                  toDelete = assignedFile;
                else
                  toDelete = cdfName;
           } else {
               int loc;
               if (numFiles == 1 && assignedFile != null)
                 toDelete = outDir + separator + assignedFile;
               else {
                 loc = cdfName.lastIndexOf(separator);
                 if (loc != -1)
                   toDelete = outDir + separator + cdfName.substring(loc+1);
                 else
                   toDelete = outDir + separator + cdfName;
               }
           }
           cdfFile = new File(toDelete);
           if (cdfFile.exists()) cdfFile.delete();
         }

         if (outDir == null)
             if (numFiles == 1 && assignedFile != null)
               createdCDF = assignedFile;
             else
               createdCDF = cdfName;
         else {
             if (numFiles == 1 && assignedFile != null)
               createdCDF = outDir+separator+assignedFile;
             else {
               int loc = cdfName.lastIndexOf(separator);
               if (loc != -1)
                 createdCDF = outDir+separator+cdfName.substring(loc+1);
               else
                 createdCDF = outDir+separator+cdfName;
             }
         }
         cdf = CDF.create(createdCDF);
         cdf.setFormat(format);
         cdf.setMajority(majority);
         cdf.setEncoding(encoding);
         if (checksum)
           cdf.setChecksum(MD5_CHECKSUM);
       }
       
       // Retrieve data from "CDFglobalAttributes" JsonArray
       JsonArray globalAttrArray = jsonObject1.getJsonArray(item2);
       if (globalAttrArray != null) {
         if (progress) System.out.println(item2+":");
         processGlobalAttributes (cdf, globalAttrArray);
       }

       // Retrieve data from "CDFVariables" JsonArray
       JsonArray cdfVariables = jsonObject1.getJsonArray(item3);
       if (cdfVariables != null) {
         if (progress) System.out.println(item3+":");
         for (JsonValue varInfo : cdfVariables) {
           for (String varName: ((JsonObject)varInfo).keySet()) {
             if (progress) System.out.println("  "+varName+":");
             JsonArray varItems = ((JsonObject)varInfo).getJsonArray(varName);
             for (int ii = 0; ii < varItems.size(); ++ii) {
               JsonObject varItem = varItems.getJsonObject(ii);
               for (String itemName: varItem.keySet()) {
                 if (itemName.equalsIgnoreCase(item4)) {
                   Object values = varItem.get(itemName);
                   if (progress) System.out.println("    "+itemName+":");
                   va = processVariableSpec (cdf, varName, values);
                 } else if (itemName.equalsIgnoreCase(item5)) {
                   if (va != null) {
                     Object values = varItem.get(itemName);
                     if (progress) System.out.println("    "+itemName+":");
                     processVariableAttributes (cdf, va, values);
                   }
                 } else if (itemName.equalsIgnoreCase(item6)) {
                   if (varData || (varRecVary == NOVARY)) {
                     if (va != null) {
                       if (progress) System.out.println("   Doing VarData....");
                       Object varData = varItem.get(itemName);
                       processVariableData (va, varData);
                     }
                   }
                 } else
                   System.out.println("***** Unknown item...: "+itemName);
               }
             }
           }
         }
       }

       JsonArray emptyAttrs = jsonObject1.getJsonArray(item7);
       if (emptyAttrs != null) {
         if (progress) System.out.println(item7+":");
         for (JsonValue groupAttrs : emptyAttrs) {
           for (String key: ((JsonObject)groupAttrs).keySet()) {
             JsonArray values = ((JsonObject)groupAttrs).getJsonArray(key);
             if (progress) System.out.println("  "+key+":"+values);
             if (key.equals("GlobalAttributes")) {
               for (int ii=0; ii < values.size(); ++ii) {
                 String attrName = values.getString(ii);
                 Attribute.create(cdf, attrName, GLOBAL_SCOPE);
               }
             } else {
               for (int ii=0; ii < values.size(); ++ii) {
                 String attrName = values.getString(ii);
                 Attribute.create(cdf, attrName, VARIABLE_SCOPE);
               }
             }
           }
         }
       }
       cdf.close();
     } catch (Exception ex) {
       System.out.println("***** Error: "+ex+" for file: "+createdCDF);
     }
   }

   /*********************************************************/
   /*  Parse the command line input that is in the form of  */
   /*     java Json2CDF [Options] cdfFileName              */
   /*                                                       */
   /*  See the top of this file for a detailed description  */
   /*  of the Options.                                      */
   /*********************************************************/
   private static void parseCmdLine (String[] args) {
       String numSpaces = null;

       if (args.length == 0)        // No input is given (i.e. java Json2CDF)
           exit("");
       else {                  // Process options
         for (int i=0; i < args.length; i++) {
            if (i == (args.length-1)) {         // Get the CDF file name
                inFile = args[i];
            }
            else {
                int loc = args[i].indexOf(":");
                if (args[i].toLowerCase().startsWith("-output:")) {
                    assignedFile = args[i].substring(loc+1);
                    if (!assignedFile.toLowerCase().endsWith(".cdf"))
                      assignedFile = assignedFile + ".cdf";
                }
                else if (args[i].toLowerCase().startsWith("-vardata:")) {
                    String toData = args[i].substring(loc+1);
                    if (toData.equalsIgnoreCase("yes")) 
                      varData = true;
                    else if (toData.equalsIgnoreCase("no"))
                      varData = false;
                    else
                      exit ("** Error: Invalid -vardata option entered **");
                }
                else if (args[i].equalsIgnoreCase("-showprogress") ||
                         args[i].equalsIgnoreCase("-progress")) {
                    progress = true;
                }
                else if (args[i].equalsIgnoreCase("-delete")) {
                    delete = true;
                }
                else if (args[i].equalsIgnoreCase("-showfile")) {
                    showfile = true;
                }
                else if (args[i].equalsIgnoreCase("-checksum")) {
                    checksum = true;
                }
                else if (args[i].equalsIgnoreCase("-nocompress")) {
                    toCompress = false;
                }
                else if (args[i].toLowerCase().startsWith("-outdir:")) {
                    outDir = args[i].substring(loc+1);
                }
                else {
                    exit ("** Error: Invalid option entered **");
                }
            }
         }
       }
   }

   private static boolean equivalentDataTypes (long dataType1, long dataType2) {
       long realDataType[] = {
          0,1,2,0,3,0,0,0,4,0,
          0,5,6,0,7,0,0,0,0,0,
          0,8,9,0,0,0,0,0,0,0,
          0,9,0,4,0,0,0,0,0,0,
          0,1,0,0,8,9,0,0,0,0,
          0,10,10,0,0,0,0,0,0,0
       };
       return (realDataType[(int)dataType1] == realDataType[(int)dataType2]);
   }

   private static Object handleSpecial (long dt, String str) {

       if (str.equals("-nan")) {
           if (dt == CDF_DOUBLE || dt == CDF_REAL8)
             return new Double(-Double.NaN);
           if (dt == CDF_FLOAT || dt == CDF_REAL4)
             return new Float(-Float.NaN);
           return new Double(-Double.NaN);
       } else if (str.equals("nan")) {
           if (dt == CDF_DOUBLE || dt == CDF_REAL8)
             return new Double(Double.NaN);
           if (dt == CDF_FLOAT || dt == CDF_REAL4)
             return new Float(Float.NaN);
           return new Double(Double.NaN);
       } else if (str.equals("-inf")) {
           if (dt == CDF_DOUBLE || dt == CDF_REAL8)
             return new Double(Double.NEGATIVE_INFINITY);
           if (dt == CDF_FLOAT || dt == CDF_REAL4)
             return new Float(Float.NEGATIVE_INFINITY);
           return new Double(Double.NEGATIVE_INFINITY);
       } else if (str.equals("inf")) {
           if (dt == CDF_DOUBLE || dt == CDF_REAL8)
             return new Double(Double.POSITIVE_INFINITY);
           if (dt == CDF_FLOAT || dt == CDF_REAL4)
             return new Float(Float.POSITIVE_INFINITY);
           return new Double(Double.POSITIVE_INFINITY);
       }
       return str;
   }

   private static Object changeData (long dt1, long dt2, Object data) {

       String signature = CDFUtils.getSignature(data);
       if (signature.indexOf("String") != -1) {
         if (signature.charAt(0) != '[') {
           String str = (String) data;
           String str2 = specialString(str);
           if (str2 != null)
             return handleSpecial (dt2, str2);
           else
             return data;
         } else {
           int items = ((String[])data).length;
           String str = null, str2 = null;
           if (dt2 == CDF_REAL4 || dt2 == CDF_FLOAT) {
             float[] nd = new float[items];
             for (int i = 0; i < items; ++i) {
               str = ((String[]) data)[i];
               str2 = specialString(str);
               if (str2 != null)
                 nd[i] = ((Float) handleSpecial (dt2, str2)).floatValue();
               else
                 nd[i] = new Float(str).floatValue();
             }
             return nd;
           } else if (dt2 == CDF_REAL8 || dt2 == CDF_DOUBLE) {
             double[] nd = new double[items];
             for (int i = 0; i < items; ++i) {
               str = ((String[]) data)[i];
               str2 = specialString(str);
               if (str2 != null)
                 nd[i] = ((Double) handleSpecial (dt2, str2)).doubleValue();
               else
                 nd[i] = new Double(str).doubleValue();
             }
             return nd;
           } else
             return data;
         }
       }
       if (equivalentDataTypes(dt1, dt2)) return data;
       if (data instanceof Double) {
         return new Float(((Double)data).floatValue());
       } else if (data instanceof Integer) {
         if (dt1 == CDF_INT1 || dt1 == CDF_BYTE)
           return new Byte(((Integer)data).byteValue());
         else if (dt1 == CDF_INT2 || dt1 == CDF_UINT1)
           return new Short(((Integer)data).shortValue());
       } else {
         if (signature.charAt(0) == '[') {
           if (signature.charAt(1) == 'D') {
             int items = ((double[])data).length;
             float[] nd = new float[items];
             for (int i = 0; i < items; ++i)
               nd[i] = (float) ((double[])data)[i];
             return nd;
           } else {
             if (dt1 == CDF_INT1 || dt1 == CDF_BYTE) {
               int items = ((int[])data).length;
               byte[] nd = new byte[items];
               for (int i = 0; i < items; ++i) 
                 nd[i] = (byte) ((int[])data)[i];
               return nd;
             } else if (dt1 == CDF_INT2 || dt1 == CDF_UINT1) {
               int items = ((int[])data).length;
               short[] nd = new short[items];
               for (int i = 0; i < items; ++i)
                 nd[i] = (short) ((int[])data)[i];
               return nd;
             } else 
               return data;
           }
         }
       }
       return data; 
   }

   private static Object handleMixed (int items, Object data) {

       int ix; 
       int iz = 0;
       for (ix = 0; ix < items; ++ix) {
          Object one = ((JsonArray)data).get(ix);
          if (one instanceof JsonString) {
            String str = ((JsonString)one).getString();
            if (specialString(str) == null) {
              ++iz;
            }
          }
       }
       if (iz != 0) {
         System.out.println("????                 mixed number & string ?????");
         return null;
       } else {
         Object outs;
         if (dataType == CDF_REAL4 || dataType == CDF_FLOAT)
           outs = new double[items];
         else
           outs = new double[items];
         for (ix = 0; ix < items; ++ix) {
           Object one = ((JsonArray)data).get(ix);
           if (one instanceof JsonString) {
             String str = ((JsonString)one).getString();
             String str2 = specialString(str);
             Object out1 = handleSpecial (dataType, str2);
             if (dataType == CDF_REAL4 || dataType == CDF_FLOAT)
               ((float[])outs)[ix] = ((Float)out1).floatValue();
             else
               ((double[])outs)[ix] = ((Double)out1).doubleValue();
           } else {
             if (dataType == CDF_REAL4 || dataType == CDF_FLOAT)
               ((float[])outs)[ix] = Float.parseFloat(((JsonNumber)one).toString());
             else
               ((double[])outs)[ix] = Double.parseDouble(((JsonNumber)one).toString());
           }
         }
         return outs;
       }
   }

   private static Object getData (Object data) {

       String signature = CDFUtils.getSignature(data);
       if (data instanceof JsonString) {
         String value = ((JsonString)data).toString();
         if (value.charAt(0) == '"')
           value = value.substring(1, value.length()-1);
         int len = value.length();
         if (len < 14 || value.charAt(4) != '-' || value.charAt(7) != '-' ||
             value.charAt(10) != 'T' || value.charAt(13) != ':') {
           dataType = CDF_CHAR;
           return value;
         } else {
           numElements = 1;
           try {
             if (value.length() == 23 || value.length() == 24) { // CDF_EPOCH
               dataType = CDF_EPOCH;
               return new Double(Epoch.toParse(value));
             } else if (value.length() == 29 || value.length() == 30) {//TT2000
               dataType = CDF_TIME_TT2000;
               return new Long(CDFTT2000.toParse(value));
             } else { // CDF_EPOCH16
               dataType = CDF_EPOCH16;
               double[] epoch16 = Epoch16.toParse(value);
               return epoch16;
             }
           } catch (Exception ee) {
             return value;
           }
         }
       } else if (data instanceof JsonNumber) {
         numElements = 1;
         if (((JsonNumber)data).isIntegral()) {
           try {
             int aa = Integer.parseInt(((JsonNumber)data).toString());
             dataType = CDF_INT4;
             return new Integer(aa);
           } catch (Exception ee) {
             long bb = Long.parseLong(((JsonNumber)data).toString());
             dataType = CDF_INT8;
             return new Long(bb);
           }
         } else {
           double bb = Double.parseDouble(((JsonNumber)data).toString());
           dataType = CDF_DOUBLE;
           return new Double(bb);
         }
       } else if (data instanceof JsonArray) {
         Object one = ((JsonArray)data).get(0);
         int items = ((JsonArray)data).size();
         int ix, iy, iz;
         iy = iz = 0;
         for (ix = 0; ix < items; ++ix) {
           Object one1 = ((JsonArray)data).get(ix);
           if (one1 instanceof JsonString) {
             ++iy;
             String stri = ((JsonString)one1).getString();
             String stri2 = specialString(stri);
             if (stri2 != null)
               ++iz;
           }
         }

         if ((iy != 0 && iy != items) || (iz == items)) {
           dataType = CDF_DOUBLE;
           return handleMixed (items, data);
         }

         if (iy == items) {
           String oneS = ((JsonString)one).toString();
           if (oneS.length() < 10 || oneS.charAt(10) != 'T') {
             dataType = CDF_CHAR;
             String[] outs = new String[items];
             for (int ii = 0; ii < items; ++ii) {
               outs[ii] = ((JsonString)((JsonArray)data).get(ii)).toString();
             }
             return outs;
           } else { // CDF epoch data
             double[] outs1 = null;
             long[] outs2 = null;
             for (int ii = 0; ii < items; ++ii) {
               Object aa = getData((Object)(((JsonArray)data).get(ii)));
               if (ii == 0) {
                 if (dataType == CDF_EPOCH) outs1 = new double[items];
                 else if (dataType == CDF_EPOCH) outs1 = new double[2*items];
                 else outs2 = new long[items];
               }
               if (dataType == CDF_EPOCH) 
                 outs1[ii] = ((Double)aa).doubleValue();
               else if (dataType == CDF_EPOCH)
                 outs2[ii] = ((Long)aa).longValue();
               else {
                 outs1[2*ii] = ((double[])aa)[0];
                 outs1[2*ii+1] = ((double[])aa)[1];
               }
             }
             if (dataType == CDF_EPOCH || dataType == CDF_EPOCH16)
               return outs1;
             else
               return outs2;
           }
         } else if (iy == 0) {
           if (((JsonNumber)one).isIntegral()) {
             try {
               dataType = CDF_INT4;
               int[] outs1 = new int[items];
               for (int ii = 0; ii < items; ++ii) {
                 outs1[ii] = Integer.parseInt(
                                      ((JsonNumber)((JsonArray)data).get(ii)).
                                      toString());
               }
               return outs1;
             } catch (Exception ee) {
               dataType = CDF_INT8;
               long[] outs2 = new long[items];
               for (int ii = 0; ii < items; ++ii) {
                 outs2[ii] = Long.parseLong(
                                      ((JsonNumber)((JsonArray)data).get(ii)).
                                      toString());
               }
               return outs2;
             }
           } else {
             dataType = CDF_DOUBLE;
             double[] outs1 = new double[items];
             for (int ii = 0; ii < items; ++ii) {
               outs1[ii] = Double.parseDouble(
                                    ((JsonNumber)((JsonArray)data).get(ii)).
                                    toString());
             }
             return outs1;
           }
         } else {
           System.out.println("                     ?????");
         }
       } else
         System.out.println("                   NONE");
       return null;    
   }

   private static String specialString (String string) {
       String str = string.toLowerCase();
       if (str.startsWith("\""))
         str = str.substring(1, str.length()-1);
       if (str.equals("nan") || str.equals("-nan") ||
           str.equals("inf") || str.equals("-inf"))
         return str;
       else
         return null; 
   }

   private static Object setData (Object values, Object buf, int items,
                                  int recNum) throws CDFException {
       
       int count = 0;
       for (int jj = 0; jj < recNum; ++jj) {
         Object data = (Object) ((JsonArray)values).get(jj);
         String signature =   CDFUtils.getSignature(data);
         Object newObj = null;
         if (data instanceof JsonString) {
           String str = ((JsonString)data).getString();
           String str2 = specialString(str);
           if (str2 != null) {
             newObj = handleSpecial (varDataType, str2);
             if (varDataType == CDF_REAL4 || varDataType == CDF_FLOAT) {
               if (items == 1)
                 return newObj;
               else
                 ((float[])buf)[jj] = ((Float)newObj).floatValue();
             } 
             if (varDataType == CDF_REAL8 || varDataType == CDF_DOUBLE) {
               if (items == 1)
                 return newObj;
               else
                 ((double[])buf)[jj] = ((Double)newObj).doubleValue();
             }
             continue;
           } else
             newObj = setData1 (data);
           if (items == 1)
             return newObj;
           else {
             if (varDataType == CDF_EPOCH) {
               ((double[])buf)[jj] = ((Double)newObj).doubleValue();
             } else if (varDataType == CDF_TIME_TT2000) {
               ((long[])buf)[jj] = ((Long)newObj).longValue();
             } else if (varDataType == CDF_EPOCH16) {
               ((double[])buf)[2*jj] = ((double[])newObj)[0];
               ((double[])buf)[2*jj+1] = ((double[])newObj)[1];
             } else {
               byte[] by = ((String)newObj).getBytes();
               int len = ((String)newObj).length();
               for (int xy = 0; xy < len; ++xy)
                 ((byte[])buf)[count+xy] = by[xy];
               for (int xy = len; xy < varNumElems; ++xy)
                 ((byte[])buf)[count+xy] = (byte) 0;
               count += varNumElems;
             }
           }
         } else if (data instanceof JsonNumber) {
           newObj = setData1 (data);
           if (items == 1)
             return newObj;
           else {
             switch ((int)varDataType) {
               case (int)CDF_BYTE:
               case (int)CDF_INT1:
                 ((byte[])buf)[jj] = ((Byte)newObj).byteValue();
                 break;
               case (int)CDF_INT2:
               case (int)CDF_UINT1:
                 ((short[])buf)[jj] = (short)((JsonNumber)data).intValue();
                 break;
               case (int)CDF_INT4:
               case (int)CDF_UINT2:
                 ((int[])buf)[jj] = ((JsonNumber)data).intValue();
                 break;
               case (int)CDF_UINT4:
               case (int)CDF_INT8:
                 ((long[])buf)[jj] = ((JsonNumber)data).longValue();
                 break;
               case (int)CDF_REAL4:
               case (int)CDF_FLOAT:
                 ((float[])buf)[jj] = (float)((JsonNumber)data).doubleValue();
                 break;
               case (int)CDF_REAL8:
               case (int)CDF_DOUBLE:
                 ((double[])buf)[jj] = ((JsonNumber)data).doubleValue();
                 break;
               default:
                 break;
             }
           }
         } else if (data instanceof JsonArray) {
           int items2 = ((JsonArray)data).size();
           Object one = ((JsonArray)data).get(0);
           if (one instanceof JsonString) {
             for (int ii = 0; ii < items2; ++ii) {
               String str = ((JsonString)((JsonArray)data).get(ii)).toString();
               Object oneObj = setData1 (str);
               if (varDataType == CDF_CHAR || varDataType == CDF_UCHAR) {
                 int lenx = ((String)oneObj).length();
                 byte[] by = ((String)oneObj).getBytes();
                 for (int xy = 0; xy < lenx; ++xy)
                   ((byte[])buf)[count+xy] = by[xy];
                 for (int xy = lenx; xy < varNumElems; ++xy)
                   ((byte[])buf)[count+xy] = (byte) 0;
                 count += varNumElems;
               } else if (varDataType == CDF_EPOCH) { // CDF epoch data
                 ((double[])buf)[count++] = ((Double)oneObj).doubleValue();
               } else if (varDataType == CDF_TIME_TT2000) { // CDF epoch data
                 ((long[])buf)[count++] = ((Long)oneObj).longValue();
               } else if (varDataType == CDF_EPOCH16) { // CDF epoch data
                 ((double[])buf)[count] = ((double[])oneObj)[0];
                 ((double[])buf)[count+1] = ((double[])oneObj)[1];
                 count += 2;
               }
             }
           } else if (one instanceof JsonNumber) {
             for (int ii = 0; ii < items2; ++ii) {
               Object single = (Object) ((JsonArray)data).get(ii);
               Object oneObj = setData1 (single);
               switch ((int)varDataType) {
                 case (int)CDF_BYTE:
                 case (int)CDF_INT1:
                   ((byte[])buf)[count++] = ((Byte)oneObj).byteValue();
                   break;
                 case (int)CDF_INT2:
                 case (int)CDF_UINT1:
                   ((short[])buf)[count++] = ((Short)oneObj).shortValue();
                   break;
                 case (int)CDF_INT4:
                 case (int)CDF_UINT2:
                   ((int[])buf)[count++] = ((Integer)oneObj).intValue();
                   break;
                 case (int)CDF_UINT4:
                 case (int)CDF_INT8:
                   ((long[])buf)[count++] = ((Long)oneObj).longValue();
                   break;
                 case (int)CDF_REAL4:
                 case (int)CDF_FLOAT:
                   ((float[])buf)[count++] = ((Float)oneObj).floatValue();
                   break;
                 case (int)CDF_REAL8:
                 case (int)CDF_DOUBLE:
                   ((double[])buf)[count++] = ((Double)oneObj).doubleValue();
                   break;
                 default:
                   break;
               } // switch
             } // end of array
           }
         } else {
           System.out.println("                     ?????");
           return null;
         }
       }
       return buf; 
   }

   private static Object setData1 (Object data) throws CDFException {

       String value;
       if (data instanceof JsonString || data instanceof String) {
         if (data instanceof JsonString)
           value = ((JsonString)data).toString();
         else
           value = (String) data;
         if (value.charAt(0) == '"')
           value = value.substring(1, value.length()-1);
         if (varDataType == CDF_EPOCH) {
           return new Double(Epoch.toParse(value));
         } else if (varDataType == CDF_TIME_TT2000) {
           return new Long(CDFTT2000.toParse(value));
         } else if (varDataType == CDF_EPOCH16) {
           double[] epoch16 = Epoch16.toParse(value);
           return epoch16;
         } else
           return value;
       } else if (data instanceof JsonNumber) {
         switch ((int)varDataType) {
           case (int)CDF_BYTE:
           case (int)CDF_INT1:
             return new Byte((byte)((JsonNumber)data).intValue());
           case (int)CDF_INT2:
           case (int)CDF_UINT1:
             return new Short((short)((JsonNumber)data).intValue());
           case (int)CDF_INT4:
           case (int)CDF_UINT2:
             return new Integer(((JsonNumber)data).intValue());
           case (int)CDF_UINT4:
           case (int)CDF_INT8:
             return new Long(((JsonNumber)data).longValue());
           case (int)CDF_REAL4:
           case (int)CDF_FLOAT:
             return new Float(((JsonNumber)data).doubleValue());
           case (int)CDF_REAL8:
           case (int)CDF_DOUBLE:
             return new Double(((JsonNumber)data).doubleValue());
           default:
             return null;
         }
       }
       return null; 
   }

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
       System.out.println ("    This program exports the contents of a Json "+
                           "file into a CDF file(s).");
       System.out.println ("");
       System.out.println ("Usage: java Json2CDF [Options] JsonFileName ");
       System.out.println ("    JsonFileName: The name of the Json file used "+
                           "to create a CDF(s).");
       System.out.println (" ");
       System.out.println ("Options: ");
       System.out.println (" ");
       System.out.println ("   -output:outputFileName");
       System.out.println ("      Use this option to specify a different "+
                           "output file name for the CDF.");
       System.out.println ("      It is only applicable to a single CDF file "+
                           "output.");
       System.out.println (" ");
       System.out.println ("   -outdir:outputDir");
       System.out.println ("      Specifies a directory to hold the output "+
                           "CDF file(s). Make sure the");
       System.out.println ("      directory exists and is writeable.");
       System.out.println (" ");
       System.out.println ("   -vardata:[yes | no].   -vardata:yes is the "+
                           "default.");
       System.out.println ("      Whether to extract var data in the Json "+
                           "file if it is there. If \"no\"");
       System.out.println ("      is specified, non-record varying variables' "+
                           "data will not be extracted.");
       System.out.println (" ");
       System.out.println ("   -[showProgress | Progress]");
       System.out.println ("      This option displays the processing "+
                           "progress on the screen.");
       System.out.println ("      It shows how many CDF variables are there "+
                           "to process and which");
       System.out.println ("      variable is being processed.");
       System.out.println (" ");
       System.out.println ("   -checksum");
       System.out.println ("      This option adds checksum to the file.");
       System.out.println (" ");
       System.out.println ("   -nocompress");
       System.out.println ("      This option forces no compression for "+
                           "variable data. By default, a");
       System.out.println ("      variable with large enough size (>1K) will "+
                           "be compressed, no matter if");
       System.out.println ("      defined by its specification or not.");
       System.out.println (" ");
       System.out.println ("   -delete");
       System.out.println ("      Over-ride the CDF file(s) if it already"+
                           "exists.");
       System.out.println ("");
       System.out.println ("   -showfile");
       System.out.println ("      Display the Json file being processed.");
       System.out.println ("");
       System.out.println ("NOTE:");
       System.out.println ("  1. All integral values are converted into 4-byte"+
                           " integer, if they can fill. ");
       System.out.println ("     Otherwise, 8-byte long are used. All floating"+
                           "-values are converted into ");
       System.out.println ("     8-byte doubles.");
       System.out.println ("  2. A varying dimension with size of 1 is "+
                           "considered as non-varying.");
       System.out.println ("     Its dimension is removed and NOT shown.");
       System.out.println ("  3. Values for variable attributes: VALIDMIN/"+
                           "VALIDMAX/SCALEMIN/SCALEMAX/FILLVAL");
       System.out.println ("     will have the same data type as the variable's.");
       System.out.println ("  4. All variables are zVariables.");
       System.out.println ("  5. All \"NaN\"|\"-NaN\" and \"INF\"|\"-INF\" "+
                           "strings are converted into");
       System.out.println ("     floating-point values. ");
       System.out.println ("");
       System.out.println ("Examples: ");

       System.out.println ("   java Json2CDF test.json (same as "+
                           "java Json2CDF -json:test.cdf test.json)");
       System.out.println ("");
       System.out.println ("The Json file should have the following form: "+
                           "(made from CDF2Json):\n"+
                           "CDFname1: \n"+
                           " CDFFileInfo: \n"+
                           "   FileVersion:..... \n"+
                           "   Format:..... \n"+
                           "   Majority:... \n"+
                           "   Encoding:..... \n"+
                           " CDFglobalAttributes: \n"+
                           "   Gattr_name1: \n"+
                           "    entry#:value \n"+
                           "      ... \n"+
                           "   Gattr_name2: \n"+
                           "    entry#:value \n"+
                           "      ... \n"+
                           "   ... \n"+
                           "   ... \n"+
                           " CDFVariables: \n"+
                           "   VarName1: \n"+
                           "    VarDescription: \n"+
                           "      DataType:.... \n"+
                           "      ... \n"+
                           "      ... \n"+
                           "    VarAttributes: \n"+
                           "      VALIDMIN:.... \n"+
                           "      ... \n"+
                           "      ... \n"+
                           "    VarData: \n"+
                           "      ... \n"+
                           "   VarName2: \n"+
                           "    VarDescription: \n"+
                           "      DataType:.... \n"+
                           "      ... \n"+
                           "      ... \n"+
                           "    VarAttributes: \n"+
                           "      ... \n"+
                           "      ... \n"+
                           "    VarData: \n"+
                           "      ... \n"+
                           " EmptyAttributes:   <== If attribute(s) defined but not assign value(s) \n"+
                           "    GlobalAttributes:    <== If global attribute(s) is not assigned value \n"+
                           "    VariableAttributes:  <== If variable attribute(s) is not assigned value \n"+
                           "  \n"+
                           "CDFname2: \n"+
                           " CDFFileInfo: \n"+
                           "  ..... \n"+
                           "  ..... \n"+
                           " CDFglobalAttributes: \n"+
                           "   Gattr_name1: \n"+
                           "      ... \n"+
                           "   Gattr_name2: \n"+
                           "      ... \n"+
                           "   ... \n"+
                           "   ... \n"+
                           " CDFVariables: \n"+
                           "   VarName1: \n"+
                           "    VarDescription: \n"+
                           "      .... \n"+
                           " ....");
 
       System.out.println (" ");
   }

   private static void processGlobalAttributes (CDF cdf, 
                                                JsonArray globalAttrArray)
                                                throws CDFException {

       for (JsonValue attrInfo : globalAttrArray) {
         for (String attrName: ((JsonObject)attrInfo).keySet()) {
           if (progress) System.out.println("  "+attrName);
           Attribute attr  = Attribute.create(cdf, attrName, GLOBAL_SCOPE);
           JsonArray entries = ((JsonObject)attrInfo).getJsonArray(attrName);
           for (int ii = 0; ii < entries.size(); ++ii) {
             JsonObject entry = entries.getJsonObject(ii);
             for (String entryID: entry.keySet()) {
               Object value = getData ((Object)(entry.get(entryID)));
               if (progress) System.out.println("    "+entryID+":"+value);
               Entry.create(attr, new Integer(entryID).intValue(),
                            dataType, value);
             }
           }
         }
       }
   }

   private static Variable processVariableSpec (CDF cdf, String varName,
                                                Object values)
                                                throws CDFException {

       Object padValue = null;
       varPadValue = null;
       varSparseRec = NO_SPARSERECORDS;
       long compression = -1, compLvl = 0; 
       long blockingFactor = -1;
       int items = ((JsonArray)values).size();
       varDataType = varNumElems = varNumDims = varMaxWritten = -1;
       for (int jj = 0; jj < items; ++jj) {
         Object data = (Object) ((JsonArray)values).get(jj);
         if (data instanceof JsonObject) {
           for (String keyx: ((JsonObject)data).keySet()) {
             Object des = ((JsonObject)data).get(keyx);
             if (progress) System.out.println("      "+keyx+":"+des);
             if (keyx.equalsIgnoreCase("varname")) {
             } else if (keyx.equalsIgnoreCase("datatype")) {
               String dt = ((JsonObject)data).getString(keyx);
               varDataType = CDFUtils.getDataTypeValue(dt);
             } else if (keyx.equalsIgnoreCase("numelements")) {
               varNumElems = (long) ((JsonObject)data).getInt(keyx);
             } else if (keyx.equalsIgnoreCase("numdims")) {
               varNumDims = (long) ((JsonObject)data).getInt(keyx);
             } else if (keyx.equalsIgnoreCase("dimsizes")) {
               JsonArray ds = ((JsonObject)data).getJsonArray(keyx);
               for (int j = 0; j < (int)varNumDims; ++j) {
                 varDimSizes[j] = (long) ds.getInt(j);
               }
             } else if (keyx.equalsIgnoreCase("recvariance")) {
               String rv = ((JsonObject)data).getString(keyx);
               if (rv.equalsIgnoreCase("vary")) varRecVary = VARY;
               else varRecVary = NOVARY;
             } else if (keyx.equalsIgnoreCase("dimvariances")) {
               JsonArray dv = ((JsonObject)data).getJsonArray(keyx);
               for (int j = 0; j < (int)varNumDims; ++j) {
                 String dv1 = dv.getString(j);
                 if (dv1.equalsIgnoreCase("vary"))
                   varDimVariances[j] = VARY;
                 else
                   varDimVariances[j] = NOVARY;
               }
             } else if (keyx.equalsIgnoreCase("compression")) {
               String cp = ((JsonObject)data).getString(keyx);
               String cType = cp.substring(0, cp.indexOf('.'));
               String cLvl = cp.substring(cp.indexOf('.')+1);
               compression = CDFUtils.getLongCompressionType(cType);
               compLvl = new Long(cLvl).longValue();
             } else if (keyx.equalsIgnoreCase("blockingfactor")) {
               blockingFactor = (long) ((JsonObject)data).getInt(keyx);
             } else if (keyx.equalsIgnoreCase("sparserecords")) {
               String sr = ((JsonObject)data).getString(keyx);
               varSparseRec = CDFUtils.getLongSparseRecord(sr);
             } else if (keyx.equalsIgnoreCase("numwrittenrecords")) {
               varNumWritten = (long) ((JsonObject)data).getInt(keyx);
             } else if (keyx.equalsIgnoreCase("maxwrittenrec")) {
               varMaxWritten = (long) ((JsonObject)data).getInt(keyx);
             } else if (keyx.equalsIgnoreCase("padvalue")) {
               padValue = (Object) ((JsonObject)data).get(keyx);
             } else
               System.out.println("NOT handling... "+keyx);
           }
         }
       }

       if (progress) {
         System.out.print("***** Creating... var:"+varName+" "+" type:"+
                          CDFUtils.getStringDataType(varDataType)+
                          " numElms="+varNumElems+" numDims="+varNumDims+" ");
         if (varNumDims > 0) {
           System.out.print("dimSize=[");
           for (int ll= 0; ll < (int)varNumDims; ++ll) {
             System.out.print(varDimSizes[ll]);
             if (ll < (varNumDims-1)) System.out.print(",");
             if (ll == (varNumDims-1)) System.out.print("] ");
           }
         }
         System.out.println("RecVary="+(varRecVary==-1?"T":"F"));
       }

       if (varDataType == -1 || varNumElems == -1 || varNumDims == -1)
         return null;
       Variable va = Variable.create(cdf, varName, varDataType, varNumElems,
                                     varNumDims, varDimSizes, varRecVary, 
                                     varDimVariances);
       varFillVal = null;
       if (padValue != null) {
         varPadValue = setData1 (padValue);
         va.setPadValue(varPadValue);
       }
       if (toCompress && varSparseRec == NO_SPARSERECORDS) {
         if (compression != -1)
           va.setCompression(compression, new long[] {compLvl});
         else {
           long nValues = (varMaxWritten==-1?0:varMaxWritten) + 1;
           for (int ll = 0; ll < (int)varNumDims; ++ll)
             nValues *= varDimSizes[ll];
           nValues *= varNumElems;
           nValues *= getDataTypeSize(varDataType);
           if (nValues > 1000) {
             compression = GZIP_COMPRESSION;
             compLvl = 6;
             va.setCompression(compression, new long[] {compLvl});
           }
         }
       }
       if (blockingFactor != -1)
         va.setBlockingFactor(blockingFactor);
       if (varSparseRec != NO_SPARSERECORDS)
         va.setSparseRecords(varSparseRec);
       return va;
   }

   private static void processVariableAttributes (CDF cdf, Variable va,
                                                  Object values)
                                                  throws CDFException {
       int items = ((JsonArray)values).size();
       for (int jj = 0; jj < items; ++jj) {
         Attribute attr = null;
         Object data = (Object) ((JsonArray)values).get(jj);
         if (data instanceof JsonObject) {
           for (String keyx: ((JsonObject)data).keySet()) {
             Object des = ((JsonObject)data).get(keyx);
             if (progress) System.out.println("      "+keyx+":"+des);
             Object outs = getData(des);
             try {
               attr = Attribute.create(cdf, keyx, VARIABLE_SCOPE);
             } catch (CDFException ss) {
               attr = cdf.getAttribute(keyx);
             }
             if (keyx.equalsIgnoreCase("validmin") ||
                 keyx.equalsIgnoreCase("validmax") ||
                 keyx.equalsIgnoreCase("scalemin") ||
                 keyx.equalsIgnoreCase("scalemax") ||
                 keyx.equalsIgnoreCase("fillval")) {
               outs = changeData(dataType, varDataType, outs);
               dataType = varDataType;
               if (keyx.equalsIgnoreCase("fillval"))
                 varFillVal = outs;
             }
             va.putEntry(attr, dataType, outs);
           }
         }
       }
   }

   private static void processVariableData (Variable va, Object varData)
                                            throws CDFException {
       int recNum, items, nItems = 1;
       recNum = 1;
       items = 1;
       for (int ii = 0; ii < varNumDims; ++ii) items *= varDimSizes[ii];
       if (varData instanceof JsonArray) {
         nItems = ((JsonArray)varData).size();
       }
       if (varRecVary == VARY) {
         if ((((JsonArray)varData).get(0)) instanceof JsonArray)
           recNum = nItems;
         else 
           recNum = nItems / items;
       }
       String signature = CDFUtils.getSignature(varData);
       int nBytes = 1;
       Object realData = null;
       Object outs = null;
       if (varDataType == CDF_CHAR || varDataType == CDF_UCHAR)
         nBytes = items * (int) varNumElems * recNum;
       if (varSparseRec == NO_SPARSERECORDS)
         items *= recNum;
       if (items > 1) {
         if (varDataType == CDF_INT1 || varDataType == CDF_BYTE) { 
           outs = (Object) new byte[items];
         } else if (varDataType == CDF_INT2 || varDataType == CDF_UINT1) {
           outs = (Object) new short[items];
         } else if (varDataType == CDF_INT4 || varDataType == CDF_UINT2) {
           outs = (Object) new int[items];
         } else if (varDataType == CDF_INT8 || varDataType == CDF_UINT4 ||
                    varDataType == CDF_TIME_TT2000) {
           outs = (Object) new long[items];
         } else if (varDataType == CDF_REAL4 || varDataType == CDF_FLOAT) {
           outs = (Object) new float[items];
         } else if (varDataType == CDF_REAL8 || varDataType == CDF_DOUBLE ||
                    varDataType == CDF_EPOCH) {
           outs = (Object) new double[items];
         } else if (varDataType == CDF_EPOCH16) {
           outs = (Object) new double[items*2];
         } else if (varDataType == CDF_CHAR || varDataType == CDF_UCHAR) {
           outs = (Object) new byte[nBytes];
         }
       }
       if (varSparseRec == NO_SPARSERECORDS) {
         if (varData instanceof JsonArray)
           realData = setData((Object) varData,
                              (items==1?null:(Object)outs), items,
                              (recNum==1?items:recNum));
         else
           realData = setData1(varData);
         va.putRecords(0L, (long) recNum, realData);
       } else {
         Object filledData = null;
         if (varSparseRec == PAD_SPARSERECORDS) {
           if (varFillVal != null) filledData = varFillVal;
           else filledData = varPadValue;
         }
         for (int ii = 0; ii < recNum; ++ii) {
           Object data = (Object) ((JsonArray)varData).get(ii);
           if (data instanceof JsonArray)
             realData = setData(data, (items==1?null:(Object)outs), items,
                                items);
           else
             realData = setData1(data);
           boolean virtual = checkForVirtual (realData, filledData);
           if (!virtual) {
             va.putRecord((long)ii, realData);
             if (varSparseRec == PREV_SPARSERECORDS) 
             filledData = realData;
           }
         }
       }
   }

   private static boolean  checkForVirtual (Object data, Object filledData) {

       if (filledData == null) return false;
       String signature = CDFUtils.getSignature(data);
       String sig2 = CDFUtils.getSignature(filledData);
       if (signature.charAt(0) == '[') {
         if (varDataType == CDF_INT1 || varDataType == CDF_BYTE) {
           int itemN = ((byte[])data).length;
           for (int ik = 0; ik < itemN; ++ik) {
             if (sig2.charAt(0) == '[') {
               if (((byte[])data)[ik] != ((byte[])filledData)[ik])
                 return false;
             } else {
               if (((byte[])data)[ik] != ((Byte)filledData).byteValue())
                 return false;
             }
           }
           return true;
         } else if (varDataType == CDF_INT2 || varDataType == CDF_UINT1) {
           int itemN = ((short[])data).length;
           for (int ik = 0; ik < itemN; ++ik) {
             if (sig2.charAt(0) == '[') {
               if (((short[])data)[ik] != ((short[])filledData)[ik])
                 return false;
             } else {
               if (((short[])data)[ik] != ((Short)filledData).shortValue())
                 return false;
             }
           }
           return true;
         } else if (varDataType == CDF_INT4 || varDataType == CDF_UINT2) {
           int itemN = ((int[])data).length;
           for (int ik = 0; ik < itemN; ++ik) {
             if (sig2.charAt(0) == '[') {
               if (((int[])data)[ik] != ((int[])filledData)[ik])
                 return false;
             } else {
               if (((int[])data)[ik] != ((Integer)filledData).intValue())
                 return false;
             }
           }
           return true;
         } else if (varDataType == CDF_INT8 || varDataType == CDF_UINT4 ||
                    varDataType == CDF_TIME_TT2000) {
           int itemN = ((long[])data).length;
           for (int ik = 0; ik < itemN; ++ik) {
             if (sig2.charAt(0) == '[') {
               if (((long[])data)[ik] != ((long[])filledData)[ik])
                 return false;
             } else {
               if (((long[])data)[ik] != ((Long)filledData).longValue())
                 return false;
             }
           }
           return true;
         } else if (varDataType == CDF_REAL4 || varDataType == CDF_FLOAT) {
           int itemN = ((float[])data).length;
           for (int ik = 0; ik < itemN; ++ik) {
             if (sig2.charAt(0) == '[') {
               if (((float[])data)[ik] != ((float[])filledData)[ik])
                 return false;
             } else {
               if (((float[])data)[ik] != ((Float)filledData).floatValue())
                 return false;
             }
           }
           return true;
         } else if (varDataType == CDF_REAL8 || varDataType == CDF_DOUBLE ||
                    varDataType == CDF_EPOCH) {
           int itemN = ((double[])data).length;
           for (int ik = 0; ik < itemN; ++ik) {
             if (sig2.charAt(0) == '[') {
               if (((double[])data)[ik] != ((double[])filledData)[ik])
                 return false;
             } else {
               if (((double[])data)[ik] != ((Double)filledData).doubleValue())
                 return false;
             }
           }
           return true;
         }
       } else {
         if (varDataType == CDF_INT1 || varDataType == CDF_BYTE) {
           if (((Byte)data).byteValue() != ((Byte)filledData).byteValue())
             return false;
           else
             return true;
         } else if (varDataType == CDF_INT2 || varDataType == CDF_UINT1) {
           if (((Short)data).shortValue() != ((Short)filledData).shortValue())
             return false;
           else
             return true;
         } else if (varDataType == CDF_INT4 || varDataType == CDF_UINT2) {
           if (((Integer)data).intValue() != ((Integer)filledData).intValue())
             return false;
           else
             return true;
         } else if (varDataType == CDF_INT8 || varDataType == CDF_UINT4 ||
                    varDataType == CDF_TIME_TT2000) {
           if (((Long)data).longValue() != ((Long)filledData).longValue())
             return false;
           else       
             return true;
         } else if (varDataType == CDF_REAL4 || varDataType == CDF_FLOAT) {
           if (((Float)data).floatValue() != ((Float)filledData).floatValue())
             return false;
           else
             return true;
         } else if (varDataType == CDF_REAL8 || varDataType == CDF_DOUBLE ||
                    varDataType == CDF_EPOCH) {
           if (((Double)data).doubleValue() != ((Double)filledData).doubleValue())
             return false;
           else
             return true;
         } else
           return false;
       }
   return false;
   }

   private static int getDimSize (Object obj) {
       String sig = CDFUtils.getSignature(obj);
       String what = sig.substring(0,2);
       if (what.equals("[I")) return ((int[])obj).length;
       else if (what.equals("[B")) return ((byte[])obj).length;
       else if (what.equals("[J")) return ((long[])obj).length;
       else if (what.equals("[S")) return ((short[])obj).length;
       else if (what.equals("[F")) return ((float[])obj).length;
       else if (what.equals("[D")) return ((double[])obj).length;
       return 1;
   }

   private static int getDataTypeSize (long dataType) {
       int size;
       switch ((int)dataType) {
         case (int)CDF_BYTE:
         case (int)CDF_INT1:
         case (int)CDF_UINT1:
         case (int)CDF_CHAR:
         case (int)CDF_UCHAR:
           size = 1;
           break;
         case (int)CDF_INT2:
         case (int)CDF_UINT2:
           size = 2;
           break;
         case (int)CDF_INT4:
         case (int)CDF_UINT4:
         case (int)CDF_REAL4:
         case (int)CDF_FLOAT:
           size = 4;
           break;
         case (int)CDF_INT8:
         case (int)CDF_TIME_TT2000:
         case (int)CDF_REAL8:
         case (int)CDF_DOUBLE:
         case (int)CDF_EPOCH:
           size = 8;
           break;
         case (int)CDF_EPOCH16:
           size = 16;
           break;
         default:
           size = 1;
           break;
       }
       return size;
   }

}

