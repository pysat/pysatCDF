import java.io.*;
import java.text.*;
import java.util.*;
import java.lang.reflect.*;
import java.lang.*;
import javax.json.*;
import gsfc.nssdc.cdf.*;
import gsfc.nssdc.cdf.util.*;

public class CDFJsonReader implements CDFConstants {

   private static boolean toGlobal = true;
   private static boolean toVarAttrs = true;
   private static boolean toVarDesc = true;
   private static boolean toVarData = false;
   private static String[] varNames = null;
   private static String inFile = null;
   private static int varCount = 0;
   private static boolean toShow = true;
   private static String item1 = "CDFFileInfo";
   private static String item2 = "CDFglobalAttributes";
   private static String item3 = "CDFVariables";
   private static String item4 = "VarDescription";
   private static String item5 = "VarAttributes";
   private static String item6 = "VarData";
   private static String item7 = "EmptyAttributes";
 
   public static void main(String[] args) throws IOException {

       parseCmdLine(args);

       InputStream fis = new FileInputStream(inFile);
		
       JsonReader jsonReader = Json.createReader(fis);
		
       JsonObject jsonObject = jsonReader.readObject();
       
       jsonReader.close();
       fis.close();

       JsonArray singleFile = jsonObject.getJsonArray(item1);
       if (singleFile != null) {
         // old CDF Json form (good for single file)
         printJsonObject(jsonObject);
       } else {
         // new CDF Json form
         for (String cdfName: jsonObject.keySet()) {
           JsonValue cdf = jsonObject.getJsonObject(cdfName);
           System.out.println(cdfName+":");
           printJsonObject((JsonObject)jsonObject.getJsonObject(cdfName));
         }
       }
   }

   private static void printJsonObject (JsonObject jsonObject1) { 
       // Retrieve data from "CDFFileInfo" JsonArray
       JsonArray fileInfoArray = jsonObject1.getJsonArray(item1);
       if (fileInfoArray != null) {
         System.out.println("  "+item1+":");
         for (JsonValue fileInfo : fileInfoArray) {
           for (String key: ((JsonObject)fileInfo).keySet()) {
             System.out.println("    "+key+":"+((JsonObject)fileInfo).
                                               getString(key));
           }
         }
       }

       // Retrieve data from "CDFglobalAttributes" JsonArray
       if (toGlobal) {
         JsonArray globalAttrArray = jsonObject1.getJsonArray(item2);
         if (globalAttrArray != null) {
           System.out.println("  "+item2+":");
           for (JsonValue attrInfo : globalAttrArray) {
             for (String key: ((JsonObject)attrInfo).keySet()) {
               System.out.println("    "+key+":");
               JsonArray entries = ((JsonObject)attrInfo).getJsonArray(key);
               for (int ii = 0; ii < entries.size(); ++ii) {
                 JsonObject entry = entries.getJsonObject(ii);
                 for (String entryID: entry.keySet()) {
                   Object value = entry.get(entryID);
                   System.out.println("      "+entryID+":"+value);
                 }
               }
             }
           }
         }
       }

       boolean toVar = false;
       // Retrieve data from "CDFVariables" JsonArray
       JsonArray cdfVariables = jsonObject1.getJsonArray(item3);
       String format = null;
       long dataType = 0;
       if (cdfVariables != null) {
         System.out.println("  "+item3+":");
         format = null;
         dataType = 0;
         for (JsonValue varInfo : cdfVariables) {
           for (String key: ((JsonObject)varInfo).keySet()) {
             JsonArray var = ((JsonObject)varInfo).getJsonArray(key);
             JsonObject varItem = var.getJsonObject(0);
             if (varCount > 0) toShow = checkVar (varItem);
             if (!toShow) continue;
             System.out.println("    "+key+":"); // Variable name
             for (int ii = 0; ii < var.size(); ++ii) { // VarDescription |
                                                       // VarAttributes |
                                                       // VarData
               varItem = var.getJsonObject(ii);
               for (String varKey: varItem.keySet()) {
                 if (varKey.equals("VarDescription")) toVar = false;
                 if (varKey.equals(item5) && !toVarAttrs) break;
                 if (varKey.equals(item6) && (!toVarData && !toVar)) break;
                 Object value = varItem.get(varKey);
                 if (value instanceof JsonString) {
                   // VarDescription:
                   System.out.println("      "+varKey+":"+value);
                 } else if (value instanceof JsonArray) {
                   // VarDescription: | VarAttributes: | VarData:
                   System.out.println("      "+varKey+":");
                   int valueN = ((JsonArray)value).size();
                   boolean breakout = false;
                   for (int jj = 0; jj < valueN; ++jj) {
                     Object data = (Object) ((JsonArray)value).get(jj);
                     if (data instanceof JsonObject) {
                       for (String keyx: ((JsonObject)data).keySet()) {
                         Object des = ((JsonObject)data).get(keyx);
                         // DataType: | VALIDMIN: 
                         System.out.println("        "+keyx+":"+des);
                         if (!toVarDesc && keyx.equalsIgnoreCase("varname"))
                           breakout = true;
                         if (keyx.equals("RecVariance")) {
                           if (des.toString().equals("\"NOVARY\"")) toVar= true;
                         }
                         if (keyx.equalsIgnoreCase("format")) {
                           format = toCformat(des.toString().
                                                  replaceAll("^\"|\"$", ""));
                         }
                         if (keyx.equalsIgnoreCase("datatype")) {
                           dataType = CDFUtils.getDataTypeValue(
                                      des.toString().replaceAll("^\"|\"$", ""));
                         }
                       }
                     } else if (data instanceof JsonArray) {
                       System.out.print("        [");
                       int sizex = ((JsonArray)data).size();
                       for (int kk = 0; kk < sizex; ++kk) {
                         Object data1 = ((JsonArray)data).get(kk);
                         System.out.print(data1+(kk<(sizex-1)?", ":"]"));
                       }
                       System.out.println("");
                     } else if (data instanceof JsonNumber ||
                                data instanceof JsonString) {
                       if (jj == 0) System.out.print("      [");
                       if (data instanceof JsonString || format == null ||
                           (dataType != CDF_REAL4 && dataType != CDF_REAL8 &&
                            dataType != CDF_FLOAT && dataType != CDF_DOUBLE))
                         System.out.print(data+(jj<(valueN-1)?", ":""));
                       else
                         System.out.print(
                            String.format(format,((JsonNumber)data).
                                                            doubleValue())+
                            (jj<(valueN-1)?", ":""));
                       if (jj == (valueN - 1)) System.out.println("]");
                     } else {
                       System.out.println("        "+data);
                     }
                     if (breakout) break;
                   }
                 } else if (value instanceof JsonObject)
                   System.out.println("  :a jsonObject: "+value);
                 else System.out.println("  :unknown");
               }
             }
           }
         }

       // Retrieve data from "CDFFileInfo" JsonArray
       JsonArray emptyAttrs = jsonObject1.getJsonArray(item7);
       if (emptyAttrs != null) {
         System.out.println("  "+item7+":");
         for (JsonValue attrs : emptyAttrs) {
           for (String key: ((JsonObject)attrs).keySet()) {
             System.out.println("    "+key+":"+((JsonObject)attrs).get(key));
           }
         }
       }
       }
       
   }

   private static void parseCmdLine (String[] args) {
        String numSpaces = null;

        if (args.length == 0)   // No input is given (i.e. java CDFJsonReader)
            exit("");

        else {                  // Process options
            for (int i=0; i < args.length; i++) {
                 if (i == (args.length-1)) {         // Get the Json file name
                     inFile = args[i];
                     if (!(new File(inFile).exists())) {
                         System.out.println ("** Error: file '"+inFile+
                                             "' does not exist **");
                         System.exit (1);
                     }
                 }
                 else {
                     int loc = args[i].indexOf(":");
                     if (args[i].toLowerCase().startsWith("-vardata:")) {
                         String varData = args[i].substring(loc+1);
                         if (!varData.equalsIgnoreCase("yes") &&
                             !varData.equalsIgnoreCase("no"))
                            exit("** Error: Invalid -vardata entered **");
                         if (varData.equalsIgnoreCase("yes"))
                           toVarData = true;
                     }
                     else if (args[i].toLowerCase().startsWith("-globalmetadata:")) {
                         String globalAttrs = args[i].substring(loc+1);
                         if (!globalAttrs.equalsIgnoreCase("yes") &&
                             !globalAttrs.equalsIgnoreCase("no"))
                            exit("** Error: Invalid -globalmetadata entered **");
                         if (globalAttrs.equalsIgnoreCase("no"))
                           toGlobal = false;
                     }
                     else if (args[i].toLowerCase().startsWith("-vardesc:")) {
                         String varDesc = args[i].substring(loc+1);
                         if (!varDesc.equalsIgnoreCase("yes") &&
                             !varDesc.equalsIgnoreCase("no"))
                            exit("** Error: Invalid -vardesc entered **");
                         if (varDesc.equalsIgnoreCase("no"))
                           toVarDesc = false;
                     }
                     else if (args[i].toLowerCase().startsWith("-varmetadata:")) {
                         String varAttrs = args[i].substring(loc+1);
                         if (!varAttrs.equalsIgnoreCase("yes") &&
                             !varAttrs.equalsIgnoreCase("no"))
                            exit("** Error: Invalid -varmetadata entered **");
                         if (varAttrs.equalsIgnoreCase("no"))
                           toVarAttrs = false;
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
                 }
            }
        }
    }

    private static boolean checkVar (JsonObject varInfo) {

        for (String varKey: varInfo.keySet()) {
          JsonArray varItems = varInfo.getJsonArray(varKey);
          JsonObject value = (JsonObject) varItems.getJsonObject(0);
          for (String key: value.keySet()) {
             Object des = value.get(key);
             if (key.equalsIgnoreCase("varname")) {
               toShow = false;
               String varname = ((JsonString)des).getString();
               for (int ll = 0; ll < varCount; ++ll) {
                 if (varname.equals(varNames[ll])) {
                    toShow = true;
                    break;
                 }
               }
             }
           }
        }
        return toShow;
    }

    private static void exit (String msg) {
        if (!msg.equals("")) System.out.println (msg);
        usage();
        System.exit(1);
    }

    private static void usage() {
       System.out.println ("\nDescription:");
       System.out.println ("    This program displays the contents of a Json "+
                           "file.");
       System.out.println ("");
       System.out.println ("Usage: java CDFJsonReader [Options] JsonFileName ");
       System.out.println (" ");
       System.out.println ("Options: ");
       System.out.println (" ");
       System.out.println ("   -globalmetadata:[yes | no].  -globalmetadata:yes"+
                           "is the default.");
       System.out.println ("      -globalmetadata:yes shows all global "+
                           "attributes.");
       System.out.println ("      -globalmetadata:no doesn't show all global "+
                           "attributes.");
       System.out.println (" ");
       System.out.println ("   -vardesc:[yes | no].  -vardesc:yes is "+
                           "the default.");
       System.out.println ("      -vardesc:yes shows each variable's name and "+
                           "its specification.");
       System.out.println ("      -vardesc:no shows the variable name only.");
       System.out.println (" ");
       System.out.println ("   -varmetadata:[yes | no].  -varmetadata:yes is "+
                           "the default.");
       System.out.println ("      -varmetadata:yes shows each variable's "+
                           "attributes.");
       System.out.println ("      -varmetadata:no doesn't show each "+
                           "variable's attributes.");
       System.out.println (" ");
       System.out.println ("   -vars:var1,var2,....   shows only the "+
                           "specified variables with");
       System.out.println ("                          variable names "+
                           "separated by a single ','.");
       System.out.println ("                          Otherwise, all variables "+
                           "are shown.");
       System.out.println (" ");
       System.out.println ("   -vardata:[yes | no].  -vardata:no is "+
                           "the default.");
       System.out.println ("      -vardata:yes shows the variable data");
       System.out.println ("      -vardata:no doesn't show any record varying "+
                           "variable data");
       System.out.println (" ");
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
         if (formatY.endsWith(")")) 
           formatY = formatY.substring(0, formatY.length()-1);
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

}
