import java.io.*;
import java.text.*;
import java.util.*;
import java.lang.*;
import java.lang.reflect.*;
import gsfc.nssdc.cdf.*;
import gsfc.nssdc.cdf.util.*;
import com.google.gson.*;

/**
 *  This program demonstrates how to read the contents of a CDF file.
 */

public class ShowCDFJson implements CDFConstants{

    public static void main(String[] args) {
      if (args.length == 0)  {
        System.out.println ("Java ShowCDFJson a_cdf_file ");
        System.exit(1);
      }
      try {
        CDF cdf = CDF.open(args[0]);

        Map acdf = cdf.readCDF();
        System.out.println("Total cdf size: " + acdf.size());
        Gson gson = new GsonBuilder().setPrettyPrinting().create();
        System.out.println(gson.toJson(acdf));
/*
        Map basic = cdf.readCDFInfo();
        System.out.println(gson.toJson(basic));

        Map globals = cdf.readCDFGlobalAttributes();
        System.out.println(gson.toJson(globals));

        Map spec = cdf.readCDFVariablesSpec();
        System.out.println(gson.toJson(spec));

        Map meta = cdf.readCDFVariablesMetaData();
        System.out.println(gson.toJson(meta));

        Map data = cdf.readCDFVariablesData();
        System.out.println(gson.toJson(data));

        for (Object key : acdf.keySet()) {
           if (((String)key).equals("Variables")) {
             Map vars = (Map) acdf.get(key);
             for (Object varName : vars.keySet()) {
               System.out.println("****** Variable=>"+varName);
               Map var = (Map) vars.get(varName);
               System.out.println("  meta=>"+var.get("VarMetaData"));
               System.out.println("  data=>");
               CDFUtils.printData(var.get("VarData"));
             }
           } else {
             System.out.println(key + " - " + acdf.get(key));
           }
        }
        System.out.println();
*/
        cdf.close(); 
      } catch (Exception ex) {
        System.out.println("**** error: "+ex);
      }
    }

}
