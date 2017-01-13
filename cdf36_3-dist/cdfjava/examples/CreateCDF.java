/******************************************************************************
* Copyright 1996-2013 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/

import java.io.*;
import java.text.*;
import java.util.*;
import gsfc.nssdc.cdf.*;
import gsfc.nssdc.cdf.util.*;

/**
 * This test program demonstrates how to create a single-file CDF 
 * called test.cdf.  It also copies a variable along with data out of
 * test.cdf and creates a new single-file CDF called test1.cdf.
 *
 * Thie program demonstrates the following techniques: 
 *
 *   - how to create a variable
 *   - how to create a global attribute
 *   - how to create a variable attribute
 *   - how to create a global variable attribute entry 
 *   - how to create a variable attribute entry 
 *   - how to add data to a variable
 *   - how to delete a variable attribute
 *   - how to rename a variable
 *   - how to rename a variable attribute
 *   - how to copy a variable with or without data
 *   - how to delete data (records) out of a variable
 *   - how to set options that are available with a CDF file and a variable
 */

public class CreateCDF implements CDFConstants{

   public static void main(String[] args) {
      CDF cdf  = null,
          cdf1 = null;

      try {

          /******************************************************************/
          /*  If the file to be created is a multifile (not a single file), */
          /*  the following restrictions apply:                             */
          /*                                                                */
          /*     - CDF file extension is not allowed (i.e. .cdf)            */
          /*     - Compression and sparse records are not allowed           */
          /*     - Deleting a variable is not allowed                       */
          /******************************************************************/
          File cdfFile = new File("test.cdf");
          if (cdfFile.exists()) cdfFile.delete();

          cdfFile = new File("test1.cdf");
          if (cdfFile.exists()) cdfFile.delete();

	  //CDF.setFileBackward(BACKWARDFILEoff);
	  cdf  = CDF.create("test");
          cdf1 = CDF.create("test1");

          // cdf.setFormat(SINGLE_FILE);              // Set to signle-file CDF
          cdf.setChecksum(MD5_CHECKSUM);

          /*************************************************/
          /*  Set the information/warning message flag     */
          /*    - By default, this flag is turned off      */
          /*    - setInfoWarningOn() turns on the flag     */
          /*    - setInfoWarningOff() turns off the flag   */
          /*************************************************/
          // cdf.setInfoWarningOn();

          /*******************************************/
          /*  Create global and variable attributes  */
          /*******************************************/
          Attribute 
             project  = Attribute.create(cdf, "Project", GLOBAL_SCOPE),
             pi       = Attribute.create(cdf, "PI", GLOBAL_SCOPE),
             test     = Attribute.create(cdf, "Test", GLOBAL_SCOPE),
             testDate = Attribute.create(cdf, "TestDate", GLOBAL_SCOPE),
             ep16TestDate = Attribute.create(cdf, "epTestDate", GLOBAL_SCOPE),

             validMin = Attribute.create(cdf, "VALIDMIN", VARIABLE_SCOPE),
             validMax = Attribute.create(cdf, "VALIDMAX", VARIABLE_SCOPE),
             snafu    = Attribute.create(cdf, "snafu", VARIABLE_SCOPE),
             dummy    = Attribute.create(cdf, "dummy", VARIABLE_SCOPE),
             myAttr   = Attribute.create(cdf, "myAttr", VARIABLE_SCOPE);

          System.out.println("Created attributes.");
                                            
          /**********************/
          /*  Create variables  */
          /**********************/
          long numElements    = 1,
               numDims        = 1,
               dimVary[]      = {VARY},
               noDimVary[]    = {NOVARY},
               dimVariance1[] = {VARY, VARY},
               dimVariance2[] = {VARY, NOVARY};

          Variable
            latitude = Variable.create(cdf, "Latitude", 
                                       CDF_INT1, 
                                       1, 1, new long [] {3},
                                       NOVARY,                  /* recVary */
                                       new long [] {VARY}),

            latitude1 = Variable.create(cdf, "Latitude1", 
                                        CDF_UINT1, 
                                        1, 1, new long [] {3},
                                        VARY,     
                                        new long [] {VARY}),

            longitude = Variable.create(cdf, "Longitude", 
                                        CDF_INT2, 
                                        numElements, numDims, 
                                        new long [] {3},        /* dimSizes */
                                        VARY,                   /* recVary  */
                                        new long [] {VARY}),

            longitude1 = Variable.create(cdf, "Longitude1", 
                                         CDF_UINT2, 
                                         numElements, numDims, 
                                         new long [] {3},        /* dimSizes */
                                         VARY,                   /* recVary  */
                                         dimVary),               /* dimVary  */

            delta = Variable.create(cdf, "Delta",
                                    CDF_INT4,
                                    1, 2, new long [] {3,2},
                                    VARY, dimVariance1),
 
            volume = Variable.create(cdf, "volume",
                                    CDF_INT4,
                                    1, 3, new long [] {2,4,2},
                                    VARY, new long [] {VARY, VARY, VARY}),
 
            time = Variable.create(cdf, "Time",
                                   CDF_UINT4,
                                   1, 2, new long [] {3,2},
                                   VARY, dimVariance1),
 
            dvar = Variable.create(cdf, "dvar", CDF_INT2, 
                                   1, 1, new long [] {3},
                                   NOVARY, 
                                   new long [] {NOVARY}),

            name = Variable.create(cdf, "Name",
                                   CDF_CHAR,
                                   10, 1, new long [] {2},
                                   VARY,
                                   new long [] {VARY}),

            temp = Variable.create(cdf, "Temp", CDF_FLOAT, 
                                   1, 1, new long [] {3},
                                   VARY, 
                                   new long [] {VARY}),

            temp1 = Variable.create(cdf, "Temp1", CDF_REAL4, 
                                    1, 1, new long [] {3},
                                    VARY, 
                                    new long [] {VARY}),

            temp2 = Variable.create(cdf, "Temperature", CDF_FLOAT, 
                                    1, 0, new long [] {1},
                                    VARY, 
                                    new long [] {NOVARY}),

            temp3 = Variable.create(cdf, "Temperature1", CDF_FLOAT, 
                                    1, 1, new long [] {3},
                                    NOVARY, 
                                    new long [] {VARY}),

            temp4 = Variable.create(cdf, "Temperature2", CDF_FLOAT, 
                                    1, 0, new long [] {1},
                                    NOVARY, 
                                    new long [] {NOVARY}),

            dp = Variable.create(cdf, "dp", CDF_DOUBLE, 
                                 1, 1, new long [] {3},
                                 VARY, 
                                 new long [] {VARY}),

            ep = Variable.create(cdf, "ep", CDF_EPOCH, 
                                 1, 0, new long [] {0},
                                 VARY, 
                                 new long [] {NOVARY}),

            ep16 = Variable.create(cdf, "ep16", CDF_EPOCH16, 
                                 1, 0, new long [] {0},
                                 VARY, 
                                 new long [] {NOVARY}),

            newI8 = Variable.create(cdf, "newI8", CDF_INT8, 
                                 1, 1, new long [] {2},
                                 VARY, 
                                 new long [] {VARY}),

            tt2000 = Variable.create(cdf, "tt2000", CDF_TIME_TT2000, 
                                 1, 0, new long [] {0},
                                 VARY, 
                                 new long [] {NOVARY}),

            dummyVar = Variable.create(cdf, "DummyVar", 
                                       CDF_EPOCH, 
                                       1, 0, new long [] {0},
                                       VARY, 
                                       new long [] {NOVARY});

          System.out.println("Created variables.");
                
          /*********************************/
          /*  Set miscelleneous settings.  */ 
          /*********************************/
          // cdf.setCompression(RLE_COMPRESSION, new long[] {0});
          cdf.setMajority(ROW_MAJOR);         /* Default is ROW_MAJOR */
          // cdf.setEncoding(SUN_ENCODING);

          cdf.selectNegtoPosfp0(NEGtoPOSfp0off);
          cdf.selectCDFCacheSize(400L);            
          cdf.selectCompressCacheSize(500L);
          cdf.selectStageCacheSize(600L);            

          System.out.println("Mode:                   "+
                             cdf.confirmzMode());
          System.out.println("Neg -0.0 to 0.0:        "+
                             cdf.confirmNegtoPosfp0());
          System.out.println("CDF Cache size:         "+
                             cdf.confirmCDFCacheSize()); 
          System.out.println("Compression Cache Size: "+
                             cdf.confirmCompressCacheSize());
          System.out.println("Stage Cache Size:       "+
                             cdf.confirmStageCacheSize());
          System.out.println("\nDefined CDF file options...");

          dvar.setDimVariances (new long [] {VARY});

          longitude.selectCacheSize(700L);
          longitude.setCompression(GZIP_COMPRESSION, new long[] {9});
//          longitude.setPadValue(new Short((short) -99));
          longitude.setBlockingFactor(130L);
          longitude.setInitialRecords(20L);
System.out.println("pad value="+longitude.getPadValue()+" status="+cdf.getStatus()+" confirm="+longitude.confirmPadValue());

          // Only applicable to compressed z variables
          longitude.selectReservePercent(15L);   
            
          System.out.println("Cache Size (longitude):          "+
                             longitude.confirmCacheSize());
          System.out.println("Reserver Percentage (longitude): "+
                              longitude.confirmReservePercent());

          temp.setSparseRecords(PAD_SPARSERECORDS);
          time.setBlockingFactor(60);
//        ep.setSparseRecords(PREV_SPARSERECORDS);

          // Only applicable to uncompressed Z vars in a single-file CDF.
          ep.allocateRecords(3L);   

          long firstRec = 9, lastRec = 20;
          time.allocateBlock(firstRec, lastRec);

          System.out.println ("time.getAllocatedFrom(4L): "+
                              time.getAllocatedFrom(4L));
          System.out.println ("time.getAllocatedTo(13L): "+
                              time.getAllocatedFrom(13L));

          /*******************************************/
          /* Add entries to global attributes        */
          /*                                         */
          /* NOTE: entry value must be a Java object */
          /*******************************************/
          Double  entryValue = new Double(5.3432);

          System.out.println("Adding global attribute entries...");
        
          Entry.create(project, 0, CDF_CHAR, "Using the CDFJava ƒ ∑ API");
          Entry.create(pi, 3, CDF_CHAR, "Ernie Els");
          System.out.println("\tchars completed.");

          Entry.create(test, 0, CDF_DOUBLE, entryValue); 
          Entry.create(test, 1, CDF_DOUBLE, new double []{5.3, 2.3});
          System.out.println("\tdoubles completed.");

          Entry.create(test, 2, CDF_FLOAT, new Float(5.5));
          Entry.create(test, 3, CDF_FLOAT, 
                       new float [] {(float)5.5,(float)10.2});
          System.out.println("\tfloats completed.");

          Entry.create(test, 4, CDF_INT1,  new Byte((byte)1));
          Entry.create(test, 5, CDF_INT1,  
                       new byte [] {(byte) 1, (byte)2, (byte)3});
          System.out.println("\tbytes completed.");

          Entry.create(test,6,CDF_INT2,new Short((short)-32768));
          Entry.create(test, 7, CDF_INT2,  
                       new short []  {(short)1,(short)2});
          System.out.println("\tshorts completed.");

          Entry.create(test, 8, CDF_INT4,  new Integer(3));
          Entry.create(test, 9, CDF_INT4, new int [] {4,5});
          System.out.println("\tintegers completed.");

          Entry.create(test, 10, CDF_CHAR,  "This is a string");
          System.out.println("\tString completed.");
            
          Entry.create(test,11,CDF_UINT4, new Long(4294967295L));
          Entry.create(test,12,CDF_UINT4, 
                       new long[] {4294967295L,2147483648L});
          System.out.println("\tUINT4 completed.");

          Entry.create(test,13,CDF_UINT2,new Integer(65535));
          Entry.create(test,14,CDF_UINT2,new int[]{65535,65534});
          System.out.println("\tUINT2 completed.");

          Entry.create(test,15,CDF_UINT1, new Short((short)255));
          Entry.create(test,16,CDF_UINT1, new short[] {255,254});
          System.out.println("\tUINT1 completed.");

          double testDateData  = Epoch.compute(2002, 4, 25, 0, 0, 0, 0);
          Entry.create(testDate, 1, CDF_EPOCH, new Double(testDateData)); 
          System.out.println("\tEPOCH completed.");

          double[] ep16Data = new double[2];
          double ep_status  = Epoch16.compute(2004, 5, 13, 15, 8,
                                              11, 22, 33, 44, 55, ep16Data);
          Entry.create(ep16TestDate, 0, CDF_EPOCH16, ep16Data);
          System.out.println("\tEPOCH16 completed.");

          long testTT2000  = CDFTT2000.compute(2008, 2, 4, 6, 8, 10, 12, 14, 16);
          Entry.create(testDate, 2, CDF_TIME_TT2000,
                       new Long(testTT2000));
          System.out.println("\tEPOCH completed.");

          /************************************/
          /*  Add variable attribute entries  */
          /************************************/
          System.out.println("Adding variable attribute entries...");

          // Entry.create(validMin, longitude.getID(), CDF_INT2,
          //             new Short((short)10));
          Entry.create(validMin, longitude.getID(), CDF_CHAR,
                       "Test by David");
          latitude.putEntry(validMin, CDF_INT2, new Short((short)20));
          System.out.println("\tAdded VALIDMIN entries.");

          longitude.putEntry(validMax, CDF_INT2, new Short((short)180));
          latitude.putEntry(validMax, CDF_INT2, new Short((short)90));
          System.out.println("\tAdded VALIDMAX entries.");
            
          longitude.putEntry(snafu,  CDF_CHAR, "test1");
          System.out.println("\tAdded snafu for Longitude.");

          double testDateData1  = Epoch.compute(2002, 5, 13, 0, 0, 0, 0);
          ep.putEntry(dummy,  CDF_EPOCH, new Double(testDateData1));
          System.out.println("\tAdded dummy for ep.");

          ep_status  = Epoch16.compute(2002L, 5L, 13L, 15L, 8L, 
                                       1L, 2L, 3L, 4L, 5L, ep16Data);
          ep16.putEntry(dummy,  CDF_EPOCH16, ep16Data);
          System.out.println("\tAdded dummy for ep16.");

          longitude.putEntry(dummy,  CDF_CHAR, "test2");
          System.out.println("\tAdded dummy for Longitude.");

          long testDateData2  = CDFTT2000.compute(2010, 5, 13, 10, 20, 30, 40,
                                                  50, 60);
          tt2000.putEntry(dummy,  CDF_TIME_TT2000, new Long(testDateData2));
          System.out.println("\tAdded dummy for tt2000.");

          char ch = 'a'; 
          String myStr = String.valueOf(ch); 
          longitude.putEntry(myAttr,  CDF_CHAR, myStr);
          System.out.println("\tAdded myAttr for Longitude.");

          /*************************/
          /*  Delete an attribute  */
          /*************************/
//        dummy.delete();

          /***********************/
          /*  Add variable data  */
          /***********************/
          long recNum    = 0,
          recCount       = 2,
          recInterval    = 1,
          indicies[]     = {0},
          dimIndicies[]  = {0},
          dimCounts[]    = {3},
          dimIntervals[] = {1};

          System.out.println("Adding variable data..."); 

          /***************************/
          /*  Add data to longitude  */
          /***************************/
          long status;
          short [][] longitudeData = {{10, 20, 30}, 
                                      {40, 32767, -32768}}; 

          longitude.putSingleData(recNum, indicies, new Short((short)100));
          status = cdf.getStatus();
          if (status != CDF_OK) {
              String statusText = CDF.getStatusText(status);
              System.out.println (statusText);
          }

          longitude.putSingleData(0L, new long[] {1}, new Short((short)200));
          longitude.putSingleData(0L, new long[] {2}, new Short((short)300));
          System.out.println("\tAdded a single longitude variable data."); 

          recNum = 2;
          longitude.putHyperData(recNum, recCount, recInterval, 
                                 dimIndicies, dimCounts, dimIntervals,
                                 longitudeData);
          System.out.println("\tAdded a hyperput longitude variable data."); 

          recNum = 10;
          longitude.putRecord (recNum, new short[] {11, 22, 33}); 
          System.out.println("\tAdded a single longitude variable data."); 
 
          /****************************/
          /*  Add data to longitude1  */
          /****************************/
          int [][] longitude1Data = {{21, 31},
                                     {51, 61}, 
                                     {32767, 65535}};

          longitude1.putSingleData(0L, indicies, new Integer((int)101));
          longitude1.putSingleData(0L, new long[] {1}, new Integer((int)201));
          longitude1.putSingleData(0L, new long[] {2}, new Integer((int)301));
          System.out.println("\tAdded a single longitude1 variable data.");

          recNum = 1;
          dimIntervals[0] = 2;
          longitude1.putHyperData(recNum, 3, recInterval,
                                  dimIndicies, new long[] {2}, 
                                  dimIntervals,
                                  longitude1Data);
          System.out.println("\tAdded a hyperput longitude1 variable data.");

          /**************************/
          /*  Add data to latitude  */
          /**************************/
          byte [][] latitudeData  = { {15, 25, 35}, 
                                      {45, 127, -128} }; 

          latitude.putSingleData(0L, indicies, new Byte((byte)1));
          latitude.putSingleData(0L, new long[] {1}, new Byte((byte)2));
          latitude.putSingleData(0L, new long[] {2}, new Byte((byte)3));
          System.out.println("\tAdded a single latitude variable data."); 

          /***************************/
          /*  Add data to latitude1  */ 
          /***************************/
          short [][] latitude1Data  = { {15, 25, 35}, 
                                        {100, 128, 255} }; 
            
          latitude1.putSingleData(0L, new long[] {2}, new Short((short)5));
          System.out.println("\tAdded a single latitude1 variable data.");
            
          // This record will overwrite the first record 

          recNum = 1;
          recCount = 2;          
          latitude1.putHyperData(recNum, recCount, recInterval, 
                                 new long[] {0}, new long[] {3}, 
                                 new long[] {1}, latitude1Data);
          System.out.println("\tAdded a hyperput latitude1 variable data.");
                                    
          /***********************/
          /*  Add data to Delta  */
          /***********************/
          int [][][] deltaData = { {{10,20}, {40,50}, {7, 8}},
                                   {{90,95},{96,97}, {32767, -32768}}
                                 };
          delta.putSingleData(0L, new long[] {0,0}, new Integer((int)110));
          delta.putSingleData(0L, new long[] {0,1}, new Integer((int)210));
          delta.putSingleData(0L, new long[] {1,0}, new Integer((int)310));
          delta.putSingleData(0L, new long[] {1,1}, new Integer((int)410));
          delta.putSingleData(0L, new long[] {2,0}, new Integer((int)510));
          delta.putSingleData(0L, new long[] {2,1}, new Integer((int)610));

          delta.putHyperData(1L, 2L, 1L, new long[] {0,0}, new long[] {3,2},
                             new long[] {1,1}, deltaData);

          System.out.println("\tAdded delta data.");

          /************************/
          /*  Add data to Volume  */
          /************************/
          int [][][] volumeData = { {{10,20}, {40,50}, {7, 8}, {9,10}},
                                    {{90,95}, {96,97}, {98, 99}, {80,85}}
                                  };
          volume.putHyperData(0L, 1L, 1L, 
                              new long[] {0,0,0},     // dimension indicies
                              new long[] {2,4,2},     // dimension sizes
                              new long[] {1,1,1},     // dimension interval
                              volumeData);

          System.out.println("\tAdded volume data.");

          /**********************/
          /*  Add data to Time  */  
          /**********************/
          long [][][] timeData = {{{10,20},{40,50},{70, 80}}, 
                                  {{90,95},{96,97},{2147483648L,4294967295L}}
                                 };
          time.putSingleData(0L, new long[] {0,0}, new Long((long)100));
          time.putSingleData(0L, new long[] {0,1}, new Long((long)200));
          time.putSingleData(0L, new long[] {1,0}, new Long((long)300));
          time.putSingleData(0L, new long[] {1,1}, new Long((long)400));
          time.putSingleData(0L, new long[] {2,0}, new Long((long)500));
          time.putSingleData(0L, new long[] {2,1}, new Long((long)600));

          time.putHyperData(5L, 2L, 1L, new long[] {0,0}, new long[] {3,2},
                            new long[] {1,1}, timeData);

          System.out.println("\tAdded time data.");

          /**********************/
          /*  Add data to dvar  */
          /**********************/
          short [][] dvarData  = {{15, 25, 35},
                                  {100, 128, 255} };

          dvar.putSingleData(0L, new long[] {0}, new Short((short)5));
          dvar.putSingleData(1L, new long[] {1}, new Short((short)6));
          System.out.println("\tAdded a single dvar variable data.");

          recNum = 1;
          recCount = 2;          
          dimIntervals[0] = 1;
          dvar.putHyperData(recNum, recCount, recInterval, 
                            dimIndicies, dimCounts, dimIntervals,
                            dvarData);
          System.out.println("\tAdded a hyperput latitude1 variable data.");

          /*********************/
          /*  Add String data  */
          /*********************/
	  name.setPadValue(new String("abc"));
          String [] ndata = new String[2];
          ndata[0] = "abcd";
          ndata[1] = "bcdefghij";
          name.putSingleData(0L, new long[] {0}, new String("123456789"));
          name.putSingleData(0L, new long[] {1}, new String("13579"));

          System.out.println("\tAdded a single string data.");

          name.putHyperData(1L, 1L, 1L, 
                            new long[] {0}, new long[] {2}, new long[] {1}, 
                            ndata);

          System.out.println("\tAdded hyperput name data.");

          /*******************/
          /*  Add Temp data  */
          /*******************/
          float [][] tempData = {{(float)96.5,  (float)97.5, (float)98.5},  
                                 {(float)100.5, (float)110.6, (float)120.7},
                                 {(float)200.5, (float)210.6, (float)220.7}};

          temp.putSingleData(0L, new long[] {0}, new Float("55.5"));
          temp.putSingleData(0L, new long[] {2}, new Float("66.6"));

          System.out.println("\tAdded a single temp data.");

          temp.putHyperData(10L, 3L, 1L, 
                            new long[] {0}, new long[] {3}, new long[] {1}, 
                            tempData);

          System.out.println("\tAdded hyperput temp data.");

          /********************/
          /*  Add Temp1 data  */
          /********************/
          float [][] temp1Data = {{(float)10.5, (float)10.6, (float)10.7},
                                  {(float)20.5, (float)20.6, (float)20.7}};

          temp1.putSingleData(0L, new long[] {0}, new Float(5.5));
          temp1.putSingleData(0L, new long[] {1}, new Float(-0.0));
          temp1.putSingleData(0L, new long[] {2}, new Float(6.6));

          System.out.println("\tAdded a single temp1 data.");

          float temp1Rec[] = {(float)9.5,  (float)-0.0, (float)8.5};
          temp1.putRecord(1L, temp1Rec); 

          temp1.putHyperData(2L, 2L, 1L,
                             new long[] {0}, new long[] {3}, new long[] {1},
                             temp1Data);

          System.out.println("\tAdded hyperput temp1 data.");

          /********************************************/
          /*  Add Temp2 data  - scalar Record Varying */
          /********************************************/
          temp2.putScalarData(0L, new Float("55.55"));
          temp2.putScalarData(1L, new Float("66.66"));

          System.out.println("\tAdded a scalar temp2 data.");

          /********************/
          /*  Add Temp3 data  */ 
          /********************/
          temp3.putRecord(temp1Rec);

          System.out.println("\tAdded a non-scalar temp3 data.");

          /************************************************/
          /*  Add Temp4 data  - scalar Non-Record Varying */
          /************************************************/
          temp4.putScalarData(new Float("77.77"));

          System.out.println("\tAdded a scalar temp4 data.");

          /*****************/
          /*  Add dp data  */
          /*****************/
          double [][] dpData = {{(double)9.5,  (double)7.5, (double)8.5},
                                {(double)10.5, (double)10.6, (double)10.7},
                                {(double)20.5, (double)20.6, (double)20.7}};

          dp.putSingleData(1L, new long[] {0}, new Double("18888.8"));
          dp.putSingleData(1L, new long[] {2}, new Double("19999.9"));

          System.out.println("\tAdded a single dp data.");

          dp.putHyperData(5L, 3L, 1L,
                          new long[] {0}, new long[] {3}, new long[] {1},
                          dpData);

          System.out.println("\tAdded hyperput dp data.");

          /*****************/
          /*  Add ep data  */
          /*****************/
          double epData  = Epoch.compute(1999, 3, 5, 5, 6, 7, 100),
                 epData1 = Epoch.compute(1998, 1, 2, 3, 4, 5, 666);
          String e0 = Epoch.encode(epData1);
          double p0 = Epoch.parse(e0);

          ep.putSingleData(0L, new long[] {0}, new Double(epData)); 
          ep.putSingleData(1L, new long[] {0}, new Double(p0)); 

          System.out.println("\tAdded putSingleData ep data.");

          /*******************/
          /*  Add ep16 data  */
          /*******************/
          ep_status  = Epoch16.compute(2004, 11, 29,     /* yyyy-mm-dd */
                                       15, 55, 23,       /* hh-mm-ss   */
                                       30,               /* millisecond */
                                       411,              /* microsecond */
                                       522,              /* nanosecond  */
                                       634,              /* picosecond  */
                                       ep16Data);        /* CDF_EPOCH16 data */
          ep16.putSingleData(0L, new long[] {0}, ep16Data);

          ep_status  = Epoch16.compute(2004, 12, 29,     /* yyyy-mm-dd */
                                       16, 56, 24,       /* hh-mm-ss   */
                                       31,               /* millisecond */
                                       411,              /* microsecond */
                                       522,              /* nanosecond  */
                                       634,              /* picosecond  */
                                       ep16Data);        /* CDF_EPOCH16 data */
          ep16.putSingleData(1L, new long[] {0}, ep16Data);

          ep_status  = Epoch16.compute(2005, 12, 29,     /* yyyy-mm-dd */
                                       16, 56, 24,       /* hh-mm-ss   */
                                       31,               /* millisecond */
                                       444,              /* microsecond */
                                       555,              /* nanosecond  */
                                       777,              /* picosecond  */
                                       ep16Data);        /* CDF_EPOCH16 data */
          ep16.putSingleData(2L, new long[] {0}, ep16Data);

          System.out.println("\tAdded putSingleData ep16 data.");

          /********************/
          /*  Add newI8 data  */
          /********************/
          
          long [][] i8Data = {{1L,  -1L},
                              {10L, -10L},
                              {Long.MAX_VALUE, Long.MIN_VALUE}};

          newI8.putSingleData(0L, new long[] {0}, new Long(88888L));
          newI8.putSingleData(0L, new long[] {1}, new Long(99999L));

          System.out.println("\tAdded a single newI8 data.");

          newI8.putHyperData(1L, 3L, 1L,
                             new long[] {0}, new long[] {2}, new long[] {1},
                             i8Data);

          System.out.println("\tAdded hyperput newI8 data.");

          /*********************/
          /*  Add tt2000 data  */
          /*********************/
          
          String dateTime = new String("2015-06-30T23:59:58.123456789");
          long [] nanosecs = new long[5];
          long nansecSinceJ2000;
          try {
            nansecSinceJ2000 = CDFTT2000.fromUTCstring(dateTime);
          } catch (CDFException ex) {
            nansecSinceJ2000 = 0;
          }
          int ix;
          for (ix = 0; ix < 5; ++ix) {
             nanosecs[ix] = nansecSinceJ2000 + (ix + 1) * 1000000000L;
          }

          tt2000.putSingleData(0L, new long[] {0}, new Long(nansecSinceJ2000));

          System.out.println("\tAdded a single tt2000  data.");

          tt2000.putHyperData(1L, 5L, 1L,
                              new long[] {0}, new long[] {1}, new long[] {1},
                              nanosecs);

          System.out.println("\tAdded hyperput tt2000 data.");

          /************************************************************/
          /* Initiate a record write for a group of CDF variables.    */
          /* Six (6) variables are involved in this data put function */
          /* and the 6th record for each variable is written.         */
	  /* Note: All data elements in each variable's record object */
	  /*       should be fully provided. Otherwise, expect to have*/
	  /*       bad data at the missing element(s) in the record.  */ 
          /************************************************************/

	  String[] strVars = {"Longitude1", // variable names in CDF
	                      "Temp1",
		              "dp",
		              "Temp",
			      "Temperature"};
	  //  long[] numVars = { 3, 9, 13, 15, 8, 10}; // variable IDs

	 /* Prepare the record objects, every object holds full      */
	 /* element data for each individual variable.               */
         /* Longitude1 -- data type: CDF_UINT2 dimensionality: 1:[3] */
	 int[] longitude1_data = {333, 444, 555}; 

         /* Temp1 -- data type: CDF_REAL4 dimensionality: 1:[3]      */
	 float[] temp1_data = {(float)333.3, (float)444.4, (float)555.5};

         /* dp -- data type: CDF_DOUBLE dimensionality: 1:[3]        */
	 double[] dp_data = {333.333, 444.444, 555.555};

         /* Temp -- data type: CDF_FLOAT dimensionality: 1:[3]       */
	 float[] temp_data = {(float)666.66, (float)777.77, (float)888.88};

         /* Temperature -- data type: CDF_FLOAT dimensionality: 0:[] */
	 Float temperature_data = new Float((float)999.99);

	 /* record, the Vector holding the record objects   */
	 /* for writing to the CDF.                         */

	 Vector record = new Vector();
	 record.add(longitude1_data);
	 record.add(temp1_data);
	 record.add(dp_data);
	 record.add(temp_data);
	 record.add(temperature_data);

	 cdf.putRecord(5L, strVars, record);  // Write a record to record #6

          System.out.println("\tAdded a record data (Logitude1, Temp1, "+
                             "dp, Temp, Temperature).");

          /****************************************/
          /*  Rename a variable and an attribute  */
          /****************************************/
          dvar.rename("foo");
          System.out.println("Renamed a variable."); 

          validMin.rename("validmin");
          System.out.println("Renamed an attribute.");

          /**************************************/
          /*  Get the attribute name and scope  */
          /**************************************/
          String scope;
          if (project.getScope() == GLOBAL_SCOPE)
              scope = "global";
          else
              scope = "variable";
          System.out.println ("Attribute 'project': \n"+
                              "\tname: "+project.getName()+
                              "\n\tscope: "+scope); 

          /************************************************/
          /*  Copy a variable - this only copies metadata */
          /************************************************/

          // The current CDF file MUST be saved first (by calling the save()
          // method) before 'copying/duplicating data records' operation is 
          // performed.  Otherwise the program will either fail or produce
          // undesired results.
          cdf.save();

          longitude.copy("longitude_copy");
          System.out.println("Copied a variable.");

          // Get the variable just copied and set its record variance.
          Variable long_copy;
          long_copy = cdf.getVariable("longitude_copy");
          // long_copy.setRecVariance(NOVARY);

          // Copy the 'longitude' variable to 'longitude_copy' and put
          // 'longitude_copy" into test1.cdf.

          longitude.copy(cdf1,"longitude_copy");   
          System.out.println("Copied a variable into another CDF.");

          longitude.copyDataRecords(long_copy);
          longitude.concatenateDataRecords(long_copy);

          /***************************************************************/ 
          /*  Duplicate a variable                                       */
          /*     - copies everything including metadata, data, and       */
          /*       other settings such as blocking factor, compression,  */
          /*       sparseness, and pad value                             */
          /***************************************************************/ 
          cdf.save();

          longitude.duplicate("longitude_dup"); 
          longitude.duplicate(cdf1, "longitude_dup");  

          /***********************/
          /*  Delete a variable  */
          /***********************/
          dummyVar.delete();
          System.out.println("Deleted a variable.");

          /****************************************/ 
          /*  Delete a variable and global entry  */
          /****************************************/ 
          // validMin.deleteEntry(latitude);
          System.out.println("Deleted a variable entry.");

          long entryID = 1;
          test.deleteEntry(entryID);  
          System.out.println("Deleted a global entry.");

          /**********************************************************/
          /*  For variables with sparse and/or compressed records,  */
          /*  the CDF file must be saved first before records can   */
          /*  be properly deleted.                                  */
          /*                                                        */
          /*  NOTE: It's always safe to save a CDF file, before     */
          /*        deleting any variable records.                  */
          /**********************************************************/
          cdf.save();

          /**********************/ 
          /*  Delete record(s)  */ 
          /**********************/ 
          firstRec = 1;
          lastRec  = 2;
          time.deleteRecords(firstRec, lastRec); 

          /************************************/
          /*  This should throw an exception  */
          /************************************/
          // Attribute badAttribute;
          // badAttribute = Attribute.create(cdf, "Project", GLOBAL_SCOPE); 
          cdf.close();
          cdf1.close();
          System.out.println("** Tested successfully **");

        } catch (Exception e) {
            System.out.println("A bad thing happened on the way to the CDF.");
            e.printStackTrace();
        }
    }
}
