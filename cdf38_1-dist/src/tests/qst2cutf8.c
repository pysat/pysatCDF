#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "cdf.h"

#if defined(vms)
#include <ssdef>
#define EXIT_SUCCESS_   SS$_NORMAL
#define EXIT_FAILURE_   SS$_ABORT
#else
#define EXIT_SUCCESS_   0
#define EXIT_FAILURE_   1
#endif

void QUIT_CDF (CDFstatus, char *);

char *spdf1o = NULL, *spdf2o = NULL, *spdf3o = NULL;

int main(int argn, char *argv[]) {		
      CDFstatus status;
      CDFid id;
      long numElems, attr_num, dimsizes[] = {0L}, len1, len2, len3;
      int i;

      char *spdf1 = "ASCII: ABCDEFG";
      char *spdf2 = "Latin1: ©æêü÷Æ¼®¢¥";
      char *spdf3 = "Chinese: 社安";
      /* Same UTF8 encoding strings in hex form */
/*    char *spdf1 = "\x41\x53\x43\x49\x49\x3a\x20\x41\x42\x43\x44\x45\x46\x47";  */
/*    char *spdf2 = "\x4c\x61\x74\x69\x6e\x31\x3a\x20\xc2\xa9\xc3\xa6\xc3\xaa\xc3\xbc\xc3\xb7\xc3\x86\xc2\xbc\xc2\xae\xc2\xa2\xc2\xa5"; */
/*    char *spdf3 = "\x43\x68\x69\x6e\x65\x73\x65\x3a\x20\xe7\xa4\xbe\xe5\xae\x89"; */

      printf("Testing UTF8/C...\n");
      status = CDFlib(CREATE_, CDF_, (argn==2?argv[1]:"TUTF8"), 0L, dimsizes,
                                     &id, 
      		      NULL_);
      if (status < CDF_OK) {
        if (status == CDF_EXISTS) {
          status = CDFlib(OPEN_, CDF_, (argn==2?argv[1]:"TUTF8"), &id,
                          NULL_);
          if (status < CDF_OK) QUIT_CDF (status, "1.0");

          status = CDFlib(DELETE_, CDF_,
                          NULL_);
          if (status < CDF_OK) QUIT_CDF (status, "1.1");

          status = CDFlib(CREATE_, CDF_, (argn==2?argv[1]:"TUTF8"), 0,
                                         dimsizes, &id,
                          NULL_);
          if (status < CDF_OK) QUIT_CDF (status, "1.2");
        } else
          QUIT_CDF (status, "1.3");
      }

      status = CDFlib(CREATE_, ATTR_, "UTF8", GLOBAL_SCOPE, &attr_num,
      		      NULL_);
      if (status < CDF_OK) QUIT_CDF (status, "12.0");

      len1 = (long)strlen(spdf1);
      len2 = (long)strlen(spdf2);
      len3 = (long)strlen(spdf3);

      spdf1o = malloc ((size_t)len1 + 1);
      spdf2o = malloc ((size_t)len2 + 1);
      spdf3o = malloc ((size_t)len3 + 1);

      printf(" spdf1=\"%s\" length:%d byte_len=%ld\n", spdf1,
                                         UTF8StrLength(spdf1, len1), len1);
      printf("   Hex: ");
      for (i = 0; i < len1; ++i)
        printf("%2X", *(((unsigned char *)spdf1)+i));
      printf("\n");
      printf(" spdf2=\"%s\" length:%d byte_len=%ld\n", spdf2, 
                                        UTF8StrLength(spdf2, len2), len2);
      printf("   Hex: ");
      for (i = 0; i < len2; ++i)
        printf("%2X", *(((unsigned char *)spdf2)+i));
      printf("\n");
      printf(" spdf3=\"%s\" length:%d byte_len=%ld\n", spdf3, 
                                        UTF8StrLength(spdf3, len3), len3);
      printf("   Hex: ");
      for (i = 0; i < len3; ++i)
        printf("%2X", *(((unsigned char *)spdf3)+i));
      printf("\n");

      status = CDFlib(SELECT_, gENTRY_, 0L,
      		      PUT_, gENTRY_DATA_, CDF_CHAR, len1, spdf1,
      		      NULL_);
      if (status < CDF_OK) QUIT_CDF (status, "13.0");

      status = CDFlib(SELECT_, gENTRY_, 1L,
      		      PUT_, gENTRY_DATA_, CDF_CHAR, len2, spdf2,
      		      NULL_);
      if (status < CDF_OK) QUIT_CDF (status, "14.0");
 
      status = CDFlib(SELECT_, gENTRY_, 2L,
      		      PUT_, gENTRY_DATA_, CDF_CHAR, len3, spdf3,
      		      NULL_);
      if (status < CDF_OK) QUIT_CDF (status, "15.0");
 
      status = CDFlib(CLOSE_, CDF_,
      		      NULL_);
      if (status < CDF_OK) QUIT_CDF (status, "16.0");

      status = CDFlib(OPEN_, CDF_, (argn==2?argv[1]:"TUTF8"), &id, 
      		      NULL_);
      if (status < CDF_OK) QUIT_CDF (status, "1.5");

      status = CDFlib(SELECT_, ATTR_NAME_, "UTF8",
      		      NULL_);
      if (status < CDF_OK) QUIT_CDF (status, "17.0");

      status = CDFlib(SELECT_, gENTRY_, 0L,
      		      GET_, gENTRY_NUMELEMS_, &numElems,
      		            gENTRY_DATA_, spdf1o,
      		      NULL_);
      if (status < CDF_OK) QUIT_CDF (status, "18.0");
      *(spdf1o+(int)numElems) = '\0';

      status = CDFlib(SELECT_, gENTRY_, 1L,
      		      GET_, gENTRY_NUMELEMS_, &numElems,
      		            gENTRY_DATA_, spdf2o,
      		      NULL_);
      if (status < CDF_OK) QUIT_CDF (status, "19.0");
      *(spdf2o+(int)numElems) = '\0';

      status = CDFlib(SELECT_, gENTRY_, 2L,
      		      GET_, gENTRY_NUMELEMS_, &numElems,
      		            gENTRY_DATA_, spdf3o,
      		      NULL_);
      if (status < CDF_OK) QUIT_CDF (status, "20.0");
      *(spdf3o+(int)numElems) = '\0';

      if (strcmp (spdf1, spdf1o) != 0) QUIT_CDF(status, "21.0");
      if (strcmp (spdf2, spdf2o) != 0) QUIT_CDF(status, "22.0");
      if (strcmp (spdf3, spdf3o) != 0) QUIT_CDF(status, "23.0");

      status = CDFlib(CLOSE_, CDF_,
      		      NULL_);
      if (status < CDF_OK) QUIT_CDF (status, "21.0");
      exit (0);
}

/*----------------------------------------------------------------------
* QUIT_CDF.  Abort test early due to CDF error.
*---------------------------------------------------------------------*/

	void QUIT_CDF (CDFstatus status, char *where) {
	  char text[CDF_ERRTEXT_LEN];
    	  printf ("Aborting at %s...", where);
	  if (status < 0) {
	    status = CDFlib (SELECT_, CDF_STATUS_, status,
                           GET_, STATUS_TEXT_, text,
                           NULL_);
    	    printf (" ERROR: %s\n", text);
	  }
    	  printf("...test aborted");
          if (spdf1o != NULL) free (spdf1o);
          if (spdf2o != NULL) free (spdf2o);
          if (spdf3o != NULL) free (spdf3o);
	  exit (EXIT_FAILURE_);
	}
