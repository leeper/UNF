/*
*  unf.C
*
* Universal numeric fingerprints. Computes a fingerprint of a vector of
* observation at a specified level of numerical precision.
*
* Part of the Accuracy package. Available from www.r-project.org and
* www.hmdc.harvard.edu/numerical_issues/
*
*    Copyright (C) 2004  Micah Altman
*
*    This program is free software; you can redistribute it and/or modify
*    it under the terms of the GNU General Public License as published by
*    the Free Software Foundation; either version 2 of the License, or
*    (at your option) any later version.
*
*    This program is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*    GNU General Public License for more details.
*
*    You should have received a copy of the GNU General Public License
*    along with this program; if not, write to the Free Software
*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/



#include "unf.h"


/*
 * TYPEDEFS 
 */

typedef unsigned char cbyte;

/*
 * PROTOTYPES FOR INTERNAL FUNCTIONS
 */

int selftest (int);
int check_little_endian(void);
uint64_t ntoh64(uint64_t);
uint64_t hton64(uint64_t);
long int myhtonl (long int) ;
long int myntohl (long int);
#ifndef PEDANTIC 
char *Genround(long double , int );
char *Genround(long long, int );
#endif
// Running with --pedantic C++
#ifndef PEDANTIC 
char *Genround(long double , int );
char *Genround(long long, int );
#endif
char *Genround(char*, int );
char *Genround(short int , int );
char *Genround(int , int ) ;
char *Genround(long, int );
char *Genround(double , int );
char *Genround(float , int ) ;

uint64_t Checksum_bytes(uint64_t previous , cbyte* sequence, int len);
uint64_t CRC64(uint64_t , cbyte* , int );
char* Canonicalize_unicode(const char*, const char*, char*, int*);


/*
 * GLOBALS
 */

int static PASSED_INIT = UNF_init(1);
int static IS_LITTLE_ENDIAN= check_little_endian();


/*
 * Universal Numeric Fingerprint routines
 *
 * This is mathematically a composition of the
 * fingerpring, canonicalization and rounding method:
 *
 * FingerPrint(Cannoicalization(Round(value,digits)))
 *
 * Algorithmically, each UNF version performs the following steps
 * 
 * - Round value to n digits using Genround()
 * - Convert value to canonical form using Canonicalize_unicode()
 * - Update checksum or cryptographic hash structure
 *
 * Parameters
 * 
 * n - value to be fingerprinted
 * digits - precison of calculation
 * previous - hash structure from previous iteration
 * miss - flag: is this is a missing value?
 *
 */

uint64_t UNF1 (UNFldouble n, int digits, uint64_t previous, int miss) {
	#ifdef FORCELOCALE
	standard_locale(0);	
	#endif 

	char *tmps, *tmpu=NULL;
	int bytes_converted;
	uint64_t r;
	
	if (miss) {
		tmpu = Canonicalize_unicode("ASCII", "UTF-32BE", "miss", &bytes_converted);
	} else {
		tmps = Genround(n, digits);
		if (tmps == NULL) {
			return (0);
		}
		tmpu = Canonicalize_unicode("ASCII", "UTF-32BE", tmps, &bytes_converted);
		free (tmps);
	} 

	if (tmpu == NULL) {
		return (0);
	}

	r = Checksum_bytes(previous , (cbyte*) tmpu, bytes_converted);
	free(tmpu);
	#ifdef FORCELOCALE
	standard_locale(1);	
	#endif 
	return(r);
}

uint64_t UNF1 (char *n, int digits, uint64_t previous, int miss) {
	#ifdef FORCELOCALE
	standard_locale(0);	
	#endif
	char *tmps, *tmpu;
	int bytes_converted;
	uint64_t r;
	
	if (miss) {
		tmpu = Canonicalize_unicode("ASCII","UTF-32BE", "miss", &bytes_converted);
	} else {
		tmps = Genround(n, digits);
		if (tmps == NULL) {
			return (0);
		}
		tmpu = Canonicalize_unicode("ASCII", "UTF-32BE", tmps, &bytes_converted);
		free (tmps);
	} 

	if (tmpu == NULL) {
		return (0);
	}

	r = Checksum_bytes(previous , (cbyte*) tmpu, bytes_converted);
	free(tmpu);
	#ifdef FORCELOCALE
	standard_locale(1);	
	#endif 
	return(r);
}

uint64_t UNF2 (UNFldouble n, int digits, uint64_t previous, int miss) {
	#ifdef FORCELOCALE
	standard_locale(0);	
	#endif
	char *tmps, *tmpu=NULL;
	int bytes_converted;
	const char *missv="\0\0\0"; int missl=3;
	uint64_t r;
	
	if (!miss) {
		tmps = Genround(n, digits);
		if (tmps == NULL) {
			return (0);
		}
		tmpu = Canonicalize_unicode("ASCII", "UTF-32BE", tmps, &bytes_converted);
		free (tmps);
		if (tmpu == NULL) {
			return (0);
		}
	} 

	// we use bytes converted +1 to include the null terminator in the
	// checksum, which ensures each entry is clearly separated from the next
	if (miss) {
		r = CRC64(previous , (cbyte*) missv, missl);
	} else {
		r = CRC64(previous , (cbyte*) tmpu, bytes_converted+1);
		free(tmpu);
	}
	#ifdef FORCELOCALE
	standard_locale(1);	
	#endif 
	return(r);
}

uint64_t UNF2 (char *n, int digits, uint64_t previous, int miss) {
	#ifdef FORCELOCALE
	standard_locale(0);	
	#endif
	char *tmps, *tmpu=NULL;
	int bytes_converted;
	const char *missv="\0\0\0"; int missl=3;
	uint64_t r;
	
	if (!miss) {
		tmps = Genround(n, digits);
		if (tmps == NULL) {
			return (0);
		}
		tmpu = Canonicalize_unicode("ASCII", "UTF-32BE", tmps, &bytes_converted);
		free (tmps);
		if (tmpu == NULL) {
			return (0);
		}
	} 

	// we use bytes converted +1 to include the null terminator in the
	// checksum, which ensures each entry is clearly separated from the next
	if (miss) {
		r = CRC64(previous , (cbyte*) missv, missl);
	} else {
		r = CRC64(previous , (cbyte*) tmpu, bytes_converted+1);
		free(tmpu);
	}
	#ifdef FORCELOCALE
	standard_locale(1);	
	#endif 
	return(r);
}

int UNF3 (char *n, int digits, md5_state_t *previous, int miss) {
	#ifdef FORCELOCALE
	standard_locale(0);	
	#endif
	char *tmps, *tmpu=NULL;
	const char *missv="\0\0\0"; int missl=3;
	int bytes_converted;
	
	if (!miss) {
		tmps = Genround(n, digits);
		#ifdef DEBUG
		fprintf (stderr,"%s\n",tmps);
		#endif 

		if (tmps == NULL) {
			return (0);
		}
		tmpu = Canonicalize_unicode("ASCII", "UTF-32BE", tmps, &bytes_converted);
		free (tmps);
		if (tmpu == NULL) {
			return (0);
		}
	} 

	if (miss) {
		#ifdef DEBUG
		fprintf (stderr,"miss\n");
		#endif
		md5_append(previous, (const md5_byte_t *) missv, missl);
	} else {
		// we use bytes converted +1 to include the null terminator in the
		// checksum, which ensures each entry is clearly separated from the next
		md5_append(previous, (const md5_byte_t *) tmpu, bytes_converted+1);
		free(tmpu);
	}
	#ifdef FORCELOCALE
	standard_locale(1);	
	#endif 
	return(1);
}

int UNF3 (UNFldouble n, int digits, md5_state_t *previous, int miss) {
	#ifdef FORCELOCALE
	standard_locale(0);	
	#endif
	char *tmps, *tmpu=NULL;
	const char *missv="\0\0\0"; int missl=3;
	int bytes_converted;
	
	if (!miss) {
		tmps = Genround(n, digits);
		#ifdef DEBUG
		fprintf (stderr,"After Genround:%s:\n",tmps);
		#endif
		if (tmps == NULL) {
			return (0);
		}
		tmpu = Canonicalize_unicode("ASCII", "UTF-32BE", tmps, &bytes_converted);
		#ifdef DEBUG
		{ int i;
		  fprintf (stderr, "UNICODE BYTES (%d): ", bytes_converted);
		  for (i =0; i<bytes_converted; i++) {
			fprintf(stderr, "%d,", (int) tmpu[i]);
		  }
		  fprintf (stderr, "\n");
		}
		#endif
		free (tmps);
		if (tmpu == NULL) {
			return (0);
		}
	} 

	if (miss) {
		#ifdef DEBUG
		fprintf (stderr,"miss\n");
		#endif
		md5_append(previous, (const md5_byte_t *) missv, missl);
	} else {
		// we use bytes converted +1 to include the null terminator in the
		// checksum, which ensures each entry is clearly separated from the next
		md5_append(previous, (const md5_byte_t *) tmpu, bytes_converted+1);
		free(tmpu);
	}
	#ifdef FORCELOCALE
	standard_locale(1);	
	#endif 
	return(1);
}

int UNF4int (char *n, int digits, sha256_context *previous, int miss, const char *dchars) {
	#ifdef FORCELOCALE
	standard_locale(0);	
	#endif
	char *tmps, *tmpu=NULL;
	const char *missv="\0\0\0"; int missl=3;
	int bytes_converted;
	
	if (!miss) {
		tmps = Genround(n, digits);
		#ifdef DEBUG
		fprintf (stderr,"%s\n",tmps);
		#endif 

		if (tmps == NULL) {
			return (0);
		}
		tmpu = Canonicalize_unicode("ASCII", dchars, tmps, &bytes_converted);
		free (tmps);
		if (tmpu == NULL) {
			return (0);
		}
	} 

	if (miss) {
		#ifdef DEBUG
		fprintf (stderr,"miss\n");
		#endif
		sha256_update(previous, (uint8 *) missv, (uint32) missl);
	} else {
		// we use bytes converted +1 to include the null terminator in the
		// checksum, which ensures each entry is clearly separated from the next
		sha256_update(previous, (uint8 *) tmpu, (uint32) bytes_converted+1);
		free(tmpu);
	}
	#ifdef FORCELOCALE
	standard_locale(1);	
	#endif 
	return(1);
}

int UNF4int (UNFldouble n, int digits, sha256_context *previous, int miss, const char *dchars) {
	#ifdef FORCELOCALE
	standard_locale(0);	
	#endif
	char *tmps, *tmpu=NULL;
	const char *missv="\0\0\0"; int missl=3;
	int bytes_converted;
	
	if (!miss) {
		tmps = Genround(n, digits);
		#ifdef DEBUG
		fprintf (stderr,"After Genround:%s:\n",tmps);
		#endif
		if (tmps == NULL) {
			return (0);
		}
		tmpu = Canonicalize_unicode("ASCII", dchars,tmps, &bytes_converted);
		#ifdef DEBUG
		{ int i;
		  fprintf (stderr, "UNICODE BYTES (%d): ", bytes_converted);
		  for (i =0; i<bytes_converted; i++) {
			fprintf(stderr, "%d,", (int) tmpu[i]);
		  }
		  fprintf (stderr, "\n");
		}
		#endif
		free (tmps);
		if (tmpu == NULL) {
			return (0);
		}
	} 

	if (miss) {
		#ifdef DEBUG
		fprintf (stderr,"miss\n");
		#endif
		sha256_update(previous, (uint8 *) missv, (uint32) missl);
	} else {
		// we use bytes converted +1 to include the null terminator in the
		// checksum, which ensures each entry is clearly separated from the next
		sha256_update(previous, (uint8 *) tmpu, (uint32) bytes_converted+1);
		free(tmpu);
	}
	#ifdef FORCELOCALE
	standard_locale(0);	
	#endif 
	return(1);
}

int UNF4(char *n, int digits, sha256_context *previous, int miss) {
	return(UNF4int(n,digits,previous,miss,"UTF-32BE"));
}
int UNF4(UNFldouble n, int digits, sha256_context *previous, int miss) {
	return(UNF4int(n,digits,previous,miss,"UTF-32BE"));
}
int UNF4_1(char *n, int digits, sha256_context *previous, int miss) {
	return(UNF4int(n,digits,previous,miss,"UTF-8"));
}
int UNF4_1(UNFldouble n, int digits, sha256_context *previous, int miss) {
	return(UNF4int(n,digits,previous,miss,"UTF-8"));
}



/*
 * Generalized Rounding Routines
 
 * Genround()
 *
 * args:
 * 	n - number
 * 	digits - significant digits to be rounded to
 *
 * returns:
 * 	Char string representing properly rounded value.
 *  	This string must be free()'d.	
 *
 * Note: requires stdio.h , locale.h, string.h 
 *
 * Assumes Init routine has been called to set locale.
 */

char *Genround(UNFldouble n, int digits ) {

	#ifdef DEBUG
	#ifdef PEDANTIC
	fprintf(stderr,"Entering genround %+#.*e\n", 15,  n);
	#else
	fprintf(stderr,"Entering genround %+#.*Le\n", 15,  n);
	#endif
	#endif 
	// STEP 1: non-finite numbers
	char *buf = (char*) malloc(digits+20) ;
 
  // Caution finite, isnan, isinf, redefined if VCPP is active
  if (!finite(n)){
    if (isnan(n)!=0) {
        sprintf(buf,"+nan\n");
    } else if (isinf(n)==-1) {
        sprintf(buf,"-inf\n");
    } else {
        sprintf(buf,"+inf\n");
    }
    return(buf);
  }
 
	
	// STEP 2: finite numbers
	char *buf2 = (char*) malloc(digits+20) ;

	#ifdef PEDANTIC
	   #ifdef INACCURATE_SPRINTF
	   sprintf(buf,"%+#.*e\n", 14,  n);
	   #ifdef DEBUG
	   fprintf(stderr,"In genround, buf %s\n",buf);
	   #endif 
	   #else 
	   sprintf(buf,"%+#.*e\n", digits-1,  n);
	   #ifdef DEBUG
	   fprintf(stderr,"In genround, buf %s\n",buf);
	   #endif 
	   #endif
	#else
	  #ifdef INACCURATE_SPRINTF
	  sprintf(buf,"%+#.*Le\n", 14,  n);
	   #ifdef DEBUG
	   fprintf(stderr,"In genround, buf %s\n",buf);
	   #endif 
	  #else 
	  sprintf(buf,"%+#.*Le\n", digits-1,  n);
	   #ifdef DEBUG
	   fprintf(stderr,"In genround, buf %s\n",buf);
	   #endif 
	  #endif
	#endif

	// Canonical form is 
	//		- leading + or -
	//		- leading digit
	//		- decimal point
	//		- up to digit-1 digits, no trailing zeros
	//		- 'e' (lower-case e)
	//		- '+' or '-'
	//		- exponent digits, with no leading zeros
	// E.g: +1.2e+1 -2.e+  +1.362e-17 +0.e+
	//remove trailing zeros in the mantissa  (can't just use %+$.*Lg )
	//remove leading zeros in the exponent
	int i,j, mantissa_end, exponent_begin;
	mantissa_end =(2+digits-1); 
	while(buf[mantissa_end]=='0' && (mantissa_end >2)) {
		mantissa_end--;
	}
	#ifdef INACCURATE_SPRINTF
	// sprintf is inaccurate, print an extra digit of 
	// precision then truncate it
	exponent_begin = 19;
	#else
	exponent_begin = 4+digits;
	#endif
	while (buf[exponent_begin]=='0') {
		exponent_begin++;
	}

	for (i=0; i<= mantissa_end; i++) {
		buf2[i]=buf[i];
	}
	buf2[i]='e';
	#ifdef INACCURATE_SPRINTF
	buf2[i+1]=buf[18];
	#else
	buf2[i+1]=buf[digits+3];
	#endif
	j=i+2;		
	for (i=exponent_begin; buf[i]!=0; i++) {
		buf2[j]=buf[i];
		j++;
	}
	buf2[j]='\0';
	free(buf);

	return(buf2);
}


char *Genround(char *n, int digits) {
	char *buf = (char*) malloc(digits+20) ;

	sprintf(buf,"%.*s\n", digits,  n);
	return(buf);
}

/* these are simply wrappers around long double versions */

#ifndef PEDANTIC
char *Genround(long long n, int digits ) {
	return(Genround((UNFldouble) n, digits));
}

char *Genround(double n, int digits ) {
	return(Genround((UNFldouble) n, digits));
}
#endif

char *Genround(short int n, int digits ) {
	return(Genround((UNFldouble) n, digits));
}

char *Genround(int n, int digits ) {
	return(Genround((UNFldouble) n, digits));
}

char *Genround(long n, int digits ) {
	return(Genround((UNFldouble) n, digits));
}

char *Genround(float n, int digits ) {
	return(Genround((UNFldouble) n, digits));
}


/*
 * Fingerprinting methods
 *
 * These take a stream of bytes and return a fingerprint for the stream,
 *
 * Algorithms to do this include (in fastest but least robust to slowest but
 * most robust order):
 *
 * 	Version 1 - checksums		( better than nothing, but not completely 
 * 						robust to accidental modification ) 
 * 	Version 2 - cyclic-redundancy-checks 	( almost completely robust
 * 					 to most accidental modification, but not to 
 * 					  intentional tampering )
 * 	Version 3,4 - message-digest algorithms	( robust to intentional tampering )
 *		- MD5
 *		- SHA256
 */

uint64_t Checksum_bytes(uint64_t previous , cbyte* sequence, int len) {
   int i;
   uint64_t r = previous;
   if (len<0) {
	   return(previous);
   }
   for ( i=0; i<len; i++) { 
	  r += (unsigned short int) sequence[i];
   }	    
   return(r);
}


uint64_t CRC64(uint64_t previous , cbyte* sequence, int len){

	// Based on SPcrc, a C implementation by Christian Iseli

  #define AUTODIN_IIREV	0xEDB88320
  #define POLY64REV	0xd800000000000000ULL
  static uint64_t CRCTable[256];
  uint64_t crc = ntoh64(previous);
  static int init = 0;
  int i;

  if (!init) {
    init = 1;
    for (i = 0; i <= 255; i++) {
      int j;
      uint64_t part = i;
      for (j = 0; j < 8; j++) {
        if (part & 1)
          part = (part >> 1) ^ POLY64REV;
        else
          part >>= 1;
      }
      CRCTable[i] = part;
    }
  }

  for (i = 0; i < len; i++) {
    uint64_t temp1 = crc >> 8;
    uint64_t temp2 = CRCTable[(crc ^ (uint64_t) sequence[i]) & 0xff];
    crc = temp1 ^ temp2;
  }

  return(hton64(crc));
  #undef AUTODIN_IIREV
  #undef POLY64REV
}
  

/* 
 * Convert string to canonical form
 *
 * Use of iconv based on example code from GCC manual.
 *
 */

char* Canonicalize_unicode(const char *charset, const char *dcharset, char *inbuf, int *bytes_converted)
{
  char *wrptr, *inptr, *outbuf;
  size_t nconv,insize;
  iconv_t cd;

  int bufsize = (strlen(inbuf)+1)* 8;
  size_t avail = bufsize;

  outbuf = (char *) calloc(bufsize, 1); 
  if (outbuf==NULL) {
      perror("calloc");
      *bytes_converted=-1;
      return(NULL);
  }

  /* set pointers to beginning of buffers */
  wrptr=outbuf;
  inptr=inbuf;

  cd = iconv_open ( dcharset, charset); if (cd == (iconv_t) -1) {
      if (errno == EINVAL)
        fprintf(stderr, "conversion from '%s' to %s  not available",
               charset, dcharset);
      else
        perror ("iconv_open");

      *bytes_converted=-1;
      free(outbuf);
      return(NULL);
   }

   /* Now write out the byte sequence to get into the
             initial state if this is necessary.  */
   iconv (cd, NULL, NULL, &wrptr, &avail);

   /* Do the conversion.  */
   insize = strlen(inbuf);
   #ifdef ICONV18
   nconv = iconv (cd, (const char **) &inptr, &insize, &wrptr, &avail);
   #else
   nconv = iconv (cd, &inptr, &insize, &wrptr, &avail);
   #endif 
   if (nconv == (size_t) -1) {
	    if (errno != E2BIG) {
            	perror ("iconv_open");
      		*bytes_converted=-1;
      		free(outbuf);
      		return(NULL);
	    }
   }

   /* Terminate the output string.  */
    *wrptr = '\0'; 

   if (iconv_close (cd) != 0)
    	perror ("iconv_close");

   *bytes_converted =  wrptr - outbuf;
   return(outbuf);
}

/*
 * Utility Routines
 */

/*
 * Init_check
 *
 * - Checks that assumptions about bit and byte length have been met.
 * - Initializes globale values and environment 
 */

int UNF_init (int quiet) {

   	int mantissa_bits= 1, bitsperbyte = 0, retval=0;
        UNFldouble x1 = 1.0, delta = 0.5, x2 = x1 + delta;

	unsigned char t=1;
	while (t!=0 && bitsperbyte< 1024) {
        	bitsperbyte++;
        	t=t<<1;
	}
	// Canonicalization of floats and ints may be different with > 8 bits
	// -- test byte size
	if (bitsperbyte != 8) {
	   if (quiet !=0) {
	   	fprintf(stderr, "Warning: math check failed, not 8 bits per byte \n");;
	   }
	   retval = -1;
	}

	// test mantissa size of long double
        while ( x1!=x2 ) {
		   delta *= 0.5;
		   x2 = x1 + delta;
		   mantissa_bits++;
        }

	if (mantissa_bits <  bitsperbyte*(signed int) sizeof(UNFllong)) {
		retval = -1;
		if (quiet!=0) {
			fprintf(stderr, "Warning: UNFllong's may lose precision when "
						"converted to UNFldoubles \n");
			fprintf(stderr, "UNFldouble mantissa %d \n", mantissa_bits);
			fprintf(stderr, "sizeof UNFllong unsigned int %d \n", 
					(signed int) sizeof (UNFllong));
		}
	}


	IS_LITTLE_ENDIAN = check_little_endian();
	return(retval);
}

/*
*
* check_little_endian
*
* Is the system little endian?
*
*/

int check_little_endian (void) {
	union {
		long l;
	        char c[sizeof (long)];
	} u;
	u.l = 1;
	return (u.c[sizeof (long) - 1] == 1);
}


/*
*
* ntoh64, hton64
*
* convert 64 bit integers to standard network byte order
*
* These are simply 64 bit versions of ntol and lton
*
*/

uint64_t ntoh64(uint64_t n) {
	/* Derived from GNUnet, by Christian Grothoff, et. al */
	if (IS_LITTLE_ENDIAN) {
		return (((uint64_t)myntohl( (long int) n) << 32) + myhtonl((long int)(n >> 32)));
	} else {
		return(n);
	}
}

uint64_t hton64 ( uint64_t n) {
	/* Derived from GNUnet, by Christian Grothoff, et. al */

	if (IS_LITTLE_ENDIAN) {
		return (((uint64_t)myhtonl( (long int) n) << 32) + myhtonl((long int)(n >> 32)));
	} else {
		return (n); 
	}
}


/* 
* 
* tobase64
*
* raw bytes in quasi-big-endian order to base 64 string (NUL-terminated) 
*
*/

void tobase64(unsigned char *out, md5_byte_t *in, int inlen)
{
   static const char base64digits[] =
   "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

   /* This code adapted from fetchmail sources by
   * Eric S. Raymond <esr@snark.thyrsus.com>.  */

	for (; inlen >= 3; inlen -= 3) {
		*out++ = base64digits[in[0] >> 2];
		*out++ = base64digits[((in[0] << 4) & 0x30) | (in[1] >> 4)];
		*out++ = base64digits[((in[1] << 2) & 0x3c) | (in[2] >> 6)];
		*out++ = base64digits[in[2] & 0x3f];
		in += 3;
	}
	if (inlen > 0) {
	unsigned char fragment;

	*out++ = base64digits[in[0] >> 2];
	fragment = (in[0] << 4) & 0x30;
	if (inlen > 1)
		fragment |= in[1] >> 4;
		*out++ = base64digits[fragment];
		*out++ = (inlen < 2) ? '=' : base64digits[(in[1] << 2) & 0x3c];
		*out++ = '=';
	}
	*out = '\0';
}

/*
	ntohl routines derived fromlinux srcs
	
	Note: we're not using netinet/in.h because mingw doesn't include it :-(
		This version is slower, but only used in version 2 unfs
		anyway. 
*/

long int myntohl(long int x) {
     if (IS_LITTLE_ENDIAN) {
      x = ((((x) & 0xff000000) >> 24) | (((x) & 0x00ff0000) >>  8) |               \
      (((x) & 0x0000ff00) <<  8) | (((x) & 0x000000ff) << 24));
     }
     return(x);
}

long int myhtonl(long int x) {
     if (IS_LITTLE_ENDIAN) {
     x= ((((x) & 0xff000000) >> 24) | (((x) & 0x00ff0000) >>  8) |               \
      (((x) & 0x0000ff00) <<  8) | (((x) & 0x000000ff) << 24));
     }
     return(x);
}

int myisinf(double x){
  #ifdef VCPP
  if (finite(x))  { return (0); }
  if (isnan(x)) {return(0); }
  if (x>0) {return (1);}
  return(-1);
  #else
  return(isinf(x));
  #endif
}

/*
	Locale setup 
*/

void standard_locale(int restore) {
   static char *oldlocale;
   #ifdef VCPP
   static unsigned int ormode;
   unsigned int curmode;
    int err;
   #else
   static int ormode;
   #endif
   if (restore) {
        setlocale(LC_ALL, oldlocale);
        #ifdef VCPP
        err = _controlfp_s(&curmode, ormode, _MCW_RC);
        #else
        fesetround(ormode);
        #endif
   } else {
        oldlocale = setlocale(LC_ALL, "POSIX");
        if (!oldlocale) {
          oldlocale= setlocale(LC_ALL, "C");
        }
        #ifdef VCPP
        err = _controlfp_s(&ormode, 0, 0);
        err = _controlfp_s(&curmode, _RC_NEAR, _MCW_RC);
        #else
        ormode=fegetround();
        fesetround(FE_TONEAREST);
        #endif
   }
}

