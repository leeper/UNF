/*
 * Runf.C
 * 
 * R wrapper for unf()
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
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include "unf.h"
#include "R.h" /* use -I/path/to/R/includes to find it*/

// extern "C" so that R can see the functions...
extern "C" {

/*
* R_unfX_[double|char]
*
* These are wrapperf sof R to call to generate a particular version of UNF 
* for either a double or a char vector
*
* The procedure is essentially:
*	- standardize locale and rounding mode
*	- initialize hash structure
*	- iterate through items, calling UNFX()
*	- finalize hash
*	- convert results to base64
*
* Parameters
*
* v -- vector to perform UNF over
* nv -- numbver of items in vector
* digits -- digits of precision in the unf
* result -- result as bytes
* result_base64 -- result as base64 string
*
*/



void R_unf1_double(double v[], int *nv, int *digits, double *result,
	char **result_base64 ) {

  standard_locale(0);
       
   int i;
   uint64_t fingerprint=0;

   for (i=0; i < *nv; i++) {
	fingerprint = UNF1(v[i], *digits, fingerprint, ISNA(v[i]));
   }
   *result = (double) fingerprint;
   tobase64((unsigned char *) result_base64[0], (md5_byte_t *) &fingerprint, sizeof(uint64_t));

  standard_locale(1);
}

void R_unf2_double(double v[], int *nv, int *digits, double *result,
	char **result_base64 ) {
   int i;
   uint64_t fingerprint=0;

  standard_locale(0);
   for (i=0; i < *nv; i++) {
	fingerprint = UNF2(v[i], *digits, fingerprint, ISNA(v[i]));
   }
   *result = (double) fingerprint;
   tobase64((unsigned char *) result_base64[0], (md5_byte_t *) &fingerprint, sizeof(uint64_t));
  standard_locale(1);
}

void R_unf3_double(double v[], int *nv, int *digits, int result[], 
	char **result_base64) {
   int i;
   md5_state_t state;
   md5_byte_t digest[16];


  standard_locale(0);
   md5_init(&state);
   for (i=0; i < *nv; i++) {
	UNF3(v[i], *digits, &state, ISNA(v[i]));
   }
   md5_finish(&state, digest); 
   for (i=0; i < 16; i++) {
	result[i]= digest[i];
   }
   tobase64((unsigned char *) result_base64[0], digest, 16);
  standard_locale(1);
}

void R_unf4_double(double v[], int *nv, int *digits, int result[], 
	char **result_base64) {
   int i;
   sha256_context state;
   uint8 digest[32];


  standard_locale(0);
   sha256_starts(&state);
   for (i=0; i < *nv; i++) {
	UNF4(v[i], *digits, &state, ISNA(v[i]));
   }
   sha256_finish(&state, digest); 
   for (i=0; i < 32; i++) {
	result[i]= (int) digest[i];
   }
   tobase64((unsigned char *) result_base64[0], digest, 32);
  standard_locale(1);
}

void R_unf4_1_double(double v[], int *nv, int *digits, int result[], 
	char **result_base64) {
   int i;
   sha256_context state;
   uint8 digest[32];


  standard_locale(0);
   sha256_starts(&state);
   for (i=0; i < *nv; i++) {
	UNF4_1(v[i], *digits, &state, ISNA(v[i]));
   }
   sha256_finish(&state, digest); 
   for (i=0; i < 32; i++) {
	result[i]= (int) digest[i];
   }
   tobase64((unsigned char *) result_base64[0], digest, 32);
  standard_locale(1);
}

void R_unf1_char(char *v[], int miss[], int *nv, int *digits, double *result,
	char **result_base64 ) {
   int i;
   uint64_t fingerprint=0;

  standard_locale(0);
   for (i=0; i < *nv; i++) {
	fingerprint = UNF1(v[i], *digits, fingerprint, miss[i]);
   }
   *result = (double) fingerprint;
   tobase64((unsigned char *) result_base64[0], (md5_byte_t *) &fingerprint, sizeof(uint64_t));
  standard_locale(1);
}

void R_unf2_char(char *v[], int miss[], int *nv, int *digits, double *result,
	char **result_base64 ) {
   int i;
   uint64_t fingerprint=0;

  standard_locale(0);
   for (i=0; i < *nv; i++) {
	fingerprint = UNF2(v[i], *digits, fingerprint, miss[i]);
   }
   *result = (double) fingerprint;
   tobase64((unsigned char *) result_base64[0], (md5_byte_t *) &fingerprint, sizeof(uint64_t));
  standard_locale(1);
}

void R_unf3_char(char *v[], int miss[], int *nv, int *digits, int result[],
	char **result_base64 ) {
   int i;
   md5_state_t state;
   md5_byte_t digest[16];

  standard_locale(0);
   md5_init(&state);
   for (i=0; i < *nv; i++) {
	UNF3(v[i], *digits, &state, miss[i]);
   }
   md5_finish(&state, digest); 
   for (i=0; i < 16; i++) {
	result[i]= digest[i];
   }
   tobase64((unsigned char *) result_base64[0], digest, 16);
  standard_locale(1);
}

void R_unf4_char(char *v[], int miss[], int *nv, int *digits, int result[],
	char **result_base64 ) {
   int i;
   sha256_context state;
   uint8 digest[32];

  standard_locale(0);
   sha256_starts(&state);
   for (i=0; i < *nv; i++) {
	UNF4(v[i], *digits, &state, miss[i]);
   }
   sha256_finish(&state, digest); 
   for (i=0; i < 32; i++) {
	result[i]= (int) digest[i];
   }
   tobase64((unsigned char *) result_base64[0], digest, 32);
  standard_locale(1);
}

void R_unf4_1_char(char *v[], int miss[], int *nv, int *digits, int result[],
	char **result_base64 ) {
   int i;
   sha256_context state;
   uint8 digest[32];

  standard_locale(0);
   sha256_starts(&state);
   for (i=0; i < *nv; i++) {
	UNF4_1(v[i], *digits, &state, miss[i]);
   }
   sha256_finish(&state, digest); 
   for (i=0; i < 32; i++) {
	result[i]= (int) digest[i];
   }
   tobase64((unsigned char *) result_base64[0], digest, 32);
  standard_locale(1);
}

} // extern "C"
