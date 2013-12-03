/*
*  unf.h
*
* Universal numeric fingerprints. Computes a fingerprint of a vector of
* observation at a specified level of numerical precision.
*
* Part of the Accuracy package. Available from www.r-project.org and
* www.hmdc.harvard.edu/numerical_issues/
*
*    Copyright (C) 2004-6  Micah Altman
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


// system include files
#include <inttypes.h>
#include <stdlib.h>
#include <locale.h>
#include <iconv.h> 
#include <stdio.h> 
#include <errno.h> 
#include <string.h>
#include <strings.h>
#include <math.h>
#include <fenv.h>

// local includes for cryptographic hashes
#include "md5.h"
#include "sha256.h"

/*
 * IFDEFS
 */

// Reset locale at every call?
// #define FORCELOCALE

// Running with --pedantic C++?
// #define PEDANTIC

// Sprintf ignores rounding mode?
// #define INACCURATE_SPRINTF 

// Using iconv v. 1.8?
// #define DICONV18 

// Building iconv under windows as static requires this
// #define DBUILDING_LIBICONV

// Extra debugging output
// #define DEBUG

/*
 * TYPEDEFS 
 */

typedef unsigned char cbyte;
#ifndef PEDANTIC 
typedef long double UNFldouble;
typedef long long UNFllong;
#else 
typedef double UNFldouble;
typedef long UNFllong;
#endif 

/*
 * Public function prototypes
 */

uint64_t UNF1 (UNFldouble , int , uint64_t , int ) ;
uint64_t UNF2 (UNFldouble , int , uint64_t , int ) ;
int UNF3 (UNFldouble , int , md5_state_t* , int ) ;
int UNF4 (UNFldouble , int , sha256_context* , int ) ;
int UNF4_1 (UNFldouble , int , sha256_context* , int ) ;

uint64_t UNF1 (char* , int , uint64_t , int ) ;
int UNF3 (char*, int , md5_state_t* , int );
int UNF4 (char*, int , sha256_context* , int );
int UNF4_1 (char*, int , sha256_context* , int );
uint64_t UNF2 (char* , int , uint64_t , int ) ;
int UNF_init(int) ;

void tobase64(unsigned char *out, md5_byte_t *in, int inlen);
void standard_locale(int restore);
