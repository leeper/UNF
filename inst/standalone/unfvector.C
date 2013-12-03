/*
 * UNF
 *
 * Copyright (c) Micah Altman
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Original copyright notice is retained at the end of this file.
 */

#include <getopt.h>
#include "unf.h"
#include "unf_plugin.h"

#define PROGVERSION "2.2"
#define NBUFSIZ 32767
#define UNFBUFFER 32767

int NDIGITS=DEFAULTNDIGITS;
int CDIGITS=DEFAULTCDIGITS;
double UNFVERSION=DEFAULTUNFVERSION;


unf_vectype TYPE;

void usage() {
	fprintf(stderr, "unf [-d #] -t [i|f|c|u] -a [3|4|4.1] \n"
	"\n Computes Universal Numeric Fingerprint for a single vector in an ASCII text file given as standard input. Assumes that '.' is a missing value."
	"\n -d # of digits  of precision to use (composite UNF's set this automatically)\n" 
	"\n -a # choose algorithmic version\n" 
	"\n -v # display version version\n" 
	"\n -t type of vector, should be integer, real , character, or unf (for computing composite UNF's) character is the default\n" );
	exit (1);
}

void version() {
	printf("Unfvector version %s\n",PROGVERSION);
	exit(1);
}

void check_digits(double *unfversion, int *ndigits, int *cdigits) {
	
	if ( *ndigits<1 || *ndigits>15) {
		fprintf(stderr,"Warning: digits %d not allowed, using default\n",*ndigits);
		*ndigits=DEFAULTNDIGITS;
	}
	if ( *cdigits<1 || *cdigits>(NBUFSIZ-100)) {
		fprintf(stderr,"Warning: digits %d not allowed, using default\n",*cdigits);
		*cdigits=DEFAULTCDIGITS;
	}
	if ( (*unfversion!=3) && (*unfversion!=4) &&  (*unfversion!=4.1)) {
		fprintf(stderr,"Warning: illegal unf version %g, using defaults\n", *unfversion);
		*unfversion=DEFAULTUNFVERSION;
	}
}


void parse_args(int argc, char **argv) {
	char c,t='c';
	int tmpdigits=0;

	while (1) {

                c = getopt(argc, argv, "d:t:ha:v");
		if (c == -1)
			break;

		switch (c) {
			case 'd':
			tmpdigits = atoi(optarg);
			break;

			case 'v':
			version();
			break;

			case 'a':
			UNFVERSION = atof(optarg);
			break;

			case 't':
			t = optarg[0];	
			break;

			case 'h':
			usage();
			break;
		}
	}

	switch(t) {
		case 'r':
			TYPE=REAL;	
			break;
		case 'i':
			TYPE = INTEGER;
			break;
		case 'c':
			TYPE = STRING;
			break;
		case 'u':
			TYPE = UNF;
			break;
		default: 
			usage();
	}

	if (tmpdigits > 0) {
		if (TYPE==REAL || TYPE==INTEGER) {
			NDIGITS=tmpdigits;
		} else if (TYPE==STRING) {
			CDIGITS=tmpdigits;
		} else {
			fprintf(stderr,"Warning: digits ignored for type UNF\n");
		}
	}


	check_digits(&UNFVERSION,&NDIGITS,&CDIGITS);
		
}

void unfvec(FILE *fd, int ndigits, int cdigits, char *unfstring, unf_vectype vt, double version ) {
   char input[NBUFSIZ], *endptr;
   int ismiss, inputi, line=0,sl;
   double inputd;

   // for composite unfs
   char *base64_list[UNFBUFFER];
   char base64[UNFBUFFER];
   int  unf_count=0; 
   int  composite_mixed_warning=0;
   int base64_length=0;
   unf_t *unf;

   // state vector
   md5_state_t state;
   sha256_context context;
   uint8 digest[32];
   if (version==3) {
   	md5_init(&state);
   } else {
	sha256_starts(&context);
   }

 while (fgets(input, NBUFSIZ, stdin) && (unf_count<UNFBUFFER)) {
   	line ++;
	ismiss=0;
        
        // get line and clean up
	sl = strlen(input);
	if (input[sl-1]=='\n') {
		input[strlen(input)-1]='\0';
	}
	if ((sl == 1) && (input[0]=='\0')) {
		ismiss=1;
	} 

	switch (vt) {
		case UNF: 
	
			unf=parse_unf(input);
                	if (!unf) {
                        	fprintf(stderr,"Warning, not a UNF, ignored: %s\n",input);
                        	break;
                	}
			if (unf_count==0) {
				ndigits=unf->ndigits;	
				cdigits=unf->cdigits;	
				version=unf->version;	
				check_digits(&version,&ndigits,&cdigits);
			} else {
				if ((unf->ndigits!=ndigits) || (unf->cdigits!=cdigits) ||
					(unf->version!=version)) {
					composite_mixed_warning=1;
				}
			}
   	           	base64_length=strlen(unf->base64);
                	base64_list[unf_count]=(char*) malloc(base64_length+1);
                	strcpy(base64_list[unf_count], unf->base64);
                	free(unf);
                	unf_count ++;	
			break;
		case STRING: 
   			if (version==3) {
        			UNF3(input, cdigits, &state, ismiss);
			} else if (version==4) {
				UNF4(input, cdigits, &context, ismiss);
			} else {
				UNF4_1(input, cdigits, &context, ismiss);
			}
			break;
		case INTEGER: 
 			inputi = strtol((const char *) input, &endptr, 10);
			if ((sl==2) && (input[0]=='.')) {
				ismiss=1;
			}
			if ((!ismiss && (endptr[0]!='\0')) || (errno !=0))  {
				fprintf(stderr, "Warning error %d at line %d\n", 
							errno, line);
			}
   			if (version==3) {
        			UNF3((UNFldouble) inputi, ndigits, &state, ismiss);
			} else if (version==4) {
				UNF4((UNFldouble) inputi, ndigits, &context, ismiss);
			} else {
				UNF4_1((UNFldouble) inputi, ndigits, &context, ismiss);
			}

			break;
		case REAL: 
 			inputd = strtod((const char *) input, &endptr);
			if ((sl==2) && (input[0]=='.')) {
				ismiss=1;
			}
			if ((!ismiss && (endptr[0]!='\0')) || (errno !=0))  {
				fprintf(stderr, "Warning error %d at line %d\n", 
							errno, line);
			}
   			if (version==3) {
        			UNF3((UNFldouble) inputd, ndigits, &state, ismiss);
			} else if (version==4) {
				UNF4((UNFldouble) inputd, ndigits, &context, ismiss);
			} else {
				UNF4_1((UNFldouble) inputd, ndigits, &context, ismiss);
			}
			break;
	}
	

	#ifdef DEBUG
	fprintf(stderr,"INPUT:%s:\n",input);
	fprintf(stderr,"ISMISS:%d\n",ismiss);
	#endif
   }
  // Applies to type UNF only, unf_count is 0 otherwise
  if (unf_count>1) {
     if (unf_count==UNFBUFFER) {
         fprintf(stderr,"Warning, buffer exceeded, only  %d records counted\n",unf_count);
     }
     if (composite_mixed_warning) {
         fprintf(stderr,"Warning, input unf's were computed  using different versions or levels of precision\n");
     }
     unf_sort(base64_list,unf_count-1);
     for (int i =0; i < unf_count; i++) {
       if (version==3) {
          UNF3(base64_list[i], cdigits, &state, 0);
       } else if (version==4) {
          UNF4(base64_list[i], cdigits, &context, 0);
       } else {
          UNF4_1(base64_list[i], cdigits, &context, 0);
       }	
     }
  }
  if (unf_count==1) {
	strcpy(base64, base64_list[0]);
  } else {
    if (version==3) {
      md5_finish(&state, digest);
      tobase64((unsigned char *) base64, digest, 16);
    } else {
      sha256_finish(&context, digest);
      tobase64((unsigned char *) base64, digest, 32);
    }
  }
   
   if (ndigits==DEFAULTNDIGITS && cdigits==DEFAULTCDIGITS) {
	sprintf(unfstring,"UNF:%g:%s", version, base64);
    } else {
	sprintf(unfstring,"UNF:%g:%d,%d:%s", version, ndigits, cdigits, base64);
   } 
}

int main(int argc, char **argv)
{
	char unfstring[NBUFSIZ];

	parse_args(argc,argv);

	unfvec(stdin, NDIGITS, CDIGITS, unfstring, TYPE, UNFVERSION);
	printf("%s\n",unfstring);
	return(0);
}
