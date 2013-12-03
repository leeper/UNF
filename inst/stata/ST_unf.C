/*
 * STunf.C
 * 
 * Stata wrapper for unf()
 * Version 2
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
#include "stplugin.h"
#include "unf_plugin.h"

#define MAXVARS 32767
char *base64_list[MAXVARS];

// extern "C" so that stata can see the functions...
extern "C" {

STDLL stata_call(int argc, char *argv[])
{
	ST_int i, v, o;
	ST_retcode rc;
	int ndigits=DEFAULTNDIGITS;
	int cdigits=DEFAULTCDIGITS;
	double version=DEFAULTUNFVERSION;

	unsigned char result_base64[256];
	char buf[256], macname[64], *s;

   	md5_state_t state;   
	sha256_context context;
   	md5_byte_t digest[32];

	for (i=0; i<argc; i++) {
		s= strstr(argv[i], "ndigits(");
		if (s) {
			ndigits=atoi(s+8);
			continue;
		}
		s= strstr(argv[i], "version(");
		if (s) {
			version=atoi(s+8);
			continue;
		}
		snprintf(buf,256,"Warning, ignoring '%s'\n",argv[i]);
		SF_display(buf);
	}

	if (ndigits<1 || ndigits > 15) {
		SF_display("Illegal number of ndigits specified.\n");
		ndigits = DEFAULTNDIGITS;
	}
	if ((version!=3) &&  (version!=4) && (version!=4.1) ) {
		SF_display("Unsupported UNF version.\n");
		version= DEFAULTUNFVERSION;
	}

	if((int) SF_nvars() < 1) { 
		    return(102) ;  	/* not enough variables specified */
	} else if((int) SF_nvars() > MAXVARS) { 
	   	snprintf(buf,256,"Warning, too many variables for composite unf\n");
		SF_display(buf);
	}

	ST_double  z;
	for(v=1; v <= SF_nvars(); v++) {
	   	if (version==3) {
        		md5_init(&state);
   		} else {
        		sha256_starts(&context);
   		}   

		for(o = SF_in1(); o <= SF_in2(); o++) {
			if(!SF_ifobs(o)) {
				break;
			}
			if((rc = SF_vdata(v,o,&z))) return(rc) ;	
			if (version==3) { 
				UNF3(z, ndigits, &state, SF_is_missing(z));
			}  else {
				UNF4(z, ndigits, &context, SF_is_missing(z));
			}
		}
   		if (version==3) {
      			md5_finish(&state, digest);
      			tobase64((unsigned char *) result_base64, digest, 16);
   		} else {
       			sha256_finish(&context, digest);
       			tobase64((unsigned char *) result_base64, digest, 32);
   		}
		if (ndigits==DEFAULTNDIGITS && cdigits==DEFAULTCDIGITS) {
        		sprintf(buf,"UNF:%g:%s\n", version, result_base64);
    		} else {
        		sprintf(buf,"UNF:%g:%d,%d:%s\n", version, ndigits, cdigits, result_base64);
   		}
		snprintf(macname,64,"unf_%d",v);
		SF_macro_save(macname,buf);
		SF_display(buf);
		base64_list[v]=(char*) malloc(strlen((const char*)result_base64)+1);
		strcpy(base64_list[v],(const char*)result_base64);
	}
	if (SF_nvars()==1) {
		return(0);
	}

	// composite UNF
	if (version==3) {
           md5_init(&state);
        } else {
          sha256_starts(&context);
   	}   
	unf_sort(base64_list,SF_nvars()-1);
	for(v=1; v <= SF_nvars() && v < MAXVARS; v++) {
		if (version==3) { 
			UNF3(base64_list[v], cdigits, &state, 0);
		}  else {
			UNF4(base64_list[v], cdigits, &context, 0);
		}
	}
   	if (version==3) {
      		md5_finish(&state, digest);
      		tobase64((unsigned char *) result_base64, digest, 16);
   	} else {
       		sha256_finish(&context, digest);
       		tobase64((unsigned char *) result_base64, digest, 32);
   	}
	SF_display("\n\n-----\n\nComposite:\n\n");
	if (ndigits==DEFAULTNDIGITS && cdigits==DEFAULTCDIGITS) {
        	sprintf(buf,"UNF:%g:%s", version, result_base64);
    	} else {
        	sprintf(buf,"UNF:%g:%d,%d:%s", version, ndigits, cdigits, result_base64);
   	}
	snprintf(macname,64,"unf_composite");
	SF_macro_save(macname,buf);
	SF_display(buf);
        return(0) ;
}

}
