#include "unf_plugin.h"

/* compare strings through pointers */
int pstrcmp(const void *p1, const void *p2)	{
	return strcmp(*(char **)p1, *(char **)p2);
}

char *lindex(const char *s, int c) {
	char *ret=NULL;
	for (int i =0; s[i]!='\0' ; i++) {
		if (s[i]==c) {
			ret= (char *) s+i;
		}
	}
	return(ret);
}

/* sort unf base64 in proper locale */
void unf_sort(char **unf_list, int unf_count) {

      if (unf_count<1) {
           return;
      }

      #ifdef FORCELOCALE
      char *oldlocale = setlocale(LC_COLLATE, "POSIX");
      if (!oldlocale) {
      	oldlocale = setlocale(LC_COLLATE, "C");
      }
      #endif
      qsort((void *)  unf_list, (size_t) unf_count, (size_t) sizeof(char*) , pstrcmp);
      #ifdef FORCELOCALE
      setlocale(LC_COLLATE, oldlocale );
      #endif
}

/* return a unf structure from a string */
unf_t *parse_unf(const char* unf_string) {
	char *tmpstring, *tmptok;
	const char *delim = ":";

	unf_t *retval = (unf_t*) malloc(sizeof(unf_t));
	tmpstring=strdup(unf_string);

	if ((strlen(unf_string)<30) || strncmp(unf_string,"UNF:", (size_t) 4)!=0) {
		goto ABORT;
	}

	tmptok = strtok(tmpstring,delim);
	tmptok = strtok(NULL,delim);
	if (!tmptok) {
		goto ABORT;
	}
	retval->version=atof(tmptok);
	tmptok = strtok(NULL,delim);
	if (!tmptok) {
		goto ABORT;
	}
	strncpy(retval->base64,tmptok,MAXBASE64LEN);
	tmptok = strtok(NULL,delim);
	if (tmptok) {
		// Hey, that wasn't the base64 after all, this is
		char *tmpdelim = lindex(retval->base64,',');
		if (tmpdelim) {
			retval->cdigits=atoi(tmpdelim+1);
			tmpdelim[0]='\0';
			retval->ndigits=atoi(retval->base64);
		} else {
			retval->ndigits=atoi(retval->base64);
			retval->cdigits=retval->ndigits;
		}
		strncpy(retval->base64,tmptok,MAXBASE64LEN);
	}  else {
		retval->cdigits = DEFAULTCDIGITS; 
		retval->ndigits = DEFAULTNDIGITS; 
	}
	free(tmpstring);
	return(retval);

	ABORT: {
		// Not a valid UNF
		free(tmpstring);
		free(retval);
		return(NULL);
	}
}
