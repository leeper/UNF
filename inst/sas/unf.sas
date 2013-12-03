/**********************************************************/
/* Macros to compute the UNF's for a set of variables     */
/*                                                        */
/* Usage: 						  */
/*							  */
/* %let unfvd  = c:;					  */
/* initialize 						  */
/* %unfinit() 						  */
/* %unfputc(one,character_variable)			  */
/* %unfputn(one,numeric_variable)			  */
/* %unfcomposite()					  */
/*                                                        */
/*                                                        */
/*                                                        */
/*  The results are printed, and stored in a data set     */
/*  called 'unfs'. The final unf listed is the composite  */
/*  unf for the dataset                                   */
/*                                                        */
/* Relies on the unfvector executable to do the unf       */
/* calculations                                           */
/*							  */
/**********************************************************/

/************************************/
/* Internal Macro for loop */
/************************************/

%macro unfputc(dsn,vname);
       data _null_ ;
       set &dsn;
       MISSING='.';
       %if  &vname='' %then &vname=left(trim('.'));
	   file unfpc;
       put &vname : $128.;
       run;
%mend unfputc;

%macro unfputn(dsn,vname);
       data _null_ ;
	   set &dsn;
       *MISSING='.';
	   file unfpr;
       put &vname E32.;
       run;
%mend unfputn;

%macro unfcomposite();
data _null_;
/* Location of temporary file, with path if necessary */
%let tmpdir = %sysfunc(pathname(work));
/******************************************/
/* compute composite UNF                  */
/******************************************/

%if &sysscp=WIN %then %do;
  call system("type &tmpdir\unfv.tmp | &unfvd\unfvector -t u  > &tmpdir\unfc.tmp");
  call system("type &tmpdir\unfc.tmp >> &tmpdir\unfv.tmp");
  %end;
%else %do;
  call system("&unfvd/unfvector -t u < &tmpdir/unfv.tmp > &tmpdir/unfc.tmp");
  call system("cat &tmpdir/unfc.tmp >> &tmpdir/unfv.tmp");
 %end;
run;

/******************************************/
/* Read results and print                 */
/******************************************/

data unfs;
   infile "&tmpdir\unfv.tmp" DELIMITER='09'x;
   length unfv $ 80;
   input unfv $;
run;

proc print data=unfs;
run;
%mend unfcomposite;
%macro unfinit();
option noxwait;
%let tmpdir = %sysfunc(pathname(work));
%if &sysscp='WIN' %then %do;
  X "type nul > &tmpdir\unfv.tmp";
  filename unfpc pipe "&unfvd\unfvector -t c >> &tmpdir\unfv.tmp";
  filename unfpr pipe "&unfvd\unfvector -t r >> &tmpdir\unfv.tmp";
  %end;
%else %do;
   X "cat /dev/null > &tmpdir/unfv.tmp";
   filename unfpc pipe "&unfvd/unfvector -t c >> &tmpdir/unfv.tmp";
   filename unfpr pipe "&unfvd/unfvector -t r >> &tmpdir/unfv.tmp";
   %end;
%mend unfinit;

/* Test Data */
data one;
	input a b $ c;
datalines;
1 joe 2
3 jan 5
;
run;
data _null_;
/* Set Location of unfvector executable, with path  */
%let unfvd  = c:;
/* initialize */
%unfinit()
/* enter the variables you want, in any order */
%unfputc(one,b)
%unfputn(one,a)
%unfputn(one,c)
/* finish*/
%unfcomposite()
run;
