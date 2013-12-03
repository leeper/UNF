#. unf.R
#
# Computes a universal numeric digital fingerprint of a vector
#
# Part of the UNF package. Available from www.r-project.org and
# www.hmdc.harvard.edu/numerical_issues/
#
#    Copyright (C) 2004-6  Micah Altman
#
#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program; if not, write to the Free Software
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

# These are the default precision levels for the UNF computation 
v4DefaultNdig = 7;
v4DefaultCdig = 128;


######################################################
# unf
#
# This is the public function for computing UNF's in R.
# It checks data types, then calls unfV for each separate vector
# to produce a vector of UNF values.
# 
# See the R documentation file for details of each argument and return value
#
######################################################


"unf" <-
function(data, 
	digits=NULL,
	ndigits= {if (is.null(digits)) {7} else (digits)}, 
	cdigits= {if (is.null(digits)) {128} else (digits)}, 
	version=4.1, 
	rowIndexVar=NULL,
	rowOrder={if(is.null(rowIndexVar)) {NULL} else {order(rowIndexVar)}}
) {


	if (version<3) {
		warning("older versions of fingerprints are not recommended, current is version 4")
	} 

	ndigits=as.integer(ndigits)
	cdigits=as.integer(cdigits)
	if (ndigits<1) {
		warning("ndigits can't be less then 1")
		ndigits =1
	}
	if (ndigits>15) {
		warning("ndigits can't be greater than 15")
		ndigits = 15
	}
	if (cdigits<1) {
		warning("cdigits can't be less then 1")
		cdigits = 1
	}

	if (is.vector(data)) {
		len = 1
	} else if (is.factor(data)) {
		len = 1
		data=as.character(data)
		warning("forcing to character")
	} else if (is.data.frame(data)) {
		len = length(data)
	} else {
		warning("forcing to data frame")
		data=as.data.frame(data)
		len = length(data)
	}

	if (len==0) {
		warning ("NULL data")
		return(NULL)
	}

	# canonicalize row order
	if(!is.null(rowOrder)) {
		df=df[rowOrder,]
	}

	r = vector(mode="list", length=len)
	if (is.vector(data)) {
		r[[1]] =  unfV(data,ndigits=ndigits,cdigits=cdigits,version=version)
	} else {
		for ( i in 1:len) {
			r[[i]] =  unfV(data[[i]],ndigits=ndigits,cdigits=cdigits,version)
        	}
	}

	class(r)="unf"
	return(r)
}

######################################################
#
# summary.unf
#
# Computes a composite unf from the entire vector, by calling 
# unf() on the sorted base64 portions
# 
# See the R documentation file for details of each argument and return value
#
######################################################

summary.unf<-function(object,...) {
	if (length(object)==1) {
		return(object)
	} 
		
	unfattr = sapply(object,attributes)
	sigs = unlist(unfattr["base64",])
	cdigits = unlist(unfattr["cdigits",])
	ndigits = unlist(unfattr["ndigits",])
	versions= unlist(unfattr["version",])

	# Sort order under US_ENGLISH is problematic
	ol = Sys.getlocale("LC_COLLATE")
	Sys.setlocale("LC_COLLATE","C")
	ret = unf(sort(sigs),cdigits=256,version=versions[1])	
	Sys.setlocale("LC_COLLATE",ol)
	attr(ret[[1]],"cdigits")=cdigits[1];
	attr(ret[[1]],"ndigits")=ndigits[1];
	if ((sum(ndigits!=ndigits[1])>0) || (sum(cdigits!=cdigits[1])>0) || 
		(sum(versions!=versions[1])>0))
		{
		warning("UNF's being combined have different precisions or versions")
		if (sum(ndigits!=ndigits[1])>0)  {
			attr(ret[[1]],"ndigits")="mixed"	
		} 
		if (sum(cdigits!=cdigits[1])>0)  {
			attr(ret[[1]],"cdigits")="mixed"	
		} 
	}

	return(ret)
}

######################################################
#
# as.character.unf 
#
# Converts the unf to a printable representation
# 
# See the R documentation file for details of each argument and return value
#
######################################################

as.character.unf<-function(x,...) {
	ret = character(length=length(x));
	for (i in 1:length(x)) {
	   version = attr(x[[i]],"version")
	   ret[i]=paste("UNF:",attr(x[[i]],"version"),":",
		 if ( (version<3) || (attr(x[[i]],"ndigits")!=v4DefaultNdig)
			 || (attr(x[[i]],"cdigits")!=v4DefaultCdig)
			) {
			paste( attr(x[[i]],"ndigits"), "," , 
		 	attr(x[[i]],"cdigits"),":",
			sep="")
		},
		 attr(x[[i]],"base64") ,sep="")  
	}
	return(ret)	
}

######################################################
#
# as.unf
#
# Takes a vector of character string representing a unf and using
# regexes converts it back into a an R unf object
#
# See the R documentation file for details of each argument and return value
# 
######################################################


as.unf<-function(char) {
	 if (!is.character(char)) {
                warning("coercing to character string")
		char=as.character(char)
         }
	ret = vector(mode="list",length=length(char));
	for (i in 1:length(char)) {
		if ( regexpr("^UNF:[0-9a-zA-Z.]+:([0-9]+(,[0-9]+)*:)?[a-zA-Z0-9+/]+=?=?",
    char[[i]])<0) {
			warning("does not appear to be a UNF")
			return(NULL)
		} else {
			tmp= strsplit(char[[i]],":")[[1]];	
			ret[[i]]=tmp[length(tmp)]
	        	class(ret[[i]])="unfV"
        		attr(ret[[i]],"base64")=tmp[length(tmp)]
        		attr(ret[[i]],"version")=tmp[2]
			attr(ret[[i]],"isnested")=FALSE

			if (length(tmp)==3) {
        			attr(ret[[i]],"ndigits")=v4DefaultNdig
        			attr(ret[[i]],"cdigits")=v4DefaultCdig
			} else  {
			 	tmpdig = strsplit(tmp[3],",")[[1]]
				if (length(tmpdig)==1) {
					attr(ret[[i]],"ndigits")=tmpdig
					attr(ret[[i]],"cdigits")=tmpdig
				} else {		
					attr(ret[[i]],"ndigits")=tmpdig[1]
					attr(ret[[i]],"cdigits")=tmpdig[2]
				}
			}
		}
	}
	class(ret)="unf"
	return(ret)	
}

######################################################
#
# unf2base64
#
# Returns a base64 character string representing the UNF
# 
# See the R documentation file for details of each argument and return value
#
######################################################

unf2base64<-function(x) {
	ret = character(length=length(x))
	for (i in 1:length(x)) {
	   ret[i]= attr(x[[i]],"base64")
	}
	return(ret)	
}

######################################################
#
# unfV
#
# [Internal Function]
#
# Boring function that does some version and type checking
# then calls the native C++ UNF bridge functions
# 
# Parameters:
# 	v - character or numeric vector
#	ndigits - digits of numeric precision
#	cdigits - digits of character precision
#	version - unf algorithm version
# 
######################################################

"unfV" <-
function(v, 
	ndigits= NULL,
	cdigits= NULL,
	version=4 ) {

	INITSTRING = sprintf("%0.64i",as.integer(0))
	if (is.null(v) || is.null(ndigits) || is.null(cdigits)) {
		warning("unV called with NULL arguments")
		return(NULL)
	}
 
  if (!is.element(version,c(1,2,3,4,4.1))){
      version=4
      warning("Unsupported version -- using version 4")
  }
  
  # build call
  
 verString = gsub("\\.","_",as.character(version))
 if (is.character(v)) {
    typeString = "char"
 }
 else 
 {
    typeString = "double"
 }
 funcName = paste("R_unf",verString,"_",typeString,sep="")
 if (version<3) {
      fingerprint=double(length=1)
 } else {
      fingerprint = integer(length=32)
 }
 base64=INITSTRING
 
 unfCall = list(funcName,NAOK=TRUE)
 if (is.R()) {
    unfCall=c(unfCall,list(PACKAGE="UNF"))
 }
 else 
 {
    unfCall=c(unfCall,list(specialsok=TRUE))
 }
 
 if(is.character(v)) {
    unfCall=c(unfCall,list(as.character(v)), list(as.integer(is.na(v))),
        as.integer(length(v)), as.integer(cdigits),
        fingerprint=list(fingerprint),
        base64=base64)
 } else {
        unfCall=c(unfCall,list(as.double(v)), 
        as.integer(length(v)), as.integer(ndigits),
        fingerprint=list(fingerprint),
        base64=base64)
 }
  
  # do call
	if (is.R()) {
	   r<-do.call(".C",unfCall)
   } else {
      r<-do.call(".C",unfCall)
      ###TESTING-- Dummy values
      #r=list()
      #r$base64= paste(sum(as.numeric(v)),"FAKE00000000000000==",sep="")
      #r$fingerprint = sum(as.numeric(v))
   }
	
	sig = r$base64;	
	class(sig)="unfV"
	attr(sig,"ndigits")=ndigits
	attr(sig,"cdigits")=cdigits
	attr(sig,"version")=version
	attr(sig,"isnested")=FALSE
	attr(sig,"base64")=r$base64
	attr(sig,"fingerprint")=r$fingerprint
	return(sig)
}

######################################################
#
# print.unf
#
# Prints the unf. Just a wrapper of as.character()
# 
# See the R documentation file for details of each argument and return value
#
######################################################

print.unf<-function(x,...) {
	invisible(print(as.character(x),...));
}

######################################################
#
# signifz
#
# Rounds a vector of numbers to n significant digits
# using round-toward-zero rounding. Not used in UNF 
# computations per se, but for rproducing UNF's internal
# rounding method in R.
# 
# See the R documentation file for details of each argument and return value
#
######################################################

signifz<-function(x,digits=6) {

  
  if (class(x)=="data.frame") {
       ret = x
       # used sapply here previously, but SPLUS doesn't like it
       for (i in 1:length(x)) {
          ret[[i]] = signifz(ret[[i]],digits=digits)
       }
       rownames(ret)=rownames(x)
     	 return(ret)
  }
  magnitude = floor(log10(abs(x)))
  scale = 10^(digits-magnitude-1)
  signs =  sign(x)

  ret=x
  g0 = which(signs>=0)
  ret[g0]= floor(x[g0]*scale[g0])/scale[g0]
  l0 = which(signs<0) 
  ret[l0]=  ceiling(x[l0]*scale[l0])/scale[l0]
  return(ret)
}

######################################################
#
# unfTest
#
# [Internal Function]
#
# Performs some sanity checks on unf computation
# and compares results to known correct output
# 
######################################################

"unfTest" <-
function(silent=TRUE) {
	ret = TRUE

   x1 = 1:20
   x2 = x1 +.00001

   if (unf2base64(unf(x1))==unf2base64(unf(x2))) {
	ret=FALSE
	if (!silent) {
		warning("Failed discrimination test.")
        }
   }
   if (unf2base64(unf(x1,digits=5))!=unf2base64(unf(x2,digits=5))) {
	ret=FALSE
	if (!silent) {
		warning("Failed significance test 1.")
        }
   }
   if (unf2base64(unf(x1))!=unf2base64(unf(x2,digits=5))) {
	ret=FALSE
	if (!silent) {
		warning("Failed significance test 2.")
        }
   }
	
  cv="HRSmPi9QZzlIA+KwmDNP8w==";
  if (unf2base64(unf(x1,version=3))!=cv) {
	ret=FALSE
	if (!silent) {
		warning("Failed replication, v3.")
       }
  }

  cvs = "E8+DS5SG4CSoM7j8KAkC9A==";
  if (unf2base64(summary(unf(as.data.frame(cbind(x1,x2)),ndigits=10,version=3)))!=cvs) {
	ret=FALSE
	if (!silent) {
		warning("Failed replication, v3.")
       }
  }

  if (!is.R()){
        longley <-
structure(list(GNP.deflator = c(83, 88.5, 88.2, 89.5, 96.2, 98.1, 
99, 100, 101.2, 104.6, 108.4, 110.8, 112.6, 114.2, 115.7, 116.9
), GNP = c(234.289, 259.426, 258.054, 284.599, 328.975, 346.999, 
365.385, 363.112, 397.469, 419.18, 442.769, 444.546, 482.704, 
502.601, 518.173, 554.894), Unemployed = c(235.6, 232.5, 368.2, 
335.1, 209.9, 193.2, 187, 357.8, 290.4, 282.2, 293.6, 468.1, 
381.3, 393.1, 480.6, 400.7), Armed.Forces = c(159, 145.6, 161.6, 
165, 309.9, 359.4, 354.7, 335, 304.8, 285.7, 279.8, 263.7, 255.2, 
251.4, 257.2, 282.7), Population = c(107.608, 108.632, 109.773, 
110.929, 112.075, 113.27, 115.094, 116.219, 117.388, 118.734, 
120.445, 121.95, 123.366, 125.368, 127.852, 130.081), Year = as.integer(c(1947, 
1948, 1949, 1950, 1951, 1952, 1953, 1954, 1955, 1956, 1957, 1958, 
1959, 1960, 1961, 1962)), Employed = c(60.323, 61.122, 60.171, 
61.187, 63.221, 63.639, 64.989, 63.761, 66.019, 67.857, 68.169, 
66.513, 68.655, 69.564, 69.331, 70.551)), .Names = c("GNP.deflator", 
"GNP", "Unemployed", "Armed.Forces", "Population", "Year", "Employed"
), row.names = c("1947", "1948", "1949", "1950", "1951", "1952", 
"1953", "1954", "1955", "1956", "1957", "1958", "1959", "1960", 
"1961", "1962"), class = "data.frame")
  }
  
  cv3="PjAV6/R6Kdg0urKrDVDzfMPWJrsBn5FfOdZVr9W8Ybg="
  if (  unf2base64(summary(unf(longley,digits=3,version=4))) != cv3 ||
	unf2base64(summary(unf(signifz(longley,digits=3),version=4))) != cv3) {
	ret = FALSE
	if (!silent) {
		warning("Failed longley v 4")
       }
  }

 cv4="8nzEDWbNacXlv5Zypp+3YCQgMao/eNusOv/u5GmBj9I="
  if (  unf2base64(summary(unf(longley,digits=3))) != cv4 ||
	unf2base64(summary(unf(signifz(longley,digits=3)))) != cv4) {
	ret = FALSE
	if (!silent) {
		warning("Failed longley v 4.1")
       }
  }

   return(ret)
}
