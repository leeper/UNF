\name{unf}
\alias{unf}
\alias{unf3}
\alias{unf4}
\alias{unf5}
\alias{print.UNF}
\title{Universal Numeric Fingerprint}
\description{
A universal numeric fingerprint is used to guarantee that a defined subset of data is substantively identical to a comparison subset. Two fingerprints will match if and only if the subset of data generating them are identical, when represented using a given number of significant digits.
}
\usage{
unf(x, ver = 5, ...)

unf3(x, digits = 7, chars = 128, dvn = TRUE, ...)

unf4(x, digits = 7, chars = 128, dvn = TRUE, ver = 4, ...)

unf5(x, digits = 7, chars = 128, dvn = TRUE, ...)
}

\arguments{
	\item{x}{A vector or data frame. For algorithm versions < 5, all non-numeric vectors are treated as character.}
	\item{ver}{Version of the UNF algorithm. Allowed values are 3, 4, 4.1, and 5. Always use the same version of the algorithm to check a UNF. Default for \code{unf} is 5 and default for \code{unf4} is 4, but can also be set to 4.1.}
	\item{digits}{The number of significant digits for rounding for numeric values. Default is 7.}
	\item{chars}{The number of characters for truncation. Default is 128.}
    \item{dvn}{A logical indicating whether to use the Dataverse Network implementation of the UNF algorithm. Default is \code{TRUE}. See details.}
	\item{\ldots}{Additional arguments passed to specific algorithm functions.}
}
\details{
A UNF is created by rounding data values (or truncating strings) to a known number of digits (or characters), representing those values in standard form (as 8-bit [for versions 4.1 and 5] or 32-bit [for versions 3 and 4] unicode-formatted strings), and applying a fingerprinting method (a cryptographic hashing function) to this representation.  UNF's are computed from data values (independent of column arrangement for rectangular data), so they directly reflect the internal representation of the data.

A UNF differs from an ordinary file checksum in several important ways:

1. \emph{UNF's are format independent.}  The UNF for the data will be the same regardless of whether the data is saved as a R binary format, SAS formatted file, Stata formatted file, etc., but file checksums will differ.

2. \emph{UNF's are robust to insignificant rounding error.} A UNF will also be the same if the data differs in non-significant digits, a file checksum not.

3.\emph{UNF's detect misinterpretation of the data by the statistical software.} If the statistical software misreads the file, the resulting UNF will not match the original, but the file checksums may match.

4.\emph{UNF's are strongly tamper resistant.} Any accidental or intentional changes to the data values will change the resulting UNF. Most file checksums's and descriptive statistics detect only certain types of changes. 


Note: The Dataverse Network implements a potentially incorrect version of the UNF algorithm with regard to the handling of zero values and boolean FALSE values in data (though the specification is unclear). Setting the \code{dvn} argument to \code{TRUE} (the default), uses the Dataverse implementation (for comparison to files stored in that archive).
}

\value{
The \code{unf} function returns a UNF object which can be converted using \code{as.character} to a signature string.

For example:
	UNF:3:10,128:ZNQRI14053UZq389x0Bffg==

This representation identifies the signature as a fingerprint, using version 3 of the algorithm, computed to 10 significant digits for numbers and 128 for characters. The segment following the final colon is the actual fingerprint in base64 encoded format.

Note: to compare two UNF's, or sets of unfs, one often wants to compare only the base64 portion. 
}

\references{
\url{http://thedata.org/book/universal-numerical-fingerprint}

Altman, M., J. Gill and M. P. McDonald.  2003.  \emph{Numerical Issues in Statistical
Computing for the Social Scientist}.  John Wiley \& Sons.
\url{http://www.hmdc.harvard.edu/numerical_issues/} [Defining the algorithm]

Altman, M., \& G. King. 2007. A Proposed Standard for the Scholarly Citation of Quantitative Data.
\emph{D-Lib} 13(3/4).
\url{http://dlib.org/dlib/march07/altman/03altman.html} [Citation standard using UNF's]
}


\examples{
# simple example
v=1:100/10 +.0111 
vr=signif(v,digits=2)

# print.unf shows in  standard format, including version and digits
print(unf(v))

# as.character will return base64 section only for comparisons
as.character(unf(v))

# this is false,  since computed  base64 values UNF's differ
unf2base64(unf(v))==unf2base64(unf(vr))

# this is true,  since computed UNF's base64 values are the same at 2 significant digits
unf2base64(unf(v, digits=2))==unf2base64(unf(vr))

# WARNING: this is false, since UNF's values are the same, but 
# number of calculated digits differ , probably not the comparison
# you intend

identical(unf(v,digits=2),unf(vr))

# compute a fingerprint of longley at 10 significant digits of accuracy for numeric values
# this fingerprint can be stored and verified when reading the dataset
# later
data(longley)
mf10<-unf(longley,ndigits=10);

# this produces the same results as using signifz(), but not signif()
mf11<-unf(signifz(longley,digits=10))

unf2base64(mf11)==unf2base64(mf10)

#printable representation, prints seven UNF's, one for each vector
print(mf10)

#  summarizes the base64 portion of the unf for each vector into a 
# single  base64 UNF representing entire dataset
summary(mf10)
\dontshow{
#self test

unfTest=get("unfTest",envir=environment(unf))
if (!unfTest(silent=FALSE)) {
	stop("failed self tests")
}

}

}

\examples{


# Version 5 #
## vectors

### just numerics
unf5(1:20) # UNF:5:/FIOZM/29oC3TK/IE52m2A==
unf5(-3:3) # UNF:5:pwzm1tdPaqypPWRWDeW6Jw==

# ???? SOMETHING ON NUMERIC PRECISION AND ROUNDING


### characters and factors
unf5(c('test','1','2','3')) # UNF:5:fH4NJMYkaAJ16OWMEE+zpQ==
unf5(as.factor(c('test','1','2','3'))) # UNF:5:fH4NJMYkaAJ16OWMEE+zpQ==

### booleans
unf5(c(TRUE,TRUE,FALSE))# UNF:5:DedhGlU7W6o2CBelrIZ3iw==

### dates
# ????

### bit-type

### missing values
unf5(c(1:5,NA)) # UNF:5:Msnz4m7QVvqBUWxxrE7kNQ==


## dataframes (variable order is irrelevant)
unf(data.frame(1:3,4:6,7:9)) # UNF:5:ukDZSJXck7fn4SlPJMPFTQ==
unf(data.frame(7:9,1:3,4:6)) # UNF:5:ukDZSJXck7fn4SlPJMPFTQ==


# Version 4 #
unf4(1:20)


# Version 3 #
unf3(1:20) # HRSmPi9QZzlIA+KwmDNP8w==


}


\author{
Thomas J. Leeper (\email{thosjleeper@gmail.com})

Original algorithm and R implementation by Micah Altman (\email{Micah\_Altman@harvard.edu})
}
%\keyword{}
