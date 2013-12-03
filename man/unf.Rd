\name{unf}
\alias{unf}
\title{Universal Numeric Fingerprint}
\description{
A universal numeric fingerprint is used to guarantee that a defined subset of data is substantively identical to a comparison subset. Two fingerprints will match if and only if the subset of data generating them are identical, when represented using a given number of significant digits.
}
\usage{
unf(x, digits = 7, chars = 128, ver = 5, ...)
}

\arguments{
	\item{x}{A vector or data frame. For algorithm versions < 5, all non-numeric vectors are treated as character.}
	\item{digits}{The number of significant digits for rounding for numeric values. Default is 7.}
	\item{chars}{The number of characters for truncation. Default is 128.}
	\item{ver}{Version of the UNF algorithm. Allowed values are 3, 4, 4.1, and 5. Always use the same version of the algorithm to check a UNF.}
	\item{\ldots}{Additional arguments passed to specific algorithm functions.}
}
\details{
A UNF is created by rounding data values (or truncating strings) to a known number of digits (or characters), representing those values in standard form (as 8- or 32-bit unicode-formatted strings), and applying a fingerprinting method (a cryptographic hashing function) to this representation.  UNF's are computed from data values (independent of column arrangement for rectangular data), so they directly reflect the internal representation of the data.

A UNF differs from an ordinary file checksum in several important ways:

1. \emph{UNF's are format independent.}  The UNF for the data will be the same regardless of whether the data is saved as a R binary format, SAS formatted file, Stata formatted file, etc., but file checksums will differ.

2. \emph{UNF's are robust to insignificant rounding error.} A UNF will also be the same if the data differs in non-significant digits, a file checksum not.

3.\emph{UNF's detect misinterpretation of the data by the statistical software.} If the statistical software misreads the file, the resulting UNF will not match the original, but the file checksums may match.

4.\emph{UNF's are strongly tamper resistant.} Any accidental or intentional changes to the data values will change the resulting UNF. Most file checksums's and descriptive statistics detect only certain types of changes. 
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

\author{
Thomas J. Leeper (\email{thosjleeper@gmail.com})

Original algorithm and R implementation by Micah Altman (\email{Micah\_Altman@harvard.edu})
}
%\keyword{}
