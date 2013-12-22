\name{unf}
\alias{unf}
\alias{unf3}
\alias{unf4}
\alias{unf5}
\alias{print.UNF}
\title{Universal Numeric Fingerprint}
\description{A universal numeric fingerprint is used to guarantee that a defined subset of data is substantively identical to a comparison subset. Two fingerprints will match if and only if the subset of data generating them are identical, when represented using a given number of significant digits.}
\usage{
unf(x, ver = 5, ...)

unf3(x, digits = 7, chars = 128, dvn = TRUE, ...)

unf4(x, digits = 7, chars = 128, dvn = TRUE, ver = 4, ...)

unf5(x, digits = 7, chars = 128, dvn = TRUE, ...)

unf6(x, digits = 7, chars = 128, dvn = TRUE, ...)
}

\arguments{
	\item{x}{For \code{unf}, a vector, matrix, dataframe, or list; for \code{unf3}, \code{unf4}, \code{unf5}, a vector. If \code{x} is a dataframe or list with one variable or one vector element, respectively, \code{unf} returns the UNF for the single vector (which is consistent with the Dataverse implementation but ambiguous in the UNF standard). For algorithm versions < 5, all non-numeric vectors are treated as character.}
	\item{ver}{Version of the UNF algorithm. Allowed values are 3, 4, 4.1, 5, and 6. Always use the same version of the algorithm to check a UNF. Default for \code{unf} is 5 and default for \code{unf4} is 4 (but can also be set to 4.1, which is identical except for using SHA256 instead of MD5).}
	\item{digits}{The number of significant digits for rounding for numeric values. Default is 7. Should be between 1 and 15.}
	\item{chars}{The number of characters for truncation. Default is 128. Should be greater than 1.}
    \item{dvn}{A logical indicating whether to use the Dataverse Network implementation of the UNF algorithm. Default is \code{TRUE}. See details.}
	\item{\ldots}{Additional arguments passed to specific algorithm functions.}
}
\details{
The Dataverse Network implements a potentially incorrect version of the UNF algorithm with regard to the handling of zero values and boolean FALSE values in data (though the specification is unclear). Setting the \code{dvn} argument to \code{TRUE} (the default), uses the Dataverse implementation (for comparison to files stored in that archive).
}

\value{
The \code{unf} function returns a list of class \code{UNF}, containing:
\item{unf}{A character string containing the universal numeric fingerprint.}
\item{hash}{A raw vector expressing the unencoded universal numeric fingerprint. This can be converted to a UNF using \code{base64Encode}.}
\item{unflong}{For \code{unf5}, a character string containing the un-truncated universal numeric fingerprint.}

The object additionally contains several attributes:

\item{version}{A one-element numeric vector specifying which version of the UNF algorithm was used to generate the object.}
\item{digits}{A one-element numeric vector specifying how many significant digits were used in rounding numeric values.}
\item{characters}{A one-element numeric vector specifying how many characters were preserved during truncation of character values.}

The default print method displays the UNF along with these attributes. For example:

UNF:3:4,128:ZNQRI14053UZq389x0Bffg==

This representation identifies the signature as UNF, using version 3 of the algorithm, computed to 4 significant digits for numbers and 128 for characters. The segment following the final colon is the actual fingerprint in base64-encoded format.
}

\references{
\url{http://thedata.org/book/universal-numerical-fingerprint}

Altman, M., J. Gill and M. P. McDonald. 2003. \emph{Numerical Issues in Statistical Computing for the Social Scientist}. John Wiley \& Sons. \url{http://www.hmdc.harvard.edu/numerical_issues/} [Describes version 3 of the algorithm]

Altman, M., \& G. King. 2007. A Proposed Standard for the Scholarly Citation of Quantitative Data. \emph{D-Lib} 13(3/4). \url{http://dlib.org/dlib/march07/altman/03altman.html} [Describes a citation standard using UNFs]

Altman, M. 2008. A Fingerprint Method for Scientific Data Verification. In T. Sobh, editor, Advances in Computer and Information Sciences and Engineering, chapter 57, pages 311--316. Springer Netherlands, Netherlands, 2008. \url{http://link.springer.com/chapter/10.1007/978-1-4020-8741-7_57} [Describes version 5 of the algorithm]

Data Citation Synthesis Group. 2013. Declaration of Data Citation Principles [DRAFT]. \url{http://www.force11.org/datacitation}. [Describes general principles of data citation, of which UNF is likely to be a part]
}
\examples{

# Version 6 #

### FORTHCOMING ###

# Version 5 #
## vectors

### just numerics
unf5(1:20) # UNF:5:/FIOZM/29oC3TK/IE52m2A==
unf5(-3:3) # UNF:5:pwzm1tdPaqypPWRWDeW6Jw==

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

## variable order and object structure is irrelevant
unf(data.frame(1:3,4:6,7:9)) # UNF:5:ukDZSJXck7fn4SlPJMPFTQ==
unf(data.frame(7:9,1:3,4:6))
unf(list(1:3,4:6,7:9))


# Version 4 #

# version 4
data(longley)
unf(longley, ver=4, digits=3) # PjAV6/R6Kdg0urKrDVDzfMPWJrsBn5FfOdZVr9W8Ybg=

# version 4.1
unf(longley, ver=4.1, digits=3) # 8nzEDWbNacXlv5Zypp+3YCQgMao/eNusOv/u5GmBj9I=


# Version 3 #

x1 <- 1:20
x2 <- x1 + .00001

unf3(x1) # HRSmPi9QZzlIA+KwmDNP8w==
unf3(x2) # OhFpUw1lrpTE+csF30Ut4Q==

# UNFs are identical at specified level of rounding
identical(unf3(x1), unf3(x2))
identical(unf3(x1, digits=5),unf3(x2, digits=5))

# dataframes, matrices, and lists are all treated identically:
unf(cbind.data.frame(x1,x2),ver=3) # E8+DS5SG4CSoM7j8KAkC9A==
unf(list(x1,x2), ver=3)
unf(cbind(x1,x2), ver=3)

}
\author{
    Current implementation by Thomas J. Leeper (\email{thosjleeper@gmail.com}).
    Original algorithm and R implementation by Micah Altman.
}
%\keyword{}
\seealso{
\code{\link{\%unf\%}} 
\code{\link{setUNF}}
}
