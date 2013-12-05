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
}

\arguments{
	\item{x}{For \code{unf}, a vector, matrix, dataframe, or list; for \code{unf3}, \code{unf4}, \code{unf5}, a vector. If \code{x} is a dataframe or list with one variable or one vector element, respectively, \code{unf} returns the UNF for the single vector (which is consistent with the Dataverse implementation but ambiguous in the UNF standard). For algorithm versions < 5, all non-numeric vectors are treated as character.}
	\item{ver}{Version of the UNF algorithm. Allowed values are 3, 4, 4.1, and 5. Always use the same version of the algorithm to check a UNF. Default for \code{unf} is 5 and default for \code{unf4} is 4 (but can also be set to 4.1, which is identical except for using SHA256 instead of MD5).}
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

UNF:3:10,128:ZNQRI14053UZq389x0Bffg==

This representation identifies the signature as UNF, using version 3 of the algorithm, computed to 10 significant digits for numbers and 128 for characters. The segment following the final colon is the actual fingerprint in base64-encoded format.
}

\references{
\url{http://thedata.org/book/universal-numerical-fingerprint}

Altman, M., J. Gill and M. P. McDonald.  2003.  \emph{Numerical Issues in Statistical
Computing for the Social Scientist}.  John Wiley \& Sons.
\url{http://www.hmdc.harvard.edu/numerical_issues/} [Version 3 of the algorithm]

Altman, M., \& G. King. 2007. A Proposed Standard for the Scholarly Citation of Quantitative Data.
\emph{D-Lib} 13(3/4).
\url{http://dlib.org/dlib/march07/altman/03altman.html} [Citation standard using UNF's]

Altman, M. 2008. A Fingerprint Method for Scientiﬁc Data Veriﬁcation. In T. Sobh, editor, Advances
in Computer and Information Sciences and Engineering, chapter 57, pages 311—-316. Springer
Netherlands, Netherlands, 2008. \url{http://link.springer.com/chapter/10.1007/978-1-4020-8741-7_57} [Version 5 of the algorithm]
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

# version 4
unf(longley, ver=4, digits=3) # PjAV6/R6Kdg0urKrDVDzfMPWJrsBn5FfOdZVr9W8Ybg=

# version 4.1
unf(longley, ver=4.1, digits=3) # 8nzEDWbNacXlv5Zypp+3YCQgMao/eNusOv/u5GmBj9I=


# Version 3 #

x1 <- 1:20
x2 <- x1 + .00001

unf3(x1) # HRSmPi9QZzlIA+KwmDNP8w==

# UNFs are identical at specified level of rounding
identical(unf3(x1), unf3(x2))
identical(unf3(x1, digits=5),unf3(x2, digits=5))

# dataframes, matrices, and lists are all treated identically:
# E8+DS5SG4CSoM7j8KAkC9A== # ????
unf(cbind.data.frame(x1,x2),ver=3)
unf(list(x1,x2), ver=3)
unf(cbind(x1,x2), ver=3)

}
\author{
    Current implementation by Thomas J. Leeper (\email{thosjleeper@gmail.com}).
    Original algorithm and R implementation by Micah Altman.
}
%\keyword{}
