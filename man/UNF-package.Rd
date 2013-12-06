\name{UNF-package}
\alias{UNF-package}
\alias{UNF}
\docType{package}
\title{Tools for creating universal numeric fingerprints for data}
\description{Computes a univeral numeric fingerprint of data objects.}
\details{
A UNF is created by rounding data values (or truncating strings) to a known number of digits (or characters), representing those values in standard form (as 8-bit [for versions 4.1 and 5] or 32-bit [for versions 3 and 4] unicode-formatted strings), and applying a fingerprinting method (a cryptographic hashing function) to this representation.  UNF's are computed from data values (independent of column arrangement for rectangular data), so they directly reflect the internal representation of the data.

A UNF differs from an ordinary file checksum in several important ways:

1. \emph{UNFs are format independent.}  The UNF for the data will be the same regardless of whether the data is saved as a R binary format, SAS formatted file, Stata formatted file, etc., but file checksums will differ.

2. \emph{UNFs are robust to insignificant rounding error.} A UNF will also be the same if the data differs in non-significant digits, a file checksum not.

3. \emph{UNFs detect misinterpretation of the data by the statistical software.} If the statistical software misreads the file, the resulting UNF will not match the original, but the file checksums may match.

4. \emph{UNFs are strongly tamper resistant.} Any accidental or intentional changes to the data values will change the resulting UNF. Most file checksums's and descriptive statistics detect only certain types of changes. 


\tabular{ll}{
Package: \tab UNF\cr
Type: \tab Package\cr
Version: \tab 2.0\cr
Date: \tab 2013-12-24\cr
License: \tab GPL-2\cr
}
}
\author{
Thomas J. Leeper, with original package version and C libraries by Micah Altman.

Maintainer: Thomas J. Leeper <thosjleeper@gmail.com>
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
in Computer and Information Sciences and Engineering, chapter 57, pages 311--316. Springer
Netherlands, Netherlands, 2008. \url{http://link.springer.com/chapter/10.1007/978-1-4020-8741-7_57} [Version 5 of the algorithm]
}
\keyword{ package }
