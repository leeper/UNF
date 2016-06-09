\name{as.unfvector}
\alias{as.unfvector}
\title{UNF Vector Representation}
\description{Standardize a vector according to UNF specifications}
\usage{
as.unfvector(x, ...)
}

\arguments{
    \item{x}{A vector to be coerced to a character string representation according to the UNF specification.}
    \item{\ldots}{Additional arguments passed to methods.}
}
\details{The UNF specifications describes how to coerce all R data types to a standardized character representation. This S3 method exposes that coercion functionality.}

\value{A character string with class \dQuote{unfvector} manipulated to follow the UNF specification. These are used internally by \code{\link{unf6}}.}

\references{
\url{http://thedata.org/book/universal-numerical-fingerprint}

Altman, M., J. Gill and M. P. McDonald. 2003. \emph{Numerical Issues in Statistical Computing for the Social Scientist}. John Wiley \& Sons. \url{http://www.hmdc.harvard.edu/numerical_issues/} [Describes version 3 of the algorithm]

Altman, M., \& G. King. 2007. A Proposed Standard for the Scholarly Citation of Quantitative Data. \emph{D-Lib} 13(3/4). \url{http://dlib.org/dlib/march07/altman/03altman.html} [Describes a citation standard using UNFs]

Altman, M. 2008. A Fingerprint Method for Scientific Data Verification. In T. Sobh, editor, Advances in Computer and Information Sciences and Engineering, chapter 57, pages 311--316. Springer Netherlands, Netherlands, 2008. \url{http://link.springer.com/chapter/10.1007/978-1-4020-8741-7_57} [Describes version 5 of the algorithm]

Data Citation Synthesis Group. 2013. Declaration of Data Citation Principles [DRAFT]. \url{http://www.force11.org/datacitation}. [Describes general principles of data citation, of which UNF is likely to be a part]
}
%\examples{}
\author{
    Thomas J. Leeper (\email{thosjleeper@gmail.com}).
}
%\keyword{}
\seealso{
\code{\link{unf}}, \code{\link{unf6}}, \code{\link{\%unf\%}} 
}
