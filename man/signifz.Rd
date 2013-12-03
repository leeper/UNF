\name{signifz}
\alias{signifz}
\title{round values to specified number of significant digits }                         
\description{Rounds the value to a specified number of significant digits, using IEEE 754 rounding towards zero rounding mode }
\usage{
	signifz (x, digits=6)
}

\arguments{
	\item{x}{A numeric vector.}	
	\item{digits}{integer indicating the precision to be used}

}

\details{
        `signifz' rounds the values in its first argument to the specified
     number of significant digits, using IEC   60559/IEEE 754   ``round towards zero'' mode.

Rounding toward zero assures that \code{signifz(signifz(x,digits=m),digits=n)} = 
\code{signifz(x,digits=n)} for $m>n$, an important property for creating approximate
fingerprints. It can, however, produce more rounding error than rounding toward nearest. 
 The maximum log relative error (LRE) for the former is (digits-1) while the
 maximum LRE for the latter is `digits'. Hence, you may wish to use one more
significant digit with \code{signifz} than with \code{signif}.
}

\seealso{\code{signif}, \code{unf}}

\examples{
# note the difference
signif(pi,digits=5)
signifz(pi,digits=5) 
}

\author{
Micah Altman
\email{Micah\_Altman@harvard.edu}

\url{http://thedata.org/index.php/Main/UNF}
}

\keyword{misc}
\keyword{math}

