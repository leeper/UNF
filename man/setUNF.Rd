\name{setUNF}
\alias{setUNF}
\alias{checkUNF}
\title{Set and check UNF for an object}
\description{Functions to add a UNF attribute to an object and to compare that attribute against the current state of the object.}
\usage{
setUNF(x, ...)

checkUNF(x, ...)
}

\arguments{
	\item{x}{A vector, matrix, dataframe, or list.}
	\item{\ldots}{Additional arguments passed to specific algorithm functions.}
}
\details{\code{setUNF} calculates the UNF for an object and returns the object with its UNF stored as an attribute. \code{checkUNF} compares a UNF stored in an object's UNF attribute to the UNF for the current object. This can be used to check whether an object has been modified from its original format.}

\value{
For \code{setUNF}, the original object \code{x} with an additional attribute \codee{UNF} containing an object of class UNF reflecting the object's current UNF.

For \code{checkUNF}, a two-element list containing the original UNF object (from the attribute) and the UNF calculated on the current object.
}

%\references{}
\examples{

x <- 1:5
x <- setUNF(x)
checkUNF(x)

x[6] <- 6
checkUNF(x)
}
\seealso{\code{\link{unf}}}
\author{Thomas J. Leeper}
%\keyword{}
