\name{unf_equal}
\alias{unf_equal}
\alias{print.unf_equal}
\title{Compare two objects}
\description{Function to compare the size, structure, arrangement, and UNFs of two objects.}
\usage{unf_equal(x, y, ...)}
\arguments{
	\item{x}{A vector, matrix, dataframe, or list.}
	\item{y}{A vector, matrix, dataframe, or list.}
    \item{\ldots}{Additional arguments passed to \code{\link{unf}}.}
}
\details{Compares two objects using \code{all.equal} and additional details based on the UNF of the two objects (and, for lists, dataframes, and matrices) the constituent vectors thereof.}
\value{An object of class \code{unf_equal} containing the results of \code{unf} for both objects and \code{all.equal} for the comparison of the two.}
%\references{}
\examples{
a <- data.frame(x1=1:10, x2=11:20)
b <- data.frame(x1=1:10, x2=11:20+.0005)
unf_equal(a,a)
unf_equal(a,b)
unf_equal(a,b, digits=3)
}
\seealso{\code{\link{unf}}}
\author{Thomas J. Leeper}
%\keyword{}
