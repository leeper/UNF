\name{\%unf\%}
\alias{\%unf\%}
\alias{print.UNFtest}
\title{Compare two objects}
\description{Function to compare the size, structure, arrangement, and UNFs of two objects.}
\usage{
`\%unf\%`(x, y, ...)
}
\arguments{
	\item{x}{A vector, matrix, dataframe, or list.}
	\item{y}{A vector, matrix, dataframe, or list.}
    \item{\ldots}{Additional arguments passed to \code{\link{unf}}.}
}
\details{Compares two objects using \code{all.equal} and additional details based on the UNF of the two objects (and, for lists, dataframes, and matrices) the constituent vectors thereof. The print method for class \code{UNFtest} prints the UNFs for both objects and summarizes any differences between the objects. This is helpful for identifying mismatching variables.}
\value{An object of class \code{UNFtest} containing the results of \code{unf} for both objects and both \code{identical} and \code{all.equal} for the comparison of the two.}
%\references{}
\examples{
a <- data.frame(x1=1:10, x2=11:20)
b <- data.frame(x1=1:10, x2=11:20+.0005)
a \%unf\% a
a \%unf\% b
`\%unf\%`(a,b, digits=3)
}
\seealso{\code{\link{unf}}}
\author{Thomas J. Leeper}
%\keyword{}
