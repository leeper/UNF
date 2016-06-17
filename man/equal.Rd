\name{\%unf\%}
\alias{\%unf\%}
\alias{print.UNFtest}
\title{Compare two objects}
\usage{
`\%unf\%`(x, y, ...)
}
\arguments{
\item{x}{A vector, matrix, dataframe, list, or object of class \dQuote{UNF}, or a one-element character vector containing a UNF signature.}

\item{y}{A vector, matrix, dataframe, list, or object of class \dQuote{UNF}, or a one-element character vector containing a UNF signature.}

\item{\ldots}{Additional arguments passed to \code{\link{unf}}.}
}
\value{
An object of class \code{UNFtest} containing the results of \code{unf} for both objects and both \code{identical} and \code{all.equal} for the comparison of the two.
}
\description{
Function to compare the size, structure, arrangement, and UNFs of two objects.
}
\details{
Compares two objects using \code{all.equal} and additional details based on the UNF of the two objects (and, for lists, dataframes, and matrices) the constituent vectors thereof. The print method for class \code{UNFtest} prints the UNFs for both objects and summarizes any differences between the objects. This is helpful for identifying mismatching variables.
}
\examples{
a <- data.frame(x1=1:10, x2=11:20)
b <- data.frame(x1=1:10, x2=11:20+.0005)
a \%unf\% a
a \%unf\% b
`\%unf\%`(a,b, digits = 3)

unf(a) \%unf\% "UNF6:aKW4lAFNBH8vfrnrDbQZjg=="

}
\author{
Thomas J. Leeper
}
\seealso{
\code{\link{unf}}
}

