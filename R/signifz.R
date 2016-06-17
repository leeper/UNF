#' @title Round values to specified number of significant digits
#' @description Rounds the value to a specified number of significant digits, using IEEE 754 rounding towards zero rounding mode.
#' @param x A numeric vector.
#' @param digits An integer indicating the precision to be used.
#' @details This function rounds the values in its first argument to the specified number of significant digits, using IEC 60559/IEEE 754 ``round towards zero'' mode. This is an alternative to the \code{\link[base]{round}} function, which rounds toward nearest, ties to even. This is designed to be used internally by \code{\link{unf3}} and \code{\link{unf4}} (though the original implementations do not seem to actually use the function). \code{\link{unf5}} uses \code{round} instead.
#' Rounding toward zero assures that \code{signifz(signifz(x,digits=m),digits=n)} = \code{signifz(x,digits=n)} for $m>n$, an important property for creating approximate fingerprints. It can, however, produce more rounding error than rounding toward nearest. The maximum log relative error (LRE) for the former is (digits-1) while the  maximum LRE for the latter is `digits'. Hence, you may wish to use one more significant digit with \code{signifz} than with \code{signif}.
#' @examples
#' # note the difference
#' signif(pi,digits=5)
#' signifz(pi,digits=5)
#'
#' @author Micah Altman
#' @seealso \code{\link[base]{signif}}, \code{\link{unf}}
#' @export
signifz <- function(x, digits=6) {
    magnitude <- floor(log10(abs(x)))
    scale <- 10^(digits-magnitude-1)
    signs <- sign(x)
    ret <- x
    g0 <- which(signs>=0)
    ret[g0] <- floor(x[g0]*scale[g0])/scale[g0]
    l0 <- which(signs<0) 
    ret[l0] <- ceiling(x[l0]*scale[l0])/scale[l0]
    return(ret)
}
