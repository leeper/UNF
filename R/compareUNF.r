compareUNF <- function(..., ver=5){
    x <- list(...)
    out <- mapply(function(i,j) unf(i, ver=j), x, ver)
    return(out)
}
