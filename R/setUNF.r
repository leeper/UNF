setUNF <- function(x, ver=5){
    attr(x, 'UNF') <- unf(x, ver=ver)
    return(x)
}
