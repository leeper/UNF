setUNF <- function(x, ...){
    attr(x, 'UNF') <- unf(x, ...)
    return(x)
}

checkUNF <- function(x, ...){
    unf <- unf(x, ...)
    if(!is.null(attr(x, 'UNF'))){
        old <- attr(x, 'UNF')
        if(identical(old,unf))
            message('UNFs are identical\n')
        else
            message('UNFs do not match\n')
    } else{
        old <- NULL
        message('No UNF attribute found in `x`\n')
    }
    return(list(Attribute = old, Current = unf))
}
