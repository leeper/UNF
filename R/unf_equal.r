unf_equal <- function(x, y, ...){
    i <- identical(x,y)
    unfx <- unf(x, ...)
    unfy <- unf(y, ...)
    l <- 
    list(   identical = i,
            dim.x = if(is.null(dim(x))) c(length(x),1) else dim(x),
            dim.y = if(is.null(dim(y))) c(length(y),1) else dim(y),
            unf.x = unfx,
            unf.y = unfy,
            x.vars = if(!is.null(unfx$variables)) unfx$variables,
            y.vars = if(!is.null(unfy$variables)) unfy$variables,
            equal = all.equal(x, y),
            x = x,
            y = y)
    class(l) <- 'unf_equal'
    return(l)
}

print.unf_equal <- function(x, ...){
    if(x$identical)
        cat('Objects are identical\n\n')
    else if(x$unf.x$unf==x$unf.y$unf)
        cat('Objects are a UNF match but not identical\n\n')
    else
        cat('Objects are not identical\n\n')
    print(x$unf.x)
    if(!x$dim.x==x$dim.y)
        cat('Dimensions: ', x$dim.x, '\n')
    if(!is.null(x$x.vars)){
        misx <- x$x.vars[!x$x.vars %in% x$y.vars]
        if(length(misx)){
            cat('Mismatched variables:\n')
            cat(paste(names(head(misx,10)),head(misx,10),sep=': '), sep='\n')
            if(length(misx)>10)
                cat('[Additional mismatches not printed]\n')
        }
    }
    cat('\n')
    print(x$unf.y)
    cat('Dimensions: ', x$dim.y, '\n')
    if(!is.null(x$y.vars)){
        misy <- x$y.vars[!x$y.vars %in% x$x.vars]
        if(length(misy)){
            cat('Mismatched variables:\n')
            cat(paste(names(head(misy,10)),head(misy,10),sep=': '), sep='\n')
            if(length(misy)>10)
                cat('[Additional mismatches not printed]\n')
        }
    }
}
