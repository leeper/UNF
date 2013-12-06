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
        cat('Objectcs are a UNF match but not identical\n\n')
    else
        cat('Objects are not identical\n\n')
    print(x$unf.x)
    cat('Dimensions: ', x$dim.x, '\n')
    if(!is.null(x$x.vars)){
        cat('Variables:\n')
        cat(paste(names(x$x.vars),x$x.vars,sep=': '), sep='\n')
    }
    cat('\n')
    print(x$unf.y)
    cat('Dimensions: ', x$dim.y, '\n')
    if(!is.null(x$y.vars)){
        cat('Variables:\n')
        cat(paste(names(x$y.vars),x$y.vars,sep=': '), sep='\n')
    }
}
