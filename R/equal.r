`%unf%` <- function(x, y, ...){
    i <- identical(x,y)
    unfx <- unf(x, ...)
    unfy <- unf(y, ...)
    if(is.list(x) | is.matrix(x))
        sorted <- identical(x[do.call(order, x), ], y[do.call(order, y), ])
    else
        sorted <- identical(sort(x),sort(y))
    if(!sorted){
        x.rows <- apply(x, 1, function(i) digest(as.character(i)))
        y.rows <- apply(y, 1, function(i) digest(as.character(i)))
    } else {
        x.rows <- y.rows <- NULL
    }
    l <- 
    list(   identical = i,
            sorted = sorted,
            dim.x = if(is.null(dim(x))) c(length(x),1) else dim(x),
            dim.y = if(is.null(dim(y))) c(length(y),1) else dim(y),
            unf.x = unfx,
            unf.y = unfy,
            x.vars = if(!is.null(unfx$variables)) unfx$variables,
            y.vars = if(!is.null(unfy$variables)) unfy$variables,
            x.rows = x.rows,
            y.rows = y.rows,
            equal = all.equal(x, y),
            x = x,
            y = y)
    class(l) <- 'unf_equal'
    return(l)
}

print.unf_equal <- function(x, ...){
    if(x$identical)
        cat('Objects are identical\n\n')
    else if(x$unf.x$unf==x$unf.y$unf){
        a <- attributes(x$unf.x)
        cat('Objects are a UNF (v',
            paste(a$version,':',a$digits,',',a$characters,sep=''),
            ') match but are not identical\n\n',sep='')
    } else if(x$sorted)
        cat('Objects are identical but sorted\n\n')
    else
        cat('Objects are not identical\n\n')
    print(x$unf.x)
    if(!identical(x$dim.x,x$dim.y))
        cat('Dimensions: ', x$dim.x, '\n')
    if(!x$sorted & !is.null(x$x.vars)){
        misx <- x$x.vars[!x$x.vars %in% x$y.vars]
        if(length(misx)){
            cat('Mismatched variables:\n')
            cat(paste(names(head(misx,10)),head(misx,10),sep=': '), sep='\n')
            if(length(misx)>10)
                cat('[Additional mismatches not printed]\n')
        }
    }
    if(!x$sorted & !is.null(x$x.rows)){
        misx <- which(!x$x.rows %in% x$y.rows)
        if(length(misx)){
            cat('Rows seemingly not in y:\n')
            print(misx)
        }
    }
    cat('\n')
    print(x$unf.y)
    if(!identical(x$dim.x,x$dim.y))
        cat('Dimensions: ', x$dim.y, '\n')
    if(!x$sorted & !is.null(x$y.vars)){
        misy <- x$y.vars[!x$y.vars %in% x$x.vars]
        if(length(misy)){
            cat('Mismatched variables:\n')
            cat(paste(names(head(misy,10)),head(misy,10),sep=': '), sep='\n')
            if(length(misy)>10)
                cat('[Additional mismatches not printed]\n')
        }
    }
    if(!x$sorted & !is.null(x$y.rows)){
        misy <- which(!x$y.rows %in% x$x.rows)
        if(length(misy)){
            cat('Rows seemingly not in x:\n')
            print(misy)
        }
    }
}
