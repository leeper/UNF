unf <- function(x, ver = 5, ...){
    if(is.matrix(x))
        x <- as.data.frame(x)
    if((is.data.frame(x) | is.list(x)) & length(x)==1)
        x <- x[[1]]
    if(is.data.frame(x) | is.list(x)){
        locale <- Sys.getlocale(category="LC_COLLATE")
        Sys.setlocale(category="LC_COLLATE", "C")
        # Apply UNF to each vector, base64 encode, sort, and apply UNF on that vector of UNFs
        if(ver==3){
            vars <- sapply(x, function(i) unf3(i, ...)$unf)
            out <- unf3(sort(vars), ...)
        } else if(ver==4){
            vars <- sapply(x, function(i) unf4(i, ...)$unf)
            out <- unf4(sort(vars), ...)
        } else if(ver==4.1){
            vars <- sapply(x, function(i) unf4(i, ver = 4.1, ...)$unf)
            out <- unf4(sort(vars), ver = 4.1, ...)
        } else if(ver==5){
            vars <- sapply(x, function(i) unf5(i, ...)$unf)
            out <- unf5(sort(vars), ...)
        }
        Sys.setlocale(category="LC_COLLATE", locale)
        out$variables <- vars
        return(out)
    } else {
        if(ver==3){
            out <- unf3(x, ...)
        } else if(ver==4){
            out <- unf4(x, ...)
        } else if(ver==4.1){
            out <- unf4(x, ver=4.1, ...)
        } else if(ver==5){
            out <- unf5(x, ...)
        }
        return(out)
    }
}

unf3 <- function(x, digits = 7, chars = 128, dvn=TRUE, ...){
    if(inherits(x, 'AsIs'))
        x <- as.character(x)
    if(is.integer(x)){
        z <- signifz(x, digits)
        char <- .expform(z, digits)
        char <- ifelse(x==0, '+0.e+\n', char) # dvn introduced 0-value bug after v3, apparently
    } else if(is.numeric(x)){
        z <- signifz(x, digits)
        char <- .expform(z, digits)
        char <- ifelse(x==0, '+0.e+\n', char) # dvn introduced 0-value bug after v3, apparently
    } else if(is.character(x)){
        # CHARACTER: truncate strings to k
        char <- paste(substring(x, 1, chars),'\n',sep='')
    } else {
        # FACTOR: treat factor as character and truncate to k
        char <- paste(substring(as.character(x), 1, chars),'\n',sep='')
    } 
    
    # deal with non-finite and missing values
    char <- .nonfinite(x, char, dvn)
    
    eol <- intToBits(0)[1]
    unicode <- iconv(char, to='UTF-32BE', toRaw=TRUE)
    out <- unlist(lapply(unicode, function(i) if(is.null(i)) intToBits(0)[1:3] else c(i,eol))) # NA handling and nul byte appending
    
    hash <- digest(out, algo='md5', serialize=FALSE, raw=TRUE)
    encoded <- base64Encode(hash)
    out <- list(unf = as.character(encoded),
                hash = hash)
    class(out) <- c('UNF')
    attr(out, 'version') <- 3
    attr(out, 'digits') <- digits
    attr(out, 'characters') <- chars
    return(out)
}

unf4 <- function(x, digits = 7, chars = 128, dvn=TRUE, ver=4, ...){
    if(inherits(x, 'AsIs'))
        x <- as.character(x)
    if(is.numeric(x)){
        # NUMERICS:
        z <- signifz(x, digits)
        char <- .expform(z, digits)
        if(dvn)
            char <- ifelse(x==0, '+0.e-6\n', char) # https://redmine.hmdc.harvard.edu/issues/3085
    } else if(is.character(x)){
        # CHARACTER: truncate strings to k
        char <- paste(substring(x, 1, chars),'\n',sep='')
    } else {
        # FACTOR: treat factor as character and truncate to k
        char <- paste(substring(as.character(x), 1, chars),'\n',sep='')
    } 

    # deal with non-finite and missing values
    char <- .nonfinite(x, char, dvn)
    
    eol <- intToBits(0)[1]
    if(ver==4)
        unicode <- iconv(char, to='UTF-32BE', toRaw=TRUE) # v4 uses UTF-32BE
    else
        unicode <- iconv(char, to='UTF-8', toRaw=TRUE) # v4.1 uses UTF-8
    out <- unlist(lapply(unicode, function(i) if(is.null(i)) intToBits(0)[1:3] else c(i,eol))) # NA handling and nul byte appending
    
    hash <- digest(out, algo='sha256', serialize=FALSE, raw=TRUE)
    if(ver==4){
        encoded <- base64Encode(hash)
        out <- list(unf = as.character(encoded),
                    hash = hash)
    } else {
        long <- base64Encode(hash)
        short <- base64Encode(hash[1:16])
        out <- list(unf = as.character(long),
                    hash = hash)
    }
    class(out) <- c('UNF')
    attr(out, 'version') <- ver
    attr(out, 'digits') <- digits
    attr(out, 'characters') <- chars
    return(out)
}

unf5 <- function(x, digits = 7, chars = 128, dvn = TRUE, ...){
    if(inherits(x, 'AsIs'))
        x <- as.character(x)
    if(is.character(x)){
        # CHARACTER: truncate strings to k
        char <- paste(substring(x, 1, chars),'\n',sep='')
    } else if(is.factor(x)){
        # FACTOR: treat factor as character and truncate to k
        char <- paste(substring(as.character(x), 1, chars),'\n',sep='')
    } else if(is.numeric(x)){
        # NUMERICS: round to nearest, ties to even
        char <- round(x, digits)
        char <- .expform(char, digits)
        if(dvn)
            char <- ifelse(x==0, '+0.e-6\n', char) # https://redmine.hmdc.harvard.edu/issues/3085
    } else if(is.logical(x)){
        # LOGICAL: normalize boolean to 0, 1, or missing, then treat as numeric
        char <- .expform(as.integer(x))
        if(dvn)
            char <- ifelse(x, char, '+0.e-6\n') # https://redmine.hmdc.harvard.edu/issues/3085
        # FALSE values not handled correctly (see: https://redmine.hmdc.harvard.edu/issues/2960)
    }
    
    # BIT: Normalize bit fields by converting to big-endian form, truncating all leading empty bits, aligning to a byte boundary by re-padding with leading zero bits, and base64 encoding to form a character string representation. No rounding is applied, and missing values are represented by three null bytes.
    
    
    # TIME/DATE:
    # Normalize time, date, and durations based on a single, unambiguous representation selected from the many described in the ISO 8601 standard.
    
    # Convert calendar dates to a character string of the form YYYY-MM-DD. Partial dates in the form YYYY or YYYY-MM are permitted.
    
    # Time representation is based on the ISO 8601 extended format, hh:mm:ss.fffff. When .fffff represents fractions of a second, it must contain no trailing (non-significant) zeroes, and is omitted if valued at zero. Other fractional representations, such as fractional minutes and hours, are not permitted. If the time zone of the observation is known, convert the time value to the UTC time zone and append a ”Z” to the time representation.
    
    #timevar<-as.POSIXct("03/19/2013 18:20:00", format = "%m/%d/%Y %H:%M:%OS", tz="GMT")
    #attr(timevar,"tzone")<-NULL
    
    # https://redmine.hmdc.harvard.edu/issues/2997
    
    # deal with non-finite and missing values
    char <- .nonfinite(x, char, dvn)
    
    eol <- intToBits(0)[1]
    unicode <- iconv(char, to='UTF-8', toRaw=TRUE)
    out <- unlist(lapply(unicode, function(i) if(is.null(i)) intToBits(0)[1:3] else c(i,eol))) # NA handling and nul byte appending
    
    hash <- digest(out, algo='sha256', serialize=FALSE, raw=TRUE)
    long <- base64Encode(hash)
    short <- base64Encode(hash[1:16]) # truncated UNF
    
    out <- list(unf = as.character(short),
                hash = hash,
                unflong = as.character(long))
    class(out) <- c('UNF')
    attr(out, 'version') <- 5
    attr(out, 'digits') <- digits
    attr(out, 'characters') <- chars
    return(out)
}

print.UNF <- function(x, ...){
    if(attr(x, 'version')<5)
        cat('Universal Numeric Fingerprint: ')
    else
        cat('Universal Numeric Fingerprint (Truncated): ')
    if(attr(x,'digits')!=7 | attr(x,'characters')!=128)
        cat(paste('UNF',attr(x, 'version'),
            paste(attr(x,'digits'),attr(x,'characters'),sep=','),
            x$unf,sep=':'),'\n')
    else
        cat(paste('UNF',attr(x, 'version'), x$unf,sep=':'),'\n')
}
