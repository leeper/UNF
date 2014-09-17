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
        } else if(ver==6){
            warning('UNFv6 not yet fully implemented')
            vars <- sapply(x, function(i) unf6(i, ...)$unf)
            out <- unf6(sort(vars), ...)
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
        } else if(ver==6){
            out <- unf6(x, ...)
        }
        return(out)
    }
}

unf3 <- 
function(x, 
         digits = 7L, 
         chars = 128L, 
         dvn=TRUE, ...){
    if(inherits(x, 'AsIs'))
        x <- as.character(x)
    if(is.factor(x)){
        # FACTOR: treat factor as character and truncate to k
        x <- as.character(x)
        # old DVN files downloaded as Tab save factors as integers w/o labels
        #warning('factors treated as character')
    }
    if(is.integer(x)){
        rounded <- signif(x, digits) # uses standard signif rounding, despite standard
        char <- .expform(rounded, digits)
        char <- ifelse(x==0, '+0.e+\n', char) # dvn introduced 0-value bug after v3, apparently
    } else if(is.numeric(x)){
        rounded <- signif(x, digits) # uses standard signif rounding, despite standard
        char <- .expform(rounded, digits)
        char <- ifelse(x==0, '+0.e+\n', char) # dvn introduced 0-value bug after v3, apparently
    } else if(is.character(x)){
        # CHARACTER: truncate strings to k
        char <- paste(substring(x, 1, chars),'\n',sep='')
        if(dvn)
            char <- ifelse(x=='',NA,char)
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

unf4 <- 
function(x, 
         digits = 7L, 
         chars = 128L, 
         truncation = 128L,
         dvn=TRUE, 
         ver=4, ...){
    if(!truncation %in% c(128,192,196,256))
        stop("'truncation' must be in 128, 192, 196, 256")
    if(truncation < chars)
        stop("'truncation' must be greater than or equal to 'chars'")
    if(inherits(x, 'AsIs'))
        x <- as.character(x)
    if(is.numeric(x)){
        # NUMERICS:
        rounded <- signifz(x, digits) # uses non-standard signifz rounding
        char <- .expform(rounded, digits)
        if(dvn)
            char <- ifelse(x==0, '+0.e-6\n', char) # https://redmine.hmdc.harvard.edu/issues/3085
    } else if(is.character(x)){
        # CHARACTER: truncate strings to k
        char <- paste(substring(x, 1, chars),'\n',sep='')
        if(dvn)
            char <- ifelse(x=='',NA,char)
    } else {
        # FACTOR: treat factor as character and truncate to k
        char <- paste(substring(as.character(x), 1, chars),'\n',sep='')
        if(dvn)
            char <- ifelse(x=='',NA,char)
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
        short <- base64Encode(hash[1:(truncation/8L)])
        out <- list(unf = as.character(long),
                    hash = hash)
    }
    class(out) <- c('UNF')
    attr(out, 'version') <- ver
    attr(out, 'digits') <- digits
    attr(out, 'characters') <- chars
    return(out)
}

unf5 <- 
function(x, 
         digits = 7L, 
         chars = 128L, 
         truncation = 128L,
         dvn = TRUE, ...){
    if(!truncation %in% c(128,192,196,256))
        stop("'truncation' must be in 128, 192, 196, 256")
    if(truncation < chars)
        stop("'truncation' must be greater than or equal to 'chars'")
    if(inherits(x, 'AsIs')){
        tmp <- as.character(x)
        #if(SOME CONDITION TBD)
        #    tmp[is.na(tmp)] <- ''
        # DVN Stata/Tab-delimited and RData files use AsIs variables differently
        # AsIs is used in RData and in calculating UNF; this is coerced to character in other file formats
        # This creates different missing data handling in different file types
        x <- tmp
    }
    # FACTOR: treat factor as character and truncate to k
    if(is.factor(x))
        x <- as.character(x)
    
    if(is.raw(x)){
        # BIT: Normalize bit fields by converting to big-endian form, truncating all leading empty bits, aligning to a byte boundary by re-padding with leading zero bits, and base64 encoding to form a character string representation. No rounding is applied, and missing values are represented by three null bytes.
        char <- sapply(x, function(i){
            r <- raw()
            a <- writeBin(i, r, endian='big')
            as.character(base64Encode(a))
        })
        char <- paste(char,'\n',sep='')
        warning('UNF is untested on raw vectors')
    } else if(inherits(x, 'Date')){
        # http://thedata.harvard.edu/guides/dataverse-user-main.html#appendix
        # A pro tip: if it is important to produce SPSS/Stata and R versions of the same data set that result in the same UNF when ingested, you may define the time variables as strings in the R data frame, and use the "YYYY-MM-DD HH:mm:ss" formatting notation. This is the formatting used by the UNF algorithm to normalize time values, so doing the above will result in the same UNF as the vector of the same time values in Stata.
        
        # DATE:
        # Normalize time, date, and durations based on a single, unambiguous representation selected from the many described in the ISO 8601 standard.
        
        # Convert calendar dates to a character string of the form YYYY-MM-DD. Partial dates in the form YYYY or YYYY-MM are permitted.
        
        # https://redmine.hmdc.harvard.edu/issues/2997
    
        char <- as.character(x)
        warning('Date-time classes currently converted to character')

    
    } else if(inherits(x, 'POSIXt')){
        # DATE-TIME:
        
        d <- getOption('digits.secs')
        options("digits.secs" = 3)
        
        # Time representation is based on the ISO 8601 extended format, hh:mm:ss.fffff. When .fffff represents fractions of a second, it must contain no trailing (non-significant) zeroes, and is omitted if valued at zero. Other fractional representations, such as fractional minutes and hours, are not permitted. If the time zone of the observation is known, convert the time value to the UTC time zone and append a "Z" to the time representation.
        
        char <- as.character(x)
        warning('Date-time classes currently converted to character')
    
        options("digits.secs" = d)
    
    } else if(is.character(x)){
        # CHARACTER: truncate strings to k
        char <- paste(substring(x, 1, chars),'\n',sep='')
        if(dvn)
            char <- ifelse(x=='',NA,char)
    } else if(is.numeric(x)){
        # NUMERICS: round to nearest, ties to even (use `round` rather than `signif` or `signifz`)
        if(dvn){
            # DVN mishandles this, but it's not exactly clear what it does wrong:
            # https://redmine.hmdc.harvard.edu/issues/3085
            char <- round(x, digits-1)
            char <- .expform(char, digits-1)
        } else{
            char <- round(x, digits-1)
            char <- .expform(char, digits-1)
        }
        if(dvn)
            char <- ifelse(x==0, '+0.e-6\n', char) # https://redmine.hmdc.harvard.edu/issues/3085
    } else if(is.logical(x)){
        # LOGICAL: normalize boolean to 0, 1, or missing, then treat as numeric
        char <- .expform(as.integer(x), digits-1)
        if(dvn)
            char <- ifelse(x, char, '+0.e-6\n') # https://redmine.hmdc.harvard.edu/issues/3085
        # FALSE values not handled correctly (see: https://redmine.hmdc.harvard.edu/issues/2960)
    }
    
    
    # deal with non-finite and missing values
    # https://redmine.hmdc.harvard.edu/issues/2867
    # https://redmine.hmdc.harvard.edu/issues/2960
    char <- .nonfinite(x, char, dvn)
    
    eol <- intToBits(0)[1]
    unicode <- iconv(char, to='UTF-8', toRaw=TRUE)
    out <- unlist(lapply(unicode, function(i) if(is.null(i)) intToBits(0)[1:3] else c(i,eol))) # NA handling and nul byte appending
    
    hash <- digest(out, algo='sha256', serialize=FALSE, raw=TRUE)
    long <- base64Encode(hash)
    short <- base64Encode(hash[1:(truncation/8L)]) # truncated UNF
    
    out <- list(unf = as.character(short),
                hash = hash,
                unflong = as.character(long))
    class(out) <- c('UNF')
    attr(out, 'version') <- 5
    attr(out, 'digits') <- digits
    attr(out, 'characters') <- chars
    attr(out, 'truncation') <- truncation
    return(out)
}

unf6 <-
function(x, 
         digits = 7L, 
         chars = 128L, 
         truncation = 128L,
         dvn = TRUE, ...){
    stop('UNFv6 not yet implemented')
    if(!truncation %in% c(128,192,196,256))
        stop("'truncation' must be in 128, 192, 196, 256")
    if(truncation < chars)
        stop("'truncation' must be greater than or equal to 'chars'")
    
    
    # algorithm here
    
    
    char <- .nonfinite(x, char, dvn)
    
    eol <- intToBits(0)[1]
    unicode <- iconv(char, to='UTF-8', toRaw=TRUE)
    out <- unlist(lapply(unicode, function(i) if(is.null(i)) intToBits(0)[1:3] else c(i,eol))) # NA handling and nul byte appending
    
    hash <- digest(out, algo='sha256', serialize=FALSE, raw=TRUE)
    long <- base64Encode(hash)
    short <- base64Encode(hash[1:(truncation/8L)]) # truncated UNF
    
    out <- list(unf = as.character(short),
                hash = hash,
                unflong = as.character(long))
    class(out) <- c('UNF')
    attr(out, 'version') <- 6
    attr(out, 'digits') <- digits
    attr(out, 'characters') <- chars
    attr(out, 'truncation') <- truncation
    return(out)
}

print.UNF <- function(x, ...){
    if(!is.null(attr(x,'version')) && attr(x, 'version')<5) {
        cat('Universal Numeric Fingerprint: UNF', attr(x, 'version'), ':', x$unf, '\n', sep="")
    } else if(!is.null(attr(x,'version')) && attr(x, 'version')==5) {
        cat('Universal Numeric Fingerprint (Truncated): UNF', attr(x, 'version'), ':', sep="")
        if((!is.null(attr(x,'digits')) && attr(x,'digits')!=7) |
            (!is.null(attr(x,'characters')) && attr(x,'characters')!=128)) {
            cat(paste(attr(x,'digits'), attr(x,'characters'), sep=','), ':', sep="")
        }
        cat(x$unf, '\n')
    } else if(!is.null(attr(x,'version')) && attr(x, 'version')==5) {
        cat('Universal Numeric Fingerprint (Truncated): UNF', attr(x, 'version'), ':', sep="")
        if((!is.null(attr(x,'digits')) && attr(x,'digits')!=7)) {
            if(!is.null(attr(x,'characters')) && attr(x,'characters')!=128)
                cat('N', attr(x,'digits'), ',H', attr(x,'characters'), ':', sep="")
            else
                cat('N', attr(x,'digits'), ':', sep="")
        }
        if(!is.null(attr(x,'characters')) && attr(x,'characters')!=128) {
            cat('H', attr(x,'characters'), ':', sep="")
        }
        cat(x$unf, '\n')
    } else {
        cat('Universal Numeric Fingerprint: UNF', attr(x, 'version'), ':', x$unf, '\n', sep="")
    }
}
