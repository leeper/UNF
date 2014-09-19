unf <- function(x, version = 6, ...){
    if(is.matrix(x))
        x <- as.data.frame(x)
    if((is.data.frame(x) | is.list(x)) & length(x)==1)
        x <- x[[1]]
    if(is.data.frame(x) | is.list(x)){
        locale <- Sys.getlocale(category="LC_COLLATE")
        Sys.setlocale(category="LC_COLLATE", "C")
        # Apply UNF to each vector, base64 encode, sort, and apply UNF on that vector of UNFs
        if(version==3){
            vars <- sapply(x, function(i) unf3(i, ...)$unf)
            out <- unf3(sort(vars), ...)
        } else if(version==4){
            vars <- sapply(x, function(i) unf4(i, ...)$unf)
            out <- unf4(sort(vars), ...)
        } else if(version==4.1){
            vars <- sapply(x, function(i) unf4(i, version = 4.1, ...)$unf)
            out <- unf4(sort(vars), version = 4.1, ...)
        } else if(version==5){
            vars <- sapply(x, function(i) unf5(i, ...)$unf)
            out <- unf5(sort(vars), ...)
        } else if(version==6){
            warning('UNFv6 not yet fully implemented')
            vars <- sapply(x, function(i) unf6(i, ...)$unf)
            out <- unf6(sort(vars), ...)
        }
        Sys.setlocale(category="LC_COLLATE", locale)
        out$variables <- vars
        return(out)
    } else {
        if(version==3){
            out <- unf3(x, ...)
        } else if(version==4){
            out <- unf4(x, ...)
        } else if(version==4.1){
            out <- unf4(x, version=4.1, ...)
        } else if(version==5){
            out <- unf5(x, ...)
        } else if(version==6){
            out <- unf6(x, ...)
        }
        return(out)
    }
}

unf3 <- 
function(x, 
         digits = 7L, 
         chars = 128L, 
         nonfinites_as_missing = TRUE, 
         empty_character_as_missing = TRUE,
         factor_as_character = TRUE,
         ...){
    if(inherits(x, 'AsIs'))
        x <- as.character(x)
    if(is.factor(x)) {
        # FACTOR: treat factor as character and truncate to k
        # old DVN files downloaded as Tab save factors as integers w/o labels
        if(factor_as_character)
            x <- as.character(x)
        else
            x <- as.numeric(x)
    }
    if(is.complex(x)){
        # COMPLEX numbers: treat as character?
        x <- as.character(x)
        warning("Complex vector converted to character")
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
        if(empty_character_as_missing)
            char <- ifelse(x=='',NA,char)
    } 
    
    # deal with non-finite and missing values
    char <- .nonfinite(x, char, nonfinites_as_missing)
    
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
         nonfinites_as_missing = TRUE, 
         empty_character_as_missing = TRUE,
         factor_as_character = TRUE,
         dvn_zero = TRUE,
         version=4, ...){
    if(!truncation %in% c(128,192,196,256))
        stop("'truncation' must be in 128, 192, 196, 256")
    if(truncation < chars)
        stop("'truncation' must be greater than or equal to 'chars'")
    if(inherits(x, 'AsIs'))
        x <- as.character(x)
    if(is.ts(x))
        x <- as.numeric(x)
    if(is.factor(x)) {
        # FACTOR: treat factor as character and truncate to k
        if(factor_as_character)
            x <- as.character(x)
        else
            x <- as.numeric(x)
    }
    if(is.complex(x)){
        # COMPLEX numbers: treat as character?
        x <- as.character(x)
        warning("Complex vector converted to character")
    }
    if(is.numeric(x)){
        # NUMERICS:
        rounded <- signifz(x, digits) # uses non-standard signifz rounding
        char <- .expform(rounded, digits)
        if(dvn_zero)
            char <- ifelse(x==0, '+0.e-6\n', char) # https://redmine.hmdc.harvard.edu/issues/3085
    } else if(is.character(x)){
        # CHARACTER: truncate strings to k
        char <- paste(substring(x, 1, chars),'\n',sep='')
        if(empty_character_as_missing)
            char <- ifelse(x=='',NA,char)
    } 
    
    # deal with non-finite and missing values
    char <- .nonfinite(x, char, nonfinites_as_missing)
    
    eol <- intToBits(0)[1]
    if(version==4)
        unicode <- iconv(char, to='UTF-32BE', toRaw=TRUE) # v4 uses UTF-32BE
    else
        unicode <- iconv(char, to='UTF-8', toRaw=TRUE) # v4.1 uses UTF-8
    out <- unlist(lapply(unicode, function(i) if(is.null(i)) intToBits(0)[1:3] else c(i,eol))) # NA handling and nul byte appending
    
    hash <- digest(out, algo='sha256', serialize=FALSE, raw=TRUE)
    if(version==4){
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
    attr(out, 'version') <- version
    attr(out, 'digits') <- digits
    attr(out, 'characters') <- chars
    return(out)
}

unf5 <- 
function(x, 
         digits = 7L, 
         chars = 128L, 
         truncation = 128L,
         nonfinites_as_missing = TRUE, 
         empty_character_as_missing = TRUE,
         raw_as_character = TRUE,
         factor_as_character = TRUE,
         dvn_zero = TRUE,
         timezone = "",
         date_format = "%F",
         ...){
    if(!truncation %in% c(128,192,196,256))
        stop("'truncation' must be in 128, 192, 196, 256")
    if(truncation < chars)
        stop("'truncation' must be greater than or equal to 'chars'")
    if(inherits(x, 'AsIs')){
        x <- as.character(x)
    }
    if(inherits(x, 'ts') | inherits(x, 'zoo') | inherits(x, 'difftime')) {
        x <- as.numeric(x)
    }
    if(is.factor(x)) {
        # FACTOR: treat factor as character and truncate to k
        if(factor_as_character)
            x <- as.character(x)
        else
            x <- as.numeric(x)
    }
    if(is.complex(x)){
        # COMPLEX numbers: treat as character?
        x <- as.character(x)
        warning("Complex vector converted to character")
    }
    if(is.raw(x)){
        if(raw_as_character) # DVN ingests raw as character
            x <- as.character(x)
        # BIT: Normalize bit fields by converting to big-endian form, truncating all leading empty bits, aligning to a byte boundary by re-padding with leading zero bits, and base64 encoding to form a character string representation.
        else {
            char <- sapply(x, function(i){
                r <- raw()
                as.character(writeBin(i, r, endian='big'))
            })
            char <- paste(char,'\n',sep='')
            warning('UNF is untested on raw vectors')
        }
    }
    if(inherits(x, 'Date')){
        # DATE:
        # Normalize time and date, based on a single, unambiguous representation selected from the many described in the ISO 8601 standard.
        # Convert calendar dates to a character string of the form YYYY-MM-DD. Partial dates in the form YYYY or YYYY-MM are permitted.
        # https://redmine.hmdc.harvard.edu/issues/2997
        if(!date_format %in% c('%Y-%m-%d', '%Y-%m', '%Y', '%F'))
            stop("'date_format' must be '%Y-%m-%d', '%Y-%m', '%Y', or '%F'")
        char <- paste0(format(x, fmt = date_format),'\n')
    } else if(inherits(x, 'POSIXt')){
        # DATE-TIME: Time representation is based on the ISO 8601 extended format, hh:mm:ss.fffff. When .fffff represents fractions of a second, it must contain no trailing (non-significant) zeroes, and is omitted if valued at zero. Other fractional representations, such as fractional minutes and hours, are not permitted. If the time zone of the observation is known, convert the time value to the UTC time zone and append a "Z" to the time representation.
        if(inherits(x, 'POSIXlt'))
            x <- as.POSIXct(x)
        d <- getOption("digits.secs")
        options("digits.secs" = 5)
        char <- paste0(format(x, "%FT%H:%M:", timezone), gsub("\\.?0+$","",format(x, "%OS", timezone)), "Z\n")
        options("digits.secs" = d)
    } else if(is.character(x)){
        # CHARACTER: truncate strings to k
        char <- paste(substring(x, 1, chars),'\n',sep='')
        if(empty_character_as_missing)
            char <- ifelse(x=='',NA,char)
    } else if(is.numeric(x)){
        # NUMERICS: round to nearest, ties to even (use `round` rather than `signif` or `signifz`)
        char <- round(x, digits-1)
        char <- .expform(char, digits-1)
        if(dvn_zero)
            char <- ifelse(x==0, '+0.e-6\n', char) # https://redmine.hmdc.harvard.edu/issues/3085
    } else if(is.logical(x)){
        # LOGICAL: normalize boolean to 0, 1, or missing, then treat as numeric
        char <- .expform(as.integer(x), digits-1)
        if(dvn_zero)
            char <- ifelse(x, char, '+0.e-6\n') # https://redmine.hmdc.harvard.edu/issues/3085
    }
    
    # replace non-finite and missing values with NA
    # https://redmine.hmdc.harvard.edu/issues/2867
    # https://redmine.hmdc.harvard.edu/issues/2960
    char <- .nonfinite(x, char, nonfinites_as_missing)
    
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
         nonfinites_as_missing = TRUE, 
         raw_as_character = TRUE,
         factor_as_character = TRUE,
         timezone = "",
         date_format = "%F",
         ...){
    if(!truncation %in% c(128,192,196,256))
        stop("'truncation' must be in 128, 192, 196, 256")
    if(truncation < chars)
        stop("'truncation' must be greater than or equal to 'chars'")
    if(inherits(x, 'AsIs')){
        x <- as.character(x)
    }
    if(inherits(x, 'ts') | inherits(x, 'zoo') | inherits(x, 'difftime')) {
        x <- as.numeric(x)
    }
    if(is.factor(x)) {
        # FACTOR: treat factor as character and truncate to k
        if(factor_as_character)
            x <- as.character(x)
        else
            x <- as.numeric(x)
    }
    if(is.complex(x)){
        # COMPLEX numbers: treat as character?
        x <- as.character(x)
        warning("Complex vector converted to character")
    }
    if(is.raw(x)){
        if(raw_as_character) # DVN ingests raw as character
            x <- as.character(x)
        # BIT: Normalize bit fields by converting to big-endian form, truncating all leading empty bits, aligning to a byte boundary by re-padding with leading zero bits, and base64 encoding to form a character string representation.
        else {
            char <- sapply(x, function(i){
                r <- raw()
                as.character(writeBin(i, r, endian='big'))
            })
            char <- paste(char,'\n',sep='')
            warning('UNF is untested on raw vectors')
        }
    }
    if(inherits(x, 'Date')){
        # DATE: Dates are converted to character strings in the form "YYYY-MM-DD", but partial dates ("YYYY" and "YYYY-MM") are permitted.
        if(!date_format %in% c('%Y-%m-%d', '%Y-%m', '%Y', '%F'))
            stop("'date_format' must be '%Y-%m-%d', '%Y-%m', '%Y', or '%F'")
        char <- paste0(format(x, fmt = date_format),'\n')
    } else if(inherits(x, 'POSIXt')){
        # DATE-TIME: Datetimes may be expressed as a concatenated date (only in the form "YYYY-MM-DD") and time, separated by "T". As an example, Fri Aug 22 12:51:05 EDT 2014 is encoded as: `"2014-08-22T16:51:05Z"`.
        if(inherits(x, 'POSIXlt'))
            x <- as.POSIXct(x)
        d <- getOption("digits.secs")
        options("digits.secs" = 5)
        char <- paste0(format(x, "%FT%H:%M:", timezone), gsub("\\.?0+$","",format(x, "%OS", timezone)), "Z\n")
        options("digits.secs" = d)
    } else if(is.character(x)){
        # CHARACTER: truncate strings to k
        char <- paste(substring(x, 1, chars),'\n',sep='')
    } else if(is.numeric(x)){
        # NUMERICS: round to nearest, ties to even (use `round` rather than `signif` or `signifz`)
        char <- round(x, digits-1)
        char <- .expform(char, digits-1)
    } else if(is.logical(x)){
        # LOGICAL: normalize boolean to 0, 1, or missing, then treat as numeric
        char <- .expform(as.integer(x), digits-1)
    }
    
    # deal with non-finite and missing values
    char <- .nonfinite(x, char, nonfinites_as_missing)
    
    eol <- intToBits(0)[1]
    unicode <- iconv(char, to='UTF-8', toRaw=TRUE)
    # NA handling and null byte appending
    out <- unlist(lapply(unicode, function(i) if(is.null(i)) intToBits(0)[1:3] else c(i,eol)))
    
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
    if(is.null(attr(x,'version'))) {
        out <- paste0('UNF:', x$unf)
    } else {
        if(attr(x, 'version')<5) {
            out <- paste0('UNF', attr(x, 'version'), ':', x$unf, '\n')
        } else if(attr(x, 'version')==5) {
            out <- paste0('UNF5:',
                if((!is.null(attr(x,'digits')) & attr(x,'digits')!=7) |
                    (!is.null(attr(x,'characters')) & attr(x,'characters')!=128)) {
                    paste0(paste(attr(x,'digits'), attr(x,'characters'), sep=','), ':', x$unf)
                } else {
                    x$unf
                })
        } else if(attr(x, 'version')==6) {
            out <- paste0('UNF6:',
                paste(ifelse(!is.null(attr(x,'digits')) & !attr(x,'digits')==7, 
                        paste0("N",attr(x,'digits')), ""),
                      ifelse(!is.null(attr(x,'characters')) & !attr(x,'characters')==128, 
                        paste0("X",attr(x,'characters')), ""),
                      ifelse(!is.null(attr(x,'truncation')) & !attr(x,'truncation')==128, 
                        paste0("H",attr(x,'truncation')), ""),
                      sep = "", collapse=","),
                if((!is.null(attr(x,'digits')) & !attr(x,'digits')==7) |
                   (!is.null(attr(x,'characters')) & !attr(x,'characters')==128) | 
                   (!is.null(attr(x,'truncation')))  & !attr(x,'truncation')==128) {
                    paste0(':', x$unf)
                } else {
                    x$unf
                })
        } else {
            out <- paste0('UNF', attr(x, 'version'), ':', x$unf)
        }
    }
    print(out)
}
