unf <- function(x, ver = 5, ...){
    if(is.data.frame(x)){
        locale <- Sys.getlocale(category="LC_COLLATE")
        Sys.setlocale(category="LC_COLLATE", "C")
        # Calculate the UNF for each lower-level data object, using a consistent UNF version and level of precision across the individual UNFs being combined.
        # Sort the base64 representation of UNFs in POSIX locale sort order.
        # Apply the UNF algorithm to the resulting vector of character strings using k at least as large as the length of the underlying character string.
        # Combine UNFs from multiple variables to form a single UNF for an entire data frame, and then combine UNFs for a set of data frames to form a single UNF that represents an entire research study.        
        if(ver==3){
            vars <- sort(sapply(x, function(i) unf3(i, ...)))
            out <- unf3(vars)
        } else if(ver==4){
            vars <- sort(sapply(x, function(i) unf4(i, ...)))
            out <- unf4(vars)
        } else if(ver==4.1){
            vars <- sort(sapply(x, function(i) unf5(i, ver=4.1, ...)))
            out <- unf4(vars, ver=4.1)
        } else if(ver==5){
            vars <- sort(sapply(x, function(i) unf5(i, ...)))
            out <- unf5(vars)
        }
        Sys.setlocale(category="LC_COLLATE", locale)
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
    }
    return(out)
}

unf3 <- function(x, digits = 7, chars = 128, dvn=TRUE, ...){
    if(is.numeric(x)){
        z <- .signifz(x, digits)
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
    char <- ifelse((!is.finite(x) & !is.character(x)),tolower(as.character(x)),char)
    char <- ifelse(char=='inf', '+inf', char)
    char <- ifelse(is.nan(x), '+nan', char)
    char <- ifelse(is.na(x) & !is.nan(x), NA, char)
    
    eol <- intToBits(0)[1]
    unicode <- iconv(char, to='UTF-32BE', toRaw=TRUE)
    out <- unlist(lapply(unicode, function(i) if(is.null(i)) intToBits(0)[1:3] else c(i,eol))) # NA handling and nul byte appending
    
    encoded <- base64Encode(digest(out, algo='md5', serialize=FALSE, raw=TRUE))
    out <- as.character(encoded)
    class(out) <- c('UNF')
    attr(out, 'version') <- 3
    return(out)
}

unf4 <- function(x, digits = 7, chars = 128, dvn=TRUE, ver=4, ...){
    if(is.numeric(x)){
        # NUMERICS:
        z <- .signifz(x, digits)
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
    char <- ifelse((!is.finite(x) & !is.character(x)),tolower(as.character(x)),char)
    char <- ifelse(char=='inf', '+inf', char)
    char <- ifelse(is.nan(x), '+nan', char)
    char <- ifelse(is.na(x) & !is.nan(x), NA, char)
    
    eol <- intToBits(0)[1]
    if(ver==4)
        unicode <- iconv(char, to='UTF-32BE', toRaw=TRUE) # v4 uses UTF-32BE
    else
        unicode <- iconv(char, to='UTF-8', toRaw=TRUE) # v4.1 uses UTF-8
    out <- unlist(lapply(unicode, function(i) if(is.null(i)) intToBits(0)[1:3] else c(i,eol))) # NA handling and nul byte appending
    
    encoded <- base64Encode(digest(out, algo='sha256', serialize=FALSE, raw=TRUE))
    out <- as.character(encoded)
    class(out) <- c('UNF')
    attr(out, 'version') <- ver
    return(out)
}

unf5 <- function(x, digits = 7, chars = 128, dvn = TRUE, ...){
    # dvn TRUE argument validates against DVN implementation (not the UNF standard)
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
    char <- ifelse((!is.finite(x) & !is.character(x)),tolower(as.character(x)),char)
    char <- ifelse(char=='inf', '+inf', char)
    char <- ifelse(is.nan(x), '+nan', char)
    char <- ifelse(is.na(x) & !is.nan(x), NA, char)
    
    # Encode each character string with Unicode bit encoding. Version 5 uses UTF-8.
    # Combine the vector of character strings into a single sequence, with each character string separated by a POSIX end-of-line character and a null byte.
    eol <- intToBits(0)[1]
    unicode <- iconv(char, to='UTF-8', toRaw=TRUE)
    out <- unlist(lapply(unicode, function(i) if(is.null(i)) intToBits(0)[1:3] else c(i,eol))) # NA handling and nul byte appending
    
    # Compute a hash on the resulting sequence using the standard SHA256 hashing algorithm. The resulting hash is base64 encoded to support readability.
    hash <- digest(out, algo='sha256', serialize=FALSE, raw=TRUE)
    long <- base64Encode(hash)
    short <- base64Encode(hash[1:16]) # truncated UNF
    
    encoded <- short
    out <- as.character(encoded)
    class(out) <- c('UNF')
    attr(out, 'version') <- 5
    return(out)
}

print.UNF <- function(x, ...){
    if(attr(x, 'version')<5)
        cat('Universal Numeric Fingerprint: ')
    else
        cat('Universal Numeric Fingerprint (Truncated): ')
    cat(paste('UNF',attr(x, 'version'),x,sep=':'),'\n')
}
