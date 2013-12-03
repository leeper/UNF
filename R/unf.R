# http://thedata.org/book/unf-version-3-0
# http://thedata.org/book/unf-version-5-0

.expform <- function(z) {
    y <- sprintf(paste('%+1.',digits-1,'e',sep=''), z)
    b <- sapply(y, function(i) strsplit(i, 'e+', fixed=TRUE)[[1]][1])
    b <- sapply(b, function(i) {
        sp <- strsplit(i,'.',fixed=TRUE)[[1]]
        dec <- gsub('0+$','',sp[2])
        paste(sp[1], dec, sep='.')
    })
    
    e <- as.numeric(sapply(y, function(i) strsplit(i, 'e+', fixed=TRUE)[[1]][2]))
    s <- substring(sprintf('%+g',sign(e)),1,1) # sign of exponent
    char <- ifelse(e==0, paste(b,'e',s,'\n',sep=''), paste(b,'e',s,e,'\n',sep=''))
    return(char)
}
    
.signifz<-function(x,digits=6) {
    if (class(x)=="data.frame") {
        ret <- sapply(x, signifz, digits=digits)
        rownames(ret)=rownames(x)
        return(ret)
    }
    magnitude = floor(log10(abs(x)))
    scale = 10^(digits-magnitude-1)
    signs =  sign(x)
    ret=x
    g0 = which(signs>=0)
    ret[g0]= floor(x[g0]*scale[g0])/scale[g0]
    l0 = which(signs<0) 
    ret[l0]=  ceiling(x[l0]*scale[l0])/scale[l0]
    return(ret)
}

unf <- function(x, digits = 7, chars = 128, ver = 5, ...){
    if(is.data.frame(x)){
        locale <- Sys.getlocale(category="LC_COLLATE")
        Sys.setlocale(category="LC_COLLATE", "C")
        if(ver==3){
            out <- unfv3(x, ...)
        } else if(ver==4){
            out <- unfv4(x, ...)
        } else if(ver==4.1){
            out <- unfv4(x, ver=4.1, ...)
        } else if(ver==5){
            vars <- sapply(x, function(i) unfv5(i))
            sorted <- sort(vars)
            out <- unfv5(sorted)
        }
        Sys.setlocale(category="LC_COLLATE", locale)
    } else {
        if(ver==3){
            out <- unfv3(x, ...)
        } else if(ver==4){
            out <- unfv4(x, ...)
        } else if(ver==4.1){
            out <- unfv4(x, ver=4.1, ...)
        } else if(ver==5){
            out <- unfv5(x, ...)
        }
    }
    unclass(out)
    attr(out, 'version') <- ver
    class(out) <- c('UNF','character')
    return(out)
}

unf(data.frame(1:3,4:6,7:9)) # UNF:5:ukDZSJXck7fn4SlPJMPFTQ==
unf(data.frame(7:9,1:3,4:6)) # UNF:5:ukDZSJXck7fn4SlPJMPFTQ==



unfv3 <- function(x, digits = 7, char = 128, dvn=TRUE, ...){
    
    
    if(is.numeric(x)){
        z <- .signifz(x, digits)
        char <- .expform(z)
        if(dvn)
            char <- ifelse(x==0, '+0.e-6\n', char) # https://redmine.hmdc.harvard.edu/issues/3085
    } else if(is.character(x)){
        # CHARACTER: truncate strings to k
        char <- paste(substring(x, 1, chars),'\n',sep='')
    } else {
        # FACTOR: treat factor as character and truncate to k
        char <- paste(substring(as.character(x), 1, chars),'\n',sep='')
    } 

    char <- ifelse((!is.finite(x) & !is.character(x)),tolower(as.character(x)),char)
    char <- ifelse(char=='inf', '+inf', char)
    char <- ifelse(is.nan(x), '+nan', char)
    char <- ifelse(is.na(x) & !is.nan(x), NA, char)
    
    eol <- intToBits(0)[1]
    unicode <- iconv(char, to='UTF-32BE', toRaw=TRUE)
    out <- unlist(lapply(unicode, function(i) if(is.null(i)) intToBits(0)[1:3] else c(i,eol))) # NA handling and nul byte appending
    
    encoded <- RCurl::base64Encode(digest::digest(out, algo='md5', serialize=FALSE, raw=TRUE))
    return(encoded)
}

unfv3(1:20) # HRSmPi9QZzlIA+KwmDNP8w==


unfv4 <- function(x, digits = 7, char = 128, dvn=TRUE, ver=4, ...){

    if(is.numeric(x)){
        z <- .signifz(x, digits)
        char <- .expform(z)
        if(dvn)
            char <- ifelse(x==0, '+0.e-6\n', char) # https://redmine.hmdc.harvard.edu/issues/3085
    } else if(is.character(x)){
        # CHARACTER: truncate strings to k
        char <- paste(substring(x, 1, chars),'\n',sep='')
    } else {
        # FACTOR: treat factor as character and truncate to k
        char <- paste(substring(as.character(x), 1, chars),'\n',sep='')
    } 

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
    
    encoded <- RCurl::base64Encode(digest::digest(out, algo='sha256', serialize=FALSE, raw=TRUE))
    return(encoded)
}



unfv5 <- function(x, digits = 7, char = 128, dvn=TRUE, ...){
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
        char <- .expform(char)
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
    hash <- digest::digest(out, algo='sha256', serialize=FALSE, raw=TRUE)
    long <- RCurl::base64Encode(hash)
    short <- RCurl::base64Encode(hash[1:16])
    short <- unclass(short)
    
    # Calculate the UNF for each lower-level data object, using a consistent UNF version and level of precision across the individual UNFs being combined.
    
    # Sort the base64 representation of UNFs in POSIX locale sort order.
    
    # Apply the UNF algorithm to the resulting vector of character strings using k at least as large as the length of the underlying character string.
    
    # Combine UNFs from multiple variables to form a single UNF for an entire data frame, and then combine UNFs for a set of data frames to form a single UNF that represents an entire research study.
    
    return(short)
}

unfv5(c('test','1','2','3')) # UNF:5:fH4NJMYkaAJ16OWMEE+zpQ==
unfv5(as.factor(c('test','1','2','3'))) # UNF:5:fH4NJMYkaAJ16OWMEE+zpQ==

unfv5(0:3) # UNF:5:CLqBRAEcfSECmZlEMWMLAQ==
unfv5(-3:3) # UNF:5:pwzm1tdPaqypPWRWDeW6Jw==

unfv5(c(TRUE,TRUE,FALSE))# UNF:5:DedhGlU7W6o2CBelrIZ3iw==
unfv5(c(TRUE,TRUE)) # UNF:5:z0H8P9cZ20/p0DzG5b2ayg==
unfv5(c(FALSE,FALSE)) # UNF:5:/84o7IcyubS+9HBrOXWJbw==

unfv5(1:20) # UNF:5:/FIOZM/29oC3TK/IE52m2A==

unfv5(c(1:5,NA)) # UNF:5:Msnz4m7QVvqBUWxxrE7kNQ==


setUNF <- function(x, ver=5){
    attr(x, 'UNF') <- unf(x, ver=ver)
    return(x)
}

compareUNF <- function(..., ver=5){
    x <- list(...)
    out <- mapply(function(i,j) unf(i, ver=j), x, ver)
    return(out)
}
