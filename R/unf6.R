unf6 <-
function(x, 
         digits = 7L, 
         chars = 128L, 
         truncation = 128L,
         raw_as_character = TRUE,
         factor_as_character = TRUE,
         complex_as_character = TRUE,
         nonfinites_as_missing = FALSE, 
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
    if(is.complex(x) & complex_as_character) {
        x <- as.character(x)
    }
    if(is.complex(x) & !complex_as_character){
        # COMPLEX numbers: format as `A,iB`
        re <- .expform(round(Re(x), digits-1), digits-1)
        co <- .expform(round(Im(x), digits-1), digits-1)
        char <- paste(substring(re, 1, nchar(re)-1), co, sep=",i")
    } else if(inherits(x, 'Date')){
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
    
    # format printable UNF
    formatted <- paste0('UNF6:',
        gsub(",+$", "", paste(ifelse(digits != 7, paste0("N", digits), ""),
              ifelse(chars != 128, paste0("X", chars), ""),
              ifelse(truncation != 128, paste0("H", truncation), ""),
              sep = ",", collapse="")),
        if((digits != 7) | (chars != 128) | (truncation != 128)) {
            paste0(':', as.character(short))
        } else {
            as.character(short)
        })
    
    out <- list(unf = as.character(short),
                hash = hash,
                unflong = as.character(long),
                formatted = formatted)
    class(out) <- c('UNF')
    attr(out, 'version') <- 6
    attr(out, 'digits') <- digits
    attr(out, 'characters') <- chars
    attr(out, 'truncation') <- truncation
    return(out)
}
