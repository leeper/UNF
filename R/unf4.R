unf4 <- 
function(x, 
         digits = 7L, 
         chars = 128L, 
         truncation = 128L,
         version = 4, 
         factor_as_character = TRUE,
         nonfinites_as_missing = FALSE, 
         empty_character_as_missing = FALSE,
         dvn_zero = FALSE,
         ...){
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
        encoded <- base64encode(hash)
        out <- list(unf = as.character(encoded),
                    hash = hash)
    } else {
        long <- base64encode(hash)
        short <- base64encode(hash[1:(truncation/8L)])
        out <- list(unf = as.character(long),
                    hash = hash)
    }
    out$formatted <- paste0('UNF',version,':',
        if((digits !=7) | (chars !=128)) {
            paste0(paste(digits, chars, sep=','), ':', out$unf)
        } else {
            out$unf
        })
    
    class(out) <- c('UNF')
    attr(out, 'version') <- version
    attr(out, 'digits') <- digits
    attr(out, 'characters') <- chars
    return(out)
}
