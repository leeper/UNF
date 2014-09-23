unf3 <- 
function(x, 
         digits = 7L, 
         chars = 128L, 
         factor_as_character = TRUE,
         nonfinites_as_missing = FALSE, 
         empty_character_as_missing = FALSE,
         dvn_zero = FALSE,
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
    encoded <- base64encode(hash)
    out <- list(unf = as.character(encoded),
                hash = hash)
    out$formatted <- paste0('UNF3:',
        if((digits !=7) | (chars !=128)) {
            paste0(paste(digits, chars, sep=','), ':', out$unf)
        } else {
            out$unf
        })
    
    class(out) <- c('UNF')
    attr(out, 'version') <- 3
    attr(out, 'digits') <- digits
    attr(out, 'characters') <- chars
    return(out)
}
