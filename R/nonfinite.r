.nonfinite <- function(x, char, dvn=TRUE){
    if(dvn){
        # Inf, -Inf, and NaN appear to be treated as missing values
        ifelse(!is.finite(x), NA, char)
        char <- ifelse(is.nan(x), NA, char)
    }else{
        # Inf, -Inf, and NaN are described as being treated as +inf, -inf, and +nan in the standard
        char <- ifelse((!is.finite(x) & !is.character(x)), paste(tolower(as.character(x)),'\n',sep=''), char)
        char <- ifelse(char=='inf\n', '+inf\n', char)
        char <- ifelse(is.nan(x), '+nan\n', char)
    }
    char <- ifelse(is.na(x) & !is.nan(x), NA, char)
    return(char)
}