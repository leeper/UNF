.nonfinite <- function(x, char, nonfinites_as_missing=FALSE){
    if(nonfinites_as_missing){
        char <- ifelse(!is.finite(x), NA, char)
    } else {
        char <- ifelse((!is.finite(x) & !is.character(x)), paste(tolower(as.character(x)),'\n',sep=''), char)
        char <- ifelse(char=='inf\n', '+inf\n', char)
        char <- ifelse(is.nan(x), '+nan\n', char)
    }
    char <- ifelse(is.na(x) & !is.nan(x), NA, char)
    return(char)
}
