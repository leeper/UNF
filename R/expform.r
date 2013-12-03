.expform <- function(z, digits = 7) {
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
