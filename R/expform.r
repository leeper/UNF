.expform <- function(z, digits = 7) {
    y <- sprintf(paste('%+1.',digits-1,'e',sep=''), z)
    plus <- grep('e+', y, fixed=TRUE)
    neg <- grep('e-', y, fixed=TRUE)
    nas <- seq_along(y)[-c(plus,neg)]
    
    b <- numeric(length=length(z))
    b[plus] <- sapply(y[plus], function(i) strsplit(i, 'e+', fixed=TRUE)[[1]][1])
    b[neg] <- sapply(y[neg], function(i) strsplit(i, 'e-', fixed=TRUE)[[1]][1])
    b[c(plus,neg)] <- sapply(b[c(plus,neg)], function(i) {
        sp <- strsplit(i,'.',fixed=TRUE)[[1]]
        dec <- gsub('0+$','',sp[2])
        paste(sp[1], dec, sep='.')
    })
    
    e <- numeric(length=length(z))
    if(length(plus))
        e[plus] <- as.numeric(sapply(y[plus], function(i) strsplit(i, 'e+', fixed=TRUE)[[1]][2]))
    if(length(neg))
        e[neg] <- as.numeric(sapply(y[neg], function(i) strsplit(i, 'e-', fixed=TRUE)[[1]][2]))
    
    char <- character(length=length(y))
    char[plus] <- ifelse(e[plus]==0, paste(b[plus],'e+\n',sep=''), paste(b[plus],'e+',e[plus],'\n',sep=''))
    char[neg] <- ifelse(e[neg]==0, paste(b[neg],'e-\n',sep=''), paste(b[neg],'e-',e[neg],'\n',sep=''))
    char[nas] <- NA
    return(char)
}
