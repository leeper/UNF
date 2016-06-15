as.unfvector <- function(x, ...) {
    UseMethod("as.unfvector")
}

as.unfvector.default <- function(x, ...) {
    as.unfvector(as.character(x), ...)
}

as.unfvector.character <- function(x, ...) {
    structure(x, class = c("unfvector", "character"))
}

as.unfvector.numeric <- function(x, digits = 7L, nonfinites_as_missing = FALSE, ...) {
    # NUMERICS: round to nearest, ties to even (use `signif` or `signifz`)
    char <- .expform(signif(x, digits), digits-1)
    if (nonfinites_as_missing) {
        char <- ifelse(!is.finite(x), NA_character_, char)
    } else {
        char <- ifelse(!is.finite(x), tolower(x), char)
        char <- ifelse(char == 'inf', '+inf', char)
        char <- ifelse(is.na(x) & !is.nan(x), NA_character_, ifelse(is.nan(x), '+nan', char))
    }
    as.unfvector(char, ...)
}

as.unfvector.integer <- function(x, digits = 7L, nonfinites_as_missing = FALSE, ...) {
    # NUMERICS: round to nearest, ties to even (use `signif` or `signifz`)
    char <- .expform(signif(x, digits), digits-1)
    if (nonfinites_as_missing) {
        char <- ifelse(!is.finite(x), NA_character_, char)
    } else {
        char <- ifelse(!is.finite(x), tolower(x), char)
        char <- ifelse(char == 'inf', '+inf', char)
        char <- ifelse(is.nan(x), ifelse(is.na(x), NA_character_, '+nan'), char)
    }
    as.unfvector(char, ...)
}

as.unfvector.logical <- function(x, digits = 7L, ...) {
    # LOGICAL: normalize boolean to 0, 1, or missing, then treat as numeric
    char <- .expform(as.integer(x), digits-1)
    as.unfvector(char, ...)
}

as.unfvector.factor <- function(x, factor_as_character = TRUE, ...) {
    # FACTOR: treat factor as character and truncate to k
    if (factor_as_character) {
        x <- as.character(x)
    } else {
        x <- as.numeric(x)
    }
    as.unfvector(x, ...)
}

as.unfvector.raw <- function(x, raw_as_character = TRUE, ...) {
    if (raw_as_character) {
        # DVN ingests raw as character
        char <- as.character(x)
    } else {
        # BIT: Normalize bit fields by converting to big-endian form,
        #      truncating all leading empty bits, 
        #      aligning to a byte boundary by re-padding with leading zero bits, and 
        #      base64 encoding to form a character string representation
        char <- sapply(x, function(i){
            r <- raw()
            as.character(writeBin(i, r, endian = 'big'))
        })
        warning('UNF is untested on raw vectors')
    }
    as.unfvector(char, ...)
}

as.unfvector.complex <- function(x, complex_as_character = TRUE, digits = 7L, ...) {
    if (complex_as_character) {
        char <- as.character(x)
    } else {
        # COMPLEX numbers: format as `A,iB`
        re <- .expform(signif(Re(x), digits), digits-1)
        co <- .expform(signif(Im(x), digits), digits-1)
        char <- paste(re, co, sep = ",i")
    }
    as.unfvector(char, ...)
}

as.unfvector.AsIs <- function(x, ...) {
    as.unfvector(as.character(x), ...)
}

as.unfvector.Date <- function(x, date_format = "%Y-%m-%d", ...) {
    # DATE: Dates are converted to character strings in the form "YYYY-MM-DD"
    #       but partial dates ("YYYY" and "YYYY-MM") are permitted.
    if (!date_format %in% c('%Y-%m-%d', '%Y-%m', '%Y', '%F')) {
        stop("'date_format' must be '%Y-%m-%d', '%Y-%m', '%Y', or '%F'")
    }
    as.unfvector(format(x, fmt = date_format), ...)
}

as.unfvector.POSIXct <- function(x, timezone = "", decimal_seconds = 5, ...) {
    char <- paste0(format(x, "%Y-%m-%dT%H:%M:", timezone), 
                   gsub("\\.?0+$","", format(x, paste0("%OS", decimal_seconds), timezone)), 
                   ifelse(timezone == "UTC", "Z", ""))
    as.unfvector(char, ...)
}

as.unfvector.POSIXlt <- function(x, ...) {
    as.unfvector(as.POSIXct(x), ...)
}

as.unfvector.ts <- function(x, ...) {
    as.unfvector(as.numeric(x), ...)
}

as.unfvector.zoo <- function(x, ...) {
    as.unfvector(as.numeric(x), ...)
}

as.unfvector.difftime <- function(x, ...) {
    as.unfvector(as.numeric(x), ...)
}

print.unfvector <- function(x, ...) {
    print(unclass(x), ...)
}
