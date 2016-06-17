#' @title UNF Vector Representation
#' @description Standardize a vector according to UNF specifications
#' @param x A vector to be coerced to a character string representation according to the UNF specification.
#' @param \ldots Additional arguments passed to methods.
#' @details The UNF specifications describes how to coerce all R data types to a standardized character representation. This S3 method exposes that coercion functionality.
#' @return A character string with class \dQuote{unfvector} manipulated to follow the UNF specification. These are used internally by \code{\link{unf6}}.
#' @author Thomas J. Leeper (\email{thosjleeper@gmail.com})
#' @seealso \code{\link{unf}}, \code{\link{unf6}}, \code{\link{\%unf\%}} 
#' @export
as.unfvector <- function(x, ...) {
    UseMethod("as.unfvector")
}

#' @export
as.unfvector.default <- function(x, ...) {
    as.unfvector(as.character(x), ...)
}

#' @export
as.unfvector.character <- function(x, ...) {
    structure(x, class = c("unfvector", "character"))
}

#' @export
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

#' @export
as.unfvector.integer <- function(x, digits = 7, ...) {
    char <- .expform(signif(x, digits), digits-1)
    as.unfvector(char, ...)
}

#' @export
as.unfvector.logical <- function(x, digits = 7L, ...) {
    # LOGICAL: normalize boolean to 0, 1, or missing, then treat as numeric
    char <- .expform(as.integer(x), digits-1)
    as.unfvector(char, ...)
}

#' @export
as.unfvector.factor <- function(x, factor_as_character = TRUE, ...) {
    # FACTOR: treat factor as character and truncate to k
    if (factor_as_character) {
        x <- as.character(x)
    } else {
        x <- as.numeric(x)
    }
    as.unfvector(x, ...)
}

#' @export
as.unfvector.raw <- function(x, raw_as_character = TRUE, ...) {
    if (raw_as_character) {
        # dataverse ingests raw as character
        char <- as.character(x)
    } else {
        # BIT: Normalize bit fields by converting to big-endian form,
        #      truncating all leading empty bits, 
        #      aligning to a byte boundary by re-padding with leading zero bits, and 
        #      base64 encoding to form a character string representation
        # convert to bits (big-endian)
        rawvec <- rawConnection(raw(0), "r+")
        writeBin(x, rawvec, endian = "big")
        b1 <- rawConnectionValue(rawvec)
        close(rawvec)
        # truncate leading empty bits
        b2 <- b1[which(b1 == 1)[1]]
        # align to byte boundary
        b3 <- c(rawToBits(as.raw(0))[length(b2) %% 8], b2)
        char <- base64encode(b3)
    }
    as.unfvector(char, ...)
}

#' @export
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

#' @export
as.unfvector.AsIs <- function(x, ...) {
    as.unfvector(as.character(x), ...)
}

#' @export
as.unfvector.Date <- function(x, date_format = "%Y-%m-%d", ...) {
    # DATE: Dates are converted to character strings in the form "YYYY-MM-DD"
    #       but partial dates ("YYYY" and "YYYY-MM") are permitted.
    if (!date_format %in% c('%Y-%m-%d', '%Y-%m', '%Y', '%F')) {
        stop("'date_format' must be '%Y-%m-%d', '%Y-%m', '%Y', or '%F'")
    }
    as.unfvector(format(x, fmt = date_format), ...)
}

#' @export
as.unfvector.POSIXct <- function(x, timezone = "", decimal_seconds = 5, ...) {
    char <- paste0(format(x, "%Y-%m-%dT%H:%M:", timezone), 
                   gsub("\\.?0+$","", format(x, paste0("%OS", decimal_seconds), timezone)), 
                   ifelse(timezone == "UTC", "Z", ""))
    as.unfvector(char, ...)
}

#' @export
as.unfvector.POSIXlt <- function(x, ...) {
    as.unfvector(as.POSIXct(x), ...)
}

#' @export
as.unfvector.ts <- function(x, ...) {
    as.unfvector(as.numeric(x), ...)
}

#' @export
as.unfvector.difftime <- function(x, ...) {
    a <- attributes(x)
    if (is.null(a[["units"]]) || a[["units"]] != "days") {
        warnings("units for difftime assumed to be days")
    }
    as.unfvector(as.numeric(x), ...)
}

print.unfvector <- function(x, ...) {
    print(unclass(x), ...)
}
