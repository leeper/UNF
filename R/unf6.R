#' @rdname unf
#' @export
unf6 <-
function(x, 
         digits = 7L, 
         characters = 128L, 
         truncation = 128L,
         raw_as_character = TRUE,
         factor_as_character = TRUE,
         complex_as_character = TRUE,
         nonfinites_as_missing = FALSE, 
         timezone = "",
         date_format = "%Y-%m-%d",
         decimal_seconds = 5,
         ...){
    if (!truncation %in% c(128,192,196,256)) {
        stop("'truncation' must be in 128, 192, 196, 256")
    }
    if (truncation < characters) {
        stop("'truncation' must be greater than or equal to 'characters'")
    }
    if (inherits(x, "POSIXlt")) {
        x <- as.POSIXct(x)
    }
    # standardize to character
    # and deal with non-finite and missing values
    char <- as.unfvector(x, digits = digits,
                            characters = characters,
                            encoding = "UTF-8", 
                            truncation = truncation,
                            raw_as_character = raw_as_character, 
                            factor_as_character = factor_as_character,
                            complex_as_character = complex_as_character,
                            nonfinites_as_missing = nonfinites_as_missing,
                            timezone = timezone,
                            date_format = date_format,
                            decimal_seconds = decimal_seconds,
                            ...)
    
    # convert to raw
    out <- unfvector_to_raw(char, encoding = "UTF-8", characters = characters)
    
    hash <- digest(out, algo = 'sha256', serialize = FALSE, raw = TRUE)
    long <- base64encode(hash)
    short <- base64encode(hash[1:(truncation/8L)]) # truncated UNF
    
    # format printable UNF
    header <- paste(if (digits != 7) paste0("N", digits) else NULL,
                    if (characters != 128) paste0("X", characters) else NULL,
                    if (truncation != 128) paste0("H", truncation) else NULL,
                    sep = ",", collapse="")
    header <- ifelse(length(header), gsub("^[[:punct:]]+", "", header), "")
    header <- ifelse(length(header), gsub("[[:punct:]]+$", "", header), "")
    header <- ifelse(length(header), gsub("[[:punct:]]{2}", ",", header), "")
    formatted <- paste0('UNF6:', ifelse(header == "", 
                                        as.character(short), 
                                        paste0(header,':', as.character(short))))
    # return UNF-class structure
    structure(list(unf = as.character(short),
                hash = hash,
                unflong = as.character(long),
                formatted = formatted),
              class = "UNF",
              version = 6,
              digits = digits,
              characters = characters,
              truncation = truncation)
}
