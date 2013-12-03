.onLoad <- function(lib, pkg) {
  #if (!unfTest()) {
  #	cat("Warning unf: failed self-test\n")
  #}
  try(print(utils::citation("UNF")),silent=FALSE)
  return(TRUE)
}


# SPLUS ONLY
.on.attach <- function(lib, pkg) {
  if (!is.R()) {
    .require <- require # WORKAROUND SO R CMD CHECK DOESN'T COMPLAIN :-(
    if (.require(pkgutils,quietly=TRUE)){
      print(citation("UNF"))
    } else  {
      cat("To cite the UNF package in publications use:\n\n",
                paste("Altman, M., Gill, J. and M.P. McDonald (2003)", 
               "Numerical Issues in Statistical Computing for the Social Scientist.",
               "John Wiley and Sons, New York.",
              "(Software version: R Package, UNF, version 1.14)",
	       "ISBN 0-471-23633-0.\n\n")
      )
    }
  }
  return(TRUE)
}
