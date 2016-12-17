#' factory function to save to derived location
#' @param .derived_location folder to save output to
#' @param .mkdir make directory of it does not exist, defaults to TRUE
#' @param verbose whether to message when creating a new directory, defaults to FALSE
#' @examples 
#' save_derived <- save_derived_factory("../data/derived")
#' save_derived(Theoph, "Theoph.rds")
save_derived_factory <- function(.derived_location, .mkdir = TRUE, verbose = FALSE) {
    .loc <- normalizePath(.derived_location, mustWork = FALSE)
    if (!dir.exists(.loc)) {
        if (!.mkdir) {
            stop(paste("could not find directory at location: ", .derived_location))
        } else {
            if (verbose) {
                message('new directory created at: ', .loc)
            }
            dir.create(.loc, recursive = T) 
        }
    }
   return(function(.o, filename, ...) {
       saveRDS(.o, file.path(.loc, filename, ...))
   }) 
}
