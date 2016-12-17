#' factory function to save to derived location
#' @param .derived_location folder to save output to
#' @param .mkdir make directory of it does not exist, defaults to TRUE
#' @examples 
#' save_derived <- save_derived_factory("../data/derived")
#' save_derived(Theoph, "Theoph.rds")
save_derived_factory <- function(.derived_location, .mkdir = TRUE) {
    .loc <- normalizePath(.derived_location)
    if (!dir.exists(.loc)) {
        if (!.mkdir) {
            stop(paste("could not find directory at location: ", .derived_location))
        } else {
            dir.create(.loc, recursive = T) 
        }
    }
   return(function(.o, filename, ...) {
       saveRDS(.o, file.path(.loc, filename, ...))
   }) 
}
