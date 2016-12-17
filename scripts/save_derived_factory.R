#' factory function to save to derived location
#' @examples 
#' save_derived <- save_derived_factory("../data/derived")
#' save_derived(Theoph, "Theoph.rds")
save_derived_factory <- function(.derived_location) {
    .loc <- normalizePath(.derived_location)
   return(function(.o, filename, ...) {
       saveRDS(.o, file.path(.loc, filename, ...))
   }) 
}
