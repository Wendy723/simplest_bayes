#' thin chains from a dataframe 
#' @param df data
#' @param .mod modulus to determine how to thin
#' @param iter_name name of column designated for iterations, defaults to ITERATION
#' @examples \dontrun {
#' df %>%
#'     thin_by(5) # only keep every 5th sample
#' }
thin_by <- function(.df, .mod, .iter_name = "ITERATION") {
    dplyr::filter_(.df, .dots = lazyeval::interp(~ .iter_name %% .mod == 0, .iter_name = as.name(.iter_name)))
}



testthat::describe("thin_by works as expected", {
    expect_equal <- testthat::expect_equal
    mock_dat <- tibble::data_frame(ITERATION = 1:50)
    mock_dat_num <- tibble::data_frame(num = 1:50)
    it("works with defaults", {
       result_by10 <- mock_dat %>%
           thin_by(10) 
       result_by2 <- mock_dat %>%
           thin_by(2)
       expect_equal(nrow(result_by10), 5)
       expect_equal(nrow(result_by2), 25)
        
    })
    it("works with with different name", {
       result_by10 <- mock_dat_num %>%
           thin_by(10, "num") 
       result_by2 <- mock_dat_num %>%
           thin_by(2, "num")
       expect_equal(nrow(result_by10), 5)
       expect_equal(nrow(result_by2), 25)
        
        
    })
})
