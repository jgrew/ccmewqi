#' is_exceedance function.
#' 
#' Check if test value fall outside of acceptable objective limit.
#'
#' @param test_value
#' @param lower_limit
#' @param upper_limit
#' @param limit
#' 
#' @export
#' @details
#' Inputs must be numeric.
#' 
#' @return boolean True if test value exceeded objective
#' 
#' @seealso \url{http://www.ccme.ca/en/resources/canadian_environmental_quality_guidelines/calculators.html}
is_exceedance <- function(test_value, lower_limit = NA_real_, upper_limit = NA_real_) {
  
  assertthat::assert_that(is.numeric(test_value))
  assertthat::assert_that(is.numeric(lower_limit))
  assertthat::assert_that(is.numeric(upper_limit))
  
  lower_exceedance <- !is.na(lower_limit) & test_value < lower_limit
  upper_exceedance <- !is.na(upper_limit) & test_value > upper_limit
  
  lower_exceedance | upper_exceedance
  
}