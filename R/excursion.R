#' excursion function
#' 
#' Calculate the magnitude of objective failures.
#'
#' The number of times by which an individual concentration is greater than
#' (or less than, when the objective is a minimum) the objective is termed
#' an "excursion"
#' 
#' @param exc
#' @param test_value
#' 
#' @export
#' 
#' @seealso \url{http://www.ccme.ca/en/resources/canadian_environmental_quality_guidelines/calculators.html}
excursion <- function(test_value, lower_limit = NA_real_, upper_limit = NA_real_) {
  
  assertthat::assert_that(is.numeric(test_value))
  assertthat::assert_that(is.numeric(lower_limit))
  assertthat::assert_that(is.numeric(upper_limit))
  
  lower_exceedance <- !is.na(lower_limit) & test_value < lower_limit
  upper_exceedance <- !is.na(upper_limit) & test_value > upper_limit
  
  ifelse(lower_exceedance | upper_exceedance,
    ifelse(lower_exceedance, return((lower_limit / test_value) - 1), return((test_value / upper_limit) - 1)),
    0)
}