#' nse function.
#' 
#' Normalized sum of excursions from objectives
#'
#' @param a Sum of excursions
#' @param b Total number of tests
#' 
#' @export
#' @details
#' Inputs must be numeric.
#' 
#' @seealso \url{http://www.ccme.ca/en/resources/canadian_environmental_quality_guidelines/calculators.html}
nse <- function(a, b) {

  assertthat::assert_that(is.numeric(a))
  assertthat::assert_that(is.numeric(b))
  
  a / b
}