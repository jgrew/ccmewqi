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

  is.numeric(a) || stop('a is non-numeric')
  is.numeric(b) || stop('b is non-numeric')
  
  a / b
}