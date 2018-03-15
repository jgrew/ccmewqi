#' F2 (Frequency) function.
#' 
#' Represents the percentage of individual tests that do not meet meet objectives ("failed tests"):
#'
#' @param a Failed tests
#' @param b Total tests
#' 
#' @export
#' @details
#' Inputs must be numeric.
#' \deqn{F_{2} = \frac{Number of failed tests}{Total number of tests} * 100}
#' 
#' @seealso \url{http://www.ccme.ca/en/resources/canadian_environmental_quality_guidelines/calculators.html}
F2 <- function(a, b) {
  
  assertthat::assert_that(is.numeric(a))
  assertthat::assert_that(is.numeric(b))
  
  (a / b) * 100
}