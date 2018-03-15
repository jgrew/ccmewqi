#' F1 (Scope) function.
#' 
#' Calculates the percentage of variables that do not meet their
#' objectives at least once during the time period under consideration
#' ("failed variables"), relative to the total number of variables
#' measured:
#'
#' @param a Failed variables
#' @param b Total variables
#' 
#' @export
#' @details
#' Inputs must be numeric.
#' \deqn{F_{1} = \frac{Number of failed variables}{Total number of variables} * 100}
#' 
#' @seealso \url{http://www.ccme.ca/en/resources/canadian_environmental_quality_guidelines/calculators.html}
F1 <- function(a, b) {
  
  assertthat::assert_that(is.numeric(a))
  assertthat::assert_that(is.numeric(b))

  (a / b) * 100
}