#' F3 (Amplitude) function.
#' 
#' Represents the amount by which failed test values do not meet their objectives
#'
#' @param nse normalized sum of excursions
#' 
#' @export
#' @details
#' Inputs must be numeric.
#' \deqn{F_{3} = \frac{Normalized sum of excursions}{0.01 * normalized sum of excursions + 0.01}}
#' 
#' @seealso \url{http://www.ccme.ca/en/resources/canadian_environmental_quality_guidelines/calculators.html}
F3 <- function(nse) {
  
  is.numeric(nse) || stop('nse is non-numeric')
  
  nse / (0.01 * nse + 0.01)
}