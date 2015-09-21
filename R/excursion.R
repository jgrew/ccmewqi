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
#' @param objective_one
#' @param objective_two
#' @param limit
#' 
#' @export
#' 
#' @seealso \url{http://www.ccme.ca/en/resources/canadian_environmental_quality_guidelines/calculators.html}
excursion <- function(exc, test_value, objective_one, objective_two, limit = c('<', '>', '<>')) {
  
  is.numeric(test_value) || stop('test_value is non-numeric')
  is.numeric(objective_one) || stop('objective_one is non-numeric')
  
  ifelse(!is.na(exc),
         ifelse(exc,
                ifelse(limit=='<>',
                       ifelse(test_value < min(objective_one, objective_two),
                              return((min(objective_one, objective_two) / test_value) - 1),
                              return((test_value / max(objective_one, objective_two)) - 1)
                       ),
                       ifelse(limit=='<',
                              return((objective_one / test_value) - 1),
                              return((test_value / objective_one) - 1)
                       )
                ),
                0
         ),
         0
  )
}