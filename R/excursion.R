#' excursion function
#' 
#' Calculate the magnitude of objective failures.
#'
#' The number of times by which an individual concentration is greater than
#' (or less than, when the objective is a minimum) the objective is termed
#' an "excursion"
#' 
#' @param test_value
#' @param objective_one
#' @param objective_two optional
#' @param limit
#' 
#' @seealso \url{http://www.ccme.ca/en/resources/canadian_environmental_quality_guidelines/calculators.html}
excursion <- function(test_value, objective_one, objective_two, limit = c('<', '>', '<>')) {
  
  is.numeric(test_value) || stop('test_value is non-numeric')
  is.numeric(objective_one) || stop('objective_one is non-numeric')
  
  if(missing(objective_two)) {
    if (is_exceedance(test_value, objective_one, objective_two, limit)) {
      if (limit == '<') {
        excursion <- (test_value / objective_one) - 1
      }
      else if (limit == '>') {
        excursion <- (objective_one / test_value) - 1
      }
    } else {
      excursion <- 0
    }
  } else {
    if (is_exceedance(test_value, objective_one, objective_two, limit)) {    
      if (limit == '<>') {
        objective_min <- min(objective_one, objective_two)
        objective_max <- max(objective_one, objective_two)
        
        if (test_value < objective_min) {
          excursion <- (test_value / objective_one) - 1  
        } else if (test_value > objective_max) {
          excursion <- (objective_max / test_value) - 1
        }
      }
    } else {
      excursion <- 0
    }
  }
  return(excursion)
}