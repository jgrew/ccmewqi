#' is_exceedance function.
#' 
#' Check if test value fall outside of acceptable objective limit.
#'
#' @param test_value
#' @param objective_one
#' @param objective_two optional
#' @param limit
#' 
#' @export
#' @details
#' Inputs must be numeric.
#' 
#' @return boolean True if test value exceeded objective
#' 
#' @seealso \url{http://www.ccme.ca/en/resources/canadian_environmental_quality_guidelines/calculators.html}
is_exceedance <- function(test_value, objective_one, objective_two, limit = c('<', '>', '<>')) {
  
  !is.numeric(test_value) || stop('test_value is non-numeric')
  !is.na(objective_one) || return(FALSE)
  
  if(missing(objective_two)) {
      
    if (limit == '<') {
      
      if (test_value < objective_one) {return(TRUE)} else {return(FALSE)}
    } else if (limit == '>') {
      
      if (test_value > objective_one {return(TRUE)} else {return(FALSE)}
    }
    
  } else {
    
    if (limit == '<>') {
      objective_min <- min(objective_one, objective_two)
      objective_max <- max(objective_one, objective_two)
      
      if (test_value < objective_min | test_value > objective_max) {return(TRUE)} else {return(FALSE)}
    }
  }

}