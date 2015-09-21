#' is_exceedance function.
#' 
#' Check if test value fall outside of acceptable objective limit.
#'
#' @param test_value
#' @param objective_one
#' @param objective_two
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
  
  is.numeric(test_value) || stop('test_value is non-numeric')
  is.numeric(objective_one) || stop('objective_one is non-numeric')
  !is.na(objective_one) || return(FALSE)
  
  ifelse(limit=='<>',
         ifelse(test_value < min(objective_one, objective_two) | test_value > max(objective_one, objective_two),
                TRUE,
                FALSE
         ),
         ifelse(limit=='<',
                ifelse(test_value < objective_one,
                       TRUE,
                       FALSE
                ),
                ifelse(test_value > objective_one,
                       TRUE,
                       FALSE
                )
         )
  )
}