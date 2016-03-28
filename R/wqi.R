#' wqi function.
#' 
#' Final CCME water quality index score
#'
#' @param F1
#' @param F2
#' @param F3
#' @param method optional CCAMP method.
#' 
#' @export
#' @details
#' Inputs must be numeric.
#' 
#' @seealso \url{http://www.ccme.ca/en/resources/canadian_environmental_quality_guidelines/calculators.html}
wqi <- function(F1, F2, F3, method = NULL) {
 
   is.numeric(F1) || stop('F1 is non-numeric')
   is.numeric(F2) || stop('F2 is non-numeric')
   is.numeric(F3) || stop('F3 is non-numeric')
   
   if(is.null(method)){
     100 - (sqrt(F1 ^ 2 + F2 ^ 2 + F3 ^ 2) / 1.732)
   } else {
     100 - (sqrt(F2 ^ 2 + F3 ^ 2) / 1.414)
   }
}