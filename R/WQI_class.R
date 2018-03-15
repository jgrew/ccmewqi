setClass(
  Class = 'WQI',
  representation = representation(
    data = 'data.frame',
    lower_limit = 'character',
    upper_limit = 'character',
    result = 'character'
  ),
  prototype(
    data = data.frame(),
    lower_limit = NA_character_,
    upper_limit = NA_character_,
    result = NA_character_
  ),
  validity = function(object) {
    cat("WQI: inspector\n")
    stopifnot(is.data.frame(object))
    stopifnot(is.character(lower_limit))
    stopifnot(is.character(upper_limit))
    stopifnot(is.character(result))
    return(TRUE)
  }
)

setMethod(f = 'initialize', signature = 'WQI', definition = function(.Object, data, lower_limit = NA_character_, upper_limit = NA_character_, result) {

  temp_data <- data[[result]]
  temp_lower_limit <- ifelse(!is.na(lower_limit), data[[lower_limit]], NA_real_)
  temp_upper_limit <- ifelse(!is.na(upper_limit), data[[upper_limit]], NA_real_)
  
  Exceedance <- is_exceedance(temp_data, temp_lower_limit, temp_upper_limit)
  Excursion <- excursion(temp_data, temp_lower_limit, temp_upper_limit)
  data['Exceedance'] <- Exceedance
  data['Excursion'] <- Excursion
  
  .Object@data = data
  .Object@lower_limit = lower_limit
  .Object@upper_limit = upper_limit
  .Object@result = result
  return(.Object)
})