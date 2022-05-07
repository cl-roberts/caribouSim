#' Find nearest value
#'
#' Finds element of vector which is nearest to a given constant
#'
#' @param vec A vector
#' @param constant A numeric, default is 0
#'
#'
#' @export
#'

nearest <- function(vec, constant = 0){
  return(which.min(abs(vec - constant)))
}
