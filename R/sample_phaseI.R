#' Phase I Sampling
#'
#' Generates a dataset of a collared caribou population that is available to biologists for sampling
#'
#' @param Xi A vector that gives the number of collared caribou in each group
#' @param Ni A vector that gives the number of caribou in each group
#'
#'
#' @export
#'

sample_phaseI <- function(Xi, Ni){
  pop <- data.frame(Xi = as.numeric(Xi), Ni = as.numeric(Ni))[which(Xi != 0),]
  return(pop)
}

