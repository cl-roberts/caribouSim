#' Phase II Sampling
#'
#' Samples collared caribou via three detection models.
#'
#' @param pop A data frame containing in the true population of radio-collared animals in the first column, and group size in the second column
#' @param r A parameter related to the probability of detection
#' @param model A character string indicating the model to determine the probability that a group with collared animals is detected pi. It can be either "H" = homogeneity model, "I" = independence model or "T" = threshold model. The default is "H".
#' @param B A numeric: a bound for the threshold model
#'
#' @export
#'

sample_phaseII <- function(pop, r, model = c("H", "I", "T"), B){
  if(model == "H"){
    pi <- rep(r, nrow(pop))
  } else if(model == "I"){
    pi <- 1 - r^pop[,1]
  } else if(model == "T"){
    pi <- ifelse(pop[,1] >= B, 1, r)
  }
  detected <- ind <- c()
  for(i in 1:nrow(pop)){
    detected[i] <- rbinom(n = 1, size = pop[i,1], prob = pi[i])
    ind[i] <- ifelse(detected[i] > 0, i, NA)
  }
  out <- pop[na.omit(ind),]
  return(out)
}
