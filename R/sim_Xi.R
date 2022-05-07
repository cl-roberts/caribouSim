#' Simulate Xi
#'
#' This function simulates a vector of collared caribou
#'
#' @param nu An integer, the total number of collars to be simulated in a caribou herd. Not a limiting number.
#' @param Ni A vector that gives the number of caribou in each group
#' @param phi A numeric that parameterizes the degree of clustering. phi = 0 indicates no clustering. 0 < phi < 1
#'
#' @export
#'

sim_Xi <- function(nu, Ni, phi = 0){
  Theta <- sum(Ni)
  M <- length(Ni)
  ind <- sample(1:M, size = round(M/2), replace = FALSE)
  places <- c(c(M:1)[ind], c(M:1)[-ind])
  weights <- c(Ni[ind]*(1 + phi), Ni[-ind]*(1 - phi))[rev(order(places))]
  probs <- weights/sum(weights)
  Xi <- rep(0, M)
  Xi <- rbinom(M, size = nu, prob = probs)
  return(Xi)
}
