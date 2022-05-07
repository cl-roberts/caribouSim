#' Simulate Ni
#'
#' This function simulates a vector of caribou groups
#'
#' @param Theta The total herd size to be simulated.
#'
#' @export
#'


sim_Ni <- function(Theta){
  T_sim <- Theta
  Ni <- c()
  sample_frame <- 1:(round(.5*T_sim)) # note that a group cannot be greater than half the total pop size
  while(T_sim > 0) {
    Ni <- append(Ni, sample(sample_frame, size = 1, prob = 1/(sample_frame)))
    T_sim <- Theta - sum(Ni)
  }
  return(sort(Ni, decreasing = TRUE))
}
