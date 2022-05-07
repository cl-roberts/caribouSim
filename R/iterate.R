#' Iterate Simulations
#'
#' This function iterates caribou simulations and returns rejection rate of each hypothesis test
#'
#' @param iters An integer giving the total number of iterations
#' @param Theta The total herd size to be simulated. An approximation, not a limiting number
#' @param nu An integer, the total number of collars to be simulated in a caribou herd. An approximation, not a limiting number.
#' @param phi A numeric that parameterizes the degree of clustering. phi = 0 indicates no clustering. 0 < phi < 1
#' @param r A parameter related to the probability of detection
#' @param modelSim A character string indicating the (phase II) model used to simulate the probability that a group with collared animals is detected pi. It can be either "H" = homogeneity model, "I" = independence model or "T" = threshold model. The default is "H".
#' @param modelEst Indicates the phase II probability model used to compute the population estimate, T.hat. The default is to use the same model as used to generate the simulation.
#' @param BSim The threshold parameter used in phase II probability detection
#' @param BEst the threshold parameter used in population estimation
#'
#' @export
#'


iterate <- function(iters, Theta, nu, phi, r, modelSim = c("H", "I", "T"), modelEst = c(modelSim, "H", "I", "T"),
                    BSim, BEst = BSim){
  ntrue <- X <- Ttrue <- That <- se_That <- p_fisher <- p_dispersion <- p_overdisp <- rhat <- se_rhat <- iteration <- c()
  library(VGAM)
  for(i in 1:iters){
    Ni <- sim_Ni(Theta = Theta)
    Xi <- sim_Xi(nu = nu, Ni = Ni, phi = phi)
    pop <- sample_phaseI(Xi = Xi, Ni = Ni)
    dat <- sample_phaseII(pop, model = modelSim, r = r, B = BSim)

    if(ncol(dat) < 2 | nrow(dat) < 2) next

    ntrue[i] <- sum(Xi)
    X[i] <- sum(dat[,1])
    Ttrue[i] <- sum(Ni)
    tryCatch({
      tmp <- caribou::abundance(mat = dat, n = ntrue[i], model = modelEst, maxT.hat = 10^8, B = BEst)
      tmp2 <- vglm(Xi ~ offset(log(Ni)), family = posnegbinomial(), data = dat)
      p_fisher[i] <- fisher.test(dat, simulate.p.value = TRUE)$p.value
      p_dispersion[i] <- 2*AER::dispersiontest(glm(Xi ~ offset(log(Ni)), family = poisson, weights = Ni, data = dat))$p.value
      p_overdisp[i] <- coef(summaryvglm(tmp2))[2,4]
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    That[i] <- tmp$T.hat
    se_That[i] <- tmp$se_T.hat
    rhat[i] <- tmp$rr
    se_rhat[i] <- tmp$se_rr
    iteration[i] <- i
  }

  diff <- Ttrue - That
  pctDiff <- ((Ttrue - That) / Ttrue) * 100

  p_fisher <- na.omit(p_fisher)
  p_dispersion <- na.omit(p_dispersion)
  p_overdisp <- na.omit(p_overdisp)

  alpha1 <- sum(ifelse(p_fisher < .05, TRUE, FALSE)) / length(p_fisher)
  alpha2 <- sum(ifelse(p_dispersion < .05, TRUE, FALSE)) / length(p_dispersion)
  alpha3 <- sum(ifelse(p_overdisp < .05, TRUE, FALSE)) / length(p_overdisp)

  out <- structure(list(collars = cbind(iteration, ntrue, X),
                        abundance = cbind(iteration, Ttrue, That, se_That, diff, pctDiff),
                        p_values = cbind(iteration, p_fisher, p_dispersion, p_overdisp),
                        rhat = cbind(iteration, rhat, se_rhat),
                        reject_rate = cbind(alpha1, alpha2, alpha3)),
                   class = "caribouSim")
  return(out)
}




