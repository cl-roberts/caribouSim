#' Plot means of simulated T and estimated T
#'
#' This is a wrapper function for plotting values of simulated T and true T.
#' Means are taken within repeated values of Ni.
#'
#' @param dat Data to be plotted
#' @param legend Legend position; passed to legend.postion()
#' @param nu An integer vector, the values for desired number of collars plotted on the x axis
#'
#' @export
#'

plot_meanTs <- function(dat, legend, nu){
  dat <- na.omit(dat)
  means <- data.frame(nu = nu,
                       Ttrue = tapply(dat[,"Ttrue"], INDEX = factor(dat$i), FUN = mean),
                       That = tapply(dat[,"That"], INDEX = factor(dat$i), FUN = mean))
  cols <- c("Estimated" = "#f04546", "True" = "#3591d1")
  requireNamespace(ggplot2)
  ggplot(data = means, aes(x = nu)) +
    geom_line(aes(y = That/1000000, color = names(cols)[1])) +
    geom_line(aes(y = Ttrue/1000000, color = names(cols)[2])) +
    scale_colour_manual(name = "", values = cols) +
    scale_y_continuous(limits = c(1.075, 1.175)) +
    ylab("Mean Population Size (Millions)") + xlab(bquote(paste(nu, " (nu)"))) +
    theme_bw() +
    theme(legend.position = legend)
}
