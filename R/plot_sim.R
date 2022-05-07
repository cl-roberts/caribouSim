#' Plot rejections
#'
#' This is a wrapper function for plotting rejections from iterate for a power analysis
#'
#' @param dat A csv file containing data to plot
#' @param legend Legend position; passed to legend.postion()
#' @param nu An integer vector, the values for desired number of collars plotted on the x axis
#' @param k An odd integer, degree of moving average
#'
#' @export
#'

plot_sim <- function(dat, legend = c("none", "top", "bottom", "left", "right"), nu, k = 9){
  cols <- c("FET" = "#f04546", "PDT" = "#3591d1", "ZTNBDT" = "#62c76b")
  MAs <- rbind(NA, NA, NA, NA,
               as.data.frame(sapply(dat, FUN = function(x) zoo::rollmean(x, k = k))),
               NA, NA, NA, NA)
  requireNamespace(ggplot2)
  suppressWarnings(
  ggplot(data = na.omit(dat), aes(x = nu)) +
    geom_line(aes(y = V1, color = names(cols)[1]), alpha = .5) +
    geom_line(aes(y = MAs$V1, color = names(cols)[1])) +
    geom_line(aes(y = V2, color = names(cols)[2]), alpha = .5) +
    geom_line(aes(y = MAs$V2, color = names(cols)[2])) +
    geom_line(aes(y = V3, color = names(cols)[3]), alpha = .5) +
    geom_line(aes(y = MAs$V3, color = names(cols)[3])) +
    ylab("Rejection Rate") + xlab(bquote(paste(nu, " (nu)"))) +
    scale_colour_manual(name = "Hypothesis Test", values = cols) +
    theme_bw() +
    theme(legend.position = legend)
  )
}
