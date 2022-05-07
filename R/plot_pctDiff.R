#' Plot percent differences
#'
#' This is a wrapper function for plotting percent differences between estimated and true population size.
#'
#' @param dat Data to be plotted
#' @param phi The value of phi (to be included on plot subtitle)
#' @param prop The proportion of iterations to be sampled and shown on plot
#'
#' @export
#'

plot_pctDiff <- function(dat, phi, prop){
  ind <- sample(1:nrow(dat), size = prop*nrow(dat), replace = FALSE)
  requireNamespace(ggplot2)
  ggplot(data = phi0[ind,]) +
    geom_line(aes(x = ntrue, y = pctDiff)) +
    scale_y_continuous(limits = c(min(phi0$pctDiff[ind]), max(phi0$pctDiff[ind]))) +
    labs(title = bquote(phi ~ " = " ~ .(phi))) +
    xlab("n") +
    ylab("Percent Difference") +
    theme_bw()
}
