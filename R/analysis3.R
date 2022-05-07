#
#
# phi0 <- na.omit(read.csv("simulations/results_phi0d.csv"))
# phi0 <- phi0[abs(phi0$pctDiff) < 200,]
# phi0_means <- data.frame(Ni = seq(10, 1000, by = 10),
#                          Ttrue = tapply(phi0[,"Ttrue"], INDEX = factor(phi0$i), FUN = mean),
#                          That = tapply(phi0[,"That"], INDEX = factor(phi0$i), FUN = mean))
#
#
# phi1 <- na.omit(read.csv("simulations/results_phi1d.csv"))
# # phi1 <- phi1[abs(phi1$pctDiff) < 20,]
# phi1_means <- data.frame(Ni = seq(10, 1000, by = 10),
#                          Ttrue = tapply(phi1[,"Ttrue"], INDEX = factor(phi1$i), FUN = mean),
#                          That = tapply(phi1[,"That"], INDEX = factor(phi1$i), FUN = mean))
#
# phi2 <- na.omit(read.csv("simulations/results_phi2d.csv"))
# phi2 <- phi2[abs(phi2$pctDiff) < 200,]
# phi2_means <- data.frame(Ni = seq(10, 1000, by = 10),
#                          Ttrue = tapply(phi2[,"Ttrue"], INDEX = factor(phi2$i), FUN = mean),
#                          That = tapply(phi2[,"That"], INDEX = factor(phi2$i), FUN = mean))
#
# phi3 <- na.omit(read.csv("simulations/results_phi3d.csv"))
# # phi3 <- phi3[abs(phi3$pctDiff) < 20,]
# phi3_means <- data.frame(Ni = seq(10, 1000, by = 10),
#                          Ttrue = tapply(phi3[,"Ttrue"], INDEX = factor(phi3$i), FUN = mean),
#                          That = tapply(phi3[,"That"], INDEX = factor(phi3$i), FUN = mean))
#
# phi4 <- na.omit(read.csv("simulations/results_phi4d.csv"))
# # phi4 <- phi4[abs(phi4$pctDiff) < 20,]
# phi4_means <- data.frame(Ni = seq(10, 1000, by = 10),
#                          Ttrue = tapply(phi4[,"Ttrue"], INDEX = factor(phi4$i), FUN = mean),
#                          That = tapply(phi4[,"That"], INDEX = factor(phi4$i), FUN = mean))
#
#
# phi5 <- na.omit(read.csv("simulations/results_phi5d.csv"))
# phi5 <- phi5[abs(phi5$pctDiff) < 200,]
# phi5_means <- data.frame(Ni = seq(10, 1000, by = 10),
#                          Ttrue = tapply(phi5[,"Ttrue"], INDEX = factor(phi5$i), FUN = mean),
#                          That = tapply(phi5[,"That"], INDEX = factor(phi5$i), FUN = mean))
#
#
#
# hist(phi0$Ttrue - phi0$That, breaks = 100)
# hist(phi1$Ttrue - phi1$That, breaks = 100)
# hist(phi2$Ttrue - phi2$That, breaks = 100)
# hist(phi3$Ttrue - phi3$That, breaks = 100)
# hist(phi4$Ttrue - phi4$That, breaks = 100)
# hist(phi5$Ttrue - phi5$That, breaks = 100)
#
# sd(phi0$Ttrue - phi0$That)
# sd(phi1$Ttrue - phi1$That)
# sd(phi2$Ttrue - phi2$That)
# sd(phi3$Ttrue - phi3$That)
# sd(phi4$Ttrue - phi4$That)
# sd(phi5$Ttrue - phi5$That)
#
#
#
# sse <- function(vec){
#   out <- sum((vec - mean(vec))^2)
#   return(out)
# }
#
# diffSSE <- function(phi){
#   tmp <- tapply(phi$Ttrue - phi$That, INDEX = phi$ntrue, FUN = sse)
#   out <- data.frame(n = names(tmp), sse = tmp)
#   return(out)
# }
#
# tmp <- merge(diffSSE(phi0), diffSSE(phi2), by = "n", all = TRUE)
# tmp2 <- merge(tmp, diffSSE(phi5), by = "n", all = TRUE)
# colnames(tmp2) <- c("n", "phi0", "phi2", "phi5")
#
# diffSSEs <- reshape2::melt(tmp2, id.var = "n", value.name = "sse", variable.name = "phi")
# diffSSEs$n <- as.numeric(diffSSEs$n)
#
# str(diffSSEs)
# diffSSEs[order(diffSSEs$n),]
#
# library(ggplot2)
# ggplot(data = diffSSEs[order(diffSSEs$n),]) +
#   # geom_line(aes(x = n, y = log(sse), color = factor(phi)), alpha = .25) +
#   geom_line(aes(x = n, y = c(rep(NA, 25), zoo::rollmean(log(sse), k = 51), rep(NA, 25)), color = phi)) +
#   labs(title = "SSE by Number of Collars") +
#   theme_bw()
#
# ggplot(mapping = aes(x = 1:1051)) +
#   geom_line(aes(y = log(diffSSE(phi0)$sse)), color = "blue", alpha = .25) +
#   geom_line(aes(y = c(rep(NA, 9), log(diffSSE(phi2)$sse))), color = "red", alpha = .25) +
#   geom_line(aes(y = c(rep(NA, 8), log(diffSSE(phi5)$sse))), color = "green", alpha = .25) +
#   geom_line(aes(y = log(diffSSE(phi0)$sse)), color = "blue", alpha = .25) +
#   geom_line(aes(y = c(rep(NA, 9), log(diffSSE(phi2)$sse))), color = "red", alpha = .25) +
#   geom_line(aes(y = c(rep(NA, 8), log(diffSSE(phi5)$sse))), color = "green", alpha = .25) +
#   labs(title = "SSE by Number of Collars") +
#   theme_bw()
#
#
#
# mean(phi5$pctDiff[phi5$ntrue < 200])
#
# plot_meanTs(phi5, legend = "bottom")
#
#
# dat <- phi2
# means <- data.frame(Ni = seq(10, 1000, by = 10),
#                     Ttrue = tapply(dat[,"Ttrue"], INDEX = factor(dat$i), FUN = mean),
#                     sdTtrue = tapply(dat[,"Ttrue"], INDEX = factor(dat$i), FUN = sd),
#                     That = tapply(dat[,"That"], INDEX = factor(dat$i), FUN = mean),
#                     sdThat = tapply(dat[,"That"], INDEX = factor(dat$i), FUN = sd)
# )
#
# cols <- c("Estimated" = "#f04546", "True" = "#3591d1")
# library(ggplot2)
# ggplot(data = means, aes(x = Ni)) +
#   geom_line(aes(y = That/1000000, color = names(cols)[1])) +
#   geom_line(aes(y = Ttrue/1000000, color = names(cols)[2])) +
#   geom_ribbon(aes(ymin = That/1000000 - sdThat/1000000,
#                   ymax = That/1000000 + sdThat/1000000), fill = "red", alpha = .25) +
#   geom_ribbon(aes(ymin = Ttrue/1000000 - sdTtrue/1000000,
#                   ymax = Ttrue/1000000 + sdTtrue/1000000), fill = "blue", alpha = .25) +
#   # scale_colour_manual(name = "", values = cols) +
#   # scale_y_continuous(limits = c(1.075, 1.175)) +
#   ylab("Mean Population Size (Millions)") + xlab(bquote(paste(nu, " (nu)"))) +
#   theme_bw()
#
#
# library(AER)
# library(caribou)
# library(caribouSim)
# data("WAH11")
#
# 2*dispersiontest(glm(xi ~ gni, family = poisson(), data = as.data.frame(WAH11)))$p.value
#
# phi0 <- read.csv("simulations/results_phi5d.csv")
# tmp <- data.frame(X = rep(1:100, times = rep(100, 100)),
#                   V1 = phi0$p_fisher,
#                   V2 = 2*phi0$p_dispersion,
#                   V3 = phi0$p_overdisp)
#
#
#
# phi0_rejections <- data.frame(X = 1:100, sapply(tmp[,2:4], FUN = function(x){
#     tapply(x, INDEX = factor(tmp$X), FUN = function(x){
#       round(mean(ifelse(x < .05, 1, 0)), 4)
#       })
#   })
# )
#
# # write.csv(phi0_rejections, "simulations/rejections_phi5d.csv")
#
# plot_sim(dat = rejections_phi5, legend = "bottom")
# plot_sim(dat = phi0_rejections, legend = "bottom")
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
