#######################################################################
# Simulating data from the 4-parameter drift diffusion model
# ----------------------------------------------------------------
# Written by Ladislas Nalborczyk
# E-mail: ladislas.nalborczyk@gmail.com
# Last update: August 28, 2020
#############################################################

if (!require("hrbrthemes") ) install.packages("hrbrthemes"); library("hrbrthemes");
if (!require("tidyverse") ) install.packages("tidyverse"); library("tidyverse");
if (!require("RWiener") ) install.packages("RWiener"); library("RWiener");

nobs = 1e2; alpha = 2; beta = 0.5; delta = 0.5; tau = 0.3;
df <- rwiener(n = nobs, alpha = alpha, tau = tau, beta = beta, delta = delta)

ddm <- function (nobs = 1e2, alpha = 2, beta = 0.5, delta = 0.5, tau = 0.3) {

  # generates some data for given values of parameters
  df <- rwiener(n = nobs, alpha = alpha, tau = tau, beta = beta, delta = delta)
  
  # plotting two densitites in ggplot2
  # https://rstudio-pubs-static.s3.amazonaws.com/344682_ba30f5e91932418f8a41a9ce4d411511.html
  
  df %>%
    ggplot(aes(x = q) ) +
    # geom_histogram(aes(x = q, y = ..density..), fill ="red", bins = 20) + 
    # geom_histogram(aes(x = q, y = -..density..), fill = "blue", bins = 20) +
    # geom_rug() +
    geom_density(
      data = . %>% filter(resp == "upper"),
      aes(y = ..density..),
      colour = "steelblue", fill = "steelblue",
      outline.type = "upper", alpha = 0.8, adjust = 1, trim = TRUE
      ) +
    geom_density(
      data = . %>% filter(resp == "lower"),
      aes(y = -..density..), colour = "orangered", fill = "orangered",
      outline.type = "upper", alpha = 0.8, adjust = 1, trim = TRUE
      ) +
    # geom_vline(xintercept = 0, lty = 2) +
    # geom_hline(yintercept = 0, lty = 2) +
    geom_segment(
      aes(x = 0, xend = tau, y = 0, yend = 0),
      arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
      size = 0.5
      ) +
    annotate(
      geom = "text",
      x = 0, y = 0, hjust = 0, vjust = -1, size = 3,
      label = "non-decision time"
    ) +
    # theme_bw(base_size = 12) +
    theme_ipsum_rc() +
    labs(x = "Reaction time (in seconds)", y = "") +
    xlim(0, NA)
    # coord_cartesian(ylim = c(0, 0.1) )
    # scale_y_continuous(expand = c(0, 0), limits = c(NA, NA) )
  
  }

# alternative plotting method
# https://stackoverflow.com/questions/44193296/plotting-the-reflection-i-e-shadow-of-a-histogram-in-r-code-and-visual-prov

# df1 <- data.frame(x = rnorm(1e4))
# p <- ggplot(df %>% filter(resp == "upper") ) +
#   geom_density(aes(x = q, y = ..density..), colour = "steelblue", fill = "steelblue", alpha = 0.8)
# 
# pg <- ggplot_build(p)
# pg <- pg$data[[1]]
# pg$mirror <- -pg$count
# pg$mirror <- -pg$density
# ggplot(pg) + geom_col(aes(x, y) ) + geom_col(aes(x, mirror), fill = "blue")

# plot method from RWiener:::plot.data.wiener

# function (x, ...) 
# {
#   rt = as.double(x$q)
#   rc = as.numeric(x$resp)
#   dpos = try(density(rt[rc == 1], from = 0))
#   dneg = try(density(rt[rc == 2], from = 0))
#   maxt = max(pretty(max(rt)))
#   maxd = max(dpos$y, dneg$y)
#   par(mar = c(0, 5, 0, 0), mfcol = c(2, 1), ask = FALSE)
#   plot(dpos, xlim = c(0, maxt), ylim = c(0, maxd), las = 2, 
#        lwd = 2, col = "green3", main = "", ylab = "", ask = FALSE)
#   rug(rt[rc == 1], col = "green3")
#   mtext("Density of positive responses", side = 2, line = 4, 
#         cex = 0.8)
#   plot(dneg, xlim = c(0, maxt), ylim = c(maxd, 0), las = 2, 
#        lwd = 2, col = "red", main = "", ylab = "", ask = FALSE)
#   mtext("Density of negative responses", side = 2, line = 4, 
#         cex = 0.8)
#   rug(rt[rc == 2], col = "red", side = 3)
# }
