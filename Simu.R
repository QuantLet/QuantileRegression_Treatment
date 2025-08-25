# Load packages
library(quantreg)
library(dplyr)

set.seed(123)

wdir = "/Users/ruting/Documents/Github/QuantileReg"
setwd(wdir)

# Simulate data
n <- 1000
X <- rnorm(n)
D <- rbinom(n, 1, 0.5)  # treatment indicator
# Outcome: treatment effect increases with quantile
# Location shift
Y <- 2 + 1*X + 0.5*D + rnorm(n)

data <- data.frame(Y, D, X)

# Fit Quantile Regression at multiple quantiles
taus <- c(0.1, 0.25, 0.5, 0.75, 0.9)
qr_results <- lapply(taus, function(tau) {
  rq(Y ~ D + X, tau = tau, data = data)
  # rq(Y ~ D + X + D:X, tau = tau, data = data)
})

# Extract treatment coefficients at each quantile
qte <- sapply(qr_results, function(mod) coef(mod)["D"])
names(qte) <- paste0("tau_", taus)
qte

# Plot QTE across quantiles
par(bg = "transparent")  

# Now plot

plot(taus, qte, type='b', pch=19, col='blue', lwd=2,
     xlab="Quantiles", ylab="",
     main="Quantile Treatment Effects",
     cex.axis=2,     # 坐标轴刻度放大
     font.axis=2,      # 坐标轴数字加粗
     cex.lab=1.8,      # 轴标签放大
     font.lab=2,       # 轴标签加粗
     cex.main=2,     # 标题放大
     font.main=2,      # 标题加粗
     las=1)            # 让刻度水平显示
abline(h = mean(Y[D==1]) - mean(Y[D==0]), col='red', lty=2, lwd=2)  # ATE line

dev.print(device = png, filename = 'Locationshift.png', width = 900, height = 900)
dev.off()

#scaling

Y <- 2 + (1+ 0.5*D)*X  + rnorm(n)

data <- data.frame(Y, D, X)

# Fit Quantile Regression at multiple quantiles
taus <- c(0.1, 0.25, 0.5, 0.75, 0.9)
qr_results <- lapply(taus, function(tau) {
  # rq(Y ~ D + X, tau = tau, data = data)
  rq(Y ~ D + X + D:X, tau = tau, data = data)
})

# Extract treatment coefficients at each quantile
qte <- sapply(qr_results, function(mod) coef(mod)["D"])
names(qte) <- paste0("tau_", taus)
qte

# Plot QTE across quantiles
par(bg = "transparent")  

plot(taus, qte, type='b', pch=19, col='blue', lwd=2,
     xlab="Quantiles", ylab="",
     main="Quantile Treatment Effects",
     cex.axis=2,     # 坐标轴刻度放大
     font.axis=2,      # 坐标轴数字加粗
     cex.lab=1.8,      # 轴标签放大
     font.lab=2,       # 轴标签加粗
     cex.main=2,     # 标题放大
     font.main=2,      # 标题加粗
     las=1)            # 让刻度水平显示
abline(h = mean(Y[D==1]) - mean(Y[D==0]), col='red', lty=2, lwd=2)  # ATE line

dev.print(device = png, filename = 'scale.png', width = 900, height = 900)
dev.off()