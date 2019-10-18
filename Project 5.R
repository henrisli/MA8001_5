library(data.table)
library(viridis)
library(reshape2)
library(geoR)
library(ggplot2)
library(MASS)
library(cowplot)
library(fields)
library(gtable)
library(ggpubr)
library(gridExtra)
library(akima)
library(party)

PoV <- function(mu,sd){return(mu*pnorm(mu/sd) + sd*dnorm(mu/sd))}
mu_vec = seq(-2,2,l=1000)
sd = 2
Post_value = PoV(mu_vec,sd)
Prior_value = pmax(mu_vec, rep(0,length(mu_vec)))

VoI = Post_value - Prior_value
plot(mu_vec, VoI, type = 'l', xlab = "Mu")

sd_vec = seq(0.1,2,l=1000)

mu = -2
Post_value = PoV(mu,sd_vec)
Prior_value = max(mu,0)

VoI = Post_value - Prior_value

plot(sd_vec, VoI, type = 'l', xlab = "sigma")


# Define area as [-2, 2] x [0.1, 2]
mu = seq(-2, 2, length.out = 30)
sigma = seq(0.1, 2, length.out = 30)
out = array(dim = c(30, 30, 3))

# Compute loglikelihood over area
for (i in 1:30){
  for (j in 1:30){
    out[i, j, 1] = mu[i]
    out[i, j, 2] = sigma[j]
    out[i, j, 3] = PoV(mu[i], sigma[j]) - max(mu[i],0)
  }
}

dfll = data.frame(mu = as.vector(out[,, 1]), sigma = as.vector(out[,,2]), voi = as.vector(out[,,3]))
#dfll_dot1 = data.frame(lambda_0 = lambda_orig[1], lambda_1 = lambda_orig[2])
#dfll_dot3 = data.frame(lambda_0 = mean(lambda_bs[, 1]), lambda_1 = mean(lambda_bs[, 2]))
#dfll_dot2 = data.frame(lambda_0 = lambda_optim[1], lambda_1 = lambda_optim[2])

ggplot() + geom_contour(data = dfll, aes(x = mu, y = sigma, z = voi, col = "VoI"), show.legend=T) + xlab("mu") + ylab("sigma")
image.plot(x = mu, y = sigma, matrix(dfll$voi, nrow = 30, ncol = 30))



# Part II


VoI <- function(rho, tau){
  sigma = matrix(c(1, rho, rho, 1), ncol = 2)
  cov_mat = sigma%*%solve(sigma + diag(2)*tau)%*%sigma
  return(sum(sqrt(diag(cov_mat)))/sqrt(2*pi))
}

plot(seq(-1,1,l=100), sapply(seq(-1,1,l=100), VoI, tau = 0.5), xlab = "rho", ylab = "VoI", type = 'l')
plot(seq(-1,1,l=100), sapply(seq(-1,1,l=100), VoI, tau = 1), xlab = "rho", ylab = "VoI", type = 'l')

# Ex d)

plot(seq(-1,1,l=100), (1 + abs(seq(-1,1,l=100)))/sqrt(2*pi*(1+0.5^2)), xlab = "rho", ylab = "VoI", type = 'l')
plot(seq(-1,1,l=100), (1 + abs(seq(-1,1,l=100)))/sqrt(2*pi*(1+1^2)), xlab = "rho", ylab = "VoI", type = 'l')

tau05 = (1 + abs(seq(-1,1,l=100)))/sqrt(2*pi*(1+0.5^2))
tau1 = (1 + abs(seq(-1,1,l=100)))/sqrt(2*pi*(1+1^2))

df = data.frame(x = seq(-1,1,l=100), y1 = tau05, y2 = tau1)
ggplot(data = df, aes(x=x)) + geom_line(aes(y = y1, col = "tau = 0.5")) + geom_line (aes(y = y2, col = "tau = 1")) + xlab("rho") + ylab("VoI") + theme(legend.title = element_blank())
