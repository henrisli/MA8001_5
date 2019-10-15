PoV <- function(mu,sd){return(mu*pnorm(mu/sd) + sd*dnorm(mu/sd))}
mu_vec = seq(-2,2,l=1000)
sd = 2
Post_value = PoV(mu_vec,sd)
Prior_value = pmax(mu_vec, rep(0,length(mu_vec)))

VoI = Post_value - Prior_value
plot(VoI)

mu_vec = seq(-2,2,l=1000)
sd_vec = seq(0.1,2,l=1000)
mu = 0
Post_value = PoV(mu,sd_vec)
Prior_value = mu

VoI = Post_value - Prior_value

plot(VoI)
