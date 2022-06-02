##################################################################=
##              SIMULAÇÃO DADOS ARTIFICIAIS - BETA              ##     
##################################################################=

library(tidyverse)
library(rstan)
library(coda)

options(OutDec = ",")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


##################################################################=
####                     N = 50                     ####       
##################################################################=


dat <- readRDS("./Dados/dados_artificiais/beta/dat_50.rds")

N <- dim(dat$y)[1]
q <- 2+1
y <- dat$y %>% as.vector()
x <- dat$x

# Normal Multivariada
m_beta <- rep(0, q)
S_beta <- 10* diag(q)
# Gama com E(zeta) = 1 e Var(zeta) = 10.
a_zeta <- 0.1
b_zeta <- 0.1
# Normal Multivariada
m_delta <- rep(0, N)
S_delta <- solve(dat$D - 0.95*dat$W)
# Gama com E(tau) = 1 e Var(tau) = 10.
a_tau <- 0.1
b_tau <- 0.1

data <- list(N = N, q = q, y = y, x = x,
             m_beta = m_beta, S_beta = S_beta,
             a_zeta = a_zeta, b_zeta = b_zeta,
             m_delta = m_delta, S_delta = S_delta,
             a_tau = a_tau, b_tau = b_tau
)

pars <- c("beta", "zeta", "delta", "tau")
init <- list()
init[[1]] <- list(beta = rep(1, q), zeta = 1, delta = rep(0, N), tau = 2)
iter <- 5000
warmup <- 2500  
chains <- 1    

output <- stan(file = "Scripts/stan/RegBetaDelta.stan",
               data = data,
               iter = iter,
               warmup = warmup,
               chains = chains,
               pars = pars,
               init = init,
               verbose = FALSE)
saveRDS(output, "Dados/outputs/novo/output_dat_50_beta.RDS")
output <- readRDS("Dados/outputs/novo/output_dat_50_beta.RDS")

rstan::traceplot(output, pars = c("beta", "zeta", "tau"))

samp <- rstan::extract(output)

plot( density(samp$beta[,1]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de beta0", col = "blue" )
plot( density(samp$beta[,2]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de beta1", col = "blue" )
plot( density(samp$beta[,3]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de beta2", col = "blue" )
plot( density(samp$zeta), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de zeta", col = "blue" )
plot( density(samp$delta[,1]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de delta1", col = "blue" )
plot( density(samp$delta[,50]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de delta50", col = "blue" )
plot( density(samp$tau), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de tau", col = "blue" )

aux <- cbind(samp$beta, samp$zeta, samp$tau)
me <- apply(aux, 2, mean)     # média
sd <- apply(aux, 2, sd)       # desvio padrão
aux <- as.mcmc(aux)
hpd <- HPDinterval(aux)
tab <- cbind(me, sd, hpd[,"lower"], hpd[,"upper"])
rownames(tab) <- c("beta0","beta1","beta2","zeta","tau")
colnames(tab) <- c("mean", "s.d.", "HPD_inf", "HPD_sup")
round(tab, 4) 

# Dados reais
real <- readRDS("Dados/dados_artificiais/beta/real_50.rds")

data.frame(est = tab[,1],
           real = c(real$beta, real$zeta, real$tau)) %>% 
  mutate(VR = (est - real)/abs(real))





##################################################################=
####                     N = 100                    ####       
##################################################################=

rm(list = ls(all = TRUE))
dat <- readRDS("./Dados/dados_artificiais/beta/dat_100.rds")

N <- dim(dat$y)[1]
q <- 2+1
y <- dat$y %>% as.vector()
x <- dat$x

# Normal Multivariada
m_beta <- rep(0, q)
S_beta <- 10* diag(q)
# Gama com E(zeta) = 1 e Var(zeta) = 10.
a_zeta <- 0.1
b_zeta <- 0.1
# Normal Multivariada
m_delta <- rep(0, N)
S_delta <- solve(dat$D - 0.95*dat$W)
# Gama com E(tau) = 1 e Var(tau) = 10.
a_tau <- 0.1
b_tau <- 0.1

data <- list(N = N, q = q, y = y, x = x,
             m_beta = m_beta, S_beta = S_beta,
             a_zeta = a_zeta, b_zeta = b_zeta,
             m_delta = m_delta, S_delta = S_delta,
             a_tau = a_tau, b_tau = b_tau
)

pars <- c("beta", "zeta", "delta", "tau")
init <- list()
init[[1]] <- list(beta = rep(1, q), zeta = 1, delta = rep(0, N), tau = 2)
iter <- 5000
warmup <- 2500  
chains <- 1    

output <- stan(file = "Scripts/stan/RegBetaDelta.stan",
               data = data,
               iter = iter,
               warmup = warmup,
               chains = chains,
               pars = pars,
               init = init,
               verbose = FALSE)
saveRDS(output, "Dados/outputs/novo/output_dat_100_beta.RDS")
output <- readRDS("Dados/outputs/novo/output_dat_100_beta.RDS")

rstan::traceplot(output, pars = c("beta", "zeta", "tau"))

samp <- rstan::extract(output)

plot( density(samp$beta[,1]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de beta0", col = "blue" )
plot( density(samp$beta[,2]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de beta1", col = "blue" )
plot( density(samp$beta[,3]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de beta2", col = "blue" )
plot( density(samp$zeta), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de zeta", col = "blue" )
plot( density(samp$delta[,1]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de delta1", col = "blue" )
plot( density(samp$delta[,50]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de delta50", col = "blue" )
plot( density(samp$tau), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de tau", col = "blue" )

aux <- cbind(samp$beta, samp$zeta, samp$tau)
me <- apply(aux, 2, mean)     # média
sd <- apply(aux, 2, sd)       # desvio padrão
aux <- as.mcmc(aux)
hpd <- HPDinterval(aux)
tab <- cbind(me, sd, hpd[,"lower"], hpd[,"upper"])
rownames(tab) <- c("beta0","beta1","beta2","zeta","tau")
colnames(tab) <- c("mean", "s.d.", "HPD_inf", "HPD_sup")
round(tab, 4) 

# Dados reais
real <- readRDS("Dados/dados_artificiais/beta/real_100.rds")

data.frame(est = tab[,1],
           real = c(real$beta, real$zeta, real$tau)) %>% 
  mutate(VR = (est - real)/abs(real))




##################################################################=
####                    N = 200                    ####       
##################################################################=

rm(list = ls(all = TRUE))
dat <- readRDS("./Dados/dados_artificiais/beta/dat_200.rds")

N <- dim(dat$y)[1]
q <- 2+1
y <- dat$y %>% as.vector()
x <- dat$x

# Normal Multivariada
m_beta <- rep(0, q)
S_beta <- 10* diag(q)
# Gama com E(zeta) = 1 e Var(zeta) = 10.
a_zeta <- 0.1
b_zeta <- 0.1
# Normal Multivariada
m_delta <- rep(0, N)
S_delta <- solve(dat$D - 0.95*dat$W)
# Gama com E(tau) = 1 e Var(tau) = 10.
a_tau <- 0.1
b_tau <- 0.1

data <- list(N = N, q = q, y = y, x = x,
             m_beta = m_beta, S_beta = S_beta,
             a_zeta = a_zeta, b_zeta = b_zeta,
             m_delta = m_delta, S_delta = S_delta,
             a_tau = a_tau, b_tau = b_tau
)

pars <- c("beta", "zeta", "delta", "tau")
init <- list()
init[[1]] <- list(beta = rep(1, q), zeta = 1, delta = rep(0, N), tau = 2)
iter <- 5000
warmup <- 2500  
chains <- 1    

output <- stan(file = "Scripts/stan/RegBetaDelta.stan",
               data = data,
               iter = iter,
               warmup = warmup,
               chains = chains,
               pars = pars,
               init = init,
               verbose = FALSE)
saveRDS(output, "Dados/outputs/novo/output_dat_200_beta.RDS")
output <- readRDS("Dados/outputs/novo/output_dat_200_beta.RDS")

rstan::traceplot(output, pars = c("beta", "zeta", "tau"))

samp <- rstan::extract(output)

plot( density(samp$beta[,1]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de beta0", col = "blue" )
plot( density(samp$beta[,2]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de beta1", col = "blue" )
plot( density(samp$beta[,3]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de beta2", col = "blue" )
plot( density(samp$zeta), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de zeta", col = "blue" )
plot( density(samp$delta[,1]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de delta1", col = "blue" )
plot( density(samp$delta[,50]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de delta50", col = "blue" )
plot( density(samp$tau), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de tau", col = "blue" )

aux <- cbind(samp$beta, samp$zeta, samp$tau)
me <- apply(aux, 2, mean)     # média
sd <- apply(aux, 2, sd)       # desvio padrão
aux <- as.mcmc(aux)
hpd <- HPDinterval(aux)
tab <- cbind(me, sd, hpd[,"lower"], hpd[,"upper"])
rownames(tab) <- c("beta0","beta1","beta2","zeta","tau")
colnames(tab) <- c("mean", "s.d.", "HPD_inf", "HPD_sup")
round(tab, 4) 

# Dados reais
real <- readRDS("Dados/dados_artificiais/beta/real_200.rds")

data.frame(est = tab[,1],
           real = c(real$beta, real$zeta, real$tau)) %>% 
  mutate(VR = (est - real)/abs(real))

