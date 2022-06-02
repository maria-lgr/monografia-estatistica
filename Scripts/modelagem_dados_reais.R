#################################################################=
####                   AJUSTES DADOS REAIS                   ####
#################################################################=

library(tidyverse)
library(rstan)
library(coda)

options(OutDec = ",")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


################################################################=
####                          2020                          ####       
################################################################=

dat <- readRDS("./Dados/dados_reais/tx_2020_EM.rds")
glimpse(dat)
S_delta <- readRDS("./Dados/dados_reais/m_neighborhood_2020_EM.rds")

N <- dim(dat)[1]
q <- 5+1
y <- (dat$tx_muni/100) %>% as.vector()
x <- matrix(
  c(rep(1, N),
    dat$taxa_urbana,
    dat$taxa_publica,
    dat$IDHM.Renda,
    dat$log_pop,
    dat$log_PIB_pc),
  ncol = 6
)

# Normal Multivariada
m_beta <- rep(0, q)
S_beta <- 10* diag(q)
# Normal Multivariada
m_gamma <- rep(0, q)
S_gamma <- 10* diag(q)
# Normal Multivariada
m_delta_t <- rep(0, N)
S_delta_t <- S_delta
# Normal Multivariada
m_delta_z <- rep(0, N)
S_delta_z <- S_delta
# Gama com E(tau) = 1 e Var(tau) = 10.
a_tau_t <- 0.1
b_tau_t <- 0.1
# Gama com E(tau) = 1 e Var(tau) = 10.
a_tau_z <- 0.1
b_tau_z <- 0.1

data <- list(N = N, q = q, y = y, x = x,
             m_beta = m_beta, S_beta = S_beta,
             m_gamma = m_gamma, S_gamma = S_gamma,
             m_delta_t = m_delta_t, S_delta_t = S_delta_t,
             m_delta_z = m_delta_z, S_delta_z = S_delta_z,
             a_tau_t = a_tau_t, b_tau_t = b_tau_t,
             a_tau_z = a_tau_z, b_tau_z = b_tau_z
)

pars <- c("beta", "gamma", "delta_theta", "delta_zeta", "tau_theta", "tau_zeta")
init <- list()
init[[1]] <- list(beta = runif(q, -0.1, 0.1), gamma = runif(q, -0.1, 0.1), 
                  delta_theta = rep(0, N), 
                  delta_zeta = rep(0, N), tau_theta = 2, tau_zeta = 2)
iter <- 5000
warmup <- 2500  
chains <- 1    

output <- stan(file = "Scripts/stan/RegBetaDeltaZetaDelta.stan",
               data = data,
               iter = iter,
               warmup = warmup,
               chains = chains,
               pars = pars,
               init = init,
               verbose = FALSE)
saveRDS(output, "Dados/outputs_reais/output_tx_2020_EM.RDS")
output <- readRDS("Dados/outputs_reais/output_tx_2020_EM.RDS")

rstan::traceplot(output, pars = c("beta", "gamma", "tau_theta", "tau_zeta"))

png("./Imagens/tp_beta_2020.png", width = 770, height = 350)
rstan::traceplot(output, pars = c("beta"))
dev.off()

png("./Imagens/tp_gamma_2020.png", width = 770, height = 350)
rstan::traceplot(output, pars = c("gamma"))
dev.off()

png("./Imagens/tp_tau_2020.png", width = 550, height = 175)
rstan::traceplot(output, pars = c("tau_theta", "tau_zeta"))
dev.off()

samp <- rstan::extract(output)

plot( density(samp$beta[,1]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de beta0", col = "blue" )
plot( density(samp$beta[,2]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de beta1", col = "blue" )
plot( density(samp$beta[,3]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de beta2", col = "blue" )
plot( density(samp$gamma[,1]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de gamma0", col = "blue" )
plot( density(samp$gamma[,2]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de gamma1", col = "blue" )
plot( density(samp$gamma[,3]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de gamma2", col = "blue" )
plot( density(samp$tau_theta), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de tau_theta", col = "blue" )
plot( density(samp$tau_zeta), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de tau_zeta", col = "blue" )


aux <- cbind(samp$beta, samp$gamma, samp$tau_theta, samp$tau_zeta)
me <- apply(aux, 2, mean)     # média
sd <- apply(aux, 2, sd)       # desvio padrão
aux <- as.mcmc(aux)
hpd <- HPDinterval(aux)
tab <- cbind(me, sd, hpd[,"lower"], hpd[,"upper"])
rownames(tab) <- c("Intercepto - beta","Taxa urbana - beta","Taxa pública - beta",
                   "IDHM Renda - beta","log(População) - beta",
                   "log(PIB per capita) - beta",
                   "Intercepto - gamma","Taxa urbana - gamma","Taxa pública - gamma",
                   "IDHM Renda - gamma","log(População) - gamma",
                   "log(PIB per capita) - gamma","tau_theta", "tau_zeta")
colnames(tab) <- c("Média", "D.P.", "HPD inf", "HPD sup")
round(tab, 3) 


################################################################=
####                          2015                          ####       
################################################################=

dat <- readRDS("./Dados/dados_reais/tx_2015_EM.rds")
glimpse(dat)
S_delta <- readRDS("./Dados/dados_reais/m_neighborhood_2015_EM.rds")

N <- dim(dat)[1]
q <- 5+1
y <- (dat$tx_muni/100) %>% as.vector()
x <- matrix(
  c(rep(1, N),
    dat$taxa_urbana,
    dat$taxa_publica,
    dat$IDHM.Renda,
    dat$log_pop,
    dat$log_PIB_pc),
  ncol = 6
)

# Normal Multivariada
m_beta <- rep(0, q)
S_beta <- 10* diag(q)
# Normal Multivariada
m_gamma <- rep(0, q)
S_gamma <- 10* diag(q)
# Normal Multivariada
m_delta_t <- rep(0, N)
S_delta_t <- S_delta
# Normal Multivariada
m_delta_z <- rep(0, N)
S_delta_z <- S_delta
# Gama com E(tau) = 1 e Var(tau) = 10.
a_tau_t <- 0.1
b_tau_t <- 0.1
# Gama com E(tau) = 1 e Var(tau) = 10.
a_tau_z <- 0.1
b_tau_z <- 0.1

data <- list(N = N, q = q, y = y, x = x,
             m_beta = m_beta, S_beta = S_beta,
             m_gamma = m_gamma, S_gamma = S_gamma,
             m_delta_t = m_delta_t, S_delta_t = S_delta_t,
             m_delta_z = m_delta_z, S_delta_z = S_delta_z,
             a_tau_t = a_tau_t, b_tau_t = b_tau_t,
             a_tau_z = a_tau_z, b_tau_z = b_tau_z
)

pars <- c("beta", "gamma", "delta_theta", "delta_zeta", "tau_theta", "tau_zeta")
init <- list()
init[[1]] <- list(beta = runif(q, -0.1, 0.1), gamma = runif(q, -0.1, 0.1), 
                  delta_theta = rep(0, N), 
                  delta_zeta = rep(0, N), tau_theta = 2, tau_zeta = 2)
iter <- 5000
warmup <- 2500  
chains <- 1    

output <- stan(file = "Scripts/stan/RegBetaDeltaZetaDelta.stan",
               data = data,
               iter = iter,
               warmup = warmup,
               chains = chains,
               pars = pars,
               init = init,
               verbose = FALSE)
saveRDS(output, "Dados/outputs_reais/output_tx_2015_EM.RDS")
output <- readRDS("Dados/outputs_reais/output_tx_2015_EM.RDS")

rstan::traceplot(output, pars = c("beta", "gamma", "tau_theta", "tau_zeta"))

samp <- rstan::extract(output)

plot( density(samp$beta[,1]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de beta0", col = "blue" )
plot( density(samp$beta[,2]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de beta1", col = "blue" )
plot( density(samp$beta[,3]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de beta2", col = "blue" )
plot( density(samp$gamma[,1]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de gamma0", col = "blue" )
plot( density(samp$gamma[,2]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de gamma1", col = "blue" )
plot( density(samp$gamma[,3]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de gamma2", col = "blue" )
plot( density(samp$tau_theta), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de tau_theta", col = "blue" )
plot( density(samp$tau_zeta), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de tau_zeta", col = "blue" )


aux <- cbind(samp$beta, samp$gamma, samp$tau_theta, samp$tau_zeta)
me <- apply(aux, 2, mean)     # média
sd <- apply(aux, 2, sd)       # desvio padrão
aux <- as.mcmc(aux)
hpd <- HPDinterval(aux)
tab <- cbind(me, sd, hpd[,"lower"], hpd[,"upper"])
rownames(tab) <- c("Intercepto - beta","Taxa urbana - beta","Taxa pública - beta",
                   "IDHM Renda - beta","log(População) - beta",
                   "log(PIB per capita) - beta",
                   "Intercepto - gamma","Taxa urbana - gamma","Taxa pública - gamma",
                   "IDHM Renda - gamma","log(População) - gamma",
                   "log(PIB per capita) - gamma","tau_theta", "tau_zeta")
colnames(tab) <- c("Média", "D.P.", "HPD inf", "HPD sup")
round(tab, 3)




################################################################=
####                          2010                          ####       
################################################################=

dat <- readRDS("./Dados/dados_reais/tx_2010_EM.rds")
glimpse(dat)
S_delta <- readRDS("./Dados/dados_reais/m_neighborhood_2010_EM.rds")

N <- dim(dat)[1]
q <- 5+1
y <- (dat$tx_muni/100) %>% as.vector()
x <- matrix(
  c(rep(1, N),
    dat$taxa_urbana,
    dat$taxa_publica,
    dat$IDHM.Renda,
    dat$log_pop,
    dat$log_PIB_pc),
  ncol = 6
)

# Normal Multivariada
m_beta <- rep(0, q)
S_beta <- 10* diag(q)
# Normal Multivariada
m_gamma <- rep(0, q)
S_gamma <- 10* diag(q)
# Normal Multivariada
m_delta_t <- rep(0, N)
S_delta_t <- S_delta
# Normal Multivariada
m_delta_z <- rep(0, N)
S_delta_z <- S_delta
# Gama com E(tau) = 1 e Var(tau) = 10.
a_tau_t <- 0.1
b_tau_t <- 0.1
# Gama com E(tau) = 1 e Var(tau) = 10.
a_tau_z <- 0.1
b_tau_z <- 0.1

data <- list(N = N, q = q, y = y, x = x,
             m_beta = m_beta, S_beta = S_beta,
             m_gamma = m_gamma, S_gamma = S_gamma,
             m_delta_t = m_delta_t, S_delta_t = S_delta_t,
             m_delta_z = m_delta_z, S_delta_z = S_delta_z,
             a_tau_t = a_tau_t, b_tau_t = b_tau_t,
             a_tau_z = a_tau_z, b_tau_z = b_tau_z
)

pars <- c("beta", "gamma", "delta_theta", "delta_zeta", "tau_theta", "tau_zeta")
init <- list()
init[[1]] <- list(beta = runif(q, -0.1, 0.1), gamma = runif(q, -0.1, 0.1), 
                  delta_theta = rep(0, N), 
                  delta_zeta = rep(0, N), tau_theta = 2, tau_zeta = 2)
iter <- 5000
warmup <- 2500  
chains <- 1    

output <- stan(file = "Scripts/stan/RegBetaDeltaZetaDelta.stan",
               data = data,
               iter = iter,
               warmup = warmup,
               chains = chains,
               pars = pars,
               init = init,
               verbose = FALSE)
saveRDS(output, "Dados/outputs_reais/output_tx_2010_EM.RDS")
output <- readRDS("Dados/outputs_reais/output_tx_2010_EM.RDS")

rstan::traceplot(output, pars = c("beta", "gamma", "tau_theta", "tau_zeta"))

samp <- rstan::extract(output)

plot( density(samp$beta[,1]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de beta0", col = "blue" )
plot( density(samp$beta[,2]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de beta1", col = "blue" )
plot( density(samp$beta[,3]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de beta2", col = "blue" )
plot( density(samp$gamma[,1]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de gamma0", col = "blue" )
plot( density(samp$gamma[,2]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de gamma1", col = "blue" )
plot( density(samp$gamma[,3]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de gamma2", col = "blue" )
plot( density(samp$tau_theta), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de tau_theta", col = "blue" )
plot( density(samp$tau_zeta), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de tau_zeta", col = "blue" )


aux <- cbind(samp$beta, samp$gamma, samp$tau_theta, samp$tau_zeta)
me <- apply(aux, 2, mean)     # média
sd <- apply(aux, 2, sd)       # desvio padrão
aux <- as.mcmc(aux)
hpd <- HPDinterval(aux)
tab <- cbind(me, sd, hpd[,"lower"], hpd[,"upper"])
rownames(tab) <- c("Intercepto - beta","Taxa urbana - beta","Taxa pública - beta",
                   "IDHM Renda - beta","log(População) - beta",
                   "log(PIB per capita) - beta",
                   "Intercepto - gamma","Taxa urbana - gamma","Taxa pública - gamma",
                   "IDHM Renda - gamma","log(População) - gamma",
                   "log(PIB per capita) - gamma","tau_theta", "tau_zeta")
colnames(tab) <- c("Média", "D.P.", "HPD inf", "HPD sup")
round(tab, 3)



################################################################=
####                   2020 SEM ZEROS                       ####       
################################################################=

dat <- readRDS("./Dados/dados_reais/tx_2020_EM_sem_zeros.rds")
glimpse(dat)
S_delta <- readRDS("./Dados/dados_reais/m_neighborhood_2020_EM_sem_zeros.rds")

N <- dim(dat)[1]
q <- 5+1
y <- (dat$tx_muni/100) %>% as.vector()
x <- matrix(
  c(rep(1, N),
    dat$taxa_urbana,
    dat$taxa_publica,
    dat$IDHM.Renda,
    dat$log_pop,
    dat$log_PIB_pc),
  ncol = 6
)

# Normal Multivariada
m_beta <- rep(0, q)
S_beta <- 10* diag(q)
# Normal Multivariada
m_gamma <- rep(0, q)
S_gamma <- 10* diag(q)
# Normal Multivariada
m_delta_t <- rep(0, N)
S_delta_t <- S_delta
# Normal Multivariada
m_delta_z <- rep(0, N)
S_delta_z <- S_delta
# Gama com E(tau) = 1 e Var(tau) = 10.
a_tau_t <- 0.1
b_tau_t <- 0.1
# Gama com E(tau) = 1 e Var(tau) = 10.
a_tau_z <- 0.1
b_tau_z <- 0.1

data <- list(N = N, q = q, y = y, x = x,
             m_beta = m_beta, S_beta = S_beta,
             m_gamma = m_gamma, S_gamma = S_gamma,
             m_delta_t = m_delta_t, S_delta_t = S_delta_t,
             m_delta_z = m_delta_z, S_delta_z = S_delta_z,
             a_tau_t = a_tau_t, b_tau_t = b_tau_t,
             a_tau_z = a_tau_z, b_tau_z = b_tau_z
)

pars <- c("beta", "gamma", "delta_theta", "delta_zeta", "tau_theta", "tau_zeta")
init <- list()
init[[1]] <- list(beta = runif(q, -0.1, 0.1), gamma = runif(q, -0.1, 0.1), 
                  delta_theta = rep(0, N), 
                  delta_zeta = rep(0, N), tau_theta = 2, tau_zeta = 2)
iter <- 5000
warmup <- 2500  
chains <- 1    

Sys.time()
output <- stan(file = "Scripts/stan/RegBetaDeltaZetaDelta.stan",
               data = data,
               iter = iter,
               warmup = warmup,
               chains = chains,
               pars = pars,
               init = init,
               verbose = FALSE)
saveRDS(output, "Dados/outputs_reais/output_tx_2020_EM_sem_zeros.RDS")
output <- readRDS("Dados/outputs_reais/output_tx_2020_EM_sem_zeros.RDS")
Sys.time()

rstan::traceplot(output, pars = c("beta", "gamma", "tau_theta", "tau_zeta"))

png("./Imagens/tp_beta_2020_sem_zeros.png", width = 770, height = 350)
rstan::traceplot(output, pars = c("beta"))
dev.off()

png("./Imagens/tp_gamma_2020_sem_zeros.png", width = 770, height = 350)
rstan::traceplot(output, pars = c("gamma"))
dev.off()

png("./Imagens/tp_tau_2020_sem_zeros.png", width = 550, height = 175)
rstan::traceplot(output, pars = c("tau_theta", "tau_zeta"))
dev.off()

samp <- rstan::extract(output)

plot( density(samp$beta[,1]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de beta0", col = "blue" )
plot( density(samp$beta[,2]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de beta1", col = "blue" )
plot( density(samp$beta[,3]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de beta2", col = "blue" )
plot( density(samp$gamma[,1]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de gamma0", col = "blue" )
plot( density(samp$gamma[,2]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de gamma1", col = "blue" )
plot( density(samp$gamma[,3]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de gamma2", col = "blue" )
plot( density(samp$tau_theta), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de tau_theta", col = "blue" )
plot( density(samp$tau_zeta), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de tau_zeta", col = "blue" )


aux <- cbind(samp$beta, samp$gamma, samp$tau_theta, samp$tau_zeta)
me <- apply(aux, 2, mean)     # média
sd <- apply(aux, 2, sd)       # desvio padrão
aux <- as.mcmc(aux)
hpd <- HPDinterval(aux)
tab <- cbind(me, sd, hpd[,"lower"], hpd[,"upper"])
rownames(tab) <- c("Intercepto - beta","Taxa urbana - beta","Taxa pública - beta",
                   "IDHM Renda - beta","log(População) - beta",
                   "log(PIB per capita) - beta",
                   "Intercepto - gamma","Taxa urbana - gamma","Taxa pública - gamma",
                   "IDHM Renda - gamma","log(População) - gamma",
                   "log(PIB per capita) - gamma","tau_theta", "tau_zeta")
colnames(tab) <- c("Média", "D.P.", "HPD inf", "HPD sup")
round(tab, 3)



################################################################=
####                   2015 SEM ZEROS                       ####       
################################################################=

rm(list = ls(all = TRUE))

dat <- readRDS("./Dados/dados_reais/tx_2015_EM_sem_zeros.rds")
glimpse(dat)
S_delta <- readRDS("./Dados/dados_reais/m_neighborhood_2015_EM_sem_zeros.rds")

N <- dim(dat)[1]
q <- 5+1
y <- (dat$tx_muni/100) %>% as.vector()
x <- matrix(
  c(rep(1, N),
    dat$taxa_urbana,
    dat$taxa_publica,
    dat$IDHM.Renda,
    dat$log_pop,
    dat$log_PIB_pc),
  ncol = 6
)

# Normal Multivariada
m_beta <- rep(0, q)
S_beta <- 10* diag(q)
# Normal Multivariada
m_gamma <- rep(0, q)
S_gamma <- 10* diag(q)
# Normal Multivariada
m_delta_t <- rep(0, N)
S_delta_t <- S_delta
# Normal Multivariada
m_delta_z <- rep(0, N)
S_delta_z <- S_delta
# Gama com E(tau) = 1 e Var(tau) = 10.
a_tau_t <- 0.1
b_tau_t <- 0.1
# Gama com E(tau) = 1 e Var(tau) = 10.
a_tau_z <- 0.1
b_tau_z <- 0.1

data <- list(N = N, q = q, y = y, x = x,
             m_beta = m_beta, S_beta = S_beta,
             m_gamma = m_gamma, S_gamma = S_gamma,
             m_delta_t = m_delta_t, S_delta_t = S_delta_t,
             m_delta_z = m_delta_z, S_delta_z = S_delta_z,
             a_tau_t = a_tau_t, b_tau_t = b_tau_t,
             a_tau_z = a_tau_z, b_tau_z = b_tau_z
)

pars <- c("beta", "gamma", "delta_theta", "delta_zeta", "tau_theta", "tau_zeta")
init <- list()
init[[1]] <- list(beta = runif(q, -0.1, 0.1), gamma = runif(q, -0.1, 0.1), 
                  delta_theta = rep(0, N), 
                  delta_zeta = rep(0, N), tau_theta = 2, tau_zeta = 2)
iter <- 5000
warmup <- 2500  
chains <- 1    

Sys.time()
output <- stan(file = "Scripts/stan/RegBetaDeltaZetaDelta.stan",
               data = data,
               iter = iter,
               warmup = warmup,
               chains = chains,
               pars = pars,
               init = init,
               verbose = FALSE)
saveRDS(output, "Dados/outputs_reais/output_tx_2015_EM_sem_zeros.RDS")
output <- readRDS("Dados/outputs_reais/output_tx_2015_EM_sem_zeros.RDS")
Sys.time()

rstan::traceplot(output, pars = c("beta", "gamma", "tau_theta", "tau_zeta"))

samp <- rstan::extract(output)

plot( density(samp$beta[,1]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de beta0", col = "blue" )
plot( density(samp$beta[,2]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de beta1", col = "blue" )
plot( density(samp$beta[,3]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de beta2", col = "blue" )
plot( density(samp$gamma[,1]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de gamma0", col = "blue" )
plot( density(samp$gamma[,2]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de gamma1", col = "blue" )
plot( density(samp$gamma[,3]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de gamma2", col = "blue" )
plot( density(samp$tau_theta), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de tau_theta", col = "blue" )
plot( density(samp$tau_zeta), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de tau_zeta", col = "blue" )


aux <- cbind(samp$beta, samp$gamma, samp$tau_theta, samp$tau_zeta)
me <- apply(aux, 2, mean)     # média
sd <- apply(aux, 2, sd)       # desvio padrão
aux <- as.mcmc(aux)
hpd <- HPDinterval(aux)
tab <- cbind(me, sd, hpd[,"lower"], hpd[,"upper"])
rownames(tab) <- c("Intercepto - beta","Taxa urbana - beta","Taxa pública - beta",
                   "IDHM Renda - beta","log(População) - beta",
                   "log(PIB per capita) - beta",
                   "Intercepto - gamma","Taxa urbana - gamma","Taxa pública - gamma",
                   "IDHM Renda - gamma","log(População) - gamma",
                   "log(PIB per capita) - gamma","tau_theta", "tau_zeta")
colnames(tab) <- c("Média", "D.P.", "HPD inf", "HPD sup")
round(tab, 3)



################################################################=
####                   2010 SEM ZEROS                       ####       
################################################################=

rm(list = ls(all = TRUE))

dat <- readRDS("./Dados/dados_reais/tx_2010_EM_sem_zeros.rds")
glimpse(dat)
S_delta <- readRDS("./Dados/dados_reais/m_neighborhood_2010_EM_sem_zeros.rds")

N <- dim(dat)[1]
q <- 5+1
y <- (dat$tx_muni/100) %>% as.vector()
x <- matrix(
  c(rep(1, N),
    dat$taxa_urbana,
    dat$taxa_publica,
    dat$IDHM.Renda,
    dat$log_pop,
    dat$log_PIB_pc),
  ncol = 6
)

# Normal Multivariada
m_beta <- rep(0, q)
S_beta <- 10* diag(q)
# Normal Multivariada
m_gamma <- rep(0, q)
S_gamma <- 10* diag(q)
# Normal Multivariada
m_delta_t <- rep(0, N)
S_delta_t <- S_delta
# Normal Multivariada
m_delta_z <- rep(0, N)
S_delta_z <- S_delta
# Gama com E(tau) = 1 e Var(tau) = 10.
a_tau_t <- 0.1
b_tau_t <- 0.1
# Gama com E(tau) = 1 e Var(tau) = 10.
a_tau_z <- 0.1
b_tau_z <- 0.1

data <- list(N = N, q = q, y = y, x = x,
             m_beta = m_beta, S_beta = S_beta,
             m_gamma = m_gamma, S_gamma = S_gamma,
             m_delta_t = m_delta_t, S_delta_t = S_delta_t,
             m_delta_z = m_delta_z, S_delta_z = S_delta_z,
             a_tau_t = a_tau_t, b_tau_t = b_tau_t,
             a_tau_z = a_tau_z, b_tau_z = b_tau_z
)

pars <- c("beta", "gamma", "delta_theta", "delta_zeta", "tau_theta", "tau_zeta")
init <- list()
init[[1]] <- list(beta = runif(q, -0.1, 0.1), gamma = runif(q, -0.1, 0.1), 
                  delta_theta = rep(0, N), 
                  delta_zeta = rep(0, N), tau_theta = 2, tau_zeta = 2)
iter <- 5000
warmup <- 2500  
chains <- 1    

Sys.time()
output <- stan(file = "Scripts/stan/RegBetaDeltaZetaDelta.stan",
               data = data,
               iter = iter,
               warmup = warmup,
               chains = chains,
               pars = pars,
               init = init,
               verbose = FALSE)
saveRDS(output, "Dados/outputs_reais/output_tx_2010_EM_sem_zeros.RDS")
output <- readRDS("Dados/outputs_reais/output_tx_2010_EM_sem_zeros.RDS")
Sys.time()

rstan::traceplot(output, pars = c("beta", "gamma", "tau_theta", "tau_zeta"))

samp <- rstan::extract(output)

plot( density(samp$beta[,1]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de beta0", col = "blue" )
plot( density(samp$beta[,2]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de beta1", col = "blue" )
plot( density(samp$beta[,3]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de beta2", col = "blue" )
plot( density(samp$beta[,4]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de beta2", col = "blue" )
plot( density(samp$beta[,5]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de beta2", col = "blue" )
plot( density(samp$beta[,6]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de beta2", col = "blue" )
plot( density(samp$gamma[,1]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de gamma0", col = "blue" )
plot( density(samp$gamma[,2]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de gamma1", col = "blue" )
plot( density(samp$gamma[,3]), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de gamma2", col = "blue" )
plot( density(samp$tau_theta), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de tau_theta", col = "blue" )
plot( density(samp$tau_zeta), cex.lab = 1.5, cex.axis = 1.5, lwd = 2,
      main = "Densidade a posteriori de tau_zeta", col = "blue" )


aux <- cbind(samp$beta, samp$gamma, samp$tau_theta, samp$tau_zeta)
me <- apply(aux, 2, mean)     # média
sd <- apply(aux, 2, sd)       # desvio padrão
aux <- as.mcmc(aux)
hpd <- HPDinterval(aux)
tab <- cbind(me, sd, hpd[,"lower"], hpd[,"upper"])
rownames(tab) <- c("Intercepto - beta","Taxa urbana - beta","Taxa pública - beta",
                   "IDHM Renda - beta","log(População) - beta",
                   "log(PIB per capita) - beta",
                   "Intercepto - gamma","Taxa urbana - gamma","Taxa pública - gamma",
                   "IDHM Renda - gamma","log(População) - gamma",
                   "log(PIB per capita) - gamma","tau_theta", "tau_zeta")
colnames(tab) <- c("Média", "D.P.", "HPD inf", "HPD sup")
round(tab, 3)
