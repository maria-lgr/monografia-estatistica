# SCRIPT GERAÇÃO DE DADOS ARTIFICIAIS - BETA ZETA (COM EFEITO NO ZETA)

rm(list = ls(all = TRUE))
library("mvtnorm")
library(tidyverse)

#################################################################=
# alpha = shape1
# beta = shape2
# 
# transformação:
# theta = alpha / (alpha + beta) = alpha / zeta
# zeta = alpha + beta
# 
# shape1 --> theta * zeta
# shape2 --> (1 - theta) * zeta
#################################################################=

sementes <- readRDS("./Dados/sim_colab/sim_beta_zeta_delta/sementes.rds")

# !!! PRO N=100 USEI A SIM 2

for (n_tamanho in c(50, 100, 200)) {
  
  rm(list = setdiff(ls(), c("n_tamanho", "sementes")))
  
  semente <- sementes %>% filter(sim == 1, n == n_tamanho) %>% pull(semente)
  set.seed(semente)
  
  N <- n_tamanho  # number of locations
  num <- 4 # neighbors/region (even number).
  W <- array(0, c(N, N))
  
  for (n1 in 1:N) {
    for (n2 in 1:N) {
      if (abs(n1-n2) <= num/2 & abs(n1-n2) > 0) {
        W[n1, n2] = 1
      }
    }
  }
  
  D <- diag(apply(W, 1, sum))
  
  # Generate the spatial random effects.
  set.seed(semente+1)
  rho <- 0.95 
  tau_theta <- 2 # true variance.
  Sig_theta <- tau_theta * solve(D-rho*W)
  delta_theta <- rmvnorm(1, rep(0, N), Sig_theta)
  
  # Generate the spatial random effects.
  set.seed(semente+2)
  rho <- 0.95 
  tau_zeta <- 1 # true variance.
  Sig_zeta <- tau_zeta * solve(D-rho*W)
  delta_zeta <- rmvnorm(1, rep(0, N), Sig_zeta)
  
  set.seed(semente)
  beta <- c(-0.75, 0.5, -1.5)  # true coefficients.
  gamma <- c(1.5, 1.0, 2.0)    # true coefficients.
  
  # covariates.
  x <- array(1, c(N, 3))
  x[,2] <- rbinom(N, 1, 0.5)
  x[,3] <- round(runif(N, -1, 1), digits = 5)
  
  # generate the gamma response.
  y <- array(0, c(N, 1))
  theta <- array(0, c(N, 1)) # E(Y) 
  zeta <- array(0, c(N, 1))
  
  for (n in 1:N) {
    zeta[n] <- exp(x[n,] %*% gamma + delta_zeta[n])
    aux = exp(-x[n,] %*% beta - delta_theta[n])    # exp[ X_i^T * Beta + Delta_i]
    theta[n] = 1 / (1+aux)
    y[n] = rbeta(1, shape1 = theta[n]*zeta[n], shape2 = (1-theta[n])*zeta[n])
  }
  
  plot(density(y))
  
  real <- list()
  real[[1]] <- theta
  real[[2]] <- zeta
  real[[3]] <- beta
  real[[4]] <- gamma
  real[[5]] <- delta_theta
  real[[6]] <- delta_zeta
  real[[7]] <- tau_theta
  real[[8]] <- tau_zeta
  real[[9]] <- rho
  names(real) <- c("theta", "zeta", "beta", "gamma", "delta_theta", 
                   "delta_zeta", "tau_theta", "tau_zeta", "rho")
  
  dat <- list()
  dat[[1]] <- y
  dat[[2]] <- x
  dat[[3]] <- D
  dat[[4]] <- W
  names(dat) <- c("y", "x", "D", "W")
  
  rm(list = setdiff(ls(), c("dat", "real", "n_tamanho", "sementes")))
  
  saveRDS(dat, paste0("./Dados/dados_artificiais/beta/dat_zeta_delta_",n_tamanho,".rds"))
  saveRDS(real, paste0("./Dados/dados_artificiais/beta/real_zeta_delta_",n_tamanho,".rds"))
  
}
