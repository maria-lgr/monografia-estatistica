########################################################=
####      GERAÇÃO DE DADOS ARTIFICIAIS - GAMA       ####
########################################################=

# beta = c(1.5, 0.5, 0.5)  
# alpha = 10  

rm(list = ls(all = TRUE))
library("mvtnorm")
library(tidyverse)

#################################################################=
# alpha = shape
# delta = rate --> rate = 1/scale
# 
# transformação:
# theta = alpha / delta 
# 
# shape --> alpha
# rate  --> alpha/theta
#################################################################=

sementes <- readRDS("./Dados/sim_colab/sim_gama/outputs/sementes.rds")

for (n_tamanho in c(50, 100, 200)) {
  
  rm(list = setdiff(ls(), c("n_tamanho", "sementes")))
  
  semente <- sementes %>% filter(sim == 1, n == n_tamanho) %>% pull(semente)
  set.seed(semente)
  
  N <- n_tamanho  # number of locations
  num <- 4        # neighbors/region (even number).
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
  rho <- 0.95 
  tau <- 2 # true variance.
  Sig <- tau * solve(D-rho*W)
  delta <- rmvnorm(1, rep(0, N), Sig)
  
  beta <- c(1.5, 0.5, 0.5) # true coefficients.
  
  # covariates.
  x <- array(1, c(N, 3))
  x[,2] <- rbinom(N, 1, 0.5)
  x[,3] <- round(runif(N, -1, 1), digits = 5)
  
  # generate the gamma response.
  y <- array(0, c(N, 1))
  theta <- array(0, c(N, 1)) # E(Y) 
  alpha <- 10  # valor real da forma da gama
  
  for (n in 1:N) {
    theta[n] = exp(x[n,] %*% beta + delta[n])    # exp[ X_i^T * Beta + Delta_i]
    y[n] = rgamma(1, shape = alpha, rate = alpha/theta[n]) 
  }
  
  hist(y)
  plot(density(y))
  
  real <- list()
  real[[1]] <- theta
  real[[2]] <- alpha
  real[[3]] <- beta
  real[[4]] <- delta
  real[[5]] <- tau
  real[[6]] <- rho
  names(real) <- c("theta", "alpha", "beta", "delta", "tau", "rho")
  
  dat <- list()
  dat[[1]] <- y
  dat[[2]] <- x
  dat[[3]] <- D
  dat[[4]] <- W
  names(dat) <- c("y", "x", "D", "W")
  
  rm(list = setdiff(ls(), c("dat", "real", "n_tamanho", "sementes")))
  
  saveRDS(dat, paste0("./Dados/dados_artificiais/gama/dat_",n_tamanho,".rds"))
  saveRDS(real, paste0("./Dados/dados_artificiais/gama/real_",n_tamanho,".rds"))
  
}
