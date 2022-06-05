########################################################=
#### ALGORITMO MONTE CARLO PARA SIMULAÇÕES NO COLAB ####
########################################################=

# BLOCO 1 - INSTALANDO O RSTAN --------------------------
library(devtools)
if(!require(cmdstanr)){
  devtools::install_github("stan-dev/cmdstanr", dependencies=c("Depends", "Imports"))
  library(cmdstanr) }
# Install cmdStan binaries.
if(!file.exists("cmdstan-2.23.0.tar.gz")){
  system("wget https://github.com/stan-dev/cmdstan/releases/download/v2.23.0/colab-cmdstan-2.23.0.tar.gz", intern=T)
  system("tar zxf colab-cmdstan-2.23.0.tar.gz", intern=T) }
list.files("cmdstan-2.23.0")
# Set cmdstan_path to cmdStan installation
set_cmdstan_path("cmdstan-2.23.0")
# -------------------------------------------------------=

# BLOCO 2 - CARREGANDO OS PACOTES -----------------------
library(tidyverse)
library(cmdstanr)
library(posterior)
library(bayesplot)
# -------------------------------------------------------=

# BLOCO 3 - OBJETOS PARA O MODELO -----------------------
dat <- readRDS("/content/dat1_50.rds")

N <- dim(dat$y)[1]
q <- 2+1
y <- dat$y %>% as.vector()
x <- dat$x

# Normal Multivariada
m_beta <- rep(0, q); S_beta <- 10* diag(q)
# Gama com E(zeta) = 1 e Var(zeta) = 10.
a_zeta <- 0.1; b_zeta <- 0.1
# Normal Multivariada
m_delta <- rep(0, N); S_delta <- solve(dat$D - 0.95*dat$W)
# Gama com E(tau) = 1 e Var(tau) = 10.
a_tau <- 0.1; b_tau <- 0.1

data <- list(N = N, q = q, y = y, x = x,
             m_beta = m_beta, S_beta = S_beta,
             a_zeta = a_zeta, b_zeta = b_zeta,
             m_delta = m_delta, S_delta = S_delta,
             a_tau = a_tau, b_tau = b_tau
)

# pars <- c("beta", "zeta", "delta", "tau")
init <- list()
init[[1]] <- list(beta = rep(1, q), zeta = 1, delta = rep(0, N), tau = 2)
chains <- 1
warmup <- 2500  
iter <- 5000
# -------------------------------------------------------=

# BLOCO 4 - ARMAZENANDO MODELO --------------------------
set_cmdstan_path(path = NULL)
file <- file.path('/content/RegBetaDelta.stan')
mod <- cmdstan_model(file)
# -------------------------------------------------------=

# BLOCO 5 - MODELO --------------------------------------
output <- mod$sample(
  data = data,
  init = init,
  chains = chains,
  iter_warmup = warmup,
  iter_sampling = iter
)
# -------------------------------------------------------=

# BLOCO 5 - SALVANDO MODELO -----------------------------
saveRDS(output, '/content/outputs/output_dat1_50_beta_sim.rds')
# -------------------------------------------------------=

# BLOCO 6 - PLOTS ---------------------------------------
beta <- as_draws_matrix(output$draws("beta"))
zeta <- as_draws_matrix(output$draws("zeta"))
tau <- as_draws_matrix(output$draws("tau"))

mcmc_trace(beta); mcmc_trace(zeta); mcmc_trace(tau)
mcmc_hist(output$draws("beta")); mcmc_hist(output$draws("zeta")); mcmc_hist(output$draws("tau"))
# -------------------------------------------------------=

# .====
# .====
# .====

########################################################=
####                     BLOCÃO                     ####
########################################################=

# REPRODUZINDO AS SIMULAÇÕES RODADAS NO COLAB COM AS MESMAS SEMENTES
# E USANDO O cmdstanr

library(devtools)
if(!require(cmdstanr)){
  install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
  library(cmdstanr) }
if(!require(posterior)){
  install.packages("posterior")
  library(posterior) }
if(!require(bayesplot)){
  install.packages("bayesplot")
  library(bayesplot) }
library(tidyverse)
library(mvtnorm)

# BLOCO 3 - FUNÇÃO PARA GERAÇÃO DOS DADOS ---------------
gera_dados <- function(input_n, input_tau, input_zeta, input_beta, seed) {
  
  set.seed(seed)
  
  N <- input_n  # number of locations
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
  rho <- 0.95 
  tau <- input_tau # true variance.
  Sig <- tau * solve(D-rho*W)
  delta <- rmvnorm(1, rep(0, N), Sig)
  
  beta <- input_beta # true coefficients.
  # covariates.
  x <- array(1, c(N, 3))
  x[,2] <- rbinom(N, 1, 0.5)
  x[,3] <- round(runif(N, -1, 1), digits = 5)
  
  # generate the response.
  y <- array(0, c(N, 1))
  theta <- array(0, c(N, 1)) # E(Y) 
  zeta <- input_zeta  # valor real de zeta
  
  for (n in 1:N) {
    aux = exp(-x[n,] %*% beta - delta[n])    # exp[ X_i^T * Beta + Delta_i]
    theta[n] = 1 / (1+aux)
    y[n] = rbeta(1, shape1 = theta[n]*zeta, shape2 = (1-theta[n])*zeta)
  }
  
  real <- list(theta = theta, zeta = zeta, beta = beta, 
               delta = delta, tau = tau, rho = rho)
  dat <- list(y = y, x = x, D = D, W = W)
  
  return(list(dat = dat, real = real))
  
}
# -------------------------------------------------------=

# GERANDO OS DADOS E SALVANDO ----

for (i in 1:10) {
  for (j in c(50,100,200)) {
    
    semente <- sementes %>% filter(n==j, sim==i) %>% pull(semente)
    lista_gerada <- gera_dados(input_n = j, 
                               input_tau = 2, 
                               input_zeta = 2,
                               input_beta = c(0.5, -3.0, 1.0),
                               seed = semente
    )
    rm(list = setdiff(ls(), c("i", "j", "lista_gerada", "gera_dados", "sementes")))
    dat <- lista_gerada[["dat"]]
    real <- lista_gerada[["real"]]
    
    plot(density(dat$y), main = paste0("DADOS SIMULAÇÃO ",i,": N=",j))
    
    saveRDS(dat, paste0('./Dados/sim_colab/dat_real/dat2_',j,'_sim',i,'.rds'))
    saveRDS(real, paste0('./Dados/sim_colab/dat_real/real2_',j,'_sim',i,'.rds'))
  }
}


# BLOCO 4 - MCMC ----------------------------------------

sementes <- readRDS("~/GitHub/Monografia/Dados/sementes.rds")

for (i in 1:10) {           # simulação
  
  for (j in c(50,100,200)) { # tamanho amostral
    
    print(paste0("INÍCIO SIMULAÇÃO ",i,": N=",j))
    
    rm(list = setdiff(ls(), c("i", "j", "gera_dados","sementes")))
    
    semente <- sementes %>% filter(n==j, sim==i) %>% pull(semente)
    
    # GERA OS DADOS
    lista_gerada <- gera_dados(input_n = j, 
                               input_tau = 2, 
                               input_zeta = 2,
                               input_beta = c(0.5, -3.0, 1.0),
                               seed = semente
    )
    rm(list = setdiff(ls(), c("i", "j", "lista_gerada", "gera_dados", "sementes")))
    dat <- lista_gerada[["dat"]]
    real <- lista_gerada[["real"]]
    
    plot(density(dat$y), main = paste0("DADOS SIMULAÇÃO ",i,": N=",j))
    
    saveRDS(dat, paste0('./Dados/sim_colab/dat_real/dat2_',j,'_sim',i,'.rds'))
    saveRDS(real, paste0('./Dados/sim_colab/dat_real/real2_',j,'_sim',i,'.rds'))
    
    # DEFINIÇÃO DOS PARÂMETROS PARA MODELAGEM
    N <- dim(dat$y)[1]
    q <- 2+1
    y <- dat$y %>% as.vector()
    x <- dat$x
    
    m_beta <- rep(0, q); S_beta <- 10* diag(q) # Normal Multivariada
    a_zeta <- 0.1; b_zeta <- 0.1 # Gama com E(zeta) = 1 e Var(zeta) = 10.
    m_delta <- rep(0, N); S_delta <- round(solve(dat$D - 0.95*dat$W), 5) # Normal Multivariada
    a_tau <- 0.1; b_tau <- 0.1 # Gama com E(tau) = 1 e Var(tau) = 10.
    
    data <- list(N = N, q = q, y = y, x = x,
                 m_beta = m_beta, S_beta = S_beta,
                 a_zeta = a_zeta, b_zeta = b_zeta,
                 m_delta = m_delta, S_delta = S_delta,
                 a_tau = a_tau, b_tau = b_tau
    )
    
    init <- list(
      list(beta = runif(q, -0.1, 0.1), zeta = 1, delta = runif(N, -0.1, 0.1), tau = 2)
    )
    chains <- 1
    warmup <- 2500  
    iter <- 2500
    
    # ARMAZENANDO O MODELO
    file <- file.path('./Scripts/stan/RegBetaDelta.stan')
    mod <- cmdstan_model(file)
    
    # MODELAGEM
    output <- mod$sample(
      data = data,
      init = init,
      chains = chains,
      iter_warmup = warmup,
      iter_sampling = iter
    )
    
    saveRDS(output, paste0('./Dados/sim_colab/outputs/output_dat2_',j,'_beta_sim',i,'.rds'))
    
    print(paste0("FIM SIMULAÇÃO ",i,": N=",j))
    
  }
  
}

# -------------------------------------------------------=