########################################################=
####  PREPARAÇÃO GRÁFICOS THETA i (ESTUDO SIMULADO) ####
########################################################=

library(tidyverse)
library(rstan)
library(coda)

options(OutDec = ",")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# dados_theta_i <- tibble("dist"="dist","N"=0,"param"="param","real"=0,"est"=0,
#                         "var1"=0, "var2"=0)
# saveRDS(dados_theta_i, "./Dados/dados_artificiais/dados_theta_i.rds")

calcula_theta_i <- function(x, samp, real, dist, N, param) {
  estimado <- matrix(ncol = N, nrow = 2500)
  
  if (dist == "gama") { 
    # gama estimando betas
    for (i in 1:2500) {
      for (n in 1:N) {
        estimado[i,n] <- exp(x[n,] %*% samp$beta[i,] + samp$delta[i,n])
      }
    }
  } else { # beta
    if (dist == "beta" | ((dist == "beta_zeta" | dist == "beta_zeta_delta_trocado") & param == "betas")) {
      # beta estimando betas ou beta zeta estimando betas ou beta zeta delta trocado
      for (i in 1:2500) {
        for (n in 1:N) {
          estimado[i,n] <- 1 / (1 + exp(-x[n,] %*% samp$beta[i,] - samp$delta[i,n]))
        }
      }
    } else if ((dist == "beta_zeta" | dist == "beta_zeta_delta_trocado") & param == "gammas") {
      # beta zeta estimando gammas ou beta zeta delta trocado estimando gammas
      print("caiu aqui")
      for (i in 1:2500) {
        for (n in 1:N) {
          estimado[i,n] <- exp(x[n,] %*% samp$gamma[i,])
        }
      }
    } else if (dist == "beta_zeta_delta" | dist == "beta_zeta_trocado") {
      if (param == "betas") {
        # beta zeta delta estimando betas ou beta zeta trocado estimando betas
        for (i in 1:2500) {
          for (n in 1:N) {
            estimado[i,n] <- 1 / (1 + exp(-x[n,] %*% samp$beta[i,] - samp$delta_theta[i,n]))
          }
        }
      } else {
        # beta zeta delta estimando gammas ou beta zeta trocado estimando gammas
        for (i in 1:2500) {
          for (n in 1:N) {
            estimado[i,n] <- exp(x[n,] %*% samp$gamma[i,] + samp$delta_zeta[i,n])
          }
        }
      }
    }
  }
  
  valores_est <- apply(estimado, 2, mean)

  dados <- data.frame("dist" = dist, "N" = N, 
                      "param" = param, "real" = real, "est" = valores_est,
                      "var1" = x[,2], "var2" = x[,3])
  return(dados)
  
}
    
    

# GAMA ----
for (n in c(50,100,200)) {
    samp <- rstan::extract(readRDS(paste0("./Dados/outputs/novo/output_dat_",n,"_gama.RDS")))
    real <- readRDS(paste0("./Dados/dados_artificiais/gama/real_",n,".rds"))$theta
    x <- readRDS(paste0("./Dados/dados_artificiais/gama/dat_",n,".rds"))$x
    dados_theta_i <- readRDS("./Dados/dados_artificiais/dados_theta_i.rds") %>%
      filter(!(dist == "gama" & N == n)) %>%
      bind_rows(calcula_theta_i(x, samp, real, "gama", n, "betas"))
    saveRDS(dados_theta_i, "./Dados/dados_artificiais/dados_theta_i.rds")
    rm("dados_theta_i","samp","real","x")
}


# BETA ----
for (n in c(50,100,200)) {
    samp <- rstan::extract(readRDS(paste0("./Dados/outputs/novo/output_dat_",n,"_beta.RDS")))
    real <- readRDS(paste0("./Dados/dados_artificiais/beta/real_",n,".rds"))$theta
    x <- readRDS(paste0("./Dados/dados_artificiais/beta/dat_",n,".rds"))$x
    dados_theta_i <- readRDS("./Dados/dados_artificiais/dados_theta_i.rds") %>%
      filter(!(dist == "beta" & N == n)) %>%
      bind_rows(calcula_theta_i(x, samp, real, "beta", n, "betas"))
    saveRDS(dados_theta_i, "./Dados/dados_artificiais/dados_theta_i.rds")
    rm("dados_theta_i","samp","real","x")
}

# BETA ZETA ----
# beta
for (n in c(50,100,200)) {
    samp <- rstan::extract(readRDS(paste0("./Dados/outputs/novo/output_dat_zeta_",n,"_beta.RDS")))
    real <- readRDS(paste0("./Dados/dados_artificiais/beta/real_zeta_",n,".rds"))$theta
    x <- readRDS(paste0("./Dados/dados_artificiais/beta/dat_zeta_",n,".rds"))$x
    dados_theta_i <- readRDS("./Dados/dados_artificiais/dados_theta_i.rds") %>%
      filter(!(dist == "beta_zeta" & N == n & param == "betas")) %>%
      bind_rows(calcula_theta_i(x, samp, real, "beta_zeta", n, "betas"))
    saveRDS(dados_theta_i, "./Dados/dados_artificiais/dados_theta_i.rds")
    rm("dados_theta_i","samp","real","x")
}

# gamma
for (n in c(50, 100, 200)) {
  samp <- rstan::extract(readRDS(paste0("./Dados/outputs/novo/output_dat_zeta_",n,"_beta.RDS")))
  real <- readRDS(paste0("./Dados/dados_artificiais/beta/real_zeta_",n,".rds"))$zeta
  x <- readRDS(paste0("./Dados/dados_artificiais/beta/dat_zeta_",n,".rds"))$x
  dados_theta_i <- readRDS("./Dados/dados_artificiais/dados_theta_i.rds") %>%
    filter(!(dist == "beta_zeta" & N == n & param == "gammas")) %>%
    bind_rows(calcula_theta_i(x, samp, real, "beta_zeta", n, "gammas"))
  saveRDS(dados_theta_i, "./Dados/dados_artificiais/dados_theta_i.rds")
  rm("dados_theta_i","samp","real","x")
}


# BETA ZETA DELTA ----
# delta_theta
for (n in c(50,100,200)) {
  samp <- rstan::extract(readRDS(paste0("./Dados/outputs/novo/output_dat_zeta_delta_",n,"_beta.RDS")))
  real <- readRDS(paste0("./Dados/dados_artificiais/beta/real_zeta_delta_",n,".rds"))$theta
  x <- readRDS(paste0("./Dados/dados_artificiais/beta/dat_zeta_delta_",n,".rds"))$x
  dados_theta_i <- readRDS("./Dados/dados_artificiais/dados_theta_i.rds") %>%
    filter(!(dist == "beta_zeta_delta" & N == n & param == "betas")) %>%
    bind_rows(calcula_theta_i(x, samp, real, "beta_zeta_delta", n, "betas"))
  saveRDS(dados_theta_i, "./Dados/dados_artificiais/dados_theta_i.rds")
  rm("dados_theta_i","samp","real","x")
}

# delta_zeta
for (n in c(50,100,200)) {
  samp <- rstan::extract(readRDS(paste0("./Dados/outputs/novo/output_dat_zeta_delta_",n,"_beta.RDS")))
  real <- readRDS(paste0("./Dados/dados_artificiais/beta/real_zeta_delta_",n,".rds"))$zeta
  x <- readRDS(paste0("./Dados/dados_artificiais/beta/dat_zeta_delta_",n,".rds"))$x
  dados_theta_i <- readRDS("./Dados/dados_artificiais/dados_theta_i.rds") %>%
    filter(!(dist == "beta_zeta_delta" & N == n & param == "gammas")) %>%
    bind_rows(calcula_theta_i(x, samp, real, "beta_zeta_delta", n, "gammas"))
  saveRDS(dados_theta_i, "./Dados/dados_artificiais/dados_theta_i.rds")
  rm("dados_theta_i","samp","real","x")
}


# BETA ZETA TROCADO ----
# delta_theta
for (n in c(50,100,200)) {
  samp <- rstan::extract(readRDS(paste0("./Dados/outputs/novo/output_dat_zeta_trocado_",n,"_beta.RDS")))
  real <- readRDS(paste0("./Dados/dados_artificiais/beta/real_zeta_",n,".rds"))$theta
  x <- readRDS(paste0("./Dados/dados_artificiais/beta/dat_zeta_",n,".rds"))$x
  dados_theta_i <- readRDS("./Dados/dados_artificiais/dados_theta_i.rds") %>%
    filter(!(dist == "beta_zeta_trocado" & N == n & param == "betas")) %>%
    bind_rows(calcula_theta_i(x, samp, real, "beta_zeta_trocado", n, "betas"))
  saveRDS(dados_theta_i, "./Dados/dados_artificiais/dados_theta_i.rds")
  rm("dados_theta_i","samp","real","x")
}

# delta_zeta
for (n in c(50,100,200)) {
  samp <- rstan::extract(readRDS(paste0("./Dados/outputs/novo/output_dat_zeta_trocado_",n,"_beta.RDS")))
  real <- readRDS(paste0("./Dados/dados_artificiais/beta/real_zeta_",n,".rds"))$zeta
  x <- readRDS(paste0("./Dados/dados_artificiais/beta/dat_zeta_",n,".rds"))$x
  dados_theta_i <- readRDS("./Dados/dados_artificiais/dados_theta_i.rds") %>%
    filter(!(dist == "beta_zeta_trocado" & N == n & param == "gammas")) %>%
    bind_rows(calcula_theta_i(x, samp, real, "beta_zeta_trocado", n, "gammas"))
  saveRDS(dados_theta_i, "./Dados/dados_artificiais/dados_theta_i.rds")
  rm("dados_theta_i","samp","real","x")
}


# BETA ZETA DELTA TROCADO ----
# beta
for (n in c(50,100,200)) {
  samp <- rstan::extract(readRDS(paste0("./Dados/outputs/novo/output_dat_zeta_delta_trocado_",n,"_beta.RDS")))
  real <- readRDS(paste0("./Dados/dados_artificiais/beta/real_zeta_delta_",n,".rds"))$theta
  x <- readRDS(paste0("./Dados/dados_artificiais/beta/dat_zeta_delta_",n,".rds"))$x
  dados_theta_i <- readRDS("./Dados/dados_artificiais/dados_theta_i.rds") %>%
    filter(!(dist == "beta_zeta_delta_trocado" & N == n & param == "betas")) %>%
    bind_rows(calcula_theta_i(x, samp, real, "beta_zeta_delta_trocado", n, "betas"))
  saveRDS(dados_theta_i, "./Dados/dados_artificiais/dados_theta_i.rds")
  rm("dados_theta_i","samp","real","x")
}

# gamma
for (n in c(50,100,200)) {
  samp <- rstan::extract(readRDS(paste0("./Dados/outputs/novo/output_dat_zeta_delta_",n,"_beta.RDS")))
  real <- readRDS(paste0("./Dados/dados_artificiais/beta/real_zeta_delta_",n,".rds"))$zeta
  x <- readRDS(paste0("./Dados/dados_artificiais/beta/dat_zeta_delta_",n,".rds"))$x
  dados_theta_i <- readRDS("./Dados/dados_artificiais/dados_theta_i.rds") %>%
    filter(!(dist == "beta_zeta_delta_trocado" & N == n & param == "gammas")) %>%
    bind_rows(calcula_theta_i(x, samp, real, "beta_zeta_delta_trocado", n, "gammas"))
  saveRDS(dados_theta_i, "./Dados/dados_artificiais/dados_theta_i.rds")
  rm("dados_theta_i","samp","real","x")
}


dados_theta_i <- readRDS("./Dados/dados_artificiais/dados_theta_i.rds")
dados_theta_i
