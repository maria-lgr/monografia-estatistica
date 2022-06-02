########################################################=
####      ANÁLISE DE RESÍDUOS - DADOS REAIS         ####
########################################################=

library(tidyverse)
library(rstan)
library(coda)
library(gridExtra)
library(spdep)

options(OutDec = ",")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


# THETAS E ZETAS i

# dados_theta_i_dados_reais <- tibble("nome" = "nome", "param" = "param", "est" = 0,
#                                     "taxa_urbana" = 0, "taxa_publica" = 0,
#                                     "IDHM.Renda" = 0, "log_pop" = 0,
#                                     "log_PIB_pc" = 0)
# saveRDS(dados_theta_i_dados_reais, "./Dados/dados_reais/dados_theta_i_dados_reais.rds")

calcula_theta_i <- function(dat, samp, param, nome) {
  
  N <- dim(dat)[1]
  x <- matrix(
    c(rep(1, N),
      dat$taxa_urbana,
      dat$taxa_publica,
      dat$IDHM.Renda,
      dat$log_pop,
      dat$log_PIB_pc),
    ncol = 6
  )
  estimado <- matrix(ncol = N, nrow = 2500)
  
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
  
  valores_est <- apply(estimado, 2, mean)
  
  dados <- data.frame("nome" = nome, "param" = param, "est" = valores_est,
                      "taxa_urbana" = x[,2], "taxa_publica" = x[,3],
                      "IDHM.Renda" = x[,4], "log_pop" = x[,5],
                      "log_PIB_pc" = x[,6])
  return(dados)
  
}

preditos <- function(dat, samp) {
  
  x <- matrix(
    c(rep(1, dim(dat)[1]),
      dat$taxa_urbana,
      dat$taxa_publica,
      dat$IDHM.Renda,
      dat$log_pop,
      dat$log_PIB_pc),
    ncol = 6
  )
  
  betas <- apply(samp$beta, 2, mean)
  delta_theta <- apply(samp$delta_theta, 2, mean)
  
  exp <- exp(x %*% betas + delta_theta)
  predito <- as.vector(exp/(1+exp))
  
  return(predito)
  
}

plot_residuo <- function(dados, var, titulo) {
  p <- dados %>% 
    mutate(x = !!sym(var)) %>% 
    ggplot(aes(x = x, y = residuo)) +
    geom_point() +
    geom_hline(yintercept = 0, color = "red", size = 1) +
    theme_minimal() +
    xlab(titulo) + ylab("Resíduo") +
    # ylim(c(-2,3.5)) +
    ggtitle(titulo)
  return(p)
  
}


# 2020 ------------------------------------------------------------

# delta_theta
samp <- rstan::extract(readRDS("./Dados/outputs_reais/output_tx_2020_EM.RDS"))
dat <- readRDS("./Dados/dados_reais/tx_2020_EM.rds")
dados_theta_i <- readRDS("./Dados/dados_reais/dados_theta_i_dados_reais.rds") %>%
  filter(!(nome == "EM 2020" & param == "betas")) %>%
  bind_rows(calcula_theta_i(dat, samp, "betas", "EM 2020"))
saveRDS(dados_theta_i, "./Dados/dados_reais/dados_theta_i_dados_reais.rds")
rm("dados_theta_i","samp","dat")

# delta_zeta
samp <- rstan::extract(readRDS("./Dados/outputs_reais/output_tx_2020_EM.RDS"))
dat <- readRDS("./Dados/dados_reais/tx_2020_EM.rds")
dados_theta_i <- readRDS("./Dados/dados_reais/dados_theta_i_dados_reais.rds") %>%
  filter(!(nome == "EM 2020" & param == "gammas")) %>%
  bind_rows(calcula_theta_i(dat, samp, "gammas", "EM 2020"))
saveRDS(dados_theta_i, "./Dados/dados_reais/dados_theta_i_dados_reais.rds")
rm("dados_theta_i","samp","dat")

samp <- rstan::extract(readRDS("./Dados/outputs_reais/output_tx_2020_EM.RDS"))
dat <- readRDS("./Dados/dados_reais/tx_2020_EM.rds")
dados_theta_i <- readRDS("./Dados/dados_reais/dados_theta_i_dados_reais.rds")
dados <- dat %>% 
  mutate(y = tx_muni/100,
         theta = dados_theta_i %>% filter(param=="betas" & nome=="EM 2020") %>% pull(est),
         zeta = dados_theta_i %>% filter(param=="gammas" & nome=="EM 2020") %>% pull(est),
         gphi = 1/(1+zeta),
         residuo = (y-theta)/sqrt(gphi*theta*(1-theta)),
         predito = theta
         )
dados %>% glimpse

png("./Imagens/residuos_2020.png", width = 550, height = 450)
grid.arrange(plot_residuo(dados, "predito", "Predito"),
             plot_residuo(dados, "taxa_urbana", "Taxa urbana (%)"),
             plot_residuo(dados, "taxa_publica", "Taxa pública (%)"),
             plot_residuo(dados, "IDHM.Renda", "IDHM Renda"),
             plot_residuo(dados, "log_pop", "log(População)"),
             plot_residuo(dados, "log_PIB_pc", "log(PIB per capita)"),
             ncol = 3,
             top = "Resíduos versus predito e variáveis do modelo para EM 2020"
)
dev.off()

dados2 <- dados %>% 
  filter(residuo != max(dados$residuo))
png("./Imagens/residuos_2020_sem_outlier.png", width = 550, height = 450)
grid.arrange(plot_residuo(dados2, "predito", "Predito"),
             plot_residuo(dados2, "taxa_urbana", "Taxa urbana (%)"),
             plot_residuo(dados2, "taxa_publica", "Taxa pública (%)"),
             plot_residuo(dados2, "IDHM.Renda", "IDHM Renda"),
             plot_residuo(dados2, "log_pop", "log(População)"),
             plot_residuo(dados2, "log_PIB_pc", "log(PIB per capita)"),
             ncol = 3,
             top = "Resíduos versus predito e variáveis do modelo para EM 2020 (sem outlier)"
)
dev.off()

# I de Moran
W.listw <- mat2listw(readRDS("./Dados/dados_reais/W_2020_EM.rds"))
moran.mc(dados$residuo, listw = W.listw, nsim = 1000) 
# statistic = -0,018641, observed rank = 123, p-value = 0,8771 < - - Não rejeita H0
# H0: Não existe associação espacial <- - - - 
# H1: Existe associação espacial 



# 2015 ------------------------------------------------------------

# delta_theta
samp <- rstan::extract(readRDS("./Dados/outputs_reais/output_tx_2015_EM.RDS"))
dat <- readRDS("./Dados/dados_reais/tx_2015_EM.rds")
dados_theta_i <- readRDS("./Dados/dados_reais/dados_theta_i_dados_reais.rds") %>%
  filter(!(nome == "EM 2015" & param == "betas")) %>%
  bind_rows(calcula_theta_i(dat, samp, "betas", "EM 2015"))
saveRDS(dados_theta_i, "./Dados/dados_reais/dados_theta_i_dados_reais.rds")
rm("dados_theta_i","samp","dat")

# delta_zeta
samp <- rstan::extract(readRDS("./Dados/outputs_reais/output_tx_2015_EM.RDS"))
dat <- readRDS("./Dados/dados_reais/tx_2015_EM.rds")
dados_theta_i <- readRDS("./Dados/dados_reais/dados_theta_i_dados_reais.rds") %>%
  filter(!(nome == "EM 2015" & param == "gammas")) %>%
  bind_rows(calcula_theta_i(dat, samp, "gammas", "EM 2015"))
saveRDS(dados_theta_i, "./Dados/dados_reais/dados_theta_i_dados_reais.rds")
rm("dados_theta_i","samp","dat")

samp <- rstan::extract(readRDS("./Dados/outputs_reais/output_tx_2015_EM.RDS"))
dat <- readRDS("./Dados/dados_reais/tx_2015_EM.rds")
dados_theta_i <- readRDS("./Dados/dados_reais/dados_theta_i_dados_reais.rds")
dados <- dat %>% 
  mutate(y = tx_muni/100,
         theta = dados_theta_i %>% filter(param=="betas" & nome=="EM 2015") %>% pull(est),
         zeta = dados_theta_i %>% filter(param=="gammas" & nome=="EM 2015") %>% pull(est),
         gphi = 1/(1+zeta),
         residuo = (y-theta)/sqrt(gphi*theta*(1-theta)),
         predito = theta
  )
dados %>% glimpse

png("./Imagens/residuos_2015.png", width = 550, height = 450)
grid.arrange(plot_residuo(dados, "predito", "Predito"),
             plot_residuo(dados, "taxa_urbana", "Taxa urbana (%)"),
             plot_residuo(dados, "taxa_publica", "Taxa pública (%)"),
             plot_residuo(dados, "IDHM.Renda", "IDHM Renda"),
             plot_residuo(dados, "log_pop", "log(População)"),
             plot_residuo(dados, "log_PIB_pc", "log(PIB per capita)"),
             ncol = 3,
             top = "Resíduos versus predito e variáveis do modelo para EM 2015"
)
dev.off()

# I de Moran
W.listw <- mat2listw(readRDS("./Dados/dados_reais/W_EM.rds"))
moran.mc(dados$residuo, listw = W.listw, nsim = 1000) 
# statistic = 0,00481, observed rank = 685, p-value = 0,3157 < - - Não rejeita H0
# H0: Não existe associação espacial <- - - - 
# H1: Existe associação espacial 


# 2010 ------------------------------------------------------------

# delta_theta
samp <- rstan::extract(readRDS("./Dados/outputs_reais/output_tx_2010_EM.RDS"))
dat <- readRDS("./Dados/dados_reais/tx_2010_EM.rds")
dados_theta_i <- readRDS("./Dados/dados_reais/dados_theta_i_dados_reais.rds") %>%
  filter(!(nome == "EM 2010" & param == "betas")) %>%
  bind_rows(calcula_theta_i(dat, samp, "betas", "EM 2010"))
saveRDS(dados_theta_i, "./Dados/dados_reais/dados_theta_i_dados_reais.rds")
rm("dados_theta_i","samp","dat")

# delta_zeta
samp <- rstan::extract(readRDS("./Dados/outputs_reais/output_tx_2010_EM.RDS"))
dat <- readRDS("./Dados/dados_reais/tx_2010_EM.rds")
dados_theta_i <- readRDS("./Dados/dados_reais/dados_theta_i_dados_reais.rds") %>%
  filter(!(nome == "EM 2010" & param == "gammas")) %>%
  bind_rows(calcula_theta_i(dat, samp, "gammas", "EM 2010"))
saveRDS(dados_theta_i, "./Dados/dados_reais/dados_theta_i_dados_reais.rds")
rm("dados_theta_i","samp","dat")

samp <- rstan::extract(readRDS("./Dados/outputs_reais/output_tx_2010_EM.RDS"))
dat <- readRDS("./Dados/dados_reais/tx_2010_EM.rds")
dados_theta_i <- readRDS("./Dados/dados_reais/dados_theta_i_dados_reais.rds")
dados <- dat %>% 
  mutate(y = tx_muni/100,
         theta = dados_theta_i %>% filter(param=="betas" & nome=="EM 2010") %>% pull(est),
         zeta = dados_theta_i %>% filter(param=="gammas" & nome=="EM 2010") %>% pull(est),
         gphi = 1/(1+zeta),
         residuo = (y-theta)/sqrt(gphi*theta*(1-theta)),
         predito = preditos(., samp)
  )
dados %>% glimpse

png("./Imagens/residuos_2010.png", width = 550, height = 450)
grid.arrange(plot_residuo(dados, "predito", "Predito"),
             plot_residuo(dados, "taxa_urbana", "Taxa urbana (%)"),
             plot_residuo(dados, "taxa_publica", "Taxa pública (%)"),
             plot_residuo(dados, "IDHM.Renda", "IDHM Renda"),
             plot_residuo(dados, "log_pop", "log(População)"),
             plot_residuo(dados, "log_PIB_pc", "log(PIB per capita)"),
             ncol = 3,
             top = "Resíduos versus predito e variáveis do modelo para EM 2010"
)
dev.off()

# I de Moran
W.listw <- mat2listw(readRDS("./Dados/dados_reais/W_2010_EM.rds"))
moran.mc(dados$residuo, listw = W.listw, nsim = 1000) 
# statistic = 0,014518, observed rank = 799, p-value = 0,2018 < - - Não rejeita H0
# H0: Não existe associação espacial <- - - - 
# H1: Existe associação espacial 
  

# 2020 SEM ZEROS ------------------------------------------------------------

# delta_theta
samp <- rstan::extract(readRDS("./Dados/outputs_reais/output_tx_2020_EM_sem_zeros.RDS"))
dat <- readRDS("./Dados/dados_reais/tx_2020_EM_sem_zeros.rds")
dados_theta_i <- readRDS("./Dados/dados_reais/dados_theta_i_dados_reais.rds") %>%
  filter(!(nome == "EM 2020 sem zeros" & param == "betas")) %>%
  bind_rows(calcula_theta_i(dat, samp, "betas", "EM 2020 sem zeros"))
saveRDS(dados_theta_i, "./Dados/dados_reais/dados_theta_i_dados_reais.rds")
rm("dados_theta_i","samp","dat")

# delta_zeta
samp <- rstan::extract(readRDS("./Dados/outputs_reais/output_tx_2020_EM_sem_zeros.RDS"))
dat <- readRDS("./Dados/dados_reais/tx_2020_EM_sem_zeros.rds")
dados_theta_i <- readRDS("./Dados/dados_reais/dados_theta_i_dados_reais.rds") %>%
  filter(!(nome == "EM 2020 sem zeros" & param == "gammas")) %>%
  bind_rows(calcula_theta_i(dat, samp, "gammas", "EM 2020 sem zeros"))
saveRDS(dados_theta_i, "./Dados/dados_reais/dados_theta_i_dados_reais.rds")
rm("dados_theta_i","samp","dat")

samp <- rstan::extract(readRDS("./Dados/outputs_reais/output_tx_2020_EM_sem_zeros.RDS"))
dat <- readRDS("./Dados/dados_reais/tx_2020_EM_sem_zeros.rds")
dados_theta_i <- readRDS("./Dados/dados_reais/dados_theta_i_dados_reais.rds")
dados <- dat %>% 
  mutate(y = tx_muni/100,
         theta = dados_theta_i %>% filter(param=="betas" & nome=="EM 2020 sem zeros") %>% pull(est),
         zeta = dados_theta_i %>% filter(param=="gammas" & nome=="EM 2020 sem zeros") %>% pull(est),
         gphi = 1/(1+zeta),
         residuo = (y-theta)/sqrt(gphi*theta*(1-theta)),
         predito = preditos(., samp)
  )
dados %>% glimpse

png("./Imagens/residuos_2020_sem_zeros.png", width = 550, height = 450)
grid.arrange(plot_residuo(dados, "predito", "Predito"),
             plot_residuo(dados, "taxa_urbana", "Taxa urbana (%)"),
             plot_residuo(dados, "taxa_publica", "Taxa pública (%)"),
             plot_residuo(dados, "IDHM.Renda", "IDHM Renda"),
             plot_residuo(dados, "log_pop", "log(População)"),
             plot_residuo(dados, "log_PIB_pc", "log(PIB per capita)"),
             ncol = 3,
             top = "Resíduos versus predito e variáveis do modelo para EM 2020 (amostra sem zeros)"
)
dev.off()

dados2 <- dados %>% 
  filter(residuo != max(dados$residuo))
png("./Imagens/residuos_2020_sem_zeros_sem_outlier.png", width = 550, height = 450)
grid.arrange(plot_residuo(dados2, "predito", "Predito"),
             plot_residuo(dados2, "taxa_urbana", "Taxa urbana (%)"),
             plot_residuo(dados2, "taxa_publica", "Taxa pública (%)"),
             plot_residuo(dados2, "IDHM.Renda", "IDHM Renda"),
             plot_residuo(dados2, "log_pop", "log(População)"),
             plot_residuo(dados2, "log_PIB_pc", "log(PIB per capita)"),
             ncol = 3,
             top = "Resíduos versus predito e variáveis do modelo para EM 2020 (amostra sem zeros; sem outlier)"
)
dev.off()

# I de Moran
W.listw <- mat2listw(readRDS("./Dados/dados_reais/W_2020_EM_sem_zeros.rds"))
moran.mc(dados$residuo, listw = W.listw, nsim = 1000) 
# statistic = -0,021938, observed rank = 264, p-value = 0,7363 < - - Não rejeita H0
# H0: Não existe associação espacial <- - - - 
# H1: Existe associação espacial 


# 2015 SEM ZEROS ------------------------------------------------------------

# delta_theta
samp <- rstan::extract(readRDS("./Dados/outputs_reais/output_tx_2015_EM_sem_zeros.RDS"))
dat <- readRDS("./Dados/dados_reais/tx_2015_EM_sem_zeros.rds")
dados_theta_i <- readRDS("./Dados/dados_reais/dados_theta_i_dados_reais.rds") %>%
  filter(!(nome == "EM 2015 sem zeros" & param == "betas")) %>%
  bind_rows(calcula_theta_i(dat, samp, "betas", "EM 2015 sem zeros"))
saveRDS(dados_theta_i, "./Dados/dados_reais/dados_theta_i_dados_reais.rds")
rm("dados_theta_i","samp","dat")

# delta_zeta
samp <- rstan::extract(readRDS("./Dados/outputs_reais/output_tx_2015_EM_sem_zeros.RDS"))
dat <- readRDS("./Dados/dados_reais/tx_2015_EM_sem_zeros.rds")
dados_theta_i <- readRDS("./Dados/dados_reais/dados_theta_i_dados_reais.rds") %>%
  filter(!(nome == "EM 2015 sem zeros" & param == "gammas")) %>%
  bind_rows(calcula_theta_i(dat, samp, "gammas", "EM 2015 sem zeros"))
saveRDS(dados_theta_i, "./Dados/dados_reais/dados_theta_i_dados_reais.rds")
rm("dados_theta_i","samp","dat")

samp <- rstan::extract(readRDS("./Dados/outputs_reais/output_tx_2015_EM_sem_zeros.RDS"))
dat <- readRDS("./Dados/dados_reais/tx_2015_EM_sem_zeros.rds")
dados_theta_i <- readRDS("./Dados/dados_reais/dados_theta_i_dados_reais.rds")
dados <- dat %>% 
  mutate(y = tx_muni/100,
         theta = dados_theta_i %>% filter(param=="betas" & nome=="EM 2015 sem zeros") %>% pull(est),
         zeta = dados_theta_i %>% filter(param=="gammas" & nome=="EM 2015 sem zeros") %>% pull(est),
         gphi = 1/(1+zeta),
         residuo = (y-theta)/sqrt(gphi*theta*(1-theta)),
         predito = preditos(., samp)
  )
dados %>% glimpse

png("./Imagens/residuos_2015_sem_zeros.png", width = 550, height = 450)
grid.arrange(plot_residuo(dados, "predito", "Predito"),
             plot_residuo(dados, "taxa_urbana", "Taxa urbana (%)"),
             plot_residuo(dados, "taxa_publica", "Taxa pública (%)"),
             plot_residuo(dados, "IDHM.Renda", "IDHM Renda"),
             plot_residuo(dados, "log_pop", "log(População)"),
             plot_residuo(dados, "log_PIB_pc", "log(PIB per capita)"),
             ncol = 3,
             top = "Resíduos versus predito e variáveis do modelo para EM 2015 (amostra sem zeros)"
)
dev.off()

# I de Moran
W.listw <- mat2listw(readRDS("./Dados/dados_reais/W_2015_EM_sem_zeros.rds"))
moran.mc(dados$residuo, listw = W.listw, nsim = 1000) 
# statistic = -0,001584, observed rank = 619, p-value = 0,3816 < - - Não rejeita H0
# H0: Não existe associação espacial <- - - - 
# H1: Existe associação espacial 



# 2010 SEM ZEROS ------------------------------------------------------------

# delta_theta
samp <- rstan::extract(readRDS("./Dados/outputs_reais/output_tx_2010_EM_sem_zeros.RDS"))
dat <- readRDS("./Dados/dados_reais/tx_2010_EM_sem_zeros.rds")
dados_theta_i <- readRDS("./Dados/dados_reais/dados_theta_i_dados_reais.rds") %>%
  filter(!(nome == "EM 2010 sem zeros" & param == "betas")) %>%
  bind_rows(calcula_theta_i(dat, samp, "betas", "EM 2010 sem zeros"))
saveRDS(dados_theta_i, "./Dados/dados_reais/dados_theta_i_dados_reais.rds")
rm("dados_theta_i","samp","dat")

# delta_zeta
samp <- rstan::extract(readRDS("./Dados/outputs_reais/output_tx_2010_EM_sem_zeros.RDS"))
dat <- readRDS("./Dados/dados_reais/tx_2010_EM_sem_zeros.rds")
dados_theta_i <- readRDS("./Dados/dados_reais/dados_theta_i_dados_reais.rds") %>%
  filter(!(nome == "EM 2010 sem zeros" & param == "gammas")) %>%
  bind_rows(calcula_theta_i(dat, samp, "gammas", "EM 2010 sem zeros"))
saveRDS(dados_theta_i, "./Dados/dados_reais/dados_theta_i_dados_reais.rds")
rm("dados_theta_i","samp","dat")

samp <- rstan::extract(readRDS("./Dados/outputs_reais/output_tx_2010_EM_sem_zeros.RDS"))
dat <- readRDS("./Dados/dados_reais/tx_2010_EM_sem_zeros.rds")
dados_theta_i <- readRDS("./Dados/dados_reais/dados_theta_i_dados_reais.rds")
dados <- dat %>% 
  mutate(y = tx_muni/100,
         theta = dados_theta_i %>% filter(param=="betas" & nome=="EM 2010 sem zeros") %>% pull(est),
         zeta = dados_theta_i %>% filter(param=="gammas" & nome=="EM 2010 sem zeros") %>% pull(est),
         gphi = 1/(1+zeta),
         residuo = (y-theta)/sqrt(gphi*theta*(1-theta)),
         predito = preditos(., samp)
  )
dados %>% glimpse

png("./Imagens/residuos_2010_sem_zeros.png", width = 550, height = 450)
grid.arrange(plot_residuo(dados, "predito", "Predito"),
             plot_residuo(dados, "taxa_urbana", "Taxa urbana (%)"),
             plot_residuo(dados, "taxa_publica", "Taxa pública (%)"),
             plot_residuo(dados, "IDHM.Renda", "IDHM Renda"),
             plot_residuo(dados, "log_pop", "log(População)"),
             plot_residuo(dados, "log_PIB_pc", "log(PIB per capita)"),
             ncol = 3,
             top = "Resíduos versus predito e variáveis do modelo para EM 2010 (amostra sem zeros)"
)
dev.off()

# I de Moran
W.listw <- mat2listw(readRDS("./Dados/dados_reais/W_2010_EM_sem_zeros.rds"))
moran.mc(dados$residuo, listw = W.listw, nsim = 1000) 
# statistic = -0,014902, observed rank = 402, p-value = 0,5984 < - - Não rejeita H0
# H0: Não existe associação espacial <- - - - 
# H1: Existe associação espacial 
