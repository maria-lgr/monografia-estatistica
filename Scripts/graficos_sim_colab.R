########################################################=
####     PREPARAÇÃO RESULTADOS SIMULAÇÕES COLAB     ####
########################################################=

# Pacotes
library(tidyverse)
library(patchwork)
library(gridExtra)


# Unindo bases beta_zeta_delta

sufix <- c("", "_10_200", "_3_200", "_4", "_4_200", "_5", "_5_200",
           "_6", "_6_200", "_7", "_7_200", "_8", "_8_200", "_9_10", "_9_200")

dados_unidos <- data.frame()

for (i in sufix) {
  dados <- readRDS(paste0("./Dados/sim_colab/sim_beta_zeta_delta/sementes",i,".rds"))
  assign(paste0("sementes", i), dados)
  dados_unidos <- dados_unidos %>% bind_rows(dados)
}

dados <- dados_unidos %>% 
  filter(n != 0) %>% 
  arrange(sim, n)
dados

saveRDS(dados, "./Dados/sim_colab/sim_beta_zeta_delta/sementes.rds")


# Unindo bases beta_zeta

sufix <- c("", "_2_3", "_4_8", "_9_10")

dados_unidos <- data.frame()

for (i in sufix) {
  dados <- readRDS(paste0("./Dados/sim_colab/sim_beta_zeta/outputs/sementes",i,".rds"))
  assign(paste0("sementes", i), dados)
  dados_unidos <- dados_unidos %>% bind_rows(dados)
}

dados <- dados_unidos %>% 
  filter(n != 0) %>% 
  arrange(sim, n)
dados

saveRDS(dados, "./Dados/sim_colab/sim_beta_zeta/outputs/sementes.rds")

# Unindo bases beta_zeta_trocado

sufix <- c("", "_10", "_3_4", "_5_6", "_7", "_8", "_9")

dados_unidos <- data.frame()

for (i in sufix) {
  dados <- readRDS(paste0("./Dados/sim_colab/sim_beta_zeta_trocado/dados_resumo",i,".rds"))
  assign(paste0("sementes", i), dados)
  dados_unidos <- dados_unidos %>% bind_rows(dados)
}

dados <- dados_unidos %>% 
  filter(n != 0) %>% 
  arrange(sim, n)
dados

saveRDS(dados, "./Dados/sim_colab/sim_beta_zeta_trocado/dados_resumo.rds")



# FUNÇÃO PARA OS BOXPLOTS ----

bp_vr <- function(df, parametro, y_lim = c(-100, 100)) {
  p <- df %>%
    filter(param == parametro) %>% 
    mutate(n = factor(n),
           vr = vr*100) %>% 
    ggplot(aes(x = n, y = vr, fill = n)) +
    geom_boxplot(color = "black") +
    geom_jitter(color = "#E00000", size = 1, alpha = 0.9) +
    ggtitle(paste0("Vícios relativos - ", parametro)
            # subtitle = paste0("Modelo ", unique(df$dist), " - Parâmetro ", parametro)
            ) +
    xlab("Tamanho da amostra") + ylab("Vício relativo (%)") +
    ylim(y_lim) +
    geom_hline(yintercept = 0, color = "orange", size = 1) +
    scale_fill_manual(values=c("#093C68", "#C8E4E2", "#5C97AD")) +
    theme_minimal() +
    theme(legend.title = element_blank(), legend.position = "none")
  return(p)
}


# BOXPLOTS GAMA -----------------------------------

dados <- readRDS("./Dados/sim_colab/sim_gama/outputs/dados_resumo.rds")
dados_gama <- dados %>% 
  filter(sim <= 10, dist != "-") %>% 
  mutate(param = rep(c("beta0","beta1","beta2","alpha","tau"),30))
dados_gama

png("./Imagens/bp_gama.png", width = 600, height = 500)
grid.arrange(bp_vr(dados_beta, "beta0", y_lim = c(-150, 300)),
             bp_vr(dados_beta, "beta1", y_lim = c(-150, 300)),
             bp_vr(dados_beta, "beta2", y_lim = c(-150, 300)),
             bp_vr(dados_beta, "zeta", y_lim = c(-110, 140)),
             bp_vr(dados_beta, "tau", y_lim = c(-110, 140)),
             ncol = 3,
             top = "Distribuição dos vícios relativos para os modelos gama"
)
dev.off()


# BOXPLOTS BETA -----------------------------------

dados <- readRDS("./Dados/sim_colab/sim_beta/outputs/dados_resumo.rds")
dados_beta <- dados %>% 
  mutate(param = case_when(param=="beta" & real == -0.75 ~ "beta0",
                           param=="beta" & real == 0.50 ~ "beta1",
                           param=="beta" & real == -1.50 ~ "beta2",
                           TRUE ~ param)) %>% 
  filter(sim <= 10, dist != "-")
dados_beta

png("./Imagens/bp_beta.png", width = 600, height = 500)
grid.arrange(bp_vr(dados_beta, "beta0", y_lim = c(-150, 300)),
             bp_vr(dados_beta, "beta1", y_lim = c(-150, 300)),
             bp_vr(dados_beta, "beta2", y_lim = c(-150, 300)),
             bp_vr(dados_beta, "zeta", y_lim = c(-100, 140)),
             bp_vr(dados_beta, "tau", y_lim = c(-100, 140)),
             ncol = 3,
             top = "Distribuição dos vícios relativos para os modelos beta"
             )
dev.off()


# BOXPLOTS BETA ZETA -----------------------------------

dados <- readRDS("./Dados/sim_colab/sim_beta_zeta/outputs/dados_resumo.rds")
dados_beta_zeta <- dados %>% 
  mutate(param = case_when(param=="beta" & real == -0.75 ~ "beta0",
                           param=="beta" & real == 0.50 ~ "beta1",
                           param=="beta" & real == -1.50 ~ "beta2",
                           param=="gamma" & real == 1.50 ~ "gamma0",
                           param=="gamma" & real == 1.00 ~ "gamma1",
                           param=="gamma" & real == 2.00 ~ "gamma2",
                           TRUE ~ param)) %>% 
  filter(sim <= 10, dist != "-")
dados_beta_zeta

png("./Imagens/bp_beta_zeta.png", width = 600, height = 750)
grid.arrange(bp_vr(dados_beta_zeta, "beta0", y_lim = c(-180, 180)),
             bp_vr(dados_beta_zeta, "beta1", y_lim = c(-180, 180)),
             bp_vr(dados_beta_zeta, "beta2", y_lim = c(-180, 180)),
             bp_vr(dados_beta_zeta, "gamma0", y_lim = c(-180, 180)),
             bp_vr(dados_beta_zeta, "gamma1", y_lim = c(-180, 180)),
             bp_vr(dados_beta_zeta, "gamma2", y_lim = c(-180, 180)),
             bp_vr(dados_beta_zeta, "tau", y_lim = c(-100, 150)),
             ncol = 3,
             top = "Distribuição dos vícios relativos para os modelos beta_zeta"
)
dev.off()



# BOXPLOTS BETA ZETA DELTA -----------------------------------

dados <- readRDS("./Dados/sim_colab/sim_beta_zeta_delta/dados_resumo.rds")

dados2 <- dados %>% 
  mutate(real = case_when(param=="tau_zeta" ~ 2,
                          TRUE ~ real),
         teste = (est - real)/abs(real))

dados_beta_zeta_delta <- dados2 %>% 
  mutate(param = case_when(param=="beta" & real == -0.75 ~ "beta0",
                           param=="beta" & real == 0.50 ~ "beta1",
                           param=="beta" & real == -1.50 ~ "beta2",
                           param=="gamma" & real == 1.50 ~ "gamma0",
                           param=="gamma" & real == 1.00 ~ "gamma1",
                           param=="gamma" & real == 2.00 ~ "gamma2",
                           TRUE ~ param)) %>% 
  filter(sim <= 10, dist != "-")
dados_beta_zeta_delta

png("./Imagens/bp_beta_zeta_delta.png", width = 600, height = 750)
grid.arrange(bp_vr(dados_beta_zeta_delta, "beta0", y_lim = c(-160, 140)),
             bp_vr(dados_beta_zeta_delta, "beta1", y_lim = c(-160, 140)),
             bp_vr(dados_beta_zeta_delta, "beta2", y_lim = c(-160, 140)),
             bp_vr(dados_beta_zeta_delta, "gamma0", y_lim = c(-160, 100)),
             bp_vr(dados_beta_zeta_delta, "gamma1", y_lim = c(-160, 140)),
             bp_vr(dados_beta_zeta_delta, "gamma2", y_lim = c(-160, 140)),
             bp_vr(dados_beta_zeta_delta, "tau_theta", y_lim = c(-110, 160)),
             bp_vr(dados_beta_zeta_delta, "tau_zeta", y_lim = c(-110, 160)),
             ncol = 3,
             top = "Distribuição dos vícios relativos para os modelos beta_zeta_delta"
)
dev.off()


# BOXPLOTS BETA ZETA TROCADO -----------------------------------

dados <- readRDS("./Dados/sim_colab/sim_beta_zeta_trocado/dados_resumo.rds")
dados_beta_zeta_trocado <- dados %>% 
  mutate(param = case_when(param=="beta" & real == -0.75 ~ "beta0",
                           param=="beta" & real == 0.50 ~ "beta1",
                           param=="beta" & real == -1.50 ~ "beta2",
                           param=="gamma" & real == 1.50 ~ "gamma0",
                           param=="gamma" & real == 1.00 ~ "gamma1",
                           param=="gamma" & real == 2.00 ~ "gamma2",
                           TRUE ~ param)) %>% 
  filter(sim <= 10, dist != "-", param != "tau_zeta")
dados_beta_zeta_trocado

png("./Imagens/bp_beta_zeta_trocado.png", width = 600, height = 750)
grid.arrange(bp_vr(dados_beta_zeta_trocado, "beta0", y_lim = c(-200, 180)),
             bp_vr(dados_beta_zeta_trocado, "beta1", y_lim = c(-200, 180)),
             bp_vr(dados_beta_zeta_trocado, "beta2", y_lim = c(-200, 180)),
             bp_vr(dados_beta_zeta_trocado, "gamma0", y_lim = c(-200, 180)),
             bp_vr(dados_beta_zeta_trocado, "gamma1", y_lim = c(-200, 180)),
             bp_vr(dados_beta_zeta_trocado, "gamma2", y_lim = c(-200, 180)),
             bp_vr(dados_beta_zeta_trocado, "tau_theta (tau)", y_lim = c(-110, 160)),
             ncol = 3,
             top = "Distribuição dos vícios relativos para os modelos beta_zeta_trocado"
)
dev.off()



# BOXPLOTS BETA ZETA DELTA TROCADO -----------------------------------

dados <- readRDS("./Dados/sim_colab/sim_beta_zeta_delta_trocado/dados_resumo.rds")
dados_beta_zeta_delta_trocado <- dados %>%
  mutate(param = case_when(param=="beta" & real == -0.75 ~ "beta0",
                           param=="beta" & real == 0.50 ~ "beta1",
                           param=="beta" & real == -1.50 ~ "beta2",
                           param=="gamma" & real == 1.50 ~ "gamma0",
                           param=="gamma" & real == 1.00 ~ "gamma1",
                           param=="gamma" & real == 2.00 ~ "gamma2",
                           TRUE ~ param)) %>%
  filter(sim <= 10, dist != "-")
dados_beta_zeta_delta_trocado

png("./Imagens/bp_beta_zeta_delta_trocado.png", width = 600, height = 750)
grid.arrange(bp_vr(dados_beta_zeta_delta_trocado, "beta0", y_lim = c(-180, 150)),
             bp_vr(dados_beta_zeta_delta_trocado, "beta1", y_lim = c(-180, 150)),
             bp_vr(dados_beta_zeta_delta_trocado, "beta2", y_lim = c(-180, 150)),
             bp_vr(dados_beta_zeta_delta_trocado, "gamma0", y_lim = c(-180, 150)),
             bp_vr(dados_beta_zeta_delta_trocado, "gamma1", y_lim = c(-180, 150)),
             bp_vr(dados_beta_zeta_delta_trocado, "gamma2", y_lim = c(-180, 150)),
             bp_vr(dados_beta_zeta_delta_trocado, "tau (tau_theta)", y_lim = c(-80, 200)),
             ncol = 3,
             top = "Distribuição dos vícios relativos para os modelos beta_zeta_delta_trocado"
)
dev.off()
