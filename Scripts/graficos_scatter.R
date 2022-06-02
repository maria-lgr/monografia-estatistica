# GRÁFICOS THETA i #####################################

library(tidyverse)
library(gridExtra)

# GAMA ----

dados_theta_i <- readRDS("./Dados/dados_artificiais/dados_theta_i.rds")


for (n in c(50,100,200)) {
  
  plot <- dados_theta_i %>%
    filter(dist == "gama"  & N == n) %>% 
    mutate(var1 = factor(var1)) %>% 
    ggplot(aes(x = real, y = est, color = var1, shape = var1)) + 
    geom_point() +
    geom_smooth(method = lm, se = F) +
    xlab("Valor real") + ylab("Valor estimado") +
    # xlim(-1.5, 1.5) + ylim(-1.5, 1.5) +
    ggtitle(paste0("N=",n)) +
    theme_minimal() +
    scale_color_manual(values=c("#093C68", "#5C97AD")) +
    scale_size_manual(values=c(3,4))
  
  if(n!=200) {
    plot <- plot + theme(legend.position = "none") 
  }
  
  assign(paste0("plot_",n), plot)
  
}


png("./Imagens/scatter_theta_i_gama.png", width = 650, height = 250)
grid.arrange(plot_50 + ylim(0, 100) + xlim(0, 100),
             plot_100 + ylim(0, 100) + xlim(0, 100),
             plot_200 + ylim(0, 100) + xlim(0, 100),
             ncol = 3,
             widths=c(1, 1, 1.3),
             top = "Valores reais de theta_i versus valores estimados - gama"
)
dev.off()


# BETA ----

for (n in c(50,100,200)) {
  
  plot <- dados_theta_i %>%
    filter(dist == "beta"  & N == n) %>% 
    mutate(var1 = factor(var1)) %>% 
    ggplot(aes(x = real, y = est, color = var1, shape = var1)) + 
    geom_point() +
    geom_smooth(method = lm, se = F) +
    xlab("Valor real") + ylab("Valor estimado") +
    xlim(0,1) + ylim(0,1) +
    ggtitle(paste0("N=",n)) +
    theme_minimal() +
    scale_color_manual(values=c("#093C68", "#5C97AD")) +
    scale_size_manual(values=c(3,4))
  
  if(n!=200) {
    plot <- plot + theme(legend.position = "none") 
  }
  
  
  assign(paste0("plot_",n), plot)
  
}

png("./Imagens/scatter_theta_i_beta.png", width = 650, height = 250)
grid.arrange(plot_50,
             plot_100,
             plot_200,
             ncol = 3,
             widths=c(1, 1, 1.3),
             top = "Valores reais de theta_i versus valores estimados - beta"
)
dev.off()


# BETA ZETA ----

# theta
for (n in c(50,100,200)) {
  
  plot <- dados_theta_i %>%
    mutate(var1 = factor(var1)) %>% 
    filter(dist == "beta_zeta" & N == n & param == "betas") %>% 
    ggplot(aes(x = real, y = est, color = var1, shape = var1)) + 
    geom_point() +
    geom_smooth(method = lm, se = F) +
    xlab("Valor real") + ylab("Valor estimado") +
    xlim(0,1) + ylim(0,1) +
    ggtitle(paste0("N=",n)) +
    theme_minimal() +
    scale_color_manual(values=c("#093C68", "#5C97AD")) +
    scale_size_manual(values=c(3,4))
  
  if(n!=200) {
    plot <- plot + theme(legend.position = "none") 
  }
  
  assign(paste0("plot_betas_",n), plot)
  
}

png("./Imagens/scatter_theta_i_beta_zeta.png", width = 650, height = 250)
grid.arrange(plot_betas_50,
             plot_betas_100,
             plot_betas_200,
             ncol = 3,
             widths=c(1, 1, 1.3),
             top = "Valores reais de theta_i versus valores estimados - beta_zeta"
)
dev.off()


# zeta
for (n in c(50,100,200)) {
  
  plot <- dados_theta_i %>%
    mutate(var1 = factor(var1)) %>% 
    filter(dist == "beta_zeta" & N == n & param == "gammas") %>% 
    ggplot(aes(x = real, y = est, color = var1, shape = var1)) + 
    geom_point() +
    geom_smooth(method = lm, se = F) +
    xlab("Valor real") + ylab("Valor estimado") +
    # xlim(-1.5, 1.5) + ylim(-1.5, 1.5) +
    ggtitle(paste0("N=",n)) +
    theme_minimal()  +
    scale_color_manual(values=c("#093C68", "#5C97AD")) +
    scale_size_manual(values=c(3,4))
  
  if(n!=200) {
    plot <- plot + theme(legend.position = "none") 
  }
  
  assign(paste0("plot_gammas_",n), plot)
  
}

png("./Imagens/scatter_zeta_i_beta_zeta.png", width = 650, height = 250)
grid.arrange(plot_gammas_50 + xlim(0,90) + ylim(-20,290),
             plot_gammas_100 + xlim(0,90) + ylim(-20,290),
             plot_gammas_200 + xlim(0,90) + ylim(-20,290),
             ncol = 3,
             widths=c(1, 1, 1.3),
             top = "Valores reais de zeta_i versus valores estimados - beta_zeta"
)
dev.off()


# BETA ZETA DELTA ----

# theta
for (n in c(50,100,200)) {
  
  plot <- dados_theta_i %>%
    mutate(var1 = factor(var1)) %>% 
    filter(dist == "beta_zeta_delta" & N == n & param == "betas") %>% 
    ggplot(aes(x = real, y = est, color = var1, shape = var1)) + 
    geom_point() +
    geom_smooth(method = lm, se = F) +
    xlab("Valor real") + ylab("Valor estimado") +
    xlim(0,1) + ylim(0,1) +
    ggtitle(paste0("N=",n)) +
    theme_minimal() +
    scale_color_manual(values=c("#093C68", "#5C97AD")) +
    scale_size_manual(values=c(3,4))
  
  if(n!=200) {
    plot <- plot + theme(legend.position = "none") 
  }
  
  assign(paste0("plot_betas_",n), plot)
  
}

png("./Imagens/scatter_theta_i_beta_zeta_delta.png", width = 650, height = 250)
grid.arrange(plot_betas_50,
             plot_betas_100,
             plot_betas_200,
             ncol = 3,
             widths=c(1, 1, 1.3),
             top = "Valores reais de theta_i versus valores estimados - beta_zeta_delta"
)
dev.off()

# zeta
for (n in c(50,100,200)) {
  
  plot <- dados_theta_i %>%
    mutate(var1 = factor(var1)) %>% 
    filter(dist == "beta_zeta_delta" & N == n & param == "gammas") %>% 
    ggplot(aes(x = real, y = est, color = var1, shape = var1)) + 
    geom_point() +
    geom_smooth(method = lm, se = F) +
    xlab("Valor real") + ylab("Valor estimado") +
    # xlim(-1.5, 1.5) + ylim(-1.5, 1.5) +
    ggtitle(paste0("N=",n)) +
    theme_minimal() +
    scale_color_manual(values=c("#093C68", "#5C97AD")) +
    scale_size_manual(values=c(3,4))
  
  if(n!=200) {
    plot <- plot + theme(legend.position = "none") 
  }
  
  assign(paste0("plot_gammas_",n), plot)
  
}

png("./Imagens/scatter_zeta_i_beta_zeta_delta.png", width = 650, height = 250)
grid.arrange(plot_gammas_50 + ylim(0, 1000) + xlim(0, 600),
             plot_gammas_100 + ylim(0, 1000) + xlim(0, 600),
             plot_gammas_200 + ylim(0, 1000) + xlim(0, 600),
             ncol = 3,
             widths=c(1, 1, 1.3),
             top = "Valores reais de zeta_i versus valores estimados - beta_zeta_delta"
)
dev.off()


# BETA ZETA TROCADO ----

# theta
for (n in c(50,100,200)) {
  
  plot <- dados_theta_i %>%
    mutate(var1 = factor(var1)) %>% 
    filter(dist == "beta_zeta_trocado" & N == n & param == "betas") %>% 
    ggplot(aes(x = real, y = est, color = var1, shape = var1)) + 
    geom_point() +
    geom_smooth(method = lm, se = F) +
    xlab("Valor real") + ylab("Valor estimado") +
    xlim(0,1) + ylim(0,1) +
    ggtitle(paste0("N=",n)) +
    theme_minimal() +
    scale_color_manual(values=c("#093C68", "#5C97AD")) +
    scale_size_manual(values=c(3,4))
  
  if(n!=200) {
    plot <- plot + theme(legend.position = "none") 
  }
  
  assign(paste0("plot_betas_",n), plot)
  
}

png("./Imagens/scatter_theta_i_beta_zeta_trocado.png", width = 650, height = 250)
grid.arrange(plot_betas_50,
             plot_betas_100,
             plot_betas_200,
             ncol = 3,
             widths=c(1, 1, 1.3),
             top = "Valores reais de theta_i versus valores estimados - beta_zeta_trocado"
)
dev.off()


# zeta
for (n in c(50,100,200)) {
  
  plot <- dados_theta_i %>%
    mutate(var1 = factor(var1)) %>% 
    filter(dist == "beta_zeta_trocado" & N == n & param == "gammas") %>% 
    ggplot(aes(x = real, y = est, color = var1, shape = var1)) + 
    geom_point() +
    geom_smooth(method = lm, se = F) +
    xlab("Valor real") + ylab("Valor estimado") +
    # xlim(-1.5, 1.5) + ylim(-1.5, 1.5) +
    ggtitle(paste0("N=",n)) +
    theme_minimal() +
    scale_color_manual(values=c("#093C68", "#5C97AD")) +
    scale_size_manual(values=c(3,4))
  
  if(n!=200) {
    plot <- plot + theme(legend.position = "none") 
  }
  
  assign(paste0("plot_gammas_",n), plot)
  
}

png("./Imagens/scatter_zeta_i_beta_zeta_trocado.png", width = 650, height = 250)
grid.arrange(plot_gammas_50 + ylim(-140, 2600) + xlim(0, 100),
             plot_gammas_100 + ylim(-40, 500) + xlim(0, 100),
             plot_gammas_200 + ylim(-40, 500) + xlim(0, 100),
             ncol = 3,
             widths=c(1, 1, 1.3),
             top = "Valores reais de zeta_i versus valores estimados - beta_zeta_trocado"
)
dev.off()


# BETA ZETA DELTA TROCADO ----

# theta
for (n in c(50,100,200)) {
  
  plot <- dados_theta_i %>%
    mutate(var1 = factor(var1)) %>% 
    filter(dist == "beta_zeta_delta_trocado" & N == n & param == "betas") %>% 
    ggplot(aes(x = real, y = est, color = var1, shape = var1)) + 
    geom_point() +
    geom_smooth(method = lm, se = F) +
    xlab("Valor real") + ylab("Valor estimado") +
    xlim(0,1) + ylim(0,1) +
    ggtitle(paste0("N=",n)) +
    theme_minimal() +
    scale_color_manual(values=c("#093C68", "#5C97AD")) +
    scale_size_manual(values=c(3,4))
  
  if(n!=200) {
    plot <- plot + theme(legend.position = "none") 
  }
  
  assign(paste0("plot_betas_",n), plot)
  
}

png("./Imagens/scatter_theta_i_beta_zeta_delta_trocado.png", width = 650, height = 250)
grid.arrange(plot_betas_50,
             plot_betas_100,
             plot_betas_200,
             ncol = 3,
             widths=c(1, 1, 1.3),
             top = "Valores reais de theta_i versus valores estimados - beta_zeta_delta_trocado"
)
dev.off()


# zeta
for (n in c(50,100,200)) {
  
  plot <- dados_theta_i %>%
    mutate(var1 = factor(var1)) %>% 
    filter(dist == "beta_zeta_delta_trocado" & N == n & param == "gammas") %>% 
    ggplot(aes(x = real, y = est, color = var1, shape = var1)) + 
    geom_point() +
    geom_smooth(method = lm, se = F) +
    xlab("Valor real") + ylab("Valor estimado") +
    # xlim(-1.5, 1.5) + ylim(-1.5, 1.5) +
    ggtitle(paste0("N=",n)) +
    theme_minimal() +
    scale_color_manual(values=c("#093C68", "#5C97AD")) +
    scale_size_manual(values=c(3,4))

  if(n!=200) {
    plot <- plot + theme(legend.position = "none") 
  }
    
  assign(paste0("plot_gammas_",n), plot)
  
}

png("./Imagens/scatter_zeta_i_beta_zeta_delta_trocado.png", width = 650, height = 250)
grid.arrange(plot_gammas_50 + ylim(0, 225) + xlim(0, 600),
             plot_gammas_100 + ylim(0, 225) + xlim(0, 600),
             plot_gammas_200 + ylim(0, 225) + xlim(0, 600),
             ncol = 3,
             widths=c(1, 1, 1.3),
             top = "Valores reais de zeta_i versus valores estimados - beta_zeta_delta_trocado"
)
dev.off()
