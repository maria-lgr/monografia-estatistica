########################################################=
####   ESTIMATIVAS DOS EFEITOS ESPACIAIS (DELTAS)   ####
########################################################=

library(tidyverse)
library(rstan)
library(coda)
library(gridExtra)

options(OutDec = ",")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

bp_vr <- function(df, amostra, y_lim = c(-3, 3)) {
  p <- df %>%
    mutate(param = factor(param)) %>% 
    ggplot(aes(x = param, y = est, fill = param)) +
    geom_boxplot(color = "black") +
    # geom_jitter(color = "#E00000", size = 1, alpha = 0.9) +
    ggtitle(paste0("Amostra de ", amostra)
            # subtitle = paste0("Modelo ", unique(df$dist), " - Parâmetro ", parametro)
    ) +
    xlab("Estrutura") + ylab("Efeito espacial estimado") +
    ylim(y_lim) +
    # geom_hline(yintercept = 0, color = "orange", size = 1) +
    scale_fill_manual(values=c("#C8E4E2", "#5C97AD")) +
    theme_minimal() +
    theme(legend.title = element_blank(), legend.position = "none")
  return(p)
}


funcao_facil <- function(ano, zero) {
  if (zero == TRUE) {
    output <- readRDS(paste0("Dados/outputs_reais/output_tx_20",ano,"_EM.RDS"))
  } else {
    output <- readRDS(paste0("Dados/outputs_reais/output_tx_20",ano,"_EM_sem_zeros.RDS"))
  }
  samp <- rstan::extract(output)
  dados <- data.frame(delta_theta = cbind(samp$delta_theta) %>% apply(2, mean),
                        delta_zeta = cbind(samp$delta_zeta) %>% apply(2, mean)
  ) %>% 
    pivot_longer(cols = everything(), names_to = "param", values_to = "est") %>% 
    mutate(param = ifelse(param=="delta_theta","Média","Dispersão"))
  return(dados)
}



# Com zeros --------------------------------------------------

png("./Imagens/bp_deltas.png", width = 600, height = 200)
grid.arrange(bp_vr(funcao_facil(20, zero = FALSE), "2020", c(-0.5, 0.9)),
             bp_vr(funcao_facil(15, zero = FALSE), "2015", c(-0.5, 0.9)),
             bp_vr(funcao_facil(10, zero = FALSE), "2010", c(-0.5, 0.9)),
             ncol = 3,
             top = "Distribuição dos efeitos aleatórios espaciais estimados - amostras originais"
)
dev.off()


# Sem zeros --------------------------------------------------

png("./Imagens/bp_deltas_sem_zeros.png", width = 600, height = 200)
grid.arrange(bp_vr(funcao_facil(20, zero = TRUE), "2020", c(-2.5, 1.25)),
             bp_vr(funcao_facil(15, zero = TRUE), "2015", c(-2.5, 1.25)),
             bp_vr(funcao_facil(10, zero = TRUE), "2010", c(-2.5, 1.25)),
             ncol = 3,
             top = "Distribuição dos efeitos aleatórios espaciais estimados - amostras sem zeros"
)
dev.off()
