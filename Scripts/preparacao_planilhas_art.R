# PREPARANDO TABELAS DOS RESULTADOS DAS SIMULAÇÕES

library(tidyverse)
library(readxl)

junta_todas <- function(df, distr) {
  
  param <- df %>%
    filter(dist == distr) %>% 
    pull(param) %>% 
    unique()
  
  df <- df %>%
    filter(dist == distr) %>% 
    select(-c(dist, real)) %>% 
    pivot_wider(names_from = "param", values_from = c("VR","est"))
  
  for (i in 1:length(param)) {
    di <- df %>% 
      select(ends_with(param[i])) %>% 
      transmute(var = paste0(round(pull(.[,1])*100, 1), "% (", round(pull(.[,2]), 2), ")")) %>% 
      rename_at(vars("var"), ~(param[i]))
    df <- df %>% bind_cols(di)
  }
  
  return(df %>%
           select(-starts_with(c("VR","est"))))
}

dados <- read_excel("./Planilhas/simulacoes/resultados_dados_artificiais.xlsx", sheet = "data.frame")

# Gama -------------------------------------------
dados %>% junta_todas("gama")

# Beta -------------------------------------------
dados %>% junta_todas("beta")

# Beta Zeta --------------------------------------
dados %>% junta_todas("beta_zeta")

# Beta Zeta Delta --------------------------------
dados %>% junta_todas("beta_zeta_delta")


# TROCADOS #######################################

# Beta Zeta no modelo Beta Zeta Delta ------------
dados %>% junta_todas("beta_zeta_trocado")

# Beta Zeta Delta no modelo Beta Zeta ------------
dados %>% junta_todas("beta_zeta_delta_trocado")

