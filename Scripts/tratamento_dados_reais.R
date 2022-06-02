# TRATAMENTO DOS BANCOS DE DADOS

# Pacotes
library(tidyverse)
library(openxlsx)
library(readxl)

# Funções

tratamento_inicial <- function(data) {
  new <- data %>% 
    mutate_at(vars(starts_with('tx_')), as.numeric) %>% 
    left_join(municipios, by = "Cod_muni") %>%
    select(-Nome_muni) %>% 
    rename("Nome_muni" = Nome_certo) %>% 
    relocate(Nome_muni, .after = Cod_muni) %>% 
    group_by(Cod_muni) %>% 
    mutate("Total_escolas" = n()) %>% 
    ungroup() %>% 
    group_by(Cod_muni, Local) %>% 
    mutate("Total_urbana" = n()) %>% 
    ungroup() %>% 
    mutate("Publica_privada" = ifelse(Dependencia == "Privada",
                                      "Privada", "Pública")) %>% 
    group_by(Cod_muni, Publica_privada) %>% 
    mutate("Total_publica" = n()) %>% 
    ungroup()
  return(new)
}

filtra_EFI <- function(dados) {
  new <- dados %>% 
    mutate(id_escola = seq(1:dim(.)[1])) %>% 
    group_by(id_escola) %>% 
    mutate(escola_valida = ifelse(sum(c(is.na(tx_ano_1o), is.na(tx_ano_2o), is.na(tx_ano_3o),
                                        is.na(tx_ano_4o), is.na(tx_ano_5o))) <= 2,
                                  1, 0)
    ) %>% 
    ungroup() %>% 
    filter(Total_escolas >=3) %>% 
    group_by(Cod_muni) %>% 
    mutate(n_escolas_validas = sum(escola_valida)) %>% 
    ungroup() %>% 
    filter(n_escolas_validas >= 3, escola_valida == 1) %>% 
    select(-escola_valida) %>% 
    group_by(id_escola) %>% 
    mutate(tx_escola = mean(c(tx_ano_1o, tx_ano_2o, tx_ano_3o, tx_ano_4o, tx_ano_5o), 
                            na.rm = T)) %>% 
    ungroup()
  return(new)
}

filtra_EFII <- function(dados) {
  new <- dados %>% 
    mutate(id_escola = seq(1:dim(.)[1])) %>% 
    group_by(id_escola) %>% 
    mutate(escola_valida = ifelse(sum(c(is.na(tx_ano_6o), is.na(tx_ano_7o), is.na(tx_ano_8o),
                                        is.na(tx_ano_9o))) <= 2,
                                  1, 0)
    ) %>% 
    ungroup() %>% 
    filter(Total_escolas >=3) %>% 
    group_by(Cod_muni) %>% 
    mutate(n_escolas_validas = sum(escola_valida)) %>% 
    ungroup() %>% 
    filter(n_escolas_validas >= 3, escola_valida == 1) %>% 
    select(-escola_valida) %>% 
    group_by(id_escola) %>% 
    mutate(tx_escola = mean(c(tx_ano_6o, tx_ano_7o, tx_ano_8o, tx_ano_9o), 
                            na.rm = T)) %>% 
    ungroup()
  return(new)
}

filtra_EM <- function(dados) {
  new <- dados %>% 
    mutate(id_escola = seq(1:dim(.)[1])) %>% 
    group_by(id_escola) %>% 
    mutate(escola_valida = ifelse(sum(c(is.na(tx_serie_1a), is.na(tx_serie_2a), 
                                        is.na(tx_serie_3a))) <= 1, 1, 0)
    ) %>% 
    ungroup() %>% 
    filter(Total_escolas >=3) %>% 
    group_by(Cod_muni) %>% 
    mutate(n_escolas_validas = sum(escola_valida)) %>% 
    ungroup() %>% 
    filter(n_escolas_validas >= 3, escola_valida == 1) %>% 
    select(-escola_valida) %>% 
    group_by(id_escola) %>% 
    mutate(tx_escola = mean(c(tx_serie_1a, tx_serie_2a, tx_serie_3a), 
                            na.rm = T)) %>% 
    ungroup()
  return(new)
}

tratamento_final <- function(dados) {
  new <- dados %>% 
    group_by(Ano, Cod_muni, Nome_muni, Total_escolas, 
             Total_urbana, Total_publica, n_escolas_validas) %>% 
    summarise(tx_muni = mean(tx_escola, na.rm = T)) %>%   # < - - - - -
    ungroup() %>% 
    distinct(Cod_muni, .keep_all = TRUE) %>% 
    as_tibble() %>% 
    mutate(tx_muni = ifelse(tx_muni == 0, 0.00001, tx_muni))
  return(new)
}

adiciona_covar <- function(data, data1, data2, data3) {
  new <- data %>% 
    left_join(data1, by = "Nome_muni") %>% 
    left_join(data2, by = "Cod_muni") %>% 
    left_join(data3, by = "Cod_muni") %>% 
    transmute(Ano,
              Cod_muni,
              Nome_muni,
              taxa_urbana = Total_urbana/Total_escolas,
              taxa_publica = Total_publica/Total_escolas,
              IDHM.Renda,
              "log_pop" = log(as.numeric(Pop)),
              "log_PIB_pc" = log(PIB_pc),
              tx_muni,
              n_escolas_validas
    )
  return(new)
}


# TAXAS 2020 -----------------------------------------------------------
raw_2020 <- read.xlsx("./Dados/dados_ic/taxas_escola/tx_rend_escolas_2020.xlsx",
                      sheet = "ABANDONO")

municipios <- readRDS("./Dados/dados_ic/municipios.rds")

tx_2020 <- raw_2020 %>% 
  filter(UF == "MG") %>% 
  select(Ano,
         "Cod_muni" = Código.do.Município,
         "Nome_muni" = Nome.do.Município,
         "Local" = Localização,
         "Dependencia" = Dependência.Administrativa,
         "tx_ano_1o" = `1º.Ano`,
         "tx_ano_2o" = `2º.Ano`,
         "tx_ano_3o" = `3º.Ano`,
         "tx_ano_4o" = `4º.Ano`,
         "tx_ano_5o" = `5º.Ano`,
         "tx_ano_6o" = `6º.Ano`,
         "tx_ano_7o" = `7º.Ano`,
         "tx_ano_8o" = `8º.Ano`,
         "tx_ano_9o" = `9º.Ano`,
         "tx_serie_1a" = `1ª.série`,
         "tx_serie_2a" = `2ª.série`,
         "tx_serie_3a" = `3ª.série`
  ) %>% 
  left_join(municipios, by = "Cod_muni") %>%
  select(-Nome_muni) %>% 
  rename("Nome_muni" = Nome_certo) %>% 
  relocate(Nome_muni, .after = Cod_muni)
glimpse(tx_2020)

tx_2010 %>% 
  group_by(Cod_muni) %>% 
  summarise(n = n()) %>% 
  pull(n) %>% sum

# IDHM -----------------------------------------------------=
idhm_2010 <- readRDS("./Dados/dados_ic/variaveis_exp/idhm_2010.rds")
glimpse(idhm_2010)

muni_tx <- tx_2020 %>% pull(Nome_muni) %>% unique %>% sort
muni_idhm <- idhm_2010 %>% pull(Nome_muni) %>% unique %>% sort

munis <- tibble(muni_idhm,muni_tx)
munis %>% filter(muni_tx != muni_idhm)

idhm_2010 <- idhm_2010 %>% 
  mutate(Nome_muni = case_when(Nome_muni == "Brasópolis" ~ "Brazópolis", 
                               Nome_muni == "Dona Eusébia" ~ "Dona Euzébia",    
                               Nome_muni == "Olhos-D'Água" ~ "Olhos-d'Água",    
                               Nome_muni == "Passa-Vinte" ~ "Passa Vinte",    
                               Nome_muni == "Pingo-D'Água" ~ "Pingo d'Água",  
                               Nome_muni == "São João Del Rei" ~ "São João del Rei",
                               TRUE ~ Nome_muni)
  )

# População ----------------------------------------------=
pop_2020 <- readRDS("./Dados/dados_ic/variaveis_exp/pop_2020.rds") %>%
  mutate(Cod_muni = as.numeric(Cod_muni)) %>% 
  select(-Nome_muni)
glimpse(pop_2020)

muni_tx <- tx_2020 %>% pull(Cod_muni) %>% unique %>% sort
muni_pop <- pop_2020 %>% pull(Cod_muni) %>% unique %>% sort

munis <- tibble(muni_pop,muni_tx)
munis %>% filter(muni_tx != muni_pop)


# PIB ----------------------------------------------------=
pib_2020 <- pib_novo <- readRDS("./Dados/dados_ic/variaveis_exp/pib_2020_novo.rds")
glimpse(pib_2020)

muni_tx <- tx_2020 %>% pull(Cod_muni) %>% unique %>% sort
muni_pib <- pib_2020 %>% pull(Cod_muni) %>% unique %>% sort

munis <- tibble(muni_pib,muni_tx)
munis %>% filter(muni_tx != muni_pib)


# Bancos finais para regressão ---------------------------

tx_2020_EFI <- tx_2020 %>% 
  tratamento_inicial() %>% 
  select(-c(tx_ano_6o, tx_ano_7o, tx_ano_8o, tx_ano_9o, tx_serie_1a, tx_serie_2a, tx_serie_3a)) %>% 
  filtra_EFI() %>% 
  tratamento_final() %>% 
  adiciona_covar(idhm_2010, pop_2020, pib_2020 %>% select(-Nome_muni))
glimpse(tx_2020_EFI)

tx_2020_EFII <- tx_2020 %>% 
  tratamento_inicial() %>% 
  select(-c(tx_ano_1o, tx_ano_2o, tx_ano_3o, tx_ano_4o, tx_ano_5o, tx_serie_1a, tx_serie_2a, tx_serie_3a)) %>% 
  filtra_EFII() %>% 
  tratamento_final() %>% 
  adiciona_covar(idhm_2010, pop_2020, pib_2020 %>% select(-Nome_muni))
glimpse(tx_2020_EFII)

tx_2020_EM <- tx_2020 %>% 
  tratamento_inicial() %>% 
  select(-c(tx_ano_1o, tx_ano_2o, tx_ano_3o, tx_ano_4o, tx_ano_5o, tx_ano_6o, tx_ano_7o, tx_ano_8o, tx_ano_9o)) %>% 
  filtra_EM() %>% 
  tratamento_final() %>% 
  adiciona_covar(idhm_2010, pop_2020, pib_2020 %>% select(-Nome_muni))
glimpse(tx_2020_EM)


saveRDS(tx_2020_EFI, "./Dados/dados_reais/tx_2020_EFI.rds")
saveRDS(tx_2020_EFII, "./Dados/dados_reais/tx_2020_EFII.rds")
saveRDS(tx_2020_EM, "./Dados/dados_reais/tx_2020_EM.rds")




# TAXAS 2015 -----------------------------------------------------------
raw_2015 <- read.xlsx("./Dados/dados_ic/taxas_escola/TX_REND_ESCOLAS_2015.xlsx",
                      sheet = "ABANDONO",
                      startRow = 2)
municipios <- readRDS("./Dados/dados_ic/municipios.rds")

tx_2015 <- raw_2015 %>% 
  filter(UF == "MG") %>% 
  select(Ano,
         "Cod_muni" = Código.do.Município,
         "Nome_muni" = Nome.do.Município,
         "Local" = Localização,
         "Dependencia" = Dependência.Administrativa,
         "tx_ano_1o" = Abandono.no.1º.Ano,
         "tx_ano_2o" = Abandono.no.2º.Ano,
         "tx_ano_3o" = Abandono.no.3º.Ano,
         "tx_ano_4o" = Abandono.no.4º.Ano,
         "tx_ano_5o" = Abandono.no.5º.Ano,
         "tx_ano_6o" = Abandono.no.6º.Ano,
         "tx_ano_7o" = Abandono.no.7º.Ano,
         "tx_ano_8o" = Abandono.no.8º.Ano,
         "tx_ano_9o" = Abandono.no.9º.Ano,
         "tx_serie_1a" = Abandono.na.1ª.série,
         "tx_serie_2a" = Abandono.na.2ª.série,
         "tx_serie_3a" = Abandono.na.3ª.série
  ) %>% 
  left_join(municipios, by = "Cod_muni") %>%
  select(-Nome_muni) %>% 
  rename("Nome_muni" = Nome_certo) %>% 
  relocate(Nome_muni, .after = Cod_muni) %>% 
  mutate(Dependencia = case_when(Dependencia == "Particular" ~ "Privada",
                                 TRUE ~ Dependencia))

# IDHM -------------------------------------------------=
idhm_2010 <- readRDS("./Dados/dados_ic/variaveis_exp/idhm_2010.rds")
glimpse(idhm_2010)

muni_tx <- tx_2015 %>% pull(Nome_muni) %>% unique %>% sort
muni_idhm <- idhm_2010 %>% pull(Nome_muni) %>% unique %>% sort

munis <- tibble(muni_idhm,muni_tx)
munis %>% filter(muni_tx != muni_idhm)

idhm_2010 <- idhm_2010 %>% 
  mutate(Nome_muni = case_when(Nome_muni == "Brasópolis" ~ "Brazópolis", 
                               Nome_muni == "Dona Eusébia" ~ "Dona Euzébia",    
                               Nome_muni == "Olhos-D'Água" ~ "Olhos-d'Água",    
                               Nome_muni == "Passa-Vinte" ~ "Passa Vinte",    
                               Nome_muni == "Pingo-D'Água" ~ "Pingo d'Água",  
                               Nome_muni == "São João Del Rei" ~ "São João del Rei",
                               TRUE ~ Nome_muni)
  )

# População ---------------------------------------------=
pop_2015 <- readRDS("./Dados/dados_ic/variaveis_exp/pop_2015.rds") %>% 
  mutate(Cod_muni = as.numeric(Cod_muni)) %>% 
  select(-Nome_muni)
glimpse(pop_2015)

muni_tx <- tx_2015 %>% pull(Cod_muni) %>% unique %>% sort
muni_pop <- pop_2015 %>% pull(Cod_muni) %>% unique %>% sort

munis <- tibble(muni_pop,muni_tx)
munis %>% filter(muni_tx != muni_pop)


# PIB --------------------------------------------------=
pib_2015 <- readRDS("./Dados/dados_ic/variaveis_exp/pib_2015_novo.rds") %>% 
  mutate(Cod_muni = as.numeric(Cod_muni)) %>% 
  select(-Nome_muni)
glimpse(pib_2015)

muni_tx <- tx_2015 %>% pull(Cod_muni) %>% unique %>% sort
muni_pib <- pib_2015 %>% pull(Cod_muni) %>% unique %>% sort

munis <- tibble(muni_pib,muni_tx)
munis %>% filter(muni_tx != muni_pib)


# Bancos finais para regressão ---------------------------

tx_2015_EFI <- tx_2015 %>% 
  tratamento_inicial() %>% 
  select(-c(tx_ano_6o, tx_ano_7o, tx_ano_8o, tx_ano_9o, tx_serie_1a, tx_serie_2a, tx_serie_3a)) %>% 
  filtra_EFI() %>% 
  tratamento_final() %>% 
  adiciona_covar(idhm_2010, pop_2015, pib_2015)
glimpse(tx_2015_EFI)

tx_2015_EFII <- tx_2015 %>% 
  tratamento_inicial() %>% 
  select(-c(tx_ano_1o, tx_ano_2o, tx_ano_3o, tx_ano_4o, tx_ano_5o, tx_serie_1a, tx_serie_2a, tx_serie_3a)) %>% 
  filtra_EFII() %>% 
  tratamento_final() %>% 
  adiciona_covar(idhm_2010, pop_2015, pib_2015)
glimpse(tx_2015_EFII)

tx_2015_EM <- tx_2015 %>% 
  tratamento_inicial() %>% 
  select(-c(tx_ano_1o, tx_ano_2o, tx_ano_3o, tx_ano_4o, tx_ano_5o, tx_ano_6o, tx_ano_7o, tx_ano_8o, tx_ano_9o)) %>% 
  filtra_EM() %>% 
  tratamento_final() %>% 
  adiciona_covar(idhm_2010, pop_2015, pib_2015)
glimpse(tx_2015_EM)


saveRDS(tx_2015_EFI, "./Dados/dados_reais/tx_2015_EFI.rds")
saveRDS(tx_2015_EFII, "./Dados/dados_reais/tx_2015_EFII.rds")
saveRDS(tx_2015_EM, "./Dados/dados_reais/tx_2015_EM.rds")




# TAXAS 2010 -----------------------------------------------------------
raw_2010 <- read_excel("./Dados/dados_ic/taxas_escola/tx_rendimento_escolas_2010_19082011.xls",
                       sheet = "ABANDONO")
municipios <- readRDS("./Dados/dados_ic/municipios.rds")

tx_2010 <- raw_2010 %>% 
  filter(UF == "MG") %>% 
  select(Ano,
         "Cod_muni" = `Código do Município`,
         "Nome_muni" = `Nome do Município`,
         "Local" = Localização,
         "Dependencia" = Rede,
         "tx_ano_1o" = `Abandono no 1º Ano do Ensino Fundamental`,
         "tx_ano_2o" = `Abandono na 1ª série/2º Ano`,
         "tx_ano_3o" = `Abandono na 2ª série/3º Ano`,
         "tx_ano_4o" = `Abandono na 3ª série/4º Ano`,
         "tx_ano_5o" = `Abandono na 4ª série/5º Ano`,
         "tx_ano_6o" = `Abandono na 5ª série/6º Ano`,
         "tx_ano_7o" = `Abandono na 6ª série/7º Ano`,
         "tx_ano_8o" = `Abandono na 7ª série/8º Ano`,
         "tx_ano_9o" = `Abandono na 8ª série/9º Ano`,
         "tx_serie_1a" = `Abandono na 1ª série - Médio`,
         "tx_serie_2a" = `Abandono na 2ª série - Médio`,
         "tx_serie_3a" = `Abandono na 3ª série - Médio`
  ) %>% 
  left_join(municipios, by = "Cod_muni") %>%
  select(-Nome_muni) %>% 
  rename("Nome_muni" = Nome_certo) %>% 
  relocate(Nome_muni, .after = Cod_muni) %>% 
  mutate(Dependencia = case_when(Dependencia == "Particular" ~ "Privada",
                                 TRUE ~ Dependencia))


# IDHM -------------------------------------------------=
idhm_2010 <- readRDS("./Dados/dados_ic/variaveis_exp/idhm_2010.rds")
glimpse(idhm_2010)

muni_tx <- tx_2010 %>% pull(Nome_muni) %>% unique %>% sort
muni_idhm <- idhm_2010 %>% pull(Nome_muni) %>% unique %>% sort

munis <- tibble(muni_idhm,muni_tx)
munis %>% filter(muni_tx != muni_idhm)

idhm_2010 <- idhm_2010 %>% 
  mutate(Nome_muni = case_when(Nome_muni == "Brasópolis" ~ "Brazópolis", 
                               Nome_muni == "Dona Eusébia" ~ "Dona Euzébia",    
                               Nome_muni == "Olhos-D'Água" ~ "Olhos-d'Água",    
                               Nome_muni == "Passa-Vinte" ~ "Passa Vinte",    
                               Nome_muni == "Pingo-D'Água" ~ "Pingo d'Água",  
                               Nome_muni == "São João Del Rei" ~ "São João del Rei",
                               TRUE ~ Nome_muni)
  )

# População
pop_2010 <- readRDS("./Dados/dados_ic/variaveis_exp/pop_2010.rds") %>% 
  mutate(Cod_muni = as.numeric(Cod_muni)) %>% 
  select(-Nome_muni)
glimpse(pop_2010)

muni_tx <- tx_2010 %>% pull(Cod_muni) %>% unique %>% sort
muni_pop <- pop_2010 %>% pull(Cod_muni) %>% unique %>% sort

munis <- tibble(muni_pop,muni_tx)
munis %>% filter(muni_tx != muni_pop)


# PIB
pib_2010 <- readRDS("./Dados/dados_ic/variaveis_exp/pib_2010_novo.rds") %>% 
  mutate(Cod_muni = as.numeric(Cod_muni)) %>% 
  select(-Nome_muni)
glimpse(pib_2010)

muni_tx <- tx_2010 %>% pull(Cod_muni) %>% unique %>% sort
muni_pib <- pib_2010 %>% pull(Cod_muni) %>% unique %>% sort

munis <- tibble(muni_pib,muni_tx)
munis %>% filter(muni_tx != muni_pib)


# Bancos finais para regressão ---------------------------

tx_2010_EFI <- tx_2010 %>% 
  tratamento_inicial() %>% 
  select(-c(tx_ano_6o, tx_ano_7o, tx_ano_8o, tx_ano_9o, tx_serie_1a, tx_serie_2a, tx_serie_3a)) %>% 
  filtra_EFI() %>% 
  tratamento_final() %>% 
  adiciona_covar(idhm_2010, pop_2010, pib_2010)
glimpse(tx_2010_EFI)

tx_2010_EFII <- tx_2010 %>% 
  tratamento_inicial() %>% 
  select(-c(tx_ano_1o, tx_ano_2o, tx_ano_3o, tx_ano_4o, tx_ano_5o, tx_serie_1a, tx_serie_2a, tx_serie_3a)) %>% 
  filtra_EFII() %>% 
  tratamento_final() %>% 
  adiciona_covar(idhm_2010, pop_2010, pib_2010)
glimpse(tx_2010_EFII)

tx_2010_EM <- tx_2010 %>% 
  tratamento_inicial() %>% 
  select(-c(tx_ano_1o, tx_ano_2o, tx_ano_3o, tx_ano_4o, tx_ano_5o, tx_ano_6o, tx_ano_7o, tx_ano_8o, tx_ano_9o)) %>% 
  filtra_EM() %>% 
  tratamento_final() %>% 
  adiciona_covar(idhm_2010, pop_2010, pib_2010)
glimpse(tx_2010_EM)


saveRDS(tx_2010_EFI, "./Dados/dados_reais/tx_2010_EFI.rds")
saveRDS(tx_2010_EFII, "./Dados/dados_reais/tx_2010_EFII.rds")
saveRDS(tx_2010_EM, "./Dados/dados_reais/tx_2010_EM.rds")


###########################################################=
# DADOS SEM OS ZEROS #######################################
###########################################################=

dados_2020 <- readRDS("./Dados/dados_reais/tx_2020_EM.rds")

dados_sem_20 <- dados_2020 %>% 
  mutate(tx_muni_aux = round(tx_muni, 2)) %>% 
  filter(tx_muni_aux > 0)
dados_sem_20 %>% dim # nova dim: 149; dim com zeros: 272

saveRDS(dados_sem_20, "./Dados/dados_reais/tx_2020_EM_sem_zeros.rds")


dados_2015 <- readRDS("./Dados/dados_reais/tx_2015_EM.rds")
dim(dados_2015)

dados_sem_15 <- dados_2015 %>% 
  mutate(tx_muni_aux = round(tx_muni, 2)) %>% 
  filter(tx_muni_aux > 0)
dados_sem_15 %>% dim # nova dim: 149; dim com zeros: 256

saveRDS(dados_sem_15, "./Dados/dados_reais/tx_2015_EM_sem_zeros.rds")


dados_2010 <- readRDS("./Dados/dados_reais/tx_2010_EM.rds")
dim(dados_2010)

dados_sem_10 <- dados_2010 %>% 
  mutate(tx_muni_aux = round(tx_muni, 2)) %>% 
  filter(tx_muni_aux > 0)
dados_sem_10 %>% dim # nova dim: 149; dim com zeros: 243

saveRDS(dados_sem_10, "./Dados/dados_reais/tx_2010_EM_sem_zeros.rds")

