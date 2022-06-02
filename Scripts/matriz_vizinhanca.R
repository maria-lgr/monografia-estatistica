# LOCALIZAÇÃO ESPACIAL DOS MUNICÍPIOS 


# Pacotes ----
library(tidyverse)


# Leitura dos dados ----

lat_long <- read.csv("./Dados/municipios.csv")
glimpse(lat_long)




# 2020 ###############################################

dados_2020_EM <- readRDS("./Dados/dados_reais/tx_2020_EM.rds")

data_ordered <- dados_2020_EM %>% 
  left_join(lat_long %>% 
              filter(codigo_uf == 31), by = c("Cod_muni" = "codigo_ibge")) %>% 
  select(latitude, longitude)

matriz <- as.matrix(data_ordered)
print(matriz)

md <- dist(matriz, diag = TRUE) %>% as.matrix()
diag(md) <- 100
md <- ifelse(md < 0.9, 1, 0)
print(md)

somas <- colSums(md)
sum(ifelse(somas == 0, somas, 1))
min(somas)
max(somas)
somas %>% sort
dw <- diag(somas) # quant vizinhos
dw
w <- md
rho <- 0.95
S_delta <- solve(dw - rho * w) # S_delta

saveRDS(S_delta, "./Dados/dados_reais/m_neighborhood_2020_EM.rds")
saveRDS(w, "./Dados/dados_reais/W_2020_EM.rds")




# 2015 ###############################################

dados_2015_EM <- readRDS("./Dados/dados_reais/tx_2015_EM.rds")

data_ordered <- dados_2015_EM %>% 
  left_join(lat_long %>% 
              filter(codigo_uf == 31), by = c("Cod_muni" = "codigo_ibge")) %>% 
  select(latitude, longitude)

matriz <- as.matrix(data_ordered)
print(matriz)

md <- dist(matriz, diag = TRUE) %>% as.matrix()
diag(md) <- 100
md <- ifelse(md < 0.87, 1, 0)
print(md)

somas <- colSums(md)
sum(ifelse(somas == 0, somas, 1))
min(somas)
max(somas)
somas %>% sort %>% head
dw <- diag(somas) # quant vizinhos
dw
w <- md
rho <- 0.95
S_delta <- solve(dw - rho * w) # S_delta

saveRDS(S_delta, "./Dados/dados_reais/m_neighborhood_2015_EM.rds")
saveRDS(w, "./Dados/dados_reais/W_EM.rds")





# 2010 ###############################################

dados_2010_EM <- readRDS("./Dados/dados_reais/tx_2010_EM.rds")

data_ordered <- dados_2010_EM %>% 
  left_join(lat_long %>% 
              filter(codigo_uf == 31), by = c("Cod_muni" = "codigo_ibge")) %>% 
  select(latitude, longitude)

matriz <- as.matrix(data_ordered)
print(matriz)

md <- dist(matriz, diag = TRUE) %>% as.matrix()
diag(md) <- 100
md <- ifelse(md < 0.87, 1, 0)
print(md)

somas <- colSums(md)
sum(ifelse(somas == 0, somas, 1))
min(somas)
max(somas)
somas %>% sort %>% head
dw <- diag(somas) # quant vizinhos
dw
w <- md
rho <- 0.95
S_delta <- solve(dw - rho * w) # S_delta

saveRDS(S_delta, "./Dados/dados_reais/m_neighborhood_2010_EM.rds")
saveRDS(w, "./Dados/dados_reais/W_2010_EM.rds")






# 2020 (SEM ZEROS) ###############################################

dados_2020_EM <- readRDS("./Dados/dados_reais/tx_2020_EM_sem_zeros.rds")

data_ordered <- dados_2020_EM %>% 
  left_join(lat_long %>% 
              filter(codigo_uf == 31), by = c("Cod_muni" = "codigo_ibge")) %>% 
  select(latitude, longitude)

matriz <- as.matrix(data_ordered)
print(matriz)

md <- dist(matriz, diag = TRUE) %>% as.matrix()
diag(md) <- 100
md <- ifelse(md < 1.25, 1, 0)
print(md)

somas <- colSums(md)
sum(ifelse(somas == 0, somas, 1))
min(somas)
max(somas)
somas %>% sort %>% head
dw <- diag(somas) # quant vizinhos
dw
w <- md
rho <- 0.95
S_delta <- solve(dw - rho * w) # S_delta

saveRDS(S_delta, "./Dados/dados_reais/m_neighborhood_2020_EM_sem_zeros.rds")
saveRDS(w, "./Dados/dados_reais/W_2020_EM_sem_zeros.rds")




# 2015 (SEM ZEROS) ###############################################

dados_2015_EM <- readRDS("./Dados/dados_reais/tx_2015_EM_sem_zeros.rds")
dim(dados_2015_EM)

data_ordered <- dados_2015_EM %>% 
  left_join(lat_long %>% 
              filter(codigo_uf == 31), by = c("Cod_muni" = "codigo_ibge")) %>% 
  select(latitude, longitude)

matriz <- as.matrix(data_ordered)
print(matriz)

md <- dist(matriz, diag = TRUE) %>% as.matrix()
diag(md) <- 100
md <- ifelse(md < 1.3, 1, 0)
print(md)

somas <- colSums(md)
sum(ifelse(somas == 0, somas, 1))
min(somas)
max(somas)
somas %>% sort %>% head
dw <- diag(somas) # quant vizinhos
dw
w <- md
rho <- 0.95
S_delta <- solve(dw - rho * w) # S_delta

saveRDS(S_delta, "./Dados/dados_reais/m_neighborhood_2015_EM_sem_zeros.rds")
saveRDS(w, "./Dados/dados_reais/W_2015_EM_sem_zeros.rds")





# 2010 (SEM ZEROS) ###############################################

dados_2010_EM <- readRDS("./Dados/dados_reais/tx_2010_EM_sem_zeros.rds")
dim(dados_2010_EM)

data_ordered <- dados_2010_EM %>% 
  left_join(lat_long %>% 
              filter(codigo_uf == 31), by = c("Cod_muni" = "codigo_ibge")) %>% 
  select(latitude, longitude)

matriz <- as.matrix(data_ordered)
print(matriz)

md <- dist(matriz, diag = TRUE) %>% as.matrix()
diag(md) <- 100
md <- ifelse(md < 1.5, 1, 0)
print(md)

somas <- colSums(md)
sum(ifelse(somas == 0, somas, 1))
min(somas)
max(somas)
somas %>% sort %>% head
dw <- diag(somas) # quant vizinhos
dw
w <- md
rho <- 0.95
S_delta <- solve(dw - rho * w) # S_delta

saveRDS(S_delta, "./Dados/dados_reais/m_neighborhood_2010_EM_sem_zeros.rds")
saveRDS(w, "./Dados/dados_reais/W_2010_EM_sem_zeros.rds")
