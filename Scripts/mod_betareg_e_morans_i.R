# MODELAGEM COM BETAREG E I DE MORAN ################


library(tidyverse)
library(betareg)
require(spdep)


# N=50 ##############################################

dat <- readRDS("./Dados/dados_artificiais/beta/dat_zeta_delta_50.rds")
dados <- cbind(dat$y %>% as.data.frame,
               dat$x %>% as.data.frame %>% .[,2:3]) %>% 
  rename("y"=V1, "x1"=V2, "x2"=V3)
glimpse(dados)

mod_50 <- betareg(y ~ x1 + x2, 
                  data = dados, 
                  link = "logit"
)
resid <- residuals(mod_50, type = "pearson")

# I de Moran
W.listw = mat2listw(dat$W)
moran.mc(resid, listw = W.listw, nsim = 1000) # valor-p = 0.000999 <-- Rej H0
# H0: Não existe associação espacial
# H1: Existe associação espacial <- - - - 


# N=100 #############################################

dat <- readRDS("./Dados/dados_artificiais/beta/dat_zeta_delta_100.rds")
dados <- cbind(dat$y %>% as.data.frame,
               dat$x %>% as.data.frame %>% .[,2:3]) %>% 
  rename("y"=V1, "x1"=V2, "x2"=V3)
glimpse(dados)

mod_100 <- betareg(y ~ x1 + x2, 
                  data = dados, 
                  link = "logit"
)

# Warning message:
#   In betareg.fit(X, Y, Z, weights, offset, link, link.phi, type, control) :
#   no valid starting value for precision parameter found, using 1 instead

resid <- residuals(mod_100, type = "pearson")

# I de Moran
W.listw = mat2listw(dat$W)
moran.mc(resid, listw = W.listw, nsim = 1000) # valor-p = 0.000999 <-- Rej H0
# H0: Não existe associação espacial
# H1: Existe associação espacial <- - - - 



# N=200 #############################################

dat <- readRDS("./Dados/dados_artificiais/beta/dat_zeta_delta_200.rds")
dados <- cbind(dat$y %>% as.data.frame,
               dat$x %>% as.data.frame %>% .[,2:3]) %>% 
  rename("y"=V1, "x1"=V2, "x2"=V3)
glimpse(dados)

mod_200 <- betareg(y ~ x1 + x2, 
                  data = dados, 
                  link = "logit"
)
resid <- residuals(mod_200, type = "pearson")

# I de Moran
W.listw = mat2listw(dat$W)
moran.mc(resid, listw = W.listw, nsim = 1000) # valor-p = 0.000999 <-- Rej H0
# H0: Não existe associação espacial
# H1: Existe associação espacial <- - - - 