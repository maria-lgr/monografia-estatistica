########################################################=
####   GRÁFICOS E TABELAS PARA ANÁLISE DESCRITIVA   ####
########################################################=

# Pacotes
library(tidyverse)
library(leaflet)
library(RColorBrewer)
library(sf)
library(ggExtra)
library(cowplot)

options(OutDec = ",")

# FUnções

hist_densi <- function(data, titulo) {
  p <- data %>% 
    ggplot(aes(x = tx_muni)) + 
    geom_histogram(aes(y = ..density..), colour = "#6D676E", fill = "white") +
    geom_density(alpha = 0.2, fill = "#FF6666", colour = "#DC0425") +
    ylab("Densidade") + 
    ggtitle(titulo) +
    xlab("Taxa (%)") +
    theme_minimal()
  return(p)
}

box_plot <- function(data) {
  p <- data %>% 
    select("value" = tx_muni) %>%
    ggplot(aes(x = "", y = value)) +
    geom_boxplot(fill = "grey", color = "black") + 
    coord_flip() +
    theme_classic() +
    xlab("") + ylab("Taxa (%)")
    # theme(axis.text.y=element_blank(),
    #       axis.ticks.y=element_blank())
  return(p)
}

dados_densi <- function(data1, data2, data3) {
  # data1 <- data1 %>% arrange(desc(tx_muni)) %>% .[1:ceiling(dim(data1)[1]/2),]
  # data2 <- data2 %>% arrange(desc(tx_muni)) %>% .[1:ceiling(dim(data2)[1]/2),]
  # data3 <- data3 %>% arrange(desc(tx_muni)) %>% .[1:ceiling(dim(data3)[1]/2),]
  new <- data1 %>% transmute(tx_muni, Ensino = "EFI") %>% 
    bind_rows(data2 %>% transmute(tx_muni, Ensino = "EFII")) %>% 
    bind_rows(data3 %>% transmute(tx_muni, Ensino = "EM")) %>% 
    arrange(tx_muni)
  return(new)
}

densidades <- function(data, titulo) {
  p <- data %>% 
    ggplot(aes(x = tx_muni, fill = Ensino)) +
    geom_density(alpha = 0.2, colour = "#434343") +
    ylab("Densidade") +
    ggtitle(titulo) +
    xlab("Taxa (%)") +
    theme_minimal()
  return(p)
}

resumos <- function(var) {
  sumario <- summary(var)
  dat <- data.frame("Média" = sumario[[4]], 
             "D.P." = sd(var),
             "Mín." = sumario[[1]],
             "1ºQ" = sumario[[2]],
             "Mediana" = sumario[[3]],
             "3ºQ" = sumario[[5]],
             "Máx." = sumario[[6]])
  return(round(dat, 2))
}

tabela <- function(data) {
  new <- resumos(data$tx_muni) %>% 
    bind_rows(resumos(data$taxa_publica*100)) %>% 
    bind_rows(resumos(data$taxa_urbana*100)) %>% 
    bind_rows(resumos(data$IDHM.Renda)) %>% 
    bind_rows(resumos(exp(data$log_pop))) %>% 
    bind_rows(resumos(data$log_pop)) %>%
    bind_rows(resumos(log(data$log_PIB_pc))) %>% 
    bind_rows(resumos(data$log_PIB_pc))
  rownames(new) <- c("Taxa abandono", "Taxa escolas públicas",
      "Taxa escolas área urbana", "IDHM Renda",
      "População", "log(População)",
      "PIB per capita", "log(PIB per capita)")
  return(new)
}

dados_mapa <- function(data) {
  ibgeCities <- readRDS('./Dados/ibgeCities.rds') %>% 
    st_transform('+proj=longlat +datum=WGS84') %>% 
    mutate(Cod_muni = as.numeric(city_code))
  new <- data %>%
    inner_join(ibgeCities, by = "Cod_muni") %>% 
    st_sf()
  return(new)
}

mapa <- function(data) {
  mypalette <- colorBin(palette = c("#deefee", "#5c97ad", "#02223e"),
                        domain = data$tx_muni, na.color = "transparent", bins = 5)
  textinho <- paste(
    "<b>",data$Nome_muni,"</b> - ",round(data$tx_muni, 1),"%</br>", 
    sep="") %>%
    lapply(htmltools::HTML)
  m <- leaflet(data) %>%
    addPolygons(color = "#666666", weight = 0.9, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 1,
                fillColor = ~mypalette(tx_muni),
                # highlightOptions = highlightOptions(color = "white", weight = 1.3,
                #                                     bringToFront = TRUE),
                label = textinho
                # labelOptions = labelOptions(
                #   style = list("font-weight" = "normal", padding = "3px 8px"),
                #   textsize = "13px", direction = "auto")
    ) %>%
    addLegend("bottomright", pal = mypalette, values = ~tx_muni, 
              # labFormat = labelFormat(transform = function(x)2^(x)-1, digits = 1),
              opacity = 1, title = "Taxa de abandono (%)")
  return(m)
}




# 2020 --------------------------------------

tx_2020_EFI <- readRDS("./Dados/dados_reais/tx_2020_EFI.rds")
tx_2020_EFII <- readRDS("./Dados/dados_reais/tx_2020_EFII.rds")
tx_2020_EM <- readRDS("./Dados/dados_reais/tx_2020_EM.rds")

tx_2020_EFI %>% glimpse

tabela(tx_2020_EFI)
tabela(tx_2020_EFII)
tabela(tx_2020_EM)

h1 <- hist_densi(tx_2020_EFI, "Taxas de abandono \npara EFI em 2020")
bp1 <- box_plot(tx_2020_EFI)

h2 <- hist_densi(tx_2020_EFII, "Taxas de abandono \npara EFII em 2020")
bp2 <- box_plot(tx_2020_EFII)

h3 <- hist_densi(tx_2020_EM, "Taxas de abandono \npara EM em 2020")
bp3 <- box_plot(tx_2020_EM)

png("./Imagens/hist_bp_2020.png", width = 650, height = 470)
plot_grid(h1, h2, h3,
          bp1, bp2, bp3,
          ncol = 3, align = "v", rel_heights = c(1, 0.325))
dev.off()


densidades(dados_densi(tx_2020_EFI, tx_2020_EFII, tx_2020_EM),
           "Taxas de abandono em 2020")

tx_2020_EFI %>% dados_mapa %>% mapa
tx_2020_EFII %>% dados_mapa %>% mapa
tx_2020_EM %>% dados_mapa %>% mapa






# 2015 --------------------------------------

tx_2015_EFI <- readRDS("./Dados/dados_reais/tx_2015_EFI.rds")
tx_2015_EFII <- readRDS("./Dados/dados_reais/tx_2015_EFII.rds")
tx_2015_EM <- readRDS("./Dados/dados_reais/tx_2015_EM.rds")

tabela(tx_2015_EFI)
tabela(tx_2015_EFII)
tabela(tx_2015_EM)

h1 <- hist_densi(tx_2015_EFI, "Taxas de abandono \npara EFI em 2015")
bp1 <- box_plot(tx_2015_EFI)

h2 <- hist_densi(tx_2015_EFII, "Taxas de abandono \npara EFII em 2015")
bp2 <- box_plot(tx_2015_EFII)

h3 <- hist_densi(tx_2015_EM, "Taxas de abandono \npara EM em 2015")
bp3 <- box_plot(tx_2015_EM)

png("./Imagens/hist_bp_2015.png", width = 650, height = 470)
plot_grid(h1, h2, h3,
          bp1, bp2, bp3,
          ncol = 3, align = "v", rel_heights = c(1, 0.325))
dev.off()

densidades(dados_densi(tx_2015_EFI, tx_2015_EFII, tx_2015_EM),
           "Taxas de abandono em 2015")

tx_2015_EFI %>% dados_mapa %>% mapa
tx_2015_EFII %>% dados_mapa %>% mapa
tx_2015_EM %>% dados_mapa %>% mapa


# 2010 --------------------------------------

tx_2010_EFI <- readRDS("./Dados/dados_reais/tx_2010_EFI.rds")
tx_2010_EFII <- readRDS("./Dados/dados_reais/tx_2010_EFII.rds")
tx_2010_EM <- readRDS("./Dados/dados_reais/tx_2010_EM.rds")

tabela(tx_2010_EFI)
tabela(tx_2010_EFII)
tabela(tx_2010_EM)

h1 <- hist_densi(tx_2010_EFI, "Taxas de abandono \npara EFI em 2010")
bp1 <- box_plot(tx_2010_EFI)

h2 <- hist_densi(tx_2010_EFII, "Taxas de abandono \npara EFII em 2010")
bp2 <- box_plot(tx_2010_EFII)

h3 <- hist_densi(tx_2010_EM, "Taxas de abandono \npara EM em 2010")
bp3 <- box_plot(tx_2010_EM)

png("./Imagens/hist_bp_2010.png", width = 650, height = 470)
plot_grid(h1, h2, h3,
          bp1, bp2, bp3,
          ncol = 3, align = "v", rel_heights = c(1, 0.325))
dev.off()

densidades(dados_densi(tx_2010_EFI, tx_2010_EFII, tx_2010_EM),
           "Taxas de abandono em 2010")

tx_2010_EFI %>% dados_mapa %>% mapa
tx_2010_EFII %>% dados_mapa %>% mapa
tx_2010_EM %>% dados_mapa %>% mapa


# SEM ZEROS ----

tx_2020_EM_sem_zero <- readRDS("./Dados/dados_reais/tx_2020_EM_sem_zeros.rds")
tx_2015_EM_sem_zero <- readRDS("./Dados/dados_reais/tx_2015_EM_sem_zeros.rds")
tx_2010_EM_sem_zero <- readRDS("./Dados/dados_reais/tx_2010_EM_sem_zeros.rds")

tabela(tx_2020_EM_sem_zero)
tabela(tx_2015_EM_sem_zero)
tabela(tx_2010_EM_sem_zero)

h1 <- hist_densi(tx_2010_EM_sem_zero, "Taxas de abandono \npara EM em 2010 \n(sem zeros)")
bp1 <- box_plot(tx_2010_EM_sem_zero)

h2 <- hist_densi(tx_2015_EM_sem_zero, "Taxas de abandono \npara EM em 2015 \n(sem zeros)")
bp2 <- box_plot(tx_2015_EM_sem_zero)

h3 <- hist_densi(tx_2020_EM_sem_zero, "Taxas de abandono \npara EM em 2020 \n(sem zeros)")
bp3 <- box_plot(tx_2020_EM_sem_zero)

png("./Imagens/hist_bp_2020_sem_zero.png", width = 650, height = 470)
plot_grid(h1, h2, h3,
          bp1, bp2, bp3,
          ncol = 3, align = "v", rel_heights = c(1, 0.325))
dev.off()


# TABELONA CIDADES DAS AMOSTRAS ---------------------------------------

tx_2020_EM <- readRDS("./Dados/dados_reais/tx_2020_EM.rds")
tx_2015_EM <- readRDS("./Dados/dados_reais/tx_2015_EM.rds")
tx_2010_EM <- readRDS("./Dados/dados_reais/tx_2010_EM.rds")
tx_2020_EM_sem_zero <- readRDS("./Dados/dados_reais/tx_2020_EM_sem_zeros.rds")
tx_2015_EM_sem_zero <- readRDS("./Dados/dados_reais/tx_2015_EM_sem_zeros.rds")
tx_2010_EM_sem_zero <- readRDS("./Dados/dados_reais/tx_2010_EM_sem_zeros.rds")

muni_20 <- tx_2020_EM$Nome_muni
muni_15 <- tx_2015_EM$Nome_muni
muni_10 <- tx_2010_EM$Nome_muni
muni_20_sz <- tx_2020_EM_sem_zero$Nome_muni
muni_15_sz <- tx_2015_EM_sem_zero$Nome_muni
muni_10_sz <- tx_2010_EM_sem_zero$Nome_muni

todos_muni <- c(muni_20,muni_10,muni_15,muni_20_sz,muni_15_sz,muni_10_sz) %>% unique %>% sort
length(todos_muni)

tab <- tibble("Município"=todos_muni) %>% 
  mutate("2010" = ifelse(todos_muni %in% muni_10, "x", ""),
         "2015" = ifelse(todos_muni %in% muni_15, "x", ""),
         "2020" = ifelse(todos_muni %in% muni_20, "x", ""),
         "2010 (sem zero)" = ifelse(todos_muni %in% muni_10_sz, "x", ""),
         "2015 (sem zero)" = ifelse(todos_muni %in% muni_15_sz, "x", ""),
         "2020 (sem zero)" = ifelse(todos_muni %in% muni_20_sz, "x", "")
         )
tab 
library(openxlsx)
write.xlsx(tab,"./Dados/dados_reais/tabela_municipios.xlsx")
