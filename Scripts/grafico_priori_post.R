# gráfico explicação priori

library(tidyverse)

set.seed(2)

valores <- seq(0,1,0.01)
amostra <- dbeta(valores, 4, 3)
priori_info <- dbeta(valores, 1.5, 3)
posteriori_info <- dbeta(valores, 3, 3.5)
priori_vaga <- dbeta(valores, 1, 1)
posteriori_vaga <- dbeta(valores, 4, 3.5)

dados <- data.frame(valores,
                    amostra,
                    priori_info,
                    posteriori_info,
                    priori_vaga,
                    posteriori_vaga)

info <- ggplot(dados, aes(x=valores)) +
  geom_line(aes(y=amostra), color = '#093C68', linetype = 1, size = 1) +
  geom_line(aes(y=priori_info), color = '#FF1C60', linetype = 5, size =1) +
  geom_line(aes(y=posteriori_info), color = '#921372', linetype = 4, size =1) +
  theme_minimal() +
  ylab("Densidade") + xlab("Valores") +
  ggtitle('Priori informativa')

vaga <- ggplot(dados, aes(x=valores)) +
  geom_line(aes(y=amostra), color = '#093C68', linetype = 1, size = 1) +
  geom_line(aes(y=priori_vaga), color = '#FF1C60', linetype = 5, size =1) +
  geom_line(aes(y=posteriori_vaga), color = '#921372', linetype = 4, size =1) +
  theme_minimal() +
  ylab("Densidade") + xlab("Valores") +
  ggtitle('Priori vaga')

png("./Imagens/priori_vaga_info.png", width = 660, height = 300)
grid.arrange(info, vaga,
             ncol = 2
)
dev.off()