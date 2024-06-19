
#------------------------------
# Limpando o diretório do R
#------------------------------

rm(list = ls())

#------------------------------
# Instalando pacotes utilizados
#------------------------------

#install.packages("data.table")
#install.packages("readxl")
#install.packages("dplyer")
#install.packages("PNADcIBGE")
#install.packages("tidyr")
#install.packages("magrittr")
#install.packages("ggplot2")
#install.packages("gridExtra")
#install.packages("readr")
#install.packages("grid")
#install.packages("pwr")
#install.packages("rgl")
#install.packages("car")
#install.packages("sandwich")
#install.packages("lmtest")
#install.packages("MASS")
#install.packages("corrplot")
#install.packages("knitr")
#install.packages("kableExtra")
#install.packages("sf")
#install.packages("geobr")
#install.packages("viridis")

#------------------------------
# Abrindo os pacotes
#------------------------------

library(PNADcIBGE)
library(data.table)
library(readxl)
library(dplyr)
library(gridExtra)
library(grid)
library(magrittr)
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(pwr)
library(rgl)
library(car)
library(sandwich)
library(lmtest)
library(MASS)
library(corrplot)
library(knitr)
library(kableExtra)
library(sf)
library(geobr)
library(viridis)

#------------------------------
# Setando o diretorio
#------------------------------

setwd("C:/Users/lhaon/Desktop/Projeto Econo/PNADC_042023")

#------------------------------
# Abrindo a base de dados (PNAD 2023 4° trimestre)
#------------------------------

pnad_1 <- read_pnadc(microdata = "PNADC_042023.txt", input_txt = "input_PNADC_trimestral.txt")
pnad_1 <- as.data.table(pnad_1)

#------------------------------
# Criando uma base de dados para fazer um Mapa (renda + uf)
#------------------------------

pnad_M <- pnad_1 %>%
  dplyr::select(UF, VD4020)

colnames(pnad_M) <- c("UF", "Renda")

pnad_M <- pnad_M %>%
  filter(!is.na(Renda) & !is.nan(Renda) & !is.infinite(Renda))

#Criando uma base com a média de renda de cada UF
media_renda_uf <- pnad_M %>%
  group_by(UF) %>%
  summarize(Media_Renda = mean(Renda))

#------------------------------
# Criando uma base de dados para fazer um Mapa (idade + uf)
#------------------------------

pnad_I <- pnad_1 %>%
  dplyr::select(UF, V2009)

colnames(pnad_I) <- c("UF", "Idade")

pnad_I <- pnad_I %>% mutate(across(everything(), as.numeric))

pnad_I <- pnad_I %>%
  filter(!is.na(Idade) & !is.nan(Idade) & !is.infinite(Idade))

#Criando uma base com a média de renda de cada UF
media_idade_uf <- pnad_I %>%
  group_by(UF) %>%
  summarize(Media_Idade = mean(Idade))

#------------------------------
# Criando uma base de dados para fazer um Mapa (educação + uf)
#------------------------------

pnad_E <- pnad_1 %>%
  dplyr::select(UF, VD3005)

colnames(pnad_E) <- c("UF", "Educ")

pnad_E <- pnad_E %>% mutate(across(everything(), as.numeric))

pnad_E <- pnad_E %>%
  filter(!is.na(Educ) & !is.nan(Educ) & !is.infinite(Educ))

#Criando uma base com a média de renda de cada UF
media_educ_uf <- pnad_E %>%
  group_by(UF) %>%
  summarize(Media_Educ = mean(Educ))

#------------------------------
# Criando uma base de dados para fazer um Mapa (pessoas + uf)
#------------------------------

pnad_P <- pnad_1 %>%
  dplyr::select(UF, V2001)

colnames(pnad_P) <- c("UF", "Pessoas")

pnad_P <- pnad_P %>% mutate(across(everything(), as.numeric))

pnad_P <- pnad_P %>%
  filter(!is.na(Pessoas) & !is.nan(Pessoas) & !is.infinite(Pessoas))

#Criando uma base com a média de renda de cada UF
media_pessoas_uf <- pnad_P %>%
  group_by(UF) %>%
  summarize(Media_Pessoas = mean(Pessoas))

#------------------------------
# Selecionando variaves que estamos interessados :
#  Renda VD4020, Idade V2009, Numero de pessoas no domicilio V2001
# Controles : 
#  Genero V2007, Anos de Educaçâo VD3005, Cor V2010, Horas trabalhadas V4039, Urbano V1022
#------------------------------

pnad_1 <- pnad_1 %>%
  dplyr::select(V2001, V2009, VD4020, V2007, V2010, V1022, V4039, VD3005)

#------------------------------
# Limpando a base
#------------------------------

#Nomeando as colunas
colnames(pnad_1) <- c( "Pessoas", "Idade", "Renda", "Sexo", "Cor", "Urbano", "Horas", "Educ") #Pessoas = numero de pessoas no domicilio

#Lendo todas as colunas da base como argumentos numéricos
pnad_1 <- pnad_1 %>% mutate(across(everything(), as.numeric))

#Retirando fatores ausentes na coluna Renda para passarmos o log
pnad_1 <- pnad_1 %>%
  filter(!is.na(Renda) & !is.nan(Renda) & !is.infinite(Renda) & Renda > 0)

#------------------------------
# Descritivas da base
#------------------------------

#Criando uma funcao para moda
calcular_moda <- function(x) {
  tab_freq <- table(x)  
  moda <- names(tab_freq)[tab_freq == max(tab_freq)]  
  return(moda)
}

#Pessoas
media_pessoas <- mean(pnad_1$Pessoas, na.rm = TRUE)
var_pessoas <- var(pnad_1$Pessoas, na.rm = TRUE)
mediana_pessoas <- median(pnad_1$Pessoas, na.rm = TRUE)
quartis_pessoas <- quantile(pnad_1$Pessoas, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
estatisticas_pessoas <- data.frame(
  Media = media_pessoas,
  Variancia = var_pessoas,
  Mediana = mediana_pessoas,
  Minimo = quartis_pessoas[1],
  Primeiro_Quartil = quartis_pessoas[2],
  Terceiro_Quartil = quartis_pessoas[4],
  Maximo = quartis_pessoas[5]
)
rownames(estatisticas_pessoas) <- NULL

#Idade
media_idade <- mean(pnad_1$Idade, na.rm = TRUE)
var_idade <- var(pnad_1$Idade, na.rm = TRUE)
mediana_idade <- median(pnad_1$Idade, na.rm = TRUE)
quartis_idade <- quantile(pnad_1$Idade, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
estatisticas_idade <- data.frame(
  Media = media_idade,
  Variancia = var_idade,
  Mediana = mediana_idade,
  Minimo = quartis_idade[1],
  Primeiro_Quartil = quartis_idade[2],
  Terceiro_Quartil = quartis_idade[4],
  Maximo = quartis_idade[5]
)
rownames(estatisticas_idade) <- NULL

#Renda
media_renda <- mean(pnad_1$Renda, na.rm = TRUE)
var_renda <- var(pnad_1$Renda, na.rm = TRUE)
mediana_renda <- median(pnad_1$Renda, na.rm = TRUE)
quartis_renda <- quantile(pnad_1$Renda, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
estatisticas_renda <- data.frame(
  Media = media_renda,
  Variancia = var_renda,
  Mediana = mediana_renda,
  Minimo = quartis_renda[1],
  Primeiro_Quartil = quartis_renda[2],
  Terceiro_Quartil = quartis_renda[4],
  Maximo = quartis_renda[5]
)
rownames(estatisticas_renda) <- NULL

#Horas
media_horas <- mean(pnad_1$Horas, na.rm = TRUE)
var_horas <- var(pnad_1$Horas, na.rm = TRUE)
mediana_horas <- median(pnad_1$Horas, na.rm = TRUE)
quartis_horas <- quantile(pnad_1$Horas, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
estatisticas_horas <- data.frame(
  Media = media_horas,
  Variancia = var_horas,
  Mediana = mediana_horas,
  Minimo = quartis_horas[1],
  Primeiro_Quartil = quartis_horas[2],
  Terceiro_Quartil = quartis_horas[4],
  Maximo = quartis_horas[5]
)
rownames(estatisticas_horas) <- NULL

#Educ
media_educ <- mean(pnad_1$Educ, na.rm = TRUE)
var_educ <- var(pnad_1$Educ, na.rm = TRUE)
mediana_educ <- median(pnad_1$Educ, na.rm = TRUE)
quartis_educ <- quantile(pnad_1$Educ, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
estatisticas_educ <- data.frame(
  Media = media_educ,
  Variancia = var_educ,
  Mediana = mediana_educ,
  Minimo = quartis_educ[1],
  Primeiro_Quartil = quartis_educ[2],
  Terceiro_Quartil = quartis_educ[4],
  Maximo = quartis_educ[5]
)
rownames(estatisticas_educ) <- NULL

#Para variáveis Dummys faremos uma análise de frequencia

#Sexo
frequencia_sexo <- table(pnad_1$Sexo)
frequencia_sexo_df <- as.data.frame(frequencia_sexo)
colnames(frequencia_sexo_df) <- c("Sexo", "Frequency")
frequencia_sexo_df$Percentage <- (frequencia_sexo_df$Frequency / sum(frequencia_sexo_df$Frequency)) * 100
total_row <- data.frame(
  Sexo = "Total",
  Frequency = sum(frequencia_sexo_df$Frequency),
  Percentage = 100
)
frequencia_sexo_df <- rbind(frequencia_sexo_df, total_row)
rownames(frequencia_sexo_df) <- NULL

#Urbano
frequencia_urbano <- table(pnad_1$Urbano)
frequencia_urbano_df <- as.data.frame(frequencia_urbano)
colnames(frequencia_urbano_df) <- c("Urbano", "Frequency")
frequencia_urbano_df$Percentage <- (frequencia_urbano_df$Frequency / sum(frequencia_urbano_df$Frequency)) * 100
total_row <- data.frame(
  Urbano = "Total",
  Frequency = sum(frequencia_urbano_df$Frequency),
  Percentage = 100
)
frequencia_urbano_df <- rbind(frequencia_urbano_df, total_row)
rownames(frequencia_urbano_df) <- NULL

#Cor
frequencia_cor <- table(pnad_1$Cor)
frequencia_cor_df <- as.data.frame(frequencia_cor)
colnames(frequencia_cor_df) <- c("Cor", "Frequency")
frequencia_cor_df$Percentage <- (frequencia_cor_df$Frequency / sum(frequencia_cor_df$Frequency)) * 100
total_row <- data.frame(
  Cor = "Total",
  Frequency = sum(frequencia_cor_df$Frequency),
  Percentage = 100
)
frequencia_cor_df <- rbind(frequencia_cor_df, total_row)
rownames(frequencia_cor_df) <- NULL

#------------------------------
# Matriz de correlação da Base (Multicolinearidade)
#------------------------------

correlation_matrix <- cor(pnad_1, use = "complete.obs")

#Plot da matriz de correlação
corrplot(correlation_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45)

#------------------------------
# Regressão base
#------------------------------

reg1 <- lm( log(Renda) ~ Pessoas + I(Idade^2) + Sexo + Cor + Urbano + Horas + Educ , data = pnad_1 )
summary(reg1)

#------------------------------
# Teste de Homo/Heteroscedasticidade
#------------------------------

#BP Teste
bp_test_1 <- bptest(reg1)
print(bp_test_1)

#Teste de White (Não imputa NA, logo realizaremos o Teste de White em uma Base de dados "limpa". OBS : reg1 já ignora NA)
pnad_1_clean <- pnad_1 %>%
  filter(!is.na(Renda) & !is.nan(Renda) & !is.infinite(Renda) & Renda > 0)
reg2 <- lm( log(Renda) ~ Pessoas + I(Idade^2) + Sexo + Cor + Urbano + Horas + Educ , data = pnad_1_clean )
summary(reg2)
white_test_1 <- bptest( reg2, ~ fitted(reg2) + I(fitted(reg2)^2) )
print(white_test_1)

#p-valor muito baixo, ou seja, para um alpha = 5%, rejeitamos H0 (ser homoscedastico) 

#------------------------------
# Regressao Robusta
#------------------------------

#Calcular a matriz de variância robusta
vcov_robust <- vcovHC(reg1, type = "HC1")

#Fazer inferências robustas
robust_results <- coeftest(reg1, vcov = vcov_robust)
print(robust_results)

#------------------------------
# Regressao sem log na Renda/Idade como modelo quadratico
#------------------------------

reg3 <- lm( Renda ~ Pessoas + I(Idade^2) + Sexo + Cor + Urbano + Horas + Educ , data = pnad_1 )
summary(reg3)

reg4 <- lm( log(Renda) ~ Pessoas + Idade + Sexo + Cor + Urbano + Horas + Educ , data = pnad_1 )
summary(reg4)

reg5 <- lm( Renda ~ Pessoas + Idade + Sexo + Cor + Urbano + Horas + Educ , data = pnad_1 )
summary(reg5)

#------------------------------
# Plots
#------------------------------

#Criar gráficos de dispersão para cada variável preditora

#Renda x Pessoas
p1 <- ggplot(pnad_1, aes(x = Pessoas, y = Renda)) +
  geom_point() + 
  theme_minimal() + 
  labs(title = "Pessoas vs Renda")

#Renda x Idade
p2 <- ggplot(pnad_1, aes(x = Idade, y = Renda)) +
  geom_point() +
  theme_minimal() + 
  labs(title = "Idade vs Renda")

#Renda x Sexo
p3 <- ggplot(pnad_1, aes(x = Sexo, y = Renda)) + 
  geom_point() +
  theme_minimal() + 
  labs(title = "Sexo vs Renda")

#Renda x Cor
p4 <- ggplot(pnad_1, aes(x = Cor, y = Renda)) +
  geom_point() +
  theme_minimal() + 
  labs(title = "Cor vs Renda")

#Renda x Urbano
p5 <- ggplot(pnad_1, aes(x = Urbano, y = Renda)) + 
  geom_point() + 
  theme_minimal() +
  labs(title = "Urbano vs Renda")

#Renda x Horas
p6 <- ggplot(pnad_1, aes(x = Horas, y = Renda)) + 
  geom_point() + 
  theme_minimal() +
  labs(title = "Horas vs Renda")

#Renda x Educ
p7 <- ggplot(pnad_1, aes(x = Educ, y = Renda)) +
  geom_point() + 
  theme_minimal() + 
  labs(title = "Educ vs Renda")

#Organizar os gráficos em uma grade
grid.arrange(p1, p2, p3, p4, p5, p6, p7, ncol = 2)

#------------------------------
# Tabelas
#------------------------------

kable(estatisticas_pessoas, format = "html", caption = "Tabela 1: Estatísticas Descritivas para a Variável Pessoas") %>%
  kable_styling(full_width = FALSE, position = "center", bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  row_spec(0, bold = TRUE)

kable(estatisticas_idade, format = "html", caption = "Tabela 2: Estatísticas Descritivas para a Variável Idade") %>%
  kable_styling(full_width = FALSE, position = "center", bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  row_spec(0, bold = TRUE)

kable(estatisticas_renda, format = "html", caption = "Tabela 3: Estatísticas Descritivas para a Variável Renda") %>%
  kable_styling(full_width = FALSE, position = "center", bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  row_spec(0, bold = TRUE)

kable(estatisticas_horas, format = "html", caption = "Tabela 4: Estatísticas Descritivas para a Variável Horas") %>%
  kable_styling(full_width = FALSE, position = "center", bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  row_spec(0, bold = TRUE)

kable(estatisticas_educ, format = "html", caption = "Tabela 5: Estatísticas Descritivas para a Variável Educ") %>%
  kable_styling(full_width = FALSE, position = "center", bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  row_spec(0, bold = TRUE)

kable(frequencia_sexo_df, format = "html", caption = "Tabela 6: Frequência e Percentual para a Variável Sexo") %>%
  kable_styling(full_width = FALSE, position = "center", bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  row_spec(0, bold = TRUE)

kable(frequencia_urbano_df, format = "html", caption = "Tabela 7: Frequência e Percentual para a Variável Urbano") %>%
  kable_styling(full_width = FALSE, position = "center", bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  row_spec(0, bold = TRUE)

kable(frequencia_cor_df, format = "html", caption = "Tabela 8: Frequência e Percentual para a Variável Cor") %>%
  kable_styling(full_width = FALSE, position = "center", bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  row_spec(0, bold = TRUE)

#------------------------------
# Mapas
#------------------------------

#Renda x UF

#Baixar dados geográficos das UFs
ufs_sf <- geobr::read_state(code_state = "all", year = 2020)

#Combinar a média de renda com os dados geográficos
ufs_sf <- ufs_sf %>%
  mutate(code_state = as.character(code_state))

map_data_R <- ufs_sf %>%
  left_join(media_renda_uf, by = c("code_state" = "UF"))

#Plot do mapa de Renda média por municipio
ggplot(map_data_R) +
  geom_sf(aes(fill = Media_Renda)) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Média de Renda") +
  theme_minimal() +
  labs(title = "Média de Renda por Unidade Federativa (UF) no Brasil",
       caption = "Fonte: PNAD continua 2023, 4º semestre")

#Educ x UF

#Combinar a média de educação com os dados geográficos
media_educ_uf <- media_educ_uf %>%
  mutate(UF = as.character(UF))

map_data_E <- ufs_sf %>%
  left_join(media_educ_uf, by = c("code_state" = "UF"))

ggplot(map_data_E) +
  geom_sf(aes(fill = Media_Educ)) +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen", name = "Média de Anos de Estudo") +
  theme_minimal() +
  labs(title = "Média de Anos de Estudo por Unidade Federativa (UF) no Brasil",
       caption = "Fonte: PNAD continua 2023, 4º semestre")

#Pessoas x UF

#Combinar a média de educação com os dados geográficos
media_pessoas_uf <- media_pessoas_uf %>%
  mutate(UF = as.character(UF))

map_data_P <- ufs_sf %>%
  left_join(media_pessoas_uf, by = c("code_state" = "UF"))

ggplot(map_data_P) +
  geom_sf(aes(fill = Media_Pessoas)) +
  scale_fill_gradient(low = "orange", high = "red", name = "Média do Número de Moradores na Residência") +
  theme_minimal() +
  labs(title = "Média do Número de Moradores da Residência por Unidade Federativa (UF) no Brasil",
       caption = "Fonte: PNAD continua 2023, 4º semestre")


#Idade x UF

#Combinar a média de educação com os dados geográficos
media_idade_uf <- media_idade_uf %>%
  mutate(UF = as.character(UF))

map_data_I <- ufs_sf %>%
  left_join(media_idade_uf, by = c("code_state" = "UF"))

ggplot(map_data_I) +
  geom_sf(aes(fill = Media_Idade)) +
  scale_fill_gradient(low = "#D8BFD8", high = "#800080", name = "Média da Idade") +
  theme_minimal() +
  labs(title = "Média da Idade por Unidade Federativa (UF) no Brasil",
       caption = "Fonte: PNAD continua 2023, 4º semestre")
