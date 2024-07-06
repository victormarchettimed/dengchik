#install.packages("remotes")
library(pkgbuild)
library(read.dbc)
#remotes::install_github("rfsaldanha/microdatasus", force=TRUE)
# Carregar as bibliotecas necessárias
library(dplyr)
library(microdatasus)
library(survival)
library(ggplot2)
library(sf)

# Buscar e processar os dados
base <- fetch_datasus(year_start = 2022, month_start = 1, uf = "ES", 
                       year_end = 2022, month_end = 12, information_system = "SIH-RD")

dados <- base

# Supondo que 'base' é o seu conjunto de dados

library(dplyr)

# Filtre os dados para incluir apenas casos de DIAG_PRINC contendo "I21" e selecione as colunas relevantes
dados <- base %>%
  filter(grepl("I21", DIAG_PRINC)) %>%
  select(MUNIC_RES, IDADE, MORTE)

# Calcule o número total de mortes e de pessoas em cada município
dados_municipio <- dados %>%
  group_by(MUNIC_RES) %>%
  summarise(total_mortes = sum(MORTE),
            total_pessoas = n())

# Calcule o número total de mortes e de pessoas em todos os municípios, excluindo o município em questão
dados_total <- dados %>%
  summarise(total_mortes = sum(MORTE),
            total_pessoas = n())

# Calcule a taxa de mortalidade no restante dos municípios
taxa_mortalidade_restante <- dados_total$total_mortes / dados_total$total_pessoas

# Calcule o risco relativo para morte em cada município
dados_municipio <- dados_municipio %>%
  mutate(risco_relativo = (total_mortes / total_pessoas) / taxa_mortalidade_restante)

# Ajustando o modelo de regressão logística
modelo <- glm(MORTE ~ MUNIC_RES + IDADE, data = dados, family = binomial())

# Obtendo os coeficientes do modelo ajustado
coeficientes <- coef(modelo)

# Extraindo os coeficientes para MUNIC_RES
coef_municipio <- coeficientes[grep("^MUNIC_RES", names(coeficientes))]

# Extraindo o coeficiente para IDADE
coef_idade <- coeficientes["IDADE"]

# Calculando o risco relativo ajustado para cada município em relação a uma referência
risco_relativo_ajustado <- exp(coef_municipio)

# Visualizando o risco relativo ajustado
print(risco_relativo_ajustado)
