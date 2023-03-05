############################ Mini Projeto 03 ############################ 

# Objetivo: Fazer explicações me modelo de Machine Learning utilizando
# Auto ML com SHAP

# Instalando pacotes
install.packages('h2o')
install.packages('tidyverse')
install.packages('ggbeeswarm')

# Carregando pacotes
library(h2o)
library(tidyverse)
library(ggbeeswarm)

# Criando dados ficticios
df <- tibble(produtividade = c(rnorm(1000), rnorm(1000, 0.25)), 
             Rendimento = runif(2000),
             Custo = rf(2000, df1 = 5, df2 = 2),
             Prioridade = c(sample(rep(c('Baixa', 'Média', 'Alta'), c(300, 300, 400))),
                            sample(c('Baixa', 'Média', 'Alta'), 1000, prob = c(0.25, 0.25, 0.5), replace = T)),
             Eficiencia = rnorm(2000),
             Manutemcao = rep(c(0, 1), c(1050, 950)))

# Visão dos dados em geral
View(df)
dim(df)
str(df)