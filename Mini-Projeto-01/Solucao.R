# Carregando dataset
dataset <- read.csv('dataset_V1.csv')
dataset$X = NULL
View(dataset)
str(dataset)

dataset$pais <- as.factor(dataset$pais)
dataset$ano <- as.factor(dataset$ano)
dataset$sup_social <- as.factor(dataset$sup_social)

# Instalando e carregando pacotes
install.packages('ggplot2')
install.packages('hrbrthemes')
install.packages('tigerstats')
install.packages("dplyr")
library(ggplot2)
library(hrbrthemes)
library(tigerstats)
library(dplyr)

# 1- O aumento do PIB per capita de um país afeta positivamente a expectativa de
# vida dos cidadãos ao nascer? Qual a correlação entre essas duas variáveis?

cor(dataset$PIB_por_cap, dataset$expec_de_vida)

ggplot(dataset, aes(x=PIB_por_cap, y=expec_de_vida, color=PIB_por_cap)) + 
  geom_point(size=2) +
  theme_bw() +
  scale_colour_gradientn(colours=terrain.colors(10))

plot(x=dataset$PIB_por_cap, dataset$expec_de_vida)

# 2- Existe uma correlação entre a escala de vida e a conscientização do público 
# em geral sobre a corrupção nos negócios e no governo? Qual a correlação entre 
# essas duas variáveis?

cor(dataset$pont_da_fel, dataset$corrupcao)

ggplot(dataset, aes(x=pont_da_fel, y=corrupcao, color=pont_da_fel)) + 
  geom_point(size=2) +
  theme_bw() +
  scale_colour_gradientn(colours=rainbow(4))

# 3- O aumento na escala de vida tem algum efeito na média de felicidade entre o 
# público em geral? Qual a correlação entre essas duas variáveis?

cor(dataset$pont_da_fel, dataset$afeto_pos)

ggplot(dataset, aes(x=pont_da_fel, y=afeto_pos, color=pont_da_fel)) + 
  geom_point(size=2) +
  theme_bw() +
  scale_colour_gradient(low = "yellow", high = "red")

# 4- O país com o menor índice de suporte social tem maior percepção de corrupção 
# em relação às empresas e ao governo no país?
?aggregate
df_alt <- dataset %>%
  select(pais, sup_social) %>%
  summarise(sup_social = sum(sup_social))

View(df_alt)

# 5- Pessoas generosas são mais felizes?

cor(dataset$doacao, dataset$pont_da_fel)

ggplot(dataset, aes(x=doacao, y=afeto_pos, color=pont_da_fel, color=pont_da_fel)) + 
  geom_point(size=2) +
  theme_bw() +
  scale_colour_gradientn(colours = topo.colors(10))