########################### Análise Exploratória ########################### 

# Carregando dados
df <- read.csv('dataset.csv')

# Instalando pacotes
install.packages('Amelia')
install.packages('dplyr')
install.packages('ggplot2')

# Carregando pacotes
library(Amelia)
library(dplyr)
library(ggplot2)

# Visualizando os dados
View(df)

# Dimensões
dim(df)

# Tipos dos dados
str(df)

# Sumário
summary(df)

# Visualizando os valores missing
missmap(df)

# Verificando a porcentagem de valores missing
casos_completos <- sum(complete.cases(df))
casos_nao_completos <- sum(!complete.cases(df))
percentual <- (casos_nao_completos / casos_completos) * 100
percentual
rm(casos_completos)
rm(casos_nao_completos)
rm(percentual)

# Verificar quantos países foram incluídos o dataset
lista_de_paises <- unique(df$Country.name)
lista_de_paises
rm(lista_de_paises)
rm(lista_var_num)

# Verificando a variável Ano
anos <- unique(df$year)
range(anos)
rm(anos)
table(df$year)

# Criando uma matriz de correlação para todas as variáveis
lista_var_num <- sapply(df, is.numeric)
num <- df[lista_var_num]
pairs(num[1:5], labels = colnames(num[1:5]))
pairs(num[6:11], labels = colnames(num[6:11]))
rm(num)

########################### Problemas Encontrados ########################### 

# Modificar o nome das variáveis
# Mudar a variavel Pais para factor
# Remover a variável Ano, pois não será usada
# Tratar valores missing
# Remover os registros relacionado ao Ano por baixa quantidade de linhas em 
# comparação aos outros anos

########################### Data Munging ########################### 

# Mudando os nomes das variáveis
colnames(df) <- c('Pais', 'Ano', 'NivelDeVida', 'PIB_Capita', 'SuporteSocial', 
                     'ExpectativaVida', 'SatisfacaoVida', 'Generosidade', 'Corrupcao', 
                     'EmoPositiva', 'EmoNegativa')

View(df)

# Transformando variáveis para o tipo Factor
df$Pais <- as.factor(df$Pais)
df$Ano <- as.factor(df$Ano)
str(df)

# Removendo valores NA
df <- na.omit(df)
missmap(df)

# Removendo valores com Ano igual a 2005
df <- df[df$Ano != 2005,]
table(df$Ano)

# Agrupando os dados por País e calculando as médias do restante das variáveis

nivel_vida_media <- df %>%
  group_by(Pais) %>%
  summarise(NivelDeVida = mean(NivelDeVida))

pib_per_media <- df %>%
  group_by(Pais) %>%
  summarise(PIB_Capita = mean(PIB_Capita))

sup_social_media <- df %>%
  group_by(Pais) %>%
  summarise(SuporteSocial = mean(SuporteSocial))

expec_media <- df %>%
  group_by(Pais) %>%
  summarise(ExpectativaVida = mean(ExpectativaVida))

satis_media <- df %>%
  group_by(Pais) %>%
  summarise(SatisfacaoVida = mean(SatisfacaoVida))

gene_media <- df %>%
  group_by(Pais) %>%
  summarise(Generosidade = mean(Generosidade))

corrup_media <- df %>%
  group_by(Pais) %>%
  summarise(Corrupcao = mean(Corrupcao))

pos_media <- df %>%
  group_by(Pais) %>%
  summarise(EmoPositiva = mean(EmoPositiva))

neg_media <- df %>%
  group_by(Pais) %>%
  summarise(EmoNegativa = mean(EmoNegativa))

# Fazendo a junção das médias para um único dataset

df_medias <- merge(pib_per_media, sup_social_media)
df_medias <- merge(df_medias, nivel_vida_media)
df_medias <- merge(df_medias, expec_media)
df_medias <- merge(df_medias, satis_media)
df_medias <- merge(df_medias, gene_media)
df_medias <- merge(df_medias, corrup_media)
df_medias <- merge(df_medias, pos_media)
df_medias <- merge(df_medias, neg_media)
View(df_medias)

# Removendo os objetos desnecessários
rm(pib_per_media)
rm(nivel_vida_media)
rm(expec_media)
rm(satis_media)
rm(sup_social_media)
rm(corrup_media)
rm(gene_media)
rm(pos_media)
rm(neg_media)

########################### Perguntas ########################### 

# 1- O aumento do PIB per capita de um país afeta positivamente a expectativa de
# vida dos cidadãos ao nascer? Qual a correlação entre essas duas variáveis?

cor(df_medias$PIB_Capita, df_medias$ExpectativaVida)

ggplot(df_medias, aes(x=PIB_Capita, y=ExpectativaVida, color=ExpectativaVida)) + 
  geom_point(size=2) +
  theme_bw() +
  scale_colour_gradientn(colours=terrain.colors(10))

# 2- Existe uma correlação entre a escala de vida e a conscientização do público 
# em geral sobre a corrupção nos negócios e no governo? Qual a correlação entre 
# essas duas variáveis?

cor(df_medias$NivelDeVida, df_medias$Corrupcao)

ggplot(df_medias, aes(x=NivelDeVida, y=Corrupcao, color=Corrupcao)) + 
  geom_point(size=2) +
  theme_bw() +
  scale_colour_gradientn(colours=rainbow(4))

# 3- O aumento na escala de vida tem algum efeito na média de felicidade entre o 
# público em geral? Qual a correlação entre essas duas variáveis?

cor(df_medias$NivelDeVida, df_medias$EmoPositiva)

ggplot(df_medias, aes(x=NivelDeVida, y=EmoPositiva, color=EmoPositiva)) + 
  geom_point(size=2) +
  theme_bw() +
  scale_colour_gradient(low = "yellow", high = "red")

# 4- O país com o menor índice de suporte social tem maior percepção de corrupção 
# em relação às empresas e ao governo no país?

# Identificando o país com menor índice de Suporte Social
df_medias[df_medias$SuporteSocial == min(df_medias$SuporteSocial),]
df_test <- df_medias[df_medias$Pais == 'Central African Republic',]
View(df_test)

# Fazendo o plot e estatística
df_P4 <- df[df$Pais == 'Central African Republic',]
View(df_P4)

cor(df_P4$SuporteSocial, df_P4$Corrupcao)

ggplot(df_P4, aes(x=SuporteSocial, y=Corrupcao)) +
  geom_point() + # Show dots
  geom_text(
    label=rownames(df_P4), 
    nudge_x = 0.003, nudge_y = 0.003, 
    check_overlap = T
)

# 5- Pessoas generosas são mais felizes?

cor(df_medias$Generosidade, df_medias$EmoPositiva)

ggplot(df_medias, aes(x=Generosidade, y=EmoPositiva, color=EmoPositiva)) + 
  geom_point(size=2) +
  theme_bw() +
  scale_colour_gradientn(colours = topo.colors(10))