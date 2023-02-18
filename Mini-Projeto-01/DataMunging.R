# Carregando dados

df_V1 <- read.csv('dataset.csv')
View(df_V1)

# Mudar os nomes das variáveis
colnames(df_V1) <- c('pais', 'ano', 'pont_da_fel', 'PIB_por_cap', 'sup_social', 
                     'expec_de_vida', 'satis_de_vida', 'doacao', 'corrupcao', 
                     'afeto_pos', 'afeto_neg')
View(df_V1)

# Seleção de variáveis
?subset
df_V2 <- subset(df_V1, select = -afeto_neg)
View(df_V2)

# Arrendondando valores da coluna sup_social
df_V2$sup_social <- round(df_V2$sup_social)
View(df_V2)

# Convertendo variáveis para o tipo factor
df_V2$pais <- as.factor(df_V2$pais)
df_V2$ano <- as.factor(df_V2$ano)
df_V2$sup_social <- as.factor(df_V2$sup_social)
str(df_V2)

# Removendo valores missing
df_V3 <- df_V2
df_V3 <- na.omit(df_V3)
View(df_V3)

# Gerando um arquivo csv
write.csv(df_V3, 'dataset_V1.csv')