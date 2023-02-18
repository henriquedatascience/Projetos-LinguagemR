# Carregando dados
df <- read.csv('dataset.csv')

# Visualizando os dados
View(df)

# Dimensões
dim(df)

# Tipos dos dados
str(df)

# Sumário
summary(df)

# Visualizando os valores missing
library(Amelia)
?missmap
missmap(df)

# Criando boxplot para cada variável
boxplot(df$`Life Ladder`)
boxplot(df$`Log GDP per capita`)
boxplot(df$`Healthy life expectancy at birth`)
boxplot(df$`Freedom to make life choices`)
boxplot(df$Generosity)
boxplot(df$`Perceptions of corruption`)

# Problemas Encontrados

# Selecionar as variáveis que serão usadas
# Modificar o nome das variáveis se necessário
# Mudar a variavel Country e Year para factor
# Tratar valores missing
# Aplicar normalização nas variáveis numéricas