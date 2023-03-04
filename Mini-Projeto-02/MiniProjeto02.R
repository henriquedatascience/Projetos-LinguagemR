########################### Mini Projeto 02 ########################### 

# 1º Objetivo: Realizar análise exploratória em linguagem SQL
# 2º Objetivo: Realizar análise de regressão em linguagem R

# Instalando pacotes
install.packages('sqldf')
install.packages('ggplot2')
install.packages('hrbrthemes')
install.packages('GGally')
install.packages('dplyr')

# Carregando pacotes
library(sqldf)
library(ggplot2)
library(hrbrthemes)
library(GGally)
library(dplyr)

# Carregando dataset
df <- read.csv('dataset.csv')
View(df)

# Visualizando dataset no geral
str(df)
dim(df)

# Mudando os nomes das variáveis
colnames(df) <- c('Idade', 'Genero', 'TempoDaInternacao', 'Raça', 
                  'CustoDaInternacao', 'GrupoDiagnostico')

View(df)

# Verificando se á valores missing
colSums(is.na(df))

# Removendo valores NA
df <- na.omit(df)
colSums(is.na(df))

# Modificando a coluna gênero
df$Genero[df$Genero == 0] <- 'M'
df$Genero[df$Genero == 1] <- 'F'
View(df)

########################### Linguagem SQL ########################### 

# Quantas raças estão presentes no dataset?
sqldf('SELECT Raça, COUNT(Raça) as NumeroDeRaças
      FROM df
      GROUP BY Raça')

# Qual a idade média dos pacientes?
sqldf('SELECT AVG(Idade) as MediaDaIdade
      FROM df')

# Qual a moda da idade dos pacientes?
sqldf('SELECT COUNT(Idade) as NumDeRegistros, Idade
      FROM df
      GROUP BY Idade
      ORDER BY NumDeRegistros DESC
      LIMIT 1')

# Qual a variância da coluna Idade?
sqldf('SELECT SUM ((Idade - (SELECT AVG(Idade) FROM df)) * 
      (Idade - (SELECT AVG(Idade) FROM df))) / (COUNT(Idade) - 1) AS Variancia 
      FROM df')

# Qual o gasto total com internações hospitalares por idade?
sqldf('SELECT Idade, SUM(CustoDaInternacao) as CustoTotal
      FROM df
      GROUP BY Idade')

# Qual idade gera o maior gasto total com internações hospitalares?
sqldf('SELECT Idade, SUM(CustoDaInternacao) as CustoTotal
      FROM df
      GROUP BY Idade
      ORDER BY CustoTotal DESC
      LIMIT 1')

# Qual o gasto total com internações hospitalares por gênero?
sqldf('SELECT SUM(CustoDaInternacao) as CustoTotal, Genero
      FROM df
      GROUP BY Genero
      ORDER BY CustoTotal DESC')

# Qual a média de gasto com internações hospitalares por raça do paciente?
sqldf('SELECT Raça, AVG(CustoDaInternacao) as CustoTotal
      FROM df
      GROUP BY Raça')

# Para  pacientes  acima  de  10  anos,  qual  a  média  de  gasto  total  com  
# internações hospitalares?
sqldf('SELECT Idade, AVG(CustoDaInternacao) as CustoTotal
      FROM df
      WHERE Idade > 10
      GROUP BY Idade')

# Considerando o item anterior, qual idade tem média de gastos superior a 3000?
sqldf('SELECT Idade, AVG(CustoDaInternacao) as CustoTotal
      FROM df
      WHERE Idade > 10
      GROUP BY Idade
      HAVING AVG(CustoDaInternacao) > 3000')

########################### Linguagem R ########################### 

# Qual a distribuição da idade dos pacientes que frequentam o hospital?
?element_text
?ggtitle
ggplot(df, aes(x = Idade)) +
  geom_histogram(binwidth=3, fill="#69b3a2", color='#e9ecef', alpha=0.9) +
  ggtitle('Distribuição de Idade dos Pacientes') +
  xlab('Idade') +
  ylab('Quantidade de Pacientes') +
  theme_light() +
  theme(plot.title = element_text(size=20, hjust = 0.5)
  )

# Qual faixa etária tem o maior gasto total no hospital?
gasto_por_idade <- aggregate(CustoDaInternacao ~ Idade,
                             FUN = sum,
                             data = df)

View(gasto_por_idade)

barplot(height=gasto_por_idade$CustoDaInternacao, names=gasto_por_idade$Idade, 
        col="#69b3a2",
        xlab="Idade", 
        ylab="Total Gasto", 
        main="Idade Dos Pacientes Com Maior Gasto", 
)

# Resposta: Pacientes com a faixa etária de 0 a 1 anos de idade tem o maior gasto

# Qual grupo baseado em diagnóstico (Aprdrg) tem o maior gasto total no hospital?
gasto_por_diag <- aggregate(CustoDaInternacao ~ GrupoDiagnostico,
                              FUN = sum,
                              data = df)

gasto_por_diag <- gasto_por_diag[order(gasto_por_diag$CustoDaInternacao, decreasing = TRUE), ]
gasto_por_diag <- slice(gasto_por_diag, 1:10)

View(gasto_por_diag)

# Resposta
gasto_por_diag[which.max(gasto_por_diag$CustoDaInternacao), ]

# O grupo 640 tem o maior gasto total, com o valor de R$ 436.822

# Gráfico
gasto_por_diag <- gasto_por_diag[order(gasto_por_diag$CustoDaInternacao), ]
barplot(height=gasto_por_diag$CustoDaInternacao, names=gasto_por_diag$GrupoDiagnostico, 
        col="#69b3a2",
        horiz = T, las = 1,
        xlab="Total de Gastos", 
        ylab="Grupo Diagnóstico", 
        main="Idade Dos Pacientes Com Maior Gasto", 
)

# A raça do paciente tem relação com o total gasto em internações no hospital?

# Teste ANOVA

# H0: Não há efeito de Raça em CustoDaInternacao
# H1: Há efeito de Raça em CustoDaInternacao

modelo_anova1 <- aov(CustoDaInternacao ~ Raça, data = df)
summary(modelo_anova1)

# Resposta: O valor-p é maior que 0.05, ou seja, falhamos em rejeitar H0.
# Isso significa que a variável Raça não influencia em CustoDaInternacao

# A combinação de idade e gênero dos pacientes influencia no gasto total em 
# internações no hospital?

# Teste ANOVA

# H0: Não há efeito de Idade e Genero em CustoDaInternacao)
# H1: As duas variável tem efeito em CustoDaInternacap

modelo_anova2 <- aov(CustoDaInternacao ~ Idade + Genero, data = df)
summary(modelo_anova2)

# Resposta: O valor-p é menor que 0.05, ou seja, falhamos em rejeitar H0.
# Isso significa que as duas variaveis (Idade, Genero) influenciam na variável CustodeInternacao

# Como o tempo de permanência é o fator crucial para pacientes internados, 
# desejamos descobrir se o tempo de permanência pode ser previsto a partir de 
# idade, gênero e raça

# Modelo de regressão linear

# H0: Não há relação entre a variáveis dependente e independentes
# H1: Há relação entre as variáveis dependente e independentes

modelo_lm1 <- lm(TempoDaInternacao ~ Idade + Genero + Raça, data = df)
summary(modelo_lm1)

# Resposta: O valor-p é maior que 0.05, ou seja, falhamos em rejeitar H0.
# Provavelmente não há relação estatística entre as variáveis dependente e independente

# Quais variável têm maior impacto nos custos de internação hospitalar?

# Modelo de Regressão Linear

# Variável dependente: CustoDaInternacao
# Variável independente: Todas as outras.

modelo_v1 <- lm(CustoDaInternacao ~ ., data = df)
summary(modelo_v1)

# Observa-se que neste primeiro modelo as variáveis Raça e Genero não possuem
# significancia estatística neste modelo, então irei remove-las da próxima versão.

modelo_v2 <- lm(CustoDaInternacao ~ 
                  Idade + 
                  TempoDaInternacao + 
                  GrupoDiagnostico, 
                  data = df)

summary(modelo_v2)

# Resposta: Nos modelos acima, observamos que a idade, o tempo de internacao e o
# Grupo diagnóstico são o que mais influenciam nos gastos das internações hospitalares.