########################### Mini Projeto 02 ########################### 

# 1º Objetivo: Realizar análise exploratória em linguagem SQL
# 2º Objetivo: Realizar análse de regressão em linguagem R

# Instalando pacotes
install.packages('sqldf')

# Carregando pacotes
library(sqldf)

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