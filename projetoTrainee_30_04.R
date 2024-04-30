library(dplyr)

dados <- read.csv("projetoFinalDados.csv", header = T)
print(dados)

names(dados)[names(dados) == "Temperature...C."] <- "Temperatura ºC"
names(dados)[names(dados) == "Pressure..kPa."] <- "Pressao kPa"
names(dados)[names(dados) == "Temperature.x.Pressure"] <- "Temperatura x Pressao"
names(dados)[names(dados) == "Material.Fusion.Metric"] <- "Metrica de fusao de materiais"
names(dados)[names(dados) == "Material.Transformation.Metric"] <- "Metrica de transformacao de material"
names(dados)[names(dados) == "Quality.Rating"] <- "Classificacao de qualidade"


y = dados$`Classificacao de qualidade`

x1 = dados$`Temperatura ºC`
x2 = dados$`Pressao kPa`
x3 = dados$`Temperatura x Pressao`
x4 = dados$`Metrica de fusao de materiais`
x5 = dados$`Metrica de transformacao de material`

#Gráficos
par(mfrow = c(3,2))
hist(y, main = 'Histograma Classificação de Qualidade', col = 'azure3',
     xlab = 'Classificação de Qualidade', ylab = 'Frequência')
hist(x1, main = 'Histograma Temperatura (ºC)', col = 'azure3',
     xlab = 'Temperatura (ºC)', ylab = 'Frequência')
hist(x2, main = 'Histograma Pressão (kPa)', col = 'azure3',
     xlab = 'Pressão (kPa)', ylab = 'Frequência')
hist(x3, main = 'Histograma Temperatura x Pressão', col = 'azure3',
     xlab = 'Temperatura x Pressão', ylab = 'Frequência')
hist(x4, main = 'Histograma Métrica de Fusão dos Materiais', col = 'azure3',
     xlab = 'Métrica de Fusão dos Materiais', ylab = 'Frequência')
hist(x5, main = 'Histograma Métrica de Transformação de Material', col = 'azure3',
     xlab = 'Métrica de transformação de material', ylab = 'Frequência')


# 1º Passo -  Análise Descritiva dos dados

###Classificação de qualidade ----------

#Medidas descritivas Classificação de qualidade
medidas_descritivas_qualidade = dados %>%
  summarise(
    media = mean(y),
    q1 = quantile(y, probs = 0.25),
    mediana = median(y),
    q3 = quantile(y, probs = 0.75),
    desv.pd = sd(y),
    minimo = min(y),
    maximo = max(y),
    curt = kurtosis(y),
    assi = skewness(y)
  )


#Histograma e Boxplot (classificação de qualidade)
par(mfrow = c(1,2))
hist(y, main = 'Histograma Classificação de Qualidade', col = 'azure3',
     xlab = 'Classificação de Qualidade', ylab = 'Frequência')

boxplot(y, main = 'Classificação de Qualidade', col = 'azure3', horizontal = T)


###Temperatura ----------

#Medidas descritivas Temperatura
medidas_descritivas_temperatura = dados %>%
  summarise(
    media = mean(x1),
    q1 = quantile(x1, probs = 0.25),
    mediana = median(x1),
    q3 = quantile(x1, probs = 0.75),
    desv.pd = sd(x1),
    minimo = min(x1),
    maximo = max(x1),
    curt = kurtosis(x1),
    assi = skewness(x1)
  )

#Histograma e Boxplot(temperatura)
par(mfrow = c(1,2))
hist(x1, main = 'Histograma Temperatura (ºC)', col = 'azure3',
     xlab = 'Temperatura (ºC)', ylab = 'Frequência')

boxplot(x1, main = 'Temperatura (ºC)', col = 'azure3', horizontal = T)


###Pressão ----------

#Medidas descritivas Pressão
medidas_descritivas_pressao = dados %>%
  summarise(
    media = mean(x2),
    q1 = quantile(x2, probs = 0.25),
    mediana = median(x2),
    q3 = quantile(x2, probs = 0.75),
    desv.pd = sd(x2),
    minimo = min(x2),
    maximo = max(x2),
    curt = kurtosis(x2),
    assi = skewness(x2)
  )

#Histograma e Boxplot(Pressão)
#par(mfrow = c(1,2))
hist(x2, main = 'Histograma Pressão (kPa)', col = 'azure3',
     xlab = 'Pressão (kPa)', ylab = 'Frequência')

boxplot(x2, main = 'Boxplot Pressão (Kpa)', col = 'azure3', horizontal = T)


###Temperatura x Pressão ----------

#Medidas descritivas Temperatura x Pressão
medidas_descritivas_temperaturaxpressao = dados %>%
  summarise(
    media = mean(x3),
    q1 = quantile(x3, probs = 0.25),
    mediana = median(x3),
    q3 = quantile(x3, probs = 0.75),
    desv.pd = sd(x3),
    minimo = min(x3),
    maximo = max(x3),
    curt = kurtosis(x3),
    assi = skewness(x3)
  )

#Histograma e Boxplot(temperaturaxPressão)
par(mfrow = c(1,2))
hist(x3, main = 'Histograma Temperatura x Pressão', col = 'azure3',
     xlab = 'Temperatura x Pressão', ylab = 'Frequência')

boxplot(x3, main = 'Boxplot Temperatura x Pressão', col = 'azure3', horizontal = T)

###Métrica de fusão dos materiais ----------

#Tabela Medidas discritivas
medidas_descritivas_MFM = dados %>%
  summarise(
    media = mean(x4),
    q1 = quantile(x4, probs = 0.25),
    mediana = median(x4),
    q3 = quantile(x4, probs = 0.75),
    desv.pd = sd(x4),
    minimo = min(x4),
    maximo = max(x4),
    curt = kurtosis(x4),
    assi = skewness(x4)
  )

#Histograma e Boxplot(Métrica de fusão dos materiais)
par(mfrow = c(1,2))
hist(x4, main = 'Histograma Métrica de Fusão dos Materiais', col = 'azure3',
     xlab = 'Métrica de Fusão dos Materiais', ylab = 'Frequência')

boxplot(x4, main = 'Boxplot Métrica de Fusão dos Materiais', col = 'azure3', horizontal = T)

###Métrica de transformação de material ----------

medidas_descritivas_MTM = dados %>%
  summarise(
    media = mean(x5),
    q1 = quantile(x5, probs = 0.25),
    mediana = median(x5),
    q3 = quantile(x5, probs = 0.75),
    desv.pd = sd(x5),
    minimo = min(x5),
    maximo = max(x5),
    curt = kurtosis(x5),
    assi = skewness(x5)
  )

#Histograma e Boxplot(Métrica de transformação de material)
par(mfrow = c(1,2))
hist(x5, main = 'Histograma Métrica de Transformação de Material', col = 'azure3',
     xlab = 'Métrica de transformação de material', ylab = 'Frequência')

boxplot(x5, main = 'Boxplot Métrica de Transformação de Material', col = 'azure3', horizontal = T)

#? --------------------------------------------------------------------------------------------------

### teste ks para temperatura (x1)
a <- min(x1)
b <- max(x1)

ks.test(x1, "punif", a, b)

### teste para temperatura x pressão
#ks.test(x3, "pgama")
# ir atras da gama

# 2º - Passo Teste de correlação e Matriz de Correlação -------------------------------------------------------------

Classificacao_de_qualidade = y
Temperatura = x1
Pressao = x2
TemperaturaxPressao = x3
Metrica_de_fusao_de_materiais = x4
Metrica_de_transformacao_de_materiais = x5


#Correlação de pearson
cor.test(Classificacao_de_qualidade,Temperatura, method = "pearson") # -0.4612785 - Correlação significativa e negativa

cor.test(Classificacao_de_qualidade, Pressao, method = "pearson") # 0.01312935 - Não há correlação significativa, p-valor alto e 0 incluido no intervalo de confiança

cor.test(Classificacao_de_qualidade, TemperaturaxPressao, method = "pearson") # -0.2584739  - Correlação significativa e negativa

cor.test(Classificacao_de_qualidade, Metrica_de_fusao_de_materiais, method = "pearson") # -0.5119715 - Correlação significativa e negativa

cor.test(Classificacao_de_qualidade, Metrica_de_transformacao_de_materiais, method = "pearson") # -0.5757561 - Correlação significativa e negativa


#Matriz de correlação
correlacoes <- cor(dados)

cor_classificacao <- correlacoes["Classificacao de qualidade", ]

cor_classificacao_ordenada <- sort(abs(cor_classificacao), decreasing = TRUE)

print(cor_classificacao_ordenada)
