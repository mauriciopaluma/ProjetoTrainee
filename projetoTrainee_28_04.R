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
hist(y, main = "Classificação de Qualidade")
hist(x1, main = "Temperatura (°C)")
hist(x2, main = "Pressão (kPa)")
hist(x3, main = "Temperatura x Pressão")
hist(x4, main =  "Métrica de fusão de materiais")
hist(x5, main = "Métrica de transformação de material")


###Classificação de qualidade ----------
#Medidas de tedencia central
mediaQualidade = mean(y); mediaQualidade
medianaQualidade = median(y); medianaQualidade
frequenciasQualidade = table(y)
modaQualidade = as.numeric(names(frequenciasQualidade)[frequenciasQualidade==max(frequenciasQualidade)]);modaQualidade

#medidas de posição
q1Qualidade = quantile(y, probs = 0.25);q1Qualidade
q2Qualidade = quantile(y, probs = 0.50);q2Qualidade
q3Qualidade = quantile(y, probs = 0.75);q3Qualidade

#medidas de dispersão
dpQualidade = sd(y); dpQualidade
varQualidade = var(y); varQualidade

#assimetria
asQualidade = (3*(mediaQualidade - medianaQualidade))/dpQualidade; asQualidade
#histograma (classificação de qualidade)
hist(y, main = 'Histograma da Classificação de Qualidade')

###Temperatura ----------
#Medidas de tedencia central
mediaTemperatura = mean(x1); mediaTemperatura
medianaTemperatura = median(x1); medianaTemperatura
frequenciasTemperatura = table(x1)
modaTemperatura = as.numeric(names(frequenciasTemperatura)[frequenciasTemperatura==max(frequenciasTemperatura)]);modaTemperatura # Temperatura é uma variável amodal

#medidas de posição
q1Temperatura = quantile(x1, probs = 0.25);q1Temperatura
q2Temperatura = quantile(x1, probs = 0.50);q2Temperatura
q3Temperatura = quantile(x1, probs = 0.75);q3Temperatura

#medidas de dispersão
dpTemperatura = sd(x1); dpTemperatura
varTemperatura = var(x1); varTemperatura

#assimetria
asTemperatura = (3*(mediaTemperatura - medianaTemperatura))/dpTemperatura; asTemperatura
#histograma (temperatura)
hist(x1, main = 'Histograma da Temperatura')

###Pressão ----------
#Medidas de tedencia central
mediaPress = mean(x2); mediaPress
medianaPress = median(x2); medianaPress
frequenciasPress = table(x2)
modaPress = as.numeric(names(frequenciasPress)[frequenciasPress==max(frequenciasPress)]);modaPress #Pressão é uma variável amodal

#medidas de posição
q1Press = quantile(x2, probs = 0.25);q1Press
q2Press = quantile(x2, probs = 0.50);q2Press
q3Press = quantile(x2, probs = 0.75);q3Press

#medidas de dispersão
dpPress = sd(x2); dpPress
varPress = var(x2); varPress

#assimetria
asPress = (3*(mediaPress
              - medianaPress))/dpPress; asPress
#histograma (temperatura)
hist(x2, main = 'Histograma da Pressão')

###Temperatura x Pressão ----------
#Medidas de tedencia central
mediaTempxPress = mean(x3); mediaTempxPress
medianaTempxPress = median(x3); medianaTempxPress
frequenciasTempxPress = table(x3) 
modaTempxPress = as.numeric(names(frequenciasTempxPress)[frequenciasTempxPress==max(frequenciasTempxPress)]);modaTempxPress # Temperatura x Pressão é uma variável amodal

#medidas de posição
q1TempxPress = quantile(x3, probs = 0.25);q1TempxPress
q2TempxPress = quantile(x3, probs = 0.50);q2TempxPress
q3TempxPress = quantile(x3, probs = 0.75);q3TempxPress

#medidas de dispersão
dpTempxPress = sd(x3); dpTempxPress
varTempxPress = var(x3); varTempxPress
#assimetria
asTempxPress = (3*(mediaTempxPress
                   - medianaTempxPress))/dpTempxPress; asTempxPress
#histograma (temperatura)
hist(x3, main = 'Histograma Temperatura x Pressão')


###Métrica de fusão dos materiais ----------
#Medidas de tedencia central
mediaMFM = mean(x4); mediaMFM
medianaMFM = median(x4); medianaMFM
frequenciasMFM = table(x4)
modaMFM = as.numeric(names(frequenciasMFM)[frequenciasMFM==max(frequenciasMFM)]);modaMFM # Métrica de fusão dos materiais é uma variável amodal

#medidas de posição
q1MFM = quantile(x4, probs = 0.25);q1MFM
q2MFM = quantile(x4, probs = 0.50);q2MFM
q3MFM = quantile(x4, probs = 0.75);q3MFM

#medidas de dispersão
dpMFM = sd(x4); dpMFM
varMFM = var(x4); varMFM
#assimetria
asMFM = (3*(mediaMFM
            - medianaMFM))/dpMFM; asMFM
#histograma (temperatura)
hist(x4, main = 'Histograma Métrica de fusão de materiais')

###Métrica de transformação de material ----------
#Medidas de tedencia central
mediaMTM = mean(x5); mediaMTM
medianaMTM = median(x5); medianaMTM
frequenciasMTM = table(x5)
modaMTM = as.numeric(names(frequenciasMTM)[frequenciasMTM==max(frequenciasMTM)]);modaMTM # Métrica de transformação de material é uma variável amodal

#medidas de posição
q1MTM = quantile(x5, probs = 0.25);q1MTM
q2MTM = quantile(x5, probs = 0.50);q2MTM
q3MTM = quantile(x5, probs = 0.75);q3MTM

#medidas de dispersão
dpMTM = sd(x5); dpMTM
varMTM = var(x5); varMTM

#assimetria
asMTM = (3*(mediaMTM
            - medianaMTM))/dpMTM; asMTM
#histograma (temperatura)
hist(x5, main = 'Histograma Métrica de transformação de materiais')


### teste ks para temperatura (x1)
a <- min(x1)
b <- max(x1)

ks.test(x1, "punif", a, b)

### teste para temperatura x pressão
#ks.test(x3, "pgama")
# ir atras da gama

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
#print(correlacoes)

cor_classificacao <- correlacoes["Classificacao de qualidade", ]

cor_classificacao_ordenada <- sort(abs(cor_classificacao), decreasing = TRUE)

print(cor_classificacao_ordenada)
