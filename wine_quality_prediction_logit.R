# set di dati contiene 1.599 vini rossi con 11 variabili sulle proprietà chimiche del vino
# cercheremo di valutare le proprietà chimiche che influiscono sulla qualità del vino rosso

# import dataset

setwd("C:/Users/gabri/OneDrive/Desktop/Life/Extra/Progetti_Data_Science_R/data/raw_data")
dati <- read.csv(file = 'winequality-red.csv')
head(dati)
attach(dati)
# dati <- read.csv("~/Desktop/wine+quality/winequality-red.csv", sep=";")
# View(dati)
# attach(dati)
# head(dati)
summary(dati)
table(quality)
str(dati)

# Creazione di una variabile dicotomica: 3-4-5 qualità bassa / 6-7-8 qualità medio-alta
dati$quality.binaria=ifelse(dati$quality>6,1,0)
summary(dati$quality.binaria)

# Rimozione dal modello della variabile quality originaria
library(dplyr)
dati=select(dati, -quality)

boxplot(dati$volatile.acidity~dati$quality.binaria)
boxplot(dati$total.sulfur.dioxide~dati$quality.binaria)
boxplot(dati$sulphates~dati$quality.binaria)
boxplot(dati$alcohol~dati$quality.binaria)
boxplot(dati$residual.sugar~dati$quality.binaria)
boxplot(dati$chlorides~dati$quality.binaria)
boxplot(dati$fixed.acidity~dati$quality.binaria)
boxplot(dati$density~dati$quality.binaria)

# La variabile quality.binaria sarà 1 se la qualità del vino è uguale o superiore a 6 e 0 altrimenti

## CREAZIONE DEL MODELLO LOGIT

logit=glm(quality.binaria~.,data=dati,family=binomial)
summary(logit)

install.packages("sjPlot")
library(sjPlot)
plot_model(logit, type = "std", vline.color = "red")

## STIMA DEL MODELLO LOGIT CON LE VARIABILI CONSIDERATE PIU' SIGNIFICATIVE

logit1=glm(quality.binaria~volatile.acidity+total.sulfur.dioxide+sulphates+alcohol,data=dati,family=binomial)
summary(logit1)

logit2=glm(quality.binaria~volatile.acidity+total.sulfur.dioxide+sulphates+alcohol+residual.sugar+chlorides,data=dati,family=binomial)
summary(logit2)

logit3=glm(quality.binaria~volatile.acidity+total.sulfur.dioxide+sulphates+alcohol+residual.sugar+chlorides+fixed.acidity+density,data=dati,family=binomial)
summary(logit3)

# Ad esempio, il coefficiente per volatile.acidity è -2.913.
# Questo indica che un aumento di una unità in volatile.acidity è associato a una diminuzione
# di circa 2.913 log-odds della probabilità di appartenenza alla classe positiva (buona qualità).

# COMMENTI sulla lettura dell'output:
## Coefficienti: I coefficienti stimati forniscono le direzioni e le dimensioni delle relazioni tra le variabili indipendenti e la variabile dipendente.
# Coefficienti positivi indicano un aumento dell'odds della classe positiva, mentre coefficienti negativi indicano una diminuzione.
## Significatività statistica: I valori p indicano la significatività statistica dei coefficienti. I valori bassi (ad esempio, < 0.05) suggeriscono che le variabili sono statisticamente significative nel modello.
## Direzione dei coefficienti: per esempio, volatile.acidity ha un coefficiente negativo,
# indicando che un aumento dell'acidità volatile è associato a una diminuzione dell'odds di avere una qualità del vino positiva.
## Devianza e AIC: La devianza del modello è una misura di quanto bene il modello si adatta ai dati.
# Una devianza inferiore indica un migliore adattamento. L'AIC (Akaike Information Criterion) tiene conto della complessità del modello, con valori inferiori che indicano una migliore adattabilità e parsimonia. 

## CORRELAZIONE TRA VARIABILI 

## Con la matrice di correlazione
dati1<-dati[,1:11]
z<-round(cor(dati1),digits=2)
z

install.packages("corrplot")
library(corrplot)
corrplot(z)
corrplot(z,method="number")
cor(dati)

## Con il VIF

install.packages("usdm")
library(usdm)
vif(dati1)

# Il VIF suggerisce la presenza di multicollinearità tra alcune delle variabili indipendenti nel modello.
# In particolare, 'fixed.acidity' e 'density' mostrano VIF elevati, indicando una potenziale correlazione tra queste variabili.
# La multicollinearità potrebbe influenzare la precisione delle stime dei coefficienti.

# Provo a togliere variabili correlate

logit4=glm(quality.binaria~volatile.acidity+total.sulfur.dioxide+sulphates+alcohol+residual.sugar+chlorides+density,data=dati,family=binomial)
summary(logit4)

logit5=glm(quality.binaria~volatile.acidity+total.sulfur.dioxide+sulphates+alcohol+residual.sugar+chlorides+fixed.acidity,data=dati,family=binomial)
summary(logit5)

# una volta scelto il modello, vado a calcolare le probabilità predette

summary(dati)

nuovi.dati=data.frame(volatile.acidity = c(0.53),total.sulfur.dioxide = c(46.47),sulphates = c(0.66),alcohol = c(10.42),residual.sugar = c(2.54),chlorides = c(0.087),fixed.acidity = c(8.32),
                      density = c(0.99))
prob_predette=predict(logit3, newdata = nuovi.dati, type = "response")
print(prob_predette)
#probabilità stimata che l'osservazione appartenga alla classe positiva (buona qualità) in base al modello logit.

# Calcolo delle probabilità predette con variazione delle variabili (prendo max/min a seconda che la variabile abbia coeff negativo o positivo)
# voglio cercare un valore delle probabilità di trovare vino buono più alta

nuovi.dati1=data.frame(volatile.acidity = c(0.12),total.sulfur.dioxide = c(6),sulphates = c(2),alcohol = c(14.90),residual.sugar = c(15.50),chlorides = c(0.012),fixed.acidity = c(15.90),
                      density = c(0.99))
prob_predette1=predict(logit3, newdata = nuovi.dati1, type = "response")
print(prob_predette1)

nuovi.dati2=data.frame(volatile.acidity = c(1.58),total.sulfur.dioxide = c(289),sulphates = c(0.33),alcohol = c(8.40),residual.sugar = c(0.9),chlorides = c(0.611),fixed.acidity = c(4.6),
                       density = c(1.0037))
prob_predette2=predict(logit3, newdata = nuovi.dati2, type = "response")
print(prob_predette2)


## ANALISI DEI RESIDUI

# ottenere i residui
residui=residuals(logit3, type = "deviance")

# grafico dei residui contro le variabili indipendenti. 
install.packages("ggplot2")
library(ggplot2)
ggplot(data = dati, aes_string(x = "fixed.acidity", y = "residui")) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  ggtitle("Residui vs fixed.acidity")

ggplot(data = dati, aes_string(x = "volatile.acidity", y = "residui")) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  ggtitle("Residui vs volatile.acidity")

ggplot(data = dati, aes_string(x = "total.sulfur.dioxide", y = "residui")) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  ggtitle("Residui vs total.sulfur.dioxide")

# grafico QQ Plot dei residui
# utilizzato per verificare se i residui seguono una distribuzione normale
# Se i residui sono normalmente distribuiti, i punti sul Q-Q plot dovrebbero allinearsi approssimativamente con la linea di 45 gradi.
qqnorm(residui)
qqline(residui)

ggplot(data = dati, aes_string(x = predict(logit3, type = "response"), y = residui)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  ggtitle("Residui deviance vs. Valori stimati")


## grafico dei residui per ogni variabile
residui=residuals(logit3, type = "response")
dati$residui=residui
dati_long <- tidyr::pivot_longer(dati, cols = -c(residui), names_to = "variabile", values_to = "valore")
grafico_residui <- ggplot(data = dati_long, aes(x = variabile, y = valore)) +
  geom_point() +
  ggtitle("Grafico dei Residui per ogni Variabile") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
grafico_residui <- grafico_residui + ylab("Valore") + xlab("Variabile")
print(grafico_residui)


# Aggiungo il binned plot che mi mostra gli intervalli di confidenza. 
# Vado così a vedere anche graficamente la bontà del modello

install.packages("arm")
library(arm)
binnedplot(fitted(logit3), 
           residuals(logit3, type = "response"), 
           nclass = NULL, 
           xlab = "Expected Values", 
           ylab = "Average residual", 
           main = "Binned residual plot", 
           cex.pts = 0.8,
           col.pts = 1, 
           col.int = "gray")


# Questo plot è simile al qqplot, non usabile per la casistica della logistic regression;
# in questo caso guardo la standard deviation dei residuals (Pearson) che, se il modello è ben fittato, dovrebbe avere andamento normale.

plot(logit3, which = 2)

## grafico dei residui per ogni variabile
residui=residuals(logit3, type = "response")
dati$residui=residui
dati_long <- tidyr::pivot_longer(dati, cols = -c(residui), names_to = "variabile", values_to = "valore")
grafico_residui <- ggplot(data = dati_long, aes(x = variabile, y = valore)) +
  geom_point() +
  ggtitle("Grafico dei Residui per ogni Variabile") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
grafico_residui <- grafico_residui + ylab("Valore") + xlab("Variabile")
print(grafico_residui)