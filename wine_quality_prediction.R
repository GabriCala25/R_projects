# import dataset

setwd("C:/Users/gabri/OneDrive/Desktop/Life/Extra/Progetti_Data_Science_R/data/raw_data")
data <- read.csv(file = 'winequality-red.csv')
head(data)

summary(data)

View(data)

# example plot
plot(quality  ~ free.sulfur.dioxide, data = data)

str(data)

#correlation between variables

z<-round(cor(data),digits=2)
z

#Correlation Matrix
install.packages("corrplot")
library(corrplot)
corrplot(z) # eventually set: method = 'number'

cor(data)

# check for missing values
sum(is.na(data))

# removing duplicates

data2 <- unique(data)

#Transforming data using 'BoxCox' transformation to remove skewness
install.packages("caret")
library(ggplot2)
library(lattice)
library(caret)
data_process<-preProcess(data2[,1:11],method ="BoxCox")
new_data<-data.frame(trans=predict(data_process,data2))
quality <- new_data[,12]

#Checking Skewness of transformed dataframe
install.packages("moments")
library(moments)
skewness(new_data$trans.fixed.acidity)
skewness(new_data$trans.volatile.acidity)
skewness(new_data$trans.citric.acid)
skewness(new_data$trans.residual.sugar)
skewness(new_data$trans.chlorides)
skewness(new_data$trans.free.sulfur.dioxide)
skewness(new_data$trans.total.sulfur.dioxide)
skewness(new_data$trans.density)
skewness(new_data$trans.pH)
skewness(new_data$trans.sulphates)
skewness(new_data$trans.alcohol)
skewness(new_data$trans.quality)


#Correlation plot of transformed data

corrplot(cor(new_data),type="lower")


#splitting dataset
install.packages("caTools")
library("caTools")

set.seed(101)
training<-sample.split(new_data,SplitRatio=0.8)
train<-subset(new_data,training==T)
test<-subset(new_data,training==F)

#Running Linear Regression

linear<-lm(trans.quality~.,train)
summary(linear)

# prediction

x <- test[,1:11]
y <- test[,12]

predictions <- predict(linear, newdata = x)


# residual mean square error
rmse <- RMSE(predictions, y)

# try linear regression model only on the 6 most significant predictors:
model_reduced <- lm(trans.quality~ trans.volatile.acidity + trans.chlorides + trans.free.sulfur.dioxide + trans.total.sulfur.dioxide + trans.sulphates + trans.alcohol, train)
summary(model_reduced)

b <- coef(model_reduced)           ###coefficienti di regressione
residuals(model_reduced) ###residui del modello
fitted(model_reduced)    ###valori della variabile risposta stimati dal modello
deviance(model_reduced)  ###devianza dei residui
formula(model_reduced)   ###formula del modello
plot (model_reduced$fitted.values, model_reduced$residuals)
abline(lm(model_reduced$residuals~model_reduced$fitted.values))


# I try to add an interaction term between free sulfur dioxide and total sulfur dioxide

model_reduced_i <- lm(trans.quality~ trans.volatile.acidity + trans.chlorides + trans.free.sulfur.dioxide + trans.total.sulfur.dioxide + trans.free.sulfur.dioxide:trans.total.sulfur.dioxide + trans.sulphates + trans.alcohol, train)
summary(model_reduced_i)



