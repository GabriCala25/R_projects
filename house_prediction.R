# import libraries
library(ggplot2)

# import dataset

actual_path <- getwd()

data_path = paste(actual_path, '/data/raw_data')
setwd("C:/Users/gabri/OneDrive/Desktop/Life/Extra/Progetti_Data_Science_R/data/raw_data")
data <- read.csv(file = 'house_prediction_data.csv')
head(data)


colnames(data) # columns names

# unique cities
unique(data$city)

maindf <- data[,c("price","bedrooms","sqft_living","floors",
                  "sqft_lot", "condition", "view", "yr_built")]
head(maindf)

#checking null values

sum(is.na(maindf))

# house age

maindf$oldbuilt <- as.integer(format(Sys.Date(), "%Y")) - maindf$yr_built

drops <- c("yr_built")
maindf = maindf[ , !(names(maindf) %in% drops)]

# correlation matrix

cor(maindf)

# plot of correlation matrix
install.packages("ggcorrplot")
library(ggcorrplot)
corr <- round(cor(maindf), 1)

# Plot
ggcorrplot(corr,
           type = "lower",
           lab = TRUE, 
           lab_size = 5,  
           colors = c("tomato2", "white", "springgreen3"),
           title="Correlogram of Housing Dataset", 
           ggtheme=theme_bw)


# plot scatterplot matrix
pairs(~bedrooms + sqft_living + floors + condition, data = maindf,
      main = "Scatterplot Matrix")

# boxplot for checking outliers
par(mfrow=c(2, 3))  # divide graph area in 2 columns
boxplot(maindf$bedrooms, main="Bedrooms")
boxplot(maindf$sqft_living, main="sqft_living")
boxplot(maindf$floors, main="floors")
boxplot(maindf$condition, main="condition")
boxplot(maindf$view, main="view")
boxplot(maindf$oldbuilt, main="oldbuilt")

# plot density plot to check normality

install.packages("e1071")
library(e1071)

par(mfrow=c(2, 3)) 

plot(density(maindf$bedrooms), main="Density Plot: Bedrooms", ylab="Frequency",
     sub=paste("Skewness:", round(e1071::skewness(maindf$bedrooms), 2)))  
polygon(density(maindf$bedrooms), col="green")

plot(density(maindf$sqft_living), main="Density Plot: sqft_living", ylab="Frequency",
     sub=paste("Skewness:", round(e1071::skewness(maindf$sqft_living), 2)))  
polygon(density(maindf$sqft_living), col="orange")

plot(density(maindf$sqft_lot), main="Density Plot: sqft_lot", ylab="Frequency",
     sub=paste("Skewness:", round(e1071::skewness(maindf$sqft_lot), 2)))  
polygon(density(maindf$sqft_lot), col="green")

plot(density(maindf$condition), main="Density Plot: condition", ylab="Frequency",
     sub=paste("Skewness:", round(e1071::skewness(maindf$condition), 2)))  
polygon(density(maindf$condition), col="orange")

plot(density(maindf$floors), main="Density Plot: floors", ylab="Frequency",
     sub=paste("Skewness:", round(e1071::skewness(maindf$floors), 2)))  
polygon(density(maindf$floors), col="green")

plot(density(maindf$oldbuilt), main="Density Plot: oldbuilt", ylab="Frequency",
     sub=paste("Skewness:", round(e1071::skewness(maindf$oldbuilt), 2)))  
polygon(density(maindf$oldbuilt), col="orange")

# simple linear regression
linearmodel = lm(price~bedrooms,
                 data = maindf)
summary(linearmodel)

#plot univariate linear regression between price and one variable
ggplot(maindf,aes(y=price,x=sqft_living)) +
  geom_point() + 
  xlim(0, 9000) +
  ylim(0, 5000000) +
  geom_smooth(formula = y ~ x,method="lm")


# multiple linear regression

linearmodel = lm(price~bedrooms + sqft_living + floors + sqft_lot + condition + view + oldbuilt,
                 data = maindf)
summary(linearmodel)


