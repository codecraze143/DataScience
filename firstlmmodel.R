#Clearing the environment
rm(list=ls(all=TRUE))

#Setting working directory,summary and structure of data
setwd("~/Desktop/LinearRegression")
housing_data <- read.csv("housing_data.csv")
housing_data
summary(housing_data)
str(housing_data)
dim(housing_data)

#head and tail of data

head(housing_data)
tail(housing_data)

#check for missing values
sum(is.na(housing_data))

#Missing values are present so we need to do central imputation
#Central Imputation is present in DMwR library
library(DMwR)
housing_data <- centralImputation(housing_data)
sum(is.na(housing_data))

summary(housing_data)

#plot some scatter plots 
par(mfrow = c(2,2))
plot( housing_data$LSTAT,housing_data$MV,  xlab = "Percentage of people in the lower economic strata",ylab = "Median House Price", main = "Housing Price vs Status")
plot( housing_data$CRIM, housing_data$MV, xlab = "Per capita crime by town",ylab = "Median House Price", main = "Housing Price vs Per Capita Crime")
plot( housing_data$NOX,housing_data$MV,  xlab = "Nitric Oxide Concentration in ppm",ylab = "Median House Price", main = "Housing Price vs NOX concentration in ppm")
plot(housing_data$INDUS,housing_data$MV,  xlab = "Proportion of non-retail business acres per town",ylab = "Median House Price", main = "Housing Price vs Non-retail business area")
dev.off()

#plot a corrleation plot to check the corrleation b/w independent variables
#corr plot will be in paired values

library(corrplot)
corrplot(cor(housing_data))

#Create Data partioning into train and test

set.seed(235)
library(caret)
train_rows <- sample(x = 1:nrow(housing_data), size = 0.7*nrow(housing_data))
train_data<-housing_data[train_rows,]
test_data <-housing_data[-train_rows,]

lmout <- lm(train_data$MV ~ . ,data=train_data)
summary(lmout)

lmout1 <- predict(lm(test_data$MV~., data=test_data))
#Lets compute the root-mean-square error between actual and predicted
RMSE(test_data$MV,lmout1)
 (Error0<-rmse(test_data$MV,lmout1))
  
summary(lmout1)

Output2Mod <- lmout1
Output2Mod[lmout1<=0] <-1   

summary(Output2Mod)
