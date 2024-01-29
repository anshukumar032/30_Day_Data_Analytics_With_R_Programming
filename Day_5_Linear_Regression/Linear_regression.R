install.packages("alr4")
install.packages("e1071")
install.packages("corrplot")
install.packages("GGally")
install.packages("psych")
install.packages("caret")
install.packages("lattice")
install.packages("ggplot2")

library(e1071)
library(alr4)
library(corrplot)
library(GGally)
library(psych)
library(caret)
library(lattice)
library(ggplot2)

getwd()
setwd("D:/30_Days_R/Day_5_Linear_Regression")

getwd()
data()
head(cars)

# scatter plot
scatter.smooth(x=cars$speed, y=cars$dist , main="Dist ~ Speed")

# divide graph area in 2 columns
par(mfrow=c(1, 2))

# box plot for 'speed'
boxplot(cars$speed, main="Speed", sub=paste("Outlier rows: ", boxplot.stats(cars$speed)$out))
# box plot for 'distance'
boxplot(cars$dist, main="Distance", sub=paste("Outlier rows: ", boxplot.stats(cars$dist)$out))


# divide graph area in 2 columns
par(mfrow=c(1, 2))

# density plot for 'speed'
plot(density(cars$speed), main="Density Plot: Speed", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2))) 
polygon(density(cars$speed), col="red")

# density plot for 'dist'
plot(density(cars$dist), main="Density Plot: Distance", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$dist), 2))) 
polygon(density(cars$dist), col="green")


#Linear regression model
linearMod <- lm(dist ~ speed, data=cars)
summary(linearMod)


modelSummary <- summary(linearMod)  # capture model summary as an object
modelCoeffs <- modelSummary$coefficients  # model coefficients
beta.estimate <- modelCoeffs["speed", "Estimate"]  # get beta estimate for speed
std.error <- modelCoeffs["speed", "Std. Error"]  # get std.error for speed
t_value <- beta.estimate/std.error  # calc t statistic
p_value <- 2*pt(-abs(t_value), df=nrow(cars)-ncol(cars))  # calc p Value
f_statistic <- linearMod$fstatistic[1]  # fstatistic
f <- summary(linearMod)$fstatistic  # parameters for model p-value calc
model_p <- pf(f[1], f[2], f[3], lower=FALSE)




