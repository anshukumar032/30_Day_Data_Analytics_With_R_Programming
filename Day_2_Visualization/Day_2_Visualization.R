#The most important libaray for graphics
library(ggplot2)
library(MASS)
library(plotrix)
setwd("D:/30_Days_R/Day_2_")
getwd()

#read data
df <- read.csv(file = "D:/30_Days_R/Day_2_/sachin.csv")
View(df)

tail(df,3)
str(df)

#quick plots for data exploration
with(df, plot(Dismissal))

#scatter plot
plot(df$BF, df$Runs)
qplot(BF, Runs, data = df)

#gglot equivatent
ggplot(df, aes(x=BF, y=Runs)) + geom_point()

#line graph
s10a = df[order(df$BF),]
plot(s10a$BF, s10a$Runs, type = 'l')
qplot(BF, Runs, data=s10a, geom=c("line", "point"))
ggplot(s10a, aes(x=BF, y=Runs))+geom_point()+geom_line()


#Bar Plot
#barplot(factor(s10$result))
qplot(result, data = s10a, geom = "bar")
ggplot(df, aes(x=result)) +geom_bar()




ggplot(df, aes(x=Runs, y=SR))+geom_point()+geom_line()


#histogram
hist(s10a$Runs)




























