#The most important libaray for graphics
library(ggplot2)
library(MASS)
library(plotrix)
setwd("D:/30_Days_R/Day_3_")
getwd()

#read data
df <- read.csv(file = "D:/30_Days_R/Day_3_/sachin.csv")
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
ggplot(Runs , data=df, binwidth = 4 )
ggplot(df, aes(x=df))+geom_histogram(binwidth = 4)

#boxplot
plot(df$Opposition, df$Runs)
ggplot(Opposition,Runs, data=df, geom="boxplot")
ggplot(df, aes(x=result, y=Runs)) + geom_boxplot()
ggplot(df,aes(x=result, y=Runs)) + geom_violin()


#Curve Plot
curve(x^2, from=0, to=20)
curve(x^3,add = TRUE, col='red')


#pie Chart
dismissal.freq=with(df,table(Dismissal))
pie(dismissal.freq)
pie(dismissal.freq,col = c("red","yellow","blue",
                           "green","violet","brown","purple","pink"))
pie(dismissal.freq,col = rainbow(8),radius = 1, labels = names(dismissal.freq))

dismissal.freq

paste("swagato","chatterjee", sep = " ")







































