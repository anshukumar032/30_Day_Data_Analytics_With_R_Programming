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

levels(df$Dismissal)
#remove rare cases
levels(df$Dismissal)=c("bowled", "caught", "others","lbw", "not out","others","others","others")
with(df,plot(Dismissal))
#barchart with color
qplot(Dismissal,geom="bar",data=df,fill=I("red"))+ylab("number of cases")
#Boxplot
qplot(result,Runs,data=df,geom="boxplot",fill = I("blue"))+xlab("Types of results")


s10a <- subset(df, Dismissal %in% c("bowled", "caught", "lbw", "not out"))
s10a$Inns <- factor(s10a$Inns)
qplot(data = s10a, Dismissal, geom = "bar",fill=I("green"))

s10c <- subset(df, Opposition %in% c("v Australia", "v South Africa", "v New Zealand"))
ggplot(data = s10c,aes(x = BF, y = SR, colour = Opposition)) + geom_point() +
  xlab("Ball Faced") + ylab("Runs Scored") + labs(title = "Ball faced vs Runs Scored")


## Examples of geoms: line,point,box,bar,...
s10b <- subset(df, result %in% c("won", "lost") & Opposition %in% c("v Australia",
                                                                     "v South Africa", "v New Zealand"))

s10b$result <- factor(s10b$result)
s10b$opp <- factor(s10b$Opposition)

ggplot(s10b, aes(opp, Runs,fill=opp)) + geom_violin()


require(chron)
qplot(x = factor(years(StartDate)), y = Runs, geom = "boxplot", fill = I("green"),
      data = s10b) + xlab("Year")+ylab("Runs scored by sachin")

## Scales

ggplot(data = s10b) + geom_point(aes(x = BF, y = Runs, colour = result)) + scale_colour_manual(values = c(lost = "red",
                                                                                                          won = "green"))

ggplot(data = s10b) + geom_point(aes(x = BF, y = Runs, size = result, shape = result)) +
  scale_size_manual(values = c(lost = 5, won = 3)) + scale_shape_manual(values = c(lost = 15,
                                                                                   won = 25))

ggplot(data = s10b) + geom_point(aes(x = BF, y = Runs, colour = result)) + scale_colour_discrete(name = "Parinaam",
                                                                                                 labels = c(lost = "HAAR", won = "JEET"))


## Facets

s10c = subset(df, (Opposition %in% c("v Australia", "v England", "v South Africa",
                                      "v Sri Lanka")) & (result %in% c("lost", "won")))


s10c$yr = years(s10c$StartDate)
qplot(data = s10c, factor(Inns), geom = "bar")
qplot(data = s10c, factor(Inns), geom = "bar",fill="red")
qplot(data = s10c, factor(Inns), fill=result, geom = "bar", position="fill")
qplot(data = s10c, factor(Inns), fill = result, geom = "bar", position = "fill",
      facets = Opposition ~ yr)


## Facets Themes

s10a <- subset(df, Dismissal %in% c("bowled", "caught", "lbw", "not out"))
s10a$Dismissal <- factor(s10a$Dismissal)
s10a$Inns <- factor(s10a$Inns)
qplot(data = s10a, Dismissal, geom = "bar", fill = Inns)
qplot(data = s10a, Dismissal, geom = "bar", fill = Inns)+scale_fill_brewer()
qplot(data = s10a, Dismissal, geom = "bar")+facet_wrap(~ Inns)
qplot(data = s10a, Dismissal, geom = "bar", fill = Inns) + theme_bw() + coord_flip()
qplot(data = s10a, Dismissal, geom = "bar", fill = Inns) + theme(panel.background = element_rect(fill = "lightblue"))
qplot(data = s10a, Dismissal, geom = "bar", fill = Inns) + theme(plot.background = element_rect(fill = "yellow"),
                                                                 panel.background = element_rect(fill = "purple"))



































