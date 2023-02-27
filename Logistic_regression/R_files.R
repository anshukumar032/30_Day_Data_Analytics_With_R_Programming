# R Project
# Submitted to: DR POOJA TRIPATHI
# Submitted By : Tarandeep Kaur (2K22/MAE/09) and Geetashree Swain(2K22/MAE/)



# PROBLEM : We have taken data of 1000 customers 
# arriving in a car showroom in a week (random sample).
# The data has 6 columns : User ID(Nominal- labeling of a particular customer),
# Gender(Male assigned value "1" and Female assigned value "0", Age(in years),
# Annual Salary (in rupees) and Cartage; Categories are assigned to different 
# age groups such as 17-24 as "1", 25-32 as "2" and so on. 
# Our analysis is about predicting whether the individual
# will purchase a car or not based upon their characteristics.





# Packages Used
install.packages("Rtools")
install.packages("readxl")
install.packages("MASS")
library(MASS)
install.packages("ggplot2")
library(ggplot2)
install.packages("plotrix")
install.packages("lessR")
library(lessR)
install.packages("graphics")
library(graphics)


# Extracting and viewing the dataset
library(readxl)
Car3 <- read_excel(file.choose())
#dataset <- read_excel("E:/car_data.xlsx")


#Summary of dataset
summary(Car3)

#Changing the Age to a factor (categorical)
Car3$AgeCatg<- as.factor(Car3$AgeCatg)

summary(Car3)

#Attaching the data 
attach(Car3)

View(Car3)



#In our dataset Gender has 2 categories male(represented as "1") and female(represented as"0")
#and the variable purchased means car purchased or not ("1" for yes and "0" for not purchased) 
cars.R <- data.frame(Car3)
head(cars.R)


table(cars.R$Gender)  #gives the number of males and females
table(cars.R$Purchased)  #gives the number of persons purchased car and not purchased car

# Calculating deviation in data
sd(cars.R$Age)
sd(cars.R$AnnualSalary)

# the total salary (GDP of our randomly chosen small economy)
sum(cars.R$AnnualSalary)

# Visualization of Data
# Box plots - to check for the outliers
# box plot displays the 5 number summary of a set of data. 
# The 5 number summary is the minimum, first quartile, 
# median, third quartile, maximum.
boxplot(cars.R$AnnualSalary, main = "Annual Salary", col = "Yellow")
boxplot(cars.R$Age, main = "Age", col = "Purple")

# Bar graph for Gender
a=table(cars.R$Gender)
barplot(a,main = "Using Barplot to depict Gender",
        ylab = "Count",
        xlab = "Gender",
        col=rainbow(2),
        legend=rownames(a))

# Pie chart for Gender

library(plotrix)

pct=round(a/sum(a)*100)
lbs=paste(c("Males","Females"),"",pct,"%",sep = " ")
pie3D(a,labels = lbs,
      main="Pie Chart depicting Gender Ratio of Males(1) and Females(0)")



# Histogram : to check Distribution of data
hist(cars.R$AnnualSalary, main = "Annual salary", col = "pink")
hist(cars.R$Age, main = "Age", col = "blue")


matplot(cars.R$Age, col = topo.colors(4))
matplot(cars.R$AnnualSalary, col = "red")




#first lets take a look at plot of Y Vs X

plot(Age,Purchased, main= "Age Vs Purchased" , ylab="(Prob. of) Purchased" ,
     ylim = c(0,1) , las=1)

# we want to model P(Purchased|Age), which is same as E(Y|X) like in Linear regression

#Adding a vertical lines, to separate the Age-Categories
abline(v=24, col="red")
abline(v=32, col="red")
abline(v=40, col="red")     
abline(v=48, col="red")
abline(v=56, col="red")
abline(v=64, col="red")

# Adding Labels to age categories

mtext("1", side=1, adj=0.15, col = "Green")
mtext("2", side=1, adj=0.30, col = "Green")
mtext("3", side=1, adj=0.45, col = "Green")
mtext("4", side=1, adj=0.60, col = "Green")
mtext("5", side=1, adj=0.75, col = "Green")
mtext("6", side=1, adj=0.90, col = "Green")


# Use AgeCatg to simplify, so that we can calculate the mean Purchases for each category
#look at the variable Purchased (if evidence of Purchased) for AgeCatg 1

Purchased[AgeCatg=="1"]

#What proportion of people in AgeCatg =1 have evidence 
mean(Purchased[AgeCatg=="1"])
# no one from age category 1 purchases a car

#look at the variable Purchased (if evidence of Purchased) for AgeCatg 2
Purchased[AgeCatg=="2"]

#What proportion of people in AgeCatg =2 have evidence 
mean(Purchased[AgeCatg=="2"])

#look at the variable Purchased (if evidence of Purchased) for AgeCatg 3
Purchased[AgeCatg=="3"]

#What proportion of people in AgeCatg =3 have evidence
mean(Purchased[AgeCatg=="3"])



#Calculate means and store them in object called "p"

p <- c(mean(Purchased[AgeCatg=="1"]), mean(Purchased[AgeCatg=="2"]),
       mean(Purchased[AgeCatg=="3"]), mean(Purchased[AgeCatg=="4"]),
       mean(Purchased[AgeCatg=="5"]), mean(Purchased[AgeCatg=="6"]))

p

#this shows us the proportion of people from each age category who visit showroom and purchase a car

# Plot them Vs the mid-point of Age Categories
MidAge  <- c(20.5,28.5,36.5,44.5,52.5,60.5)

#add in the Points...
points(MidAge , p, pch="p", col="red")

#Looking at the plot showing Purchased =Yes1/no0 
#Trying to fit a linear model
mod1 <-lm(Purchased~Age)
mod1
#add in the Reg line (where we fit a linear regression)
abline(mod1 , col="magenta", lwd=2)

# Helping the visual, to see the problem
abline(h=0, col="black") # Prob = 0
abline(h=1, col="black") # Prob = 1

#Problems
# 1) Not linear...likely S-shaped curve usually because it is bounded between 0 and 1
# 2) line goes below 0 and above 1....probability becomes negative or greater than 1

#logit model (Logistic Regression Model- where Dependent variable is categorical)

# Multiple Logistic Regression Model/ Generalized Linear Model
# taking Purchased as dependent variable and Gender,
# age and Annual Salary as independent variables.
cars_purchased <- glm(Purchased ~ Gender + Age + AnnualSalary, data = cars.R, family = binomial(link="logit") )

# here link ="logit" is the default function in R
# and it will work even if we don't specify
# Binomial family is chosen as there are only two outcomes in the dependent variable

cars_purchased
View(cars_purchased)
summary(cars_purchased)
#purchased = -1.218e+01 +3.184e-01(Gender) + 2.195e-01(Age) + 3.370e-05(Annual Salary)



# Age and Annual Salary are highly significant and
# Gender is not significant
# Null deviance is similar to Total Sum of Squares
# Residual Deviance is Similar to Residual Sum of Squares

anova(cars_purchased)

# Gender reduces the residuals by 2.23 and lost one degrees of freedom,
# whereas Age reduces residuals by a huge margin of 459.45 and Annual Salary 
# reduces residuals by 142.99 by losing one degree of freedom each;
# therefore gender is not explaining much of variation in decision
# regarding purchasing a car or not, according to our dataset/sample.   

# Removing gender and checking whether models are
# similar to each other or different from each other
# Running model without gender
cars_purchased1 <- glm(Purchased ~  Age + AnnualSalary, data = cars.R, family = binomial(link="logit") )

#Hypothesis testing 
# H0 : Models are not different from each other
# H1 : Models are different from each other
# Using Chi- Square test for non normal data

anova(cars_purchased, cars_purchased1, test = "Chi")

# we do not reject the null hypothesis(P value > 0.05)
# both models are as good as each other

#Updating an existing model
# In car_purchased1 model we removed gender, to add it back we update it
car_purchased_updated <- update(cars_purchased1,~.+Gender)
summary(cars_purchased1)
summary(car_purchased_updated)


#Prediction using the model
# Prediction for a Male , aged 45 years having annual salary of Rs 150000
newdata <- data.frame(Gender= 1, Age = 45, AnnualSalary = 150000 )
predict.glm(cars_purchased, newdata)

fitted(cars_purchased)
# According to dataset gives the probability of each of the person purchasing a car, 
# depending upon their characteristics 


# Deviance Residuals : Approximately Normally Distributed
residuals(cars_purchased)

#Pearson Residuals : Skewed for Non Normal data
residuals(cars_purchased, type= "pearson")

#Both residuals are identical in case of normal data but different for non normal data


#plot of regression : To check whether Regression is correct and best fit or not
plot(cars_purchased)

# Residuals vs Fitted: fitted/ predicted values given by red line and residuals plotted with black dots.
# the model seems to be a good fit
# Normal Q-Q : it tells us whether our standard deviation residuals following the theoretical 
# quantiles as per the dotted line
# Scale Location: To check for the homoscedasticity assumption.
# the residuals are scattered around the red line with equal variability at all fitted values 
# Residuals vs Leverage: Helps to identify influential data points on the model


#To check normality of the error terms
hist(residuals(cars_purchased))  
# Near to normally distributed










