#Start with a vector 
a = 0
# Replace 1 with "Yes" in the Present in Endline column
b = 5

# longer vector 
a = 1:15
b <- c(2,5,6,8,9)

length(a)
# Finding the lenght of b and saving it in c
c <- length(b)

class(a)
class(b)
class(c)


## Sequence vector
a = seq(1,30, by=20)
help(seq)

## Repeat vector
b = rep(2,20)
b
length(b)

#subset of a vector
b[5]

c = c(5,7,8,9)
a[c]
b[2]
c[4]

#Conditional Subsetting 
a > 7
a[a>7]

a[a>15 | a < 8]

a[a>=6 & a<=10]

## Change vector 
a[5] = 23

a[c(5,9,10)]=23

a[c(5,9,10)] = c(27,36,111)

#Character vector
m<- c(rep("png",5),rep("hul",10),rep("marico",5))
as.numeric(m)
m

mm = as.factor(m)

levels(mm)

levels(mm)[2]="nestle"
levels(mm)=c("swagato","arpita","anubhab")





