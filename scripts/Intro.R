## R objects
a = 5
b = "Hello"
c = TRUE

a
b
c

class(a)
class(b)
class(c)

## Vectors
numbers <- c(1,2,3,4,5,6,7)
days  <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

class(numbers)
class(days)

## Data frames
df <- data.frame(Number = numbers, Day = days)

df
df$Number

class(df)
class(df$Number)
class(df$Day)

values <- c(6,9,4,NA,7,12,1)

## functions
## mean() function 

values <- c(6,9,4,"A",7,12,1)
mean(values)
class(values)

## summary functions
summary(df)
summary(df$Number)
summary(df$Day)

## factors are categorical labels
df$working <- c("Yes", "No", "Yes", "Yes", "No", "No", "Yes")
df
summary(df$working)
df$working <- as.factor(df$working)
summary(df$working)

## matrices
m <- matrix(1:9, nrow = 3, ncol = 3)
m

## lists
my_list <- list(a = a, numbers = numbers, days = days, df = df)
my_list

mean(numbers)
mean(values, na.rm = TRUE)

mydata  <- read.table("http://bifx-core3.bio.ed.ac.uk/data.tsv", header = T, sep = "\t")

mydata[11,3]
mydata[mydata$D==4,]
mydata[mydata$B == max(mydata$B),]
mydata[seq(2,nrow(mydata),by =2),]
mydata[,c(1,3,5)]

order(mydata$B,decreasing = T)
mydata[order(mydata$B,decreasing = T),]

ChickWeight
summary(ChickWeight)
class(ChickWeight$Diet)
mean(ChickWeight$weight)

nrow(ChickWeight)
dim(ChickWeight)
tail(ChickWeight)
set.seed(123)
rnorm(10, mean=6, sd=3)

## Create a folder in the working directory
dir.create("data")

## Download the qPCR file and save it in the data folder
download.file("http://bifx-core3.bio.ed.ac.uk/training/DSB/data/qPCR_data.xlsx", destfile = "data/qpcr_data.xlsx")

library(readxl)
qpcr<-readxl::read_excel("data/qpcr_data.xlsx")

mean(qpcr$ct_gene[qpcr$Condition == "Control"])


