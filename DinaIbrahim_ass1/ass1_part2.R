
#Import the data
bank <-read.csv("bank-additional-full.csv", header =TRUE, sep =",")

install.packages("dplyr")
library(dplyr)

#reduce the dataset to only four predictors
bank<- select(bank,1,4,13,14,21)

#Explain why the field pdays is essentially useless until you handle the 999 code
hist(bank$pdays)

#Change the field value 999 to "NA"
bank$pdays[bank$pdays == 999] <- NA

#histogram of the pdays
hist(bank$pdays)




#Transform the data values of the education
bank$education[bank$education == 'basic.4y']<- 4
bank$education[bank$education == 'high.school']<- 12
bank$education[bank$education == 'basic.6y']<- 6
bank$education[bank$education == 'basic.9y']<- 9
bank$education[bank$education == 'professional.course']<- 14
bank$education[bank$education == 'unknown']<- NA
bank$education[bank$education == 'university.degree']<- 16
bank$education[bank$education == 'illiterate']<- 0
bank


#Compute the mean, median & mode
mean<-mean(bank$age)
mean
median<-median(bank$age)
median
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
mode<-Mode(bank$age)
mode

#a boxplot
boxplot(bank$age)


#summary of the data
summary(bank)

#Plot the quantile 
q <- quantile(bank$age)
qqnorm(q)

# Standardize the age variable
scaled.age <- scale(bank$age)
scaled.age

# save it as a new variable, age_z
bank$age_z<-scaled.age


#outliers
outliers<- bank[which(bank$age_z < -2  | bank$age_z > 3), ]
outliers


