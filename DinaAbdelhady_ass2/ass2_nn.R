

install.packages('e1071', dependencies=TRUE)
install.packages("corrplot")

library(corrplot)
library(reshape2)
library(ggplot2)
library(plyr)
library(scales)

library(caTools)
library(rpart)
library(rpart.plot)
library(caret)
library(dplyr)

# install library
install.packages("neuralnet ")

# load library
library(neuralnet)

#########################################
data <-read.csv("diabetes.csv", header =TRUE, sep =",")
str(data)
data$BMI <-as.numeric(as.character(data$BMI),na.rm =TRUE)
data$Glucose <-as.numeric(as.character(data$Glucose),na.rm =TRUE)
data$Insulin <-as.numeric(as.character(data$Insulin),na.rm =TRUE)
data$BloodPressure  <-as.numeric(as.character(data$BloodPressure ),na.rm =TRUE)
data$SkinThickness  <-as.numeric(as.character(data$SkinThickness ),na.rm =TRUE)

datamedian<-data

#mean
ave_BMI<-ave(data$BMI, FUN = function(x) mean(x, na.rm =TRUE))
data$BMI<-ifelse(is.na(data$BMI), ave_BMI, data$BMI)

ave_Glucose <-ave(data$Glucose, FUN = function(x) mean(x, na.rm =TRUE))
data$Glucose<-ifelse(is.na(data$Glucose), ave_Glucose, data$Glucose)

ave_Insulin <-ave(data$Insulin, FUN = function(x) mean(x, na.rm =TRUE))
data$Insulin<-ifelse(is.na(data$Insulin), ave_Insulin, data$Insulin)

ave_BloodPressure <-ave(data$BloodPressure, FUN = function(x) mean(x, na.rm =TRUE))
data$BloodPressure<-ifelse(is.na(data$BloodPressure), ave_BloodPressure, data$BloodPressure)

ave_SkinThickness <-ave(data$SkinThickness, FUN = function(x) mean(x, na.rm =TRUE))
data$SkinThickness<-ifelse(is.na(data$SkinThickness), ave_SkinThickness, data$SkinThickness)



#median

median_BMI <-ave(datamedian$BMI, FUN = function(x) median(x, na.rm =TRUE))
datamedian$BMI<-ifelse(is.na(datamedian$BMI), median_BMI , datamedian$BMI)

median_Glucose <-ave(datamedian$Glucose, FUN = function(x) median(x, na.rm =TRUE))
datamedian$Glucose<-ifelse(is.na(datamedian$Glucose), median_Glucose, datamedian$Glucose)

median_Insulin<-ave(datamedian$Insulin, FUN = function(x) median(x, na.rm =TRUE))
datamedian$Insulin<-ifelse(is.na(datamedian$Insulin), median_Insulin, datamedian$Insulin)

median_BloodPressure <-ave(datamedian$BloodPressure, FUN = function(x) median(x, na.rm =TRUE))
datamedian$BloodPressure<-ifelse(is.na(datamedian$BloodPressure), median_BloodPressure, datamedian$BloodPressure)

median_SkinThickness <-ave(datamedian$SkinThickness, FUN = function(x) median(x, na.rm =TRUE))
datamedian$SkinThickness<-ifelse(is.na(datamedian$SkinThickness), median_SkinThickness, datamedian$SkinThickness)




corrplot(cor(data,method = "pearson"),diag = FALSE,method = "ellipse",tl.cex = 0.7, tl.col = "black", cl.ratio = 0.2)
corrplot(cor(datamedian,method = "pearson"),diag = FALSE,method = "ellipse",tl.cex = 0.7, tl.col = "black", cl.ratio = 0.2)

#####################################################################################

#without Rescaling

# Random sampling
samplesize = 0.75 * nrow(data)
set.seed(80) #to generate same random sample every time & maintain consistency
index = sample( seq_len (nrow(data)), size = samplesize )

# Create training and test set
datatrain = data[ index, ]
datatest = data[ -index, ]
trainNN = data[ index, ]
testNN = data[ -index, ]



# fit neural network
set.seed(2)
NN = neuralnet(Outcome  ~ Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age , trainNN, hidden = 2 , linear.output = T)

# plot neural network
plot(NN)

## Prediction using neural network

predict_testNN = compute(NN, testNN[,c(1:8)])

predict_testNN = (predict_testNN$net.result * (max(data$Outcome - min(data$Outcome))) + min(data$Outcome))


prob <- predict_testNN
pred <- ifelse(prob>0.5, 1, 0)

cm <- confusionMatrix(factor(pred), factor(testNN$Outcome), dnn = c("Prediction", "Reference"))

plt <- as.data.frame(cm$table)
plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))

ggplot(plt, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction") 


# Calculate Root Mean Square Error (RMSE)
RMSE_withoutRescaling.NN = (sum((datatest$Outcome - predict_testNN)^2) / nrow(datatest)) ^ 0.5

##############################################################

# Random sampling
samplesize = 0.75 * nrow(data)
set.seed(80) #to generate same random sample every time & maintain consistency
index = sample( seq_len (nrow(data)), size = samplesize )

# Create training and test set
datatrain = data[ index, ]
datatest = data[ -index, ]
trainNN = data[ index, ]
testNN = data[ -index, ]


## Fit neural network 2 hidden nodes



# Create training and test set
datatrain = data[ index, ]
datatest = data[ -index, ]

max = apply(data , 2 , max)
min = apply(data, 2 , min)

scaled = as.data.frame(scale(data, center = min, scale = max - min))

# creating training and test set
trainNN = scaled[index , ]
testNN = scaled[-index , ]

# fit neural network
set.seed(2)
NN = neuralnet(Outcome  ~ Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age , trainNN, hidden = 2,linear.output = T )

# plot neural network
plot(NN)

## Prediction using neural network

predict_testNN = compute(NN, testNN[,c(1:8)])

predict_testNN = (predict_testNN$net.result * (max(data$Outcome - min(data$Outcome))) + min(data$Outcome))


prob <- predict_testNN
pred <- ifelse(prob>0.5, 1, 0)


cm <- confusionMatrix(factor(pred), factor(testNN$Outcome), dnn = c("Prediction", "Reference"))
cm
plt <- as.data.frame(cm$table)
plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))

ggplot(plt, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction") 


# Calculate Root Mean Square Error (RMSE)
RMSE_2hidden.NN = (sum((datatest$Outcome - predict_testNN)^2) / nrow(datatest)) ^ 0.5

##############################################################
## Fit neural network h 2 layers & 5 nodes
# Create training and test set
datatrain = data[ index, ]
datatest = data[ -index, ]

max = apply(data , 2 , max)
min = apply(data, 2 , min)

scaled = as.data.frame(scale(data, center = min, scale = max - min))

# creating training and test set
trainNN = scaled[index , ]
testNN = scaled[-index , ]
# fit neural network
set.seed(2)
NN = neuralnet(Outcome  ~ Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin +BMI +DiabetesPedigreeFunction+Age , trainNN, hidden = c(5,5) , linear.output = T ,stepmax=1e6)

# plot neural network
plot(NN)

## Prediction using neural network

predict_testNN = compute(NN, testNN[,c(1:8)])
data$Outcome
predict_testNN = (predict_testNN$net.result * (max(data$Outcome - min(data$Outcome))) + min(data$Outcome))

prob <- predict_testNN
pred <- ifelse(prob>0.5, 1, 0)



cm <- confusionMatrix(factor(pred), factor(testNN$Outcome), dnn = c("Prediction", "Reference"))

cm

plt <- as.data.frame(cm$table)
plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))

ggplot(plt, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#FFFF00") +
  labs(x = "Reference",y = "Prediction") 



# Calculate Root Mean Square Error (RMSE)
RMSE_5hidden.NN = (sum((datatest$Outcome - predict_testNN)^2) / nrow(datatest)) ^ 0.5
#####################################3

## Fit neural network h 2 layers & 5 nodes
# Create training and test set
datatrain = data[ index, ]
datatest = data[ -index, ]

max = apply(data , 2 , max)
min = apply(data, 2 , min)

scaled = as.data.frame(scale(data, center = min, scale = max - min))

# creating training and test set
trainNN = scaled[index , ]
testNN = scaled[-index , ]
# fit neural network
set.seed(2)
NN = neuralnet(Outcome  ~ Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin +BMI +DiabetesPedigreeFunction+Age , trainNN, hidden = c(5,5) , linear.output = T ,stepmax=1e7)

# plot neural network
plot(NN)

## Prediction using neural network

predict_testNN = compute(NN, testNN[,c(1:8)])
data$Outcome
predict_testNN = (predict_testNN$net.result * (max(data$Outcome - min(data$Outcome))) + min(data$Outcome))

prob <- predict_testNN
pred <- ifelse(prob>0.5, 1, 0)
pred
factor(pred)
                  
plot(datatest$Outcome, pred, col='yellow', pch=16, ylab = "predicted Outcome NN", xlab = "real Outcome")

cm <- confusionMatrix(factor(pred), factor(testNN$Outcome), dnn = c("Prediction", "Reference"))

plt <- as.data.frame(cm$table)
plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))

ggplot(plt, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#FFFF00") +
  labs(x = "Reference",y = "Prediction") 



# Calculate Root Mean Square Error (RMSE)
RMSE_5hMORESTEP.NN = (sum((datatest$Outcome - predict_testNN)^2) / nrow(datatest)) ^ 0.5
##################################################################################################

#change in code
# Create training and test set
datatrain = data[ index, ]
datatest = data[ -index, ]

max = apply(data , 2 , max)
min = apply(data, 2 , min)

scaled = as.data.frame(scale(data, center = min, scale = max - min))

# creating training and test set
trainNN = scaled[index , ]
testNN = scaled[-index , ]

## Fit neural network 2 hidden nodes

# fit neural network
set.seed(2)
NN = neuralnet(Outcome  ~ Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age , trainNN, hidden = 2, act.fct =  'logistic' , linear.output = T )

# plot neural network
plot(NN)

## Prediction using neural network

predict_testNN = compute(NN, testNN[,c(1:8)])

predict_testNN = (predict_testNN$net.result * (max(data$Outcome - min(data$Outcome))) + min(data$Outcome))


prob <- predict_testNN
pred <- ifelse(prob>0.5, 1, 0)


plot(datatest$Outcome, pred, col='blue', pch=16, ylab = "predicted Outcome NN", xlab = "real Outcome")
cm <- confusionMatrix(factor(pred), factor(testNN$Outcome), dnn = c("Prediction", "Reference"))
cm
plt <- as.data.frame(cm$table)
plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))

ggplot(plt, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction") 


# Calculate Root Mean Square Error (RMSE)
RMSE_2hlogistic.NN = (sum((datatest$Outcome - predict_testNN)^2) / nrow(datatest)) ^ 0.5
##################################################

## Fit neural network 2 hidden nodes
# Create training and test set
datatrain = data[ index, ]
datatest = data[ -index, ]

max = apply(data , 2 , max)
min = apply(data, 2 , min)

scaled = as.data.frame(scale(data, center = min, scale = max - min))

# creating training and test set
trainNN = scaled[index , ]
testNN = scaled[-index , ]
# fit neural network
set.seed(2)
NN = neuralnet(Outcome  ~ Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age , trainNN, hidden = 2, act.fct = function(x) {1 / (1 + exp(-x)) },  linear.output = T )

# plot neural network
plot(NN)

## Prediction using neural network

predict_testNN = compute(NN, testNN[,c(1:8)])

predict_testNN = (predict_testNN$net.result * (max(data$Outcome - min(data$Outcome))) + min(data$Outcome))


prob <- predict_testNN
pred <- ifelse(prob>0.5, 1, 0)


cm <- confusionMatrix(factor(pred), factor(testNN$Outcome), dnn = c("Prediction", "Reference"))
cm
plt <- as.data.frame(cm$table)
plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))

ggplot(plt, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction") 


# Calculate Root Mean Square Error (RMSE)
RMSE_2hsigmoid.NN = (sum((datatest$Outcome - predict_testNN)^2) / nrow(datatest)) ^ 0.5

#####################################################
#with median
# Create training and test set
datatrain = datamedian[ index, ]
datatest = datamedian[ -index, ]

max = apply(data , 2 , max)
min = apply(data, 2 , min)

scaled = as.data.frame(scale(data, center = min, scale = max - min))

# creating training and test set
trainNN = scaled[index , ]
testNN = scaled[-index , ]

# fit neural network
set.seed(2)
NN = neuralnet(Outcome  ~ Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age , trainNN, hidden = 2,linear.output = T )

# plot neural network
plot(NN)

## Prediction using neural network

predict_testNN = compute(NN, testNN[,c(1:8)])

predict_testNN = (predict_testNN$net.result * (max(datamedian$Outcome - min(datamedian$Outcome))) + min(datamedian$Outcome))


prob <- predict_testNN
pred <- ifelse(prob>0.5, 1, 0)


plot(datatest$Outcome, pred, col='blue', pch=16, ylab = "predicted Outcome NN", xlab = "real Outcome")
cm <- confusionMatrix(factor(pred), factor(testNN$Outcome), dnn = c("Prediction", "Reference"))
cm
plt <- as.data.frame(cm$table)
plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))

ggplot(plt, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction") 


# Calculate Root Mean Square Error (RMSE)
RMSE_2hmedian.NN = (sum((datatest$Outcome - predict_testNN)^2) / nrow(datatest)) ^ 0.5

#######################################################
## Fit neural network 2 hidden nodes
# Create training and test set
datatrain = data[ index, ]
datatest = data[ -index, ]

max = apply(data , 2 , max)
min = apply(data, 2 , min)

scaled = as.data.frame(scale(data, center = min, scale = max - min))

# creating training and test set
trainNN = scaled[index , ]
testNN = scaled[-index , ]
# fit neural network
set.seed(2)
NN = neuralnet(Outcome  ~ Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age , trainNN, hidden = 2,learningrate = 1,linear.output = T )

# plot neural network
plot(NN)

## Prediction using neural network

predict_testNN = compute(NN, testNN[,c(1:8)])

predict_testNN = (predict_testNN$net.result * (max(data$Outcome - min(data$Outcome))) + min(data$Outcome))


prob <- predict_testNN
pred <- ifelse(prob>0.5, 1, 0)


cm <- confusionMatrix(factor(pred), factor(testNN$Outcome), dnn = c("Prediction", "Reference"))
cm
plt <- as.data.frame(cm$table)
plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))

ggplot(plt, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction") 


# Calculate Root Mean Square Error (RMSE)
RMSE_2hlearningrate.NN = (sum((datatest$Outcome - predict_testNN)^2) / nrow(datatest)) ^ 0.5

############################################
