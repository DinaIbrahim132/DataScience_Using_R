#BUILDING A SIMPLE DECISION TREE III

#Step 1: load require libraries & dataset
install.packages("tidyverse")

library(tidyverse) #perform data manipulation & visualization 
library(caret) # cross - validation methods
library(dplyr)
library(corrplot)
library(reshape2)
library(ggplot2)
library(plyr)
library(scales)

library(caTools)
library(rpart)
library(rpart.plot)
library(caret)



#step 2: Data Manipulation
#Load the data set
set.seed(80)
data <-read.csv("hypothyroid.csv", header =TRUE, sep =",")

table(data$Class) #check values present in the "Class" column




#Step 3: Clean the data
data[data == "?"] <- NA
drops <- c("TBG","TBG_measured")
data<-data[ , !(names(data) %in% drops)]
ndata <- data[!rowSums((is.na(data))),]

str(ndata)


ndata$sex <- ifelse(ndata$sex %in% c("f","F"), 0,ndata$sex)
ndata$sex <- ifelse(ndata$sex %in% c("m","M"),1,ndata$sex)

ndata$on_thyroxine <- ifelse(ndata$on_thyroxine %in% c("T","t"),1,ndata$on_thyroxine)
ndata$on_thyroxine <- ifelse(ndata$on_thyroxine %in% c("F","f"),0,ndata$on_thyroxine)

ndata$query_on_thyroxine  <- ifelse(ndata$query_on_thyroxine  %in% c("T","t"),1,ndata$query_on_thyroxine )
ndata$query_on_thyroxine  <- ifelse(ndata$query_on_thyroxine  %in% c("F","f"),0,ndata$query_on_thyroxine )

ndata$on_antithyroid_medication <- ifelse(ndata$on_antithyroid_medication %in% c("T","t"),1,ndata$on_antithyroid_medication)
ndata$on_antithyroid_medication <- ifelse(ndata$on_antithyroid_medication %in% c("F","f"),0,ndata$on_antithyroid_medication)

ndata$sick <- ifelse(ndata$sick %in% c("T","t"),1,ndata$sick)
ndata$sick <- ifelse(ndata$sick %in% c("F","f"),0,ndata$sick)

ndata$pregnant <- ifelse(ndata$pregnant %in% c("T","t"),1,ndata$pregnant)
ndata$pregnant <- ifelse(ndata$pregnant %in% c("F","f"),0,ndata$pregnant)

ndata$thyroid_surgery <- ifelse(ndata$thyroid_surgery %in% c("T","t"),1,ndata$thyroid_surgery)
ndata$thyroid_surgery <- ifelse(ndata$thyroid_surgery %in% c("F","f"),0,ndata$thyroid_surgery)

ndata$I131_treatment <- ifelse(ndata$I131_treatment %in% c("T","t"),1,ndata$I131_treatment)
ndata$I131_treatment <- ifelse(ndata$I131_treatment %in% c("F","f"),0,ndata$I131_treatment)

ndata$query_hypothyroid <- ifelse(ndata$query_hypothyroid %in% c("T","t"),1,ndata$query_hypothyroid)
ndata$query_hypothyroid <- ifelse(ndata$query_hypothyroid %in% c("F","f"),0,ndata$query_hypothyroid)

ndata$query_hyperthyroid  <- ifelse(ndata$query_hyperthyroid  %in% c("T","t"),1,ndata$query_hyperthyroid )
ndata$query_hyperthyroid  <- ifelse(ndata$query_hyperthyroid  %in% c("F","f"),0,ndata$query_hyperthyroid )

ndata$lithium  <- ifelse(ndata$lithium  %in% c("T","t"),1,ndata$lithium )
ndata$lithium  <- ifelse(ndata$lithium  %in% c("F","f"),0,ndata$lithium )

ndata$goitre <- ifelse(ndata$goitre %in% c("T","t"),1,ndata$goitre)
ndata$goitre <- ifelse(ndata$goitre %in% c("F","f"),0,ndata$goitre)

ndata$tumor  <- ifelse(ndata$tumor  %in% c("T","t"),1,ndata$tumor )
ndata$tumor  <- ifelse(ndata$tumor  %in% c("F","f"),0,ndata$tumor )

ndata$hypopituitary<- ifelse(ndata$hypopituitary%in% c("T","t"),1,ndata$hypopituitary)
ndata$hypopituitary<- ifelse(ndata$hypopituitary%in% c("F","f"),0,ndata$hypopituitary)

ndata$psych<- ifelse(ndata$psych%in% c("T","t"),1,ndata$psych)
ndata$psych<- ifelse(ndata$psych%in% c("F","f"),0,ndata$psych)


ndata$referral_source<- ifelse(ndata$referral_source%in% c("SVI"),0,ndata$referral_source)
ndata$referral_source<- ifelse(ndata$referral_source%in% c("SVHD"),1,ndata$referral_source)
ndata$referral_source<- ifelse(ndata$referral_source%in% c("SVHC"),2,ndata$referral_source)
ndata$referral_source<- ifelse(ndata$referral_source%in% c("STMW"),3,ndata$referral_source)
ndata$referral_source<- ifelse(ndata$referral_source%in% c("other"),3,ndata$referral_source)


ndata$age <-as.numeric(as.character(ndata$age),na.rm =TRUE)
ndata$TSH <-as.numeric(as.character(ndata$TSH),na.rm =TRUE)
ndata$T3 <-as.numeric(as.character(ndata$T3),na.rm =TRUE)
ndata$TT4  <-as.numeric(as.character(ndata$TT4 ),na.rm =TRUE)
ndata$T4U  <-as.numeric(as.character(ndata$T4U),na.rm =TRUE)
ndata$FTI  <-as.numeric(as.character(ndata$FTI),na.rm =TRUE)


ndata$sex <-as.numeric(as.character(ndata$sex),na.rm =TRUE)
ndata$on_thyroxine <-as.numeric(as.character(ndata$on_thyroxine),na.rm =TRUE)
ndata$on_antithyroid_medication <-as.numeric(as.character(ndata$on_antithyroid_medication),na.rm =TRUE)
ndata$lithium  <-as.numeric(as.character(ndata$TT4 ),na.rm =TRUE)
ndata$goitre  <-as.numeric(as.character(ndata$T4U),na.rm =TRUE)
ndata$tumor  <-as.numeric(as.character(ndata$FTI),na.rm =TRUE)

ndata$hypopituitary  <-as.numeric(as.character(ndata$hypopituitary),na.rm =TRUE)
ndata$psych<-as.numeric(as.character(ndata$on_thyroxine),na.rm =TRUE)
ndata$query_on_thyroxine<-as.numeric(as.character(ndata$query_on_thyroxine ),na.rm =TRUE)
ndata$sick<-as.numeric(as.character(ndata$sick),na.rm =TRUE)

ndata$pregnant  <-as.numeric(as.character(ndata$pregnant),na.rm =TRUE)
ndata$thyroid_surgery <-as.numeric(as.character(ndata$thyroid_surgery ),na.rm =TRUE)
ndata$I131_treatment<-as.numeric(as.character(ndata$I131_treatment ),na.rm =TRUE)
ndata$query_hypothyroid<-as.numeric(as.character(ndata$query_hypothyroid),na.rm =TRUE)
ndata$query_hyperthyroid<-as.numeric(as.character(ndata$query_hyperthyroid),na.rm =TRUE)
ndata$referral_source<-as.numeric(as.character(ndata$referral_source),na.rm =TRUE)
ndata$query_hyperthyroid<-as.numeric(as.character(ndata$query_hyperthyroid),na.rm =TRUE)



drops <- c("TSH_measured","T3_measured","TT4_measured","T4U_measured","FTI_measured")
ndata<-ndata[ , !(names(ndata) %in% drops)]




drops <- c("Class")
corrdata<-ndata[ , !(names(ndata) %in% drops)]


corrplot(cor(corrdata,method = "pearson"),diag = FALSE,method = "ellipse",tl.cex = 0.7, tl.col = "black", cl.ratio = 0.2)

#########################################

#without attribuit selection

#fit a decision tree model and use k-fold CV to evaluate performance to all data
model2<- train(Class~., data = ndata, method = "rpart")

#Step 5: Evaluate - view summary of k-fold CV               
print(model2) #metrics give us an idea of how well the model performed on previously unseen data

#view final model
model2$finalModel
prp(model2$finalModel, box.palette = "Reds", tweak = 1.2) #view the tree using prop() function

test_pred_model <- predict(model2, data=ndata)

confusionMatrix(test_pred_model, as.factor(ndata$Class) )  #check accuracy

# estimate variable importance
importance <- varImp(model2, scale=FALSE)

# plot importance
plot(importance)

#psych,lithium,goitre,tumor
#TT4,T4U,FTI,on_thyroxine

#############################################

#Step 4: Model Building with attribute selection

drops <- c('lithium','FTI')
selecteddata<-ndata[ , !(names(ndata) %in% drops)]

table(selecteddata$Class)
selecteddata <- selecteddata[ !(selecteddata$Class %in% c("secondary_hypothyroid")), ]
table(selecteddata$Class)
str(selecteddata)


shuffle_index <- sample(1:nrow(selecteddata)) 
head(shuffle_index)
selecteddata <- selecteddata[shuffle_index, ]

table(selecteddata$Class) 
anyNA(selecteddata)



###########################################################3

#split 50:50
set.seed(80)
sample_split <- sample.split(Y = selecteddata$Class, SplitRatio = 0.50)
train_set <- subset(x = selecteddata, sample_split == TRUE)
test_set <- subset(x = selecteddata, sample_split == FALSE)



#specify the cross-validation method
ctrl <- trainControl(method = "cv", number = 10)

#fit a decision tree model and use k-fold CV to evaluate performance
model<- train(Class~., data = train_set, method = "rpart", trControl = ctrl, tuneLength = 10)

#Step 5: Evaluate - view summary of k-fold CV               
print(model) #metrics give us an idea of how well the model performed on previously unseen data

#view final model
model$finalModel
prp(model$finalModel, box.palette = "Reds", tweak = 1.2) #view the tree using prop() function

#view predictions for each fold
model$resample


test_pred_model <- predict(model, newdata = test_set)

cm <- confusionMatrix(factor(test_pred_model), factor(test_set$Class), dnn = c("Prediction", "Reference"))
cm
plt <- as.data.frame(cm$table)
plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))

ggplot(plt, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction") 
############################################
# split 80:20

set.seed(80)
sample_split <- sample.split(Y = selecteddata$Class, SplitRatio = 0.80)
train_set <- subset(x = selecteddata, sample_split == TRUE)
test_set <- subset(x = selecteddata, sample_split == FALSE)



#specify the cross-validation method
ctrl <- trainControl(method = "cv", number = 10)

#fit a decision tree model and use k-fold CV to evaluate performance
model<- train(Class~., data = train_set, method = "rpart", trControl = ctrl, tuneLength = 10)

#Step 5: Evaluate - view summary of k-fold CV               
print(model) #metrics give us an idea of how well the model performed on previously unseen data

#view final model
model$finalModel
prp(model$finalModel, box.palette = "Reds", tweak = 1.2) #view the tree using prop() function

#view predictions for each fold
model$resample


test_pred_model <- predict(model, newdata = test_set)

confusionMatrix(test_pred_model, as.factor(test_set$Class) )  #check accuracy

cm <- confusionMatrix(factor(test_pred_model), factor(test_set$Class), dnn = c("Prediction", "Reference"))

plt <- as.data.frame(cm$table)
plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))

ggplot(plt, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction") 



##################################

#prune

z <- prune.rpart (model$finalModel, cp=0.1)
prp(z, box.palette = "Reds", tweak = 1.2)
model$finalModel = z

test_pred1 <- predict(model, newdata = selecteddata)
confusionMatrix(factor(test_pred1), factor(selecteddata$Class) )

