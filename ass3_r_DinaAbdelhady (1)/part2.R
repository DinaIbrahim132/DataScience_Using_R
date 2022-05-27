# Read the Data
data <- read.csv("customer_churn.csv",header=TRUE);
data=na.omit(data)
data<- data[ -c(1) ]


# Holdout method
# using random IDs
random_ids <- order(runif(nrow(data)))
data_train <- data[random_ids[1:ceiling(nrow(data)*0.67)],]
data_test <- data[random_ids[(ceiling(nrow(data)*0.67)+1):nrow(data)], ]

#draw train vs test
count<-c(nrow(data_train),nrow(data_test))
ratio<-c(paste(as.integer(nrow(data_train)/nrow(data)*100),"%"),paste(ceiling(nrow(data_test)/nrow(data)*100),"%") )

bar <- barplot(count,
        main="train vs test",
        xlab=c('train','test'),
        ylab="Count",
        names.arg = c("Train", "Test")
)

text(x=bar,label=ratio, pos=3)

#____________________________________

nrow(data_train)


res<-which(data_train$Churn == 'Yes')
length(res)

resno<-which(data_train$Churn == 'No')
length(resno)

yes=(length(res)/nrow(data_train)) *100
yes


count<-c(length(res),(nrow(data_train)-length(res)))
ratio<-c(paste(as.integer(yes),"%"),paste(ceiling((length(resno)/nrow(data_train)) *100),"%") )

bar <- barplot(count,
               main="imbalance data_train ",
               ylab="Count",
               names.arg = c("yes", "no")
)

text(x=bar,label=ratio, pos=3)

new_yes=(nrow(data_train)*30)/100
new_yes


add_yes=abs(ceiling(new_yes)-length(res)) 
add_yes

resadd<-which(data_test$Churn == 'Yes')
length(resadd)

index = sample( x=resadd, size = add_yes )
length(index)

data_train=rbind(data_train,data_test[index,])

data_test=data_test[-index,]

resno<-which(data_train$Churn == 'No')
length(resno)

indexno = sample( x=resno, size = add_yes )
length(indexno)

data_train=data_train[-indexno,]

res<-which(data_train$Churn == 'Yes')
length(res)

resno<-which(data_train$Churn == 'No')
length(resno)

yes=(length(res)/nrow(data_train)) *100
yes


count<-c(length(res),(nrow(data_train)-length(res)))
ratio<-c(paste(as.integer(yes),"%"),paste(ceiling((length(resno)/nrow(data_train)) *100),"%") )

bar <- barplot(count,
               main="imbalance data_train ",
               ylab="Count",
               names.arg = c("yes", "no")
)

text(x=bar,label=ratio, pos=3)

#___________________________________

library(caTools)
library(rpart)
library(rpart.plot)
library(caret)
library(dplyr)
install.packages("corrplot")
library(corrplot)
install.packages('e1071', dependencies=TRUE)


data[1:20] <- lapply(data[1:20], factor)
data[1:20] <- lapply(data[1:20], as.numeric)


str(data)
corrplot(cor(data,method = "pearson"),diag = FALSE,method = "ellipse",tl.cex = 0.7, tl.col = "black", cl.ratio = 0.2)


model <- rpart(Churn ~ ., data =data_train,method = "class" )

#plot the model
rpart.plot(model)

#Make predictions
preds <- predict(model ,data_test, type = "class") #use the predict() function and pass in the testing subset





#Print the confusion Matrix

cm <-confusionMatrix(as.factor(data_test$Churn), preds)
cm

plt <- as.data.frame(cm$table)
plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))

ggplot(plt, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction") 

#_____________________________________
library(randomForest)


data_train[1:20] <- lapply(data_train[1:20], factor)
data_train[1:20] <- lapply(data_train[1:20], as.numeric)


str(data_train)

rfModel <- randomForest(Churn  ~., data = data_train )
print(rfModel)
plot(rfModel)

data_test[1:20] <- lapply(data_test[1:20], factor)
data_test[1:20] <- lapply(data_test[1:20], as.numeric)


pred_rf <- predict(rfModel, data_test)
pred_rf
pred_rf <- ifelse(pred_rf>=1.5, 2, 1)


cm <-confusionMatrix(as.factor(data_test$Churn), as.factor(pred_rf))
cm

plt <- as.data.frame(cm$table)
plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))

ggplot(plt, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#850094") +
  labs(x = "Reference",y = "Prediction") 

#___________________________________________________________


data_train[1:20] <- lapply(data_train[1:20], factor)
data_train[1:20] <- lapply(data_train[1:20], as.numeric)


rfModel2 <- randomForest(Churn  ~., data = data_train ,nodesize = 15,maxnodes = 3)
print(rfModel2)
plot(rfModel2)



pred_rf2 <- predict(rfModel2, data_test)
pred_rf2 <- ifelse(pred_rf>=1.5, 2, 1)


cm <-confusionMatrix(as.factor(data_test$Churn), as.factor(pred_rf2))
cm

plt <- as.data.frame(cm$table)
plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))

ggplot(plt, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#f8fc0d") +
  labs(x = "Reference",y = "Prediction") 
#_______________________________________________________________


library(caret)
ctrl <- trainControl(method = "repeatedcv",
                     number = 10, repeats = 10,
                     selectionFunction = "best",
                     savePredictions = TRUE,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)

# auto-tune a random forest
grid_rf <- expand.grid(mtry = c(2, 4, 8, 16))

# test a random forest with the above settings
# note: this may take a long time to run (~10 minutes)
set.seed(300)
m_rf <- train(Churn ~ ., data = data_train, method = "rf",
              metric = "ROC", trControl = ctrl,
              nodesize = 5,maxnodes = 5, 
              tuneGrid = grid_rf)
m_rf
plot(m_rf)
# auto-tune a boosted C5.0 decision tree
grid_c50 <- expand.grid(model = "tree",
                        trials = c(10, 25, 50, 100),
                        winnow = FALSE)


set.seed(300)
m_c50 <- train(Churn ~ ., data = data_train, method = "C5.0",
               metric = "ROC", trControl = ctrl,
               tuneGrid = grid_c50)
m_c50

# compare their ROC curves
library(pROC)
roc_rf <- roc(m_rf$pred$obs, m_rf$pred$yes)
roc_c50 <- roc(m_c50$pred$obs, m_c50$pred$yes)

plot(roc_rf, col = "red", legacy.axes = TRUE)
plot(roc_c50, col = "blue", add = TRUE)
