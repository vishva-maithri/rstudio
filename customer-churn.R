customerAllData = read.csv("C:/Users/VMAITVI/Desktop/MBA/DPA/Customer Data.csv", header = TRUE)

#filter data with appropriate fields
customerAllData.sub<-customerAllData[c('tenure','InternetService','OnlineSecurity','OnlineBackup','StreamingTV','StreamingMovies','Contract','MonthlyCharges','TotalCharges','Churn')]
summary(customerAllData.sub)

#load caTools library
library(caTools)

#70:30 data split
sample.split(customerAllData.sub$Churn,SplitRatio = 0.70)->splitValues

#assign splitted data to trainingSet and testDataSet
subset(customerAllData.sub,splitValues==T)->trainingSet
subset(customerAllData.sub,splitValues==F)->testDataSet

#load rpart library
library(rpart)

#classification model
rpart(Churn~.,data=trainingSet)->actual
predict(modalClass,testDataSet,type="class")->prediction
table(testDataSet$Churn,prediction)

#Accuracy check
(1467+221)/(1467+221+340+85)

#Confusion Matrix Visualization
Prediction <- factor(c(0, 0, 1, 1))
Actual <- factor(c(0, 1, 0, 1))
Y      <- c(1467, 340, 85, 221)
df <- data.frame(TClass, PClass, Y)

library(ggplot2)
ggplot(data =  df, mapping = aes(x = Prediction, y = Actual)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_bw() + theme(legend.position = "none")