##############################################################
#####Predicting the manner in which they did the Exercise#####
##############################################################
##############################################################

##Load in the Data##
pmldata <- read.csv("pml-training.csv")
pmltestdata <- read.csv("pml-testing.csv")


##Clean the data## NOTE: MUST BE RUN IN THIS ORDER
pmldata <- pmldata[,!is.na(pmltestdata[1,])] 
pmltestdata <- pmltestdata[,!is.na(pmltestdata[1,])] #removes missing columns
pmldata <- pmldata[,-c(1,3,4,5)]
pmltestdata <- pmltestdata[,-c(1,3,4,5)] #removes unneccessary columns

#Load Relevant Libraries
library(caret)
library(rattle)
library(ggplot2)

##Create Training and Testing Set
set.seed(313)
inTrain <- createDataPartition(y=pmldata$classe,p=0.7,list=FALSE)
training <- pmldata[inTrain,]
testing <- pmldata[-inTrain,]


##Exploratory Analysis##
summary(training[,c(1,2,3,7,20,33,46,56)])
featurePlot(x=training[,c(7,20,33,46,56)], 
            y=training$classe, 
            plot="pairs")

##Modeling##
modFit1 <- train(classe~.,method="rpart",tuneLength=100,data=training)
modFit1$finalModel
fancyRpartPlot(modFit1$finalModel)
confusionMatrix(predict(modFit1,training),training$classe)


control <- trainControl(method = "repeatedcv",number = 5)
modFit3 <- train(classe~.,method="rf",data=training,trControl=control)
confusionMatrix(predict(modFit3,training),training$classe)
confusionMatrix(predict(modFit3,testing),testing$classe)

##Answers
answers <- predict(modFit3,pmltestdata)

pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
                filename = paste0("problem_id_",i,".txt")
                write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
}

pml_write_files(answers)