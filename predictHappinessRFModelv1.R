library(randomForest)
library(caret)
URL<-"A:\\SKS\\R\\AnalyticsVidhya\\One\\"
# place the files in the project root folder

raw.test.data<-read.csv("Test_L4P23N3.csv", header=TRUE)  
raw.train.data<-read.csv("train_FBFog7d.csv", header=TRUE) 
alcohol<-read.csv("NewVariable_Alcohol.csv", header=TRUE) 

raw.train.data<- merge(raw.train.data, alcohol)
raw.test.data<-merge(raw.test.data,alcohol)


test<-raw.test.data
trn<-raw.train.data

levels(trn$Alcohol_Consumption)<-c(1:length(levels(trn$Alcohol_Consumption)))
levels(trn$Var1)<-c(1:length(levels(trn$Var1)))
levels(trn$WorkStatus)<-c(1:length(levels(trn$WorkStatus)))
#levels(trn$Score)<-c(1:length(levels(trn$Score)))
levels(trn$Divorce)<-c(1:length(levels(trn$Divorce)))
levels(trn$Widowed)<-c(1:length(levels(trn$Widowed)))
trn$Education<-as.factor(trn$Education)
levels(trn$Education)<-c(1:length(levels(trn$Education)))
levels(trn$Residence_Region)<-c(1:length(levels(trn$Residence_Region)))
trn$babies<-as.factor(trn$babies)
trn$preteen<-as.factor(trn$preteen)
trn$teens<-as.factor(trn$teens)
levels(trn$babies)<-c(1:length(levels(trn$babies)))
levels(trn$preteen)<-c(1:length(levels(trn$preteen)))
levels(trn$teens)<-c(1:length(levels(trn$teens)))
trn$Var2  <-as.factor(trn$Var2)
trn$Gender <-as.factor(trn$Gender)
trn$Unemployed10 <-as.factor(trn$Unemployed10)
levels(trn$income)<-c(1:length(levels(trn$income)))
levels(trn$Engagement_Religion)<-c(1:length(levels(trn$Engagement_Religion)))
levels(trn$Var2)<-c(1:length(levels(trn$Var2)))
#levels(trn$TVhours)<-c(1:length(levels(trn$TVhours)))
levels(trn$Gender)<-c(1:length(levels(trn$Gender)))
levels(trn$Unemployed10)<-c(1:length(levels(trn$Unemployed10)))
#trn$TVhours<-as.numeric(trn$TVhours)


#test

levels(test$Alcohol_Consumption)<-c(1:length(levels(test$Alcohol_Consumption)))

levels(test$Var1)<-c(1:length(levels(test$Var1)))
levels(test$WorkStatus)<-c(1:length(levels(test$WorkStatus)))
#levels(test$Score)<-c(1:length(levels(test$Score)))
levels(test$Divorce)<-c(1:length(levels(test$Divorce)))
levels(test$Widowed)<-c(1:length(levels(test$Widowed)))
test$Education<-as.factor(test$Education)
levels(test$Education)<-c(1:length(levels(test$Education)))

levels(test$Residence_Region)<-c(1:length(levels(test$Residence_Region)))
test$babies<-as.factor(test$babies)
test$preteen<-as.factor(test$preteen)
test$teens<-as.factor(test$teens)
test$Var2  <-as.factor(test$Var2)
test$Gender <-as.factor(test$Gender)
test$Unemployed10 <-as.factor(test$Unemployed10)
levels(test$babies)<-c(1:length(levels(test$babies)))
levels(test$preteen)<-c(1:length(levels(test$preteen)))
levels(test$teens)<-c(1:length(levels(test$teens)))
levels(test$income)<-c(1:length(levels(test$income)))
levels(test$Engagement_Religion)<-c(1:length(levels(test$Engagement_Religion)))
levels(test$Var2)<-c(1:length(levels(test$Var2)))
#levels(test$TVhours)<-c(1:length(levels(test$TVhours)))
levels(test$Gender)<-c(1:length(levels(test$Gender)))
levels(test$Unemployed10)<-c(1:length(levels(trn$Unemployed10)))
#test$TVhours<-as.numeric(test$TVhours)

test$babies<-as.integer(test$babies)
test$teens<-as.integer(test$teens)
test[test$teens==6&!is.na(test$teens),]$teens<-5
test[test$babies==6&!is.na(test$babies),]$babies<-5
test$babies<-as.factor(test$babies)
test$teens<-as.factor(test$teens)

x<-NULL
for (i in 1:ncol(raw.train.data) ) x<-
  c(x,length(which(is.na
                   (raw.train.data[,i])==TRUE)))

y<-NULL
for (i in 1:ncol(raw.test.data) ) y<-
  c(y,length(which(is.na
                   (raw.test.data[,i])==TRUE)))
x<-round(x/nrow(raw.train.data)*100)

y<-round(y/nrow(raw.test.data)*100)


test1<-test[,y<30]
trn1<-trn[,x<30]
trn1<-trn1[,c(1:13,15,14)]
trn1<-trn1[,c(2:13,15,14)]
test1<-na.roughfix(test1)
trn1<-trn1[complete.cases(trn1),]

cv.index<-createDataPartition(y=trn1$Happy,p=0.75,list=FALSE)
cv.trn<-trn1[cv.index,]
cv.test<-trn1[-cv.index,]

folds.trn <- createFolds(y=cv.trn$Happy,k=10,list=TRUE,returnTrain=TRUE)
folds.test <- createFolds(y=cv.trn$Happy,k=10,list=TRUE,returnTrain=TRUE)

mod.rfcv<-randomForest(formula = Happy ~ ., data = cv.trn[folds.trn[[2]], ], method = "class")


mod.rfvv1<-train(Happy~.,method='rf',data=trn1[,-1])

predict.happy<-predict(mod.rfvv1,test1[,-1])

write.csv(data.frame(ID=test1[,1],Happy=predict.happy),"rfvvv1.csv", row.names = FALSE)



###
mod.rf.2<-randomForest(Happy ~., method = "class", data=d[folds.trn[[5]],])

predict.happy<-predict(mod.rf.2,dtest[,-1])

write.csv(data.frame(ID=dtest[,1],Happy=predict.happy),"4.csv", row.names = FALSE)