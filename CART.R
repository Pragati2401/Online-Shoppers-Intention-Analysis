require(tidymodels)
require(dplyr)
require(fastDummies)

rm(list=ls()); gc()

#load data
online_Shopping = read.csv("/Users/Cleaned_online_shoppers_intention.csv", stringsAsFactors=F, head=T)
str(online_Shopping)
summary(online_Shopping)
online_Shopping =dummy_cols(online_Shopping, select_columns =  NULL,remove_most_frequent_dummy = T,remove_selected_columns = T)                                                                           

#data split
online_Shopping<- online_Shopping%>% select(-X)
set.seed(574)
n.train = floor( nrow(online_Shopping)*0.75 )
shopping.train = sample(1:nrow(online_Shopping), n.train)
shopping.test = setdiff(1:nrow(online_Shopping), shopping.train)

train = online_Shopping[shopping.train,]
test = online_Shopping[shopping.test,]
str(test)
str(train)


library(rpart); library(rpart.plot)

require("rpart.plot")
#install.packages("rpart")
require("rpart")


library(caret)
library(ISLR)
#under_train$six_months_OD1<- under_train$six_months_OD1

######full tree
fit= rpart(Revenue_True ~ ., method="class", data=train, minsplit=10,control=rpart.control(cp=.005))
prp(fit, main="FUll Tree")


###Full Tree charts and error
fit$variable.importance
printcp(fit)
fit$cptable
rpart.plot(fit, main="FUll Tree")
varImpPlot(fit)
varImp(fit)
plotcp(fit)

######## predict full with test
predict_under_train_full= predict(fit,test,type="class")
confusionMatrix(predict_under_train_full,as.factor(test$Revenue))


#####prune min error
pfit.me = prune(fit, cp = fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
pfit.me = prune(fit, cp = fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

plotcp(pfit.me)

rpart.plot(pfit.me, main = 'Min Error Tree')
pfit.me$cptable
prp(pfit.me,  main = 'Min Error Tree')
printcp(pfit.me)
##########Min error Predict traing set
predict_under_train_min= predict(pfit.me,train,type="class")
confusionMatrix(predict_under_train_min,as.factor(train$Revenue_True),)
nj 
##########Min error Predict test
predict_under_test_min= predict(pfit.me,test,type="class")
prp(pfit.me,  main = 'Min Error Tree')

hist(predict_under_test_min)
confusionMatrix(predict_under_test_min,as.factor(test$Revenue_True))
x=printcp(pfit.me)
plot(pfit.me$variable.importance)

#####prob .8
 prob.me = predict(pfit.me, test, type = "prob")[,2]
  pred.class.me = as.numeric(prob.me > .8)
  ytest = as.numeric(test$Revenue_True)  # Be careful! Check the variable type of your outcome
  err.bp.newCut = mean(pred.class.me != ytest)-1
  err.bp.newCut
  hist(prob.me)
   table(pred.class.me,ytest)
 
  confusionMatrix(as.factor(pred.class.me), as.factor(ytest))


######## best pruned tree
k<-20
ind = which.min(fit$cptable[,"xerror"]) # xerror: cross-validation error
se1 = fit$cptable[ind,"xstd"]/sqrt(k) # 1 standard error
xer1 = min(fit$cptable[,"xerror"]) + se1 # targeted error: min + 1 SE
ind0 = which.min(abs(fit$cptable[1:ind,"xerror"] - xer1)) # select the tree giving closest xerror to xer1
pfit.bp = prune(fit, cp = fit$cptable[ind0,"CP"])
rpart.plot(pfit.bp, main = 'Best Pruned Tree')
summary(fit)
pfit.bp$cptable
pfit.bp$variable.importance
plotcp(pfit.bp, main="Cp Plot Best Pruned Tree")

prp(pfit.bp)
#######predict best pruned with training
predict_under_train_best= predict(pfit.bp,train,type="class")
confusionMatrix(predict_under_train_best,as.factor(train$Revenue_True))
printcp(pfit.bp)

#######predict best pruned with testing
predict_under_test_best= predict(pfit.bp,test,type="class")
confusionMatrix(predict_under_test_best,as.factor(test$Revenue))
 printcp(pfit.bp)

##### .8 bp

prob.bp = predict(pfit.bp, test, type = "prob")[,2]
pred.class.bp = as.numeric(prob.bp > .8)
ytest = as.numeric(test$Revenue_True)  # Be careful! Check the variable type of your outcome
err.bp.newCut = mean(pred.class.bp != ytest)-1
err.bp.newCut

###bp .8 confusion matrix and plot of tree
confusionMatrix(as.factor(pred.class.bp), as.factor(test$Revenue_True))
prp(pfit.bp)


####random forecast
require("randomForest")
str(train)

####RF Tree
rf<-  randomForest(as.factor(Revenue_True)~., data=train, ntree=500, mtry=4, nodesize=5, importance=TRUE,type="class",proximity=TRUE )

###rf predict and cp, variable importance
rf.pred<- predict(rf,test)
confusionMatrix(rf.pred,as.factor(test$Revenue_True))
varImpPlot(rf)

varImp(rf)
summary(rf)

###RF 50% probability confusion matrix
predict_under_test_best= predict(rf,test,type="class")
confusionMatrix(predict_under_test_best,as.factor(test$Revenue))
importance(rf)

 

### cutoff .8
str(test)
prob.rf = predict(rf, test, type = "prob")[,2]
pred.class.rf = as.numeric(prob.rf > .8)
ytest = as.numeric(test$Revenue_True)  # Be careful! Check the variable type of your outcome
err.rf.newCut = mean(pred.class.rf != ytest)-1
err.rf.newCut
confusionMatrix(as.factor(pred.class.rf), as.factor(ytest))
plot(rf$confusion)

#############boosted tree
require("adabag")
require("caret")
require("rpart")
train$Revenue_True<- as.factor(train$Revenue_True)
test$Revenue_True<- as.factor(test$Revenue_True)

boosted_tree<-boosting(Revenue_True ~ ., data= train)
pred.boosted<-predict(boosted_tree,test)
boosted_tree$weights
confusionMatrix(as.factor(pred.boosted$class),as.factor(test$Revenue_True))

boosted_tree$importance


