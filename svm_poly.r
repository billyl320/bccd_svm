#r simple svm model

rm(list=ls())


library(xtable) #for table creation for latex
library(MASS)#for qda
library(plyr)#for obtaining means by factor
library(e1071)#for svm

#defining proper scientific notation

data = read.table('ultima.csv', sep=',', header=TRUE)

#eis
ei_w = read.table('wbc.txt', sep=',', header=TRUE)
ei_r = read.table('rbc.txt', sep=',', header=TRUE)
ei_p = read.table('plates.txt', sep=',', header=TRUE)

ei = rbind(ei_p, ei_w, ei_r)

labs2<-as.factor(c(rep("Plate", table(data$train)[1]),
                  rep("White", table(data$train)[2]),
                  rep("Red", table(data$train)[3])    ) )

#
#counts plot
temp<-as.data.frame(cbind(ei, data))

#simple model to check if it is even possible to do well with svm
#using all of the data

test<-as.data.frame(cbind(as.factor(temp$train), temp[,-3]))
colnames(test)[1]<-"labs_qda"

keep<-c(1:9)

svmfit=svm(labs_qda ~ .,
       data=test[,keep],
       #kernel='radial',
       kernel="polynomial",
       cost=1, coef0= 2, degree=5)
       #gamma=0.1)

#plot(svmfit, test, Shape_eccent ~ sp)
summary(svmfit)

ypred=predict(svmfit ,test[,keep])
table(predict=ypred, truth=test$labs_qda)
mean(ypred==as.factor(as.numeric(test$labs_qda)))


#now let's tune the svm model using 5-folds on t-set and validaiton

keep1<-which(test$labs_qda==1)
keep2<-which(test$labs_qda==2)
keep3<-which(test$labs_qda==3)

valid_1<-sample(keep1, floor(length(keep1)*0.30) )
valid_2<-sample(keep2, floor(length(keep2)*0.30))
valid_3<-sample(keep3, floor(length(keep3)*0.30) )

valid<-c(valid_1, valid_2, valid_3)

tc <- tune.control(cross = 5)

tune.out<-tune(svm, as.factor(labs_qda) ~.,
          data=test[-valid,keep],
          kernel='polynomial',
          ranges=list(cost=1, coef0=c(1:5, 50), degree=c(2:5)) ,
          #validation.x=valid2[,-1],
          #validation.y=as.factor(valid2[,1]),
          tunecontrol = tc)

summary(tune.out)

ypred=predict(tune.out$best.model ,test[-valid,])
table(predict=ypred, truth=test$labs_qda[-valid])
mean(ypred==as.factor(as.numeric(test$labs_qda[-valid])))

ypred=predict(tune.out$best.model ,test[valid,])
table(predict=ypred, truth=test$labs_qda[valid])
mean(ypred==as.factor(as.numeric(test$labs_qda[valid])))


#now doing it via kfolds and t-set and validation




#
