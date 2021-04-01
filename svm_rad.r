#r simple svm model

rm(list=ls())


library(xtable) #for table creation for latex
library(MASS)#for qda
library(plyr)#for obtaining means by factor
library(e1071)#for svm

#setting seed
set.seed(76976)

w = read.table('wbc_SHAPES.txt', sep=',', header=TRUE)
r = read.table('rbc_SHAPES.txt', sep=',', header=TRUE)
p = read.table('plates_SHAPES.txt', sep=',', header=TRUE)

data<-rbind(w, r, p)

#eis
ei_w = read.table('wbc.txt', sep=',', header=TRUE)
ei_r = read.table('rbc.txt', sep=',', header=TRUE)
ei_p = read.table('plates.txt', sep=',', header=TRUE)

ei = rbind(ei_p, ei_w, ei_r)
sp = ei[,1]/(ei[,1]+ei[,2])

labs<-as.factor(c(rep(1, dim(w)[1]),
                  rep(2, dim(r)[1]),
                  rep(3, dim(p)[1])    ) )


labs2<-as.factor(c(rep("Plate", dim(w)[1]),
                  rep("White", dim(r)[1]),
                  rep("Red", dim(p)[1])    ) )

#counts plot
temp<-as.data.frame(cbind(sp, ei, data))

#simple model to check if it is even possible to do well with svm
#using all of the data

test<-as.data.frame(cbind(as.factor(labs), temp))
colnames(test)[1]<-"labs_svm"

keep<-c(1:9)

#now let's tune the svm model using 5-folds on t-set and validaiton

keep1<-which(test$labs_svm==1)
keep2<-which(test$labs_svm==2)
keep3<-which(test$labs_svm==3)

valid_1<-sample(keep1, floor(length(keep1)*0.30) )
valid_2<-sample(keep2, floor(length(keep2)*0.30))
valid_3<-sample(keep3, floor(length(keep3)*0.30) )

valid<-c(valid_1, valid_2, valid_3)

tc <- tune.control(cross = 5)

tune.out<-tune(svm, as.factor(labs_svm) ~.,
          data=test[-valid,keep],
          kernel='radial',
          ranges=list(cost=c(1, 1.01, 0.75, 0.50, 1.50),
                      gamma=c(0.3, 0.305, 0.350, 0.375,
                              0.31, 0.32, 0.33, 0.3305,
                              0.3350, 0.3375, 0.3295, 0.3275, 0.34)) ,
          tunecontrol = tc,
          scale=TRUE)

summary(tune.out)

ypred=predict(tune.out$best.model ,test[-valid,])
table(predict=ypred, truth=test$labs_svm[-valid])
mean(ypred==as.factor(as.numeric(test$labs_svm[-valid])))

ypred=predict(tune.out$best.model ,test[valid,])
table(predict=ypred, truth=test$labs_svm[valid])
mean(ypred==as.factor(as.numeric(test$labs_svm[valid])))


#
