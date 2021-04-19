#r simple svm model

rm(list=ls())


library(xtable) #for table creation for latex
library(MASS)#for qda
library(plyr)#for obtaining means by factor
library(e1071)#for svm
library(scatterplot3d)#for 3d plot in r
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


labs2<-as.factor(c(rep("WBC", dim(w)[1]),
                  rep("RBC", dim(r)[1]),
                  rep("Platelet", dim(p)[1])    ) )

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
          kernel='linear',
          ranges=list(cost=c(1, 1.01, 0.75, 0.50, 1.50) ) ,
          tunecontrol = tc,
          scale=TRUE)

summary(tune.out)

ypred=predict(tune.out$best.model ,test[-valid,])
table(predict=ypred, truth=test$labs_svm[-valid])
mean(ypred==as.factor(as.numeric(test$labs_svm[-valid])))

ypred=predict(tune.out$best.model ,test[valid,])
table(predict=ypred, truth=test$labs_svm[valid])
mean(ypred==as.factor(as.numeric(test$labs_svm[valid])))

#calculating varibale importance
w <- t(tune.out$best.model$coefs) %*% tune.out$best.model$SV    # weight vectors
w <- apply(w, 2, function(v){sqrt(sum(v^2))})                   # weight
w <- sort(w, decreasing = T)

#max weight
w_max<-(w[1])

#normalized weights relative to the max
w_norm<- w / w_max

#table for Latex
xtable(as.matrix(w_norm), digits=3)

#checking all above 0.90
keep3<-c(1, 2, 7, 9)

tune.out3<-tune(svm, as.factor(labs_svm) ~.,
          data=test[-valid,keep],
          kernel='linear',
          ranges=list(cost=c(1, 1.01, 0.75, 0.50, 1.50) ) ,
          tunecontrol = tc,
          scale=TRUE)

summary(tune.out3)

ypred=predict(tune.out3$best.model ,test[-valid,])
table(predict=ypred, truth=test$labs_svm[-valid])
mean(ypred==as.factor(as.numeric(test$labs_svm[-valid])))

ypred=predict(tune.out3$best.model ,test[valid,])
table(predict=ypred, truth=test$labs_svm[valid])
mean(ypred==as.factor(as.numeric(test$labs_svm[valid])))


fit3<-svm(as.factor(labs_svm) ~.,
          data=test[-valid,keep3],
          kernel='linear',
          cost=c(1))


summary(fit3)

ypred=predict(fit3 ,test[-valid,])
table(predict=ypred, truth=test$labs_svm[-valid])
mean(ypred==as.factor(as.numeric(test$labs_svm[-valid])))

ypred=predict(fit3 ,test[valid,])
table(predict=ypred, truth=test$labs_svm[valid])
mean(ypred==as.factor(as.numeric(test$labs_svm[valid])))

#
fit3<-tune(svm, as.factor(labs_svm) ~.,
          data=test[-valid,keep3],
          kernel='linear',
          ranges=list(cost=c(1, 1.01, 0.75, 0.50, 1.50) ) ,
          tunecontrol = tc,
          scale=TRUE)

#
ypred=predict(fit3$best.model ,test[-valid,])
table(predict=ypred, truth=test$labs_svm[-valid])
mean(ypred==as.factor(as.numeric(test$labs_svm[-valid])))

ypred=predict(fit3$best.model ,test[valid,])
table(predict=ypred, truth=test$labs_svm[valid])
mean(ypred==as.factor(as.numeric(test$labs_svm[valid])))



#checking all above 0.84
keep4<-c(1, 4, 7, 9)

tune.out4<-tune(svm, as.factor(labs_svm) ~.,
          data=test[-valid,keep4],
          kernel='linear',
          ranges=list(cost=c(1, 1.01, 0.75, 0.50, 1.50) ) ,
          tunecontrol = tc,
          scale=TRUE)

summary(tune.out4)

ypred=predict(tune.out4$best.model ,test[-valid,])
table(predict=ypred, truth=test$labs_svm[-valid])
mean(ypred==as.factor(as.numeric(test$labs_svm[-valid])))

ypred=predict(tune.out4$best.model ,test[valid,])
table(predict=ypred, truth=test$labs_svm[valid])
mean(ypred==as.factor(as.numeric(test$labs_svm[valid])))


colors <- c("#999999", "#E69F00", "#56B4E9")
colors <- colors[as.numeric(test$labs_svm)]
shapes = c(16, 17, 18)
shapes <- shapes[as.numeric(test$labs_svm)]

s3d<-scatterplot3d(test$sp, test$Shape_e1, test$Shape_corn,
              main="3D Scatterplot of Blood Cells",
              xlab="SP",
              ylab="1st Eigenvalue",
              zlab="# Corners",
              pch=shapes,
              color=colors)
legend(s3d$xyz.convert(0.1, 5000, 225), legend = levels(labs2),
      col =  c("#999999", "#E69F00", "#56B4E9"), pch = c(16, 17, 18))

#
s3d<-scatterplot3d(test$black, test$Shape_e1, test$Shape_corn,
              main="3D Scatterplot of Blood Cells",
              xlab="Black EI",
              ylab="1st Eigenvalue",
              zlab="# Corners",
              pch=shapes,
              color=colors,
              angle=150)
legend(s3d$xyz.convert(66500, 5000, 225), legend = levels(labs2),
      col =  c("#999999", "#E69F00", "#56B4E9"), pch = c(16, 17, 18))



plot(test[,keep4],
     col=colors,
     pch=shapes,
     labels=c("Blood Type", "SP", "Black EI","1st Eigenvalue", "# Corners"))

#
