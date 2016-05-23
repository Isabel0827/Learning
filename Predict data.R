Tdata.train <- as.data.frame(modelData(data.model,
              data.window=c('1970-01-02','1999-12-31')))
Tdata.eval <- na.omit(as.data.frame(modelData(data.model,
               data.window=c('2000-01-01','2009-09-15'))))
Tform <- as.formula('T.ind.GSPC ~.')

#Arifical Neural Networks
set.seed(1234)
library(nnet)
norm.data <- scale(Tdata.train)
nn <- nnet(Tform,norm.data[1:1000,],size=10,decay=0.01,
           maxit=1000,linout=T,trace=F)
norm.preds <- predict(nn,norm.data[1001:2000,])
preds <- unscale(norm.preds,norm.data)

sigs.nn <- trading.signals(preds,0.1,-0.1)
true.sigs <- trading.signals(Tdata.train[1001:2000,"T.ind.GSPC"],
            0.1,-0.1)
sigs.PR(sigs.nn, true.sigs)

signals <- trading.signals(Tdata.train[,"T.ind.GSPC"],0.1,-0.1)
norm.data <- data.frame(signals=signals,scale(Tdata.train[,-1]))
nn <- nnet(signals ~.,norm.data[1:1000,],size=10,decay=0.01,maxit=1000,trace=F)
preds <- predict(nn,norm.data[1001:2000,],type="class")
sigs.PR(preds,norm.data[1001:2000,1])

#Support Vector Machines
library(e1071)
sv <- svm(Tform,Tdata.train[1:1000,],gamma=0.001,cost=100)
s.preds <- predict(sv,Tdata.train[1001:2000,])
sigs.svm <- trading.signals(s.preds,0.1,-0.1)
true.sigs <- trading.signals(Tdata.train[1001:2000,"T.ind.GSPC"],0.1,-0.1)
sigs.PR(sigs.svm,true.sigs)

library(kernlab)
data <- cbind(signals=signals,Tdata.train[,-1])














