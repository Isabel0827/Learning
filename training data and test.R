data(GSPC)
library(randomForest)
data.model <- specifyModel(T.ind(GSPC) ~ Delt(Cl(GSPC),k=1:10)+
                myATR(GSPC)+mySMI(GSPC)+myADX(GSPC)+myAroon(GSPC)+
                myBB(GSPC)+myChaikinVol(GSPC)+myCLV(GSPC)+
                CMO(Cl(GSPC))+EMA(Delt(Cl(GSPC)))+myEMV(GSPC)+
                myVolat(GSPC)+myMACD(GSPC)+myMFI(GSPC)+RSI(Cl(GSPC))+
                mySAR(GSPC)+runMean(Cl(GSPC))+runSD(Cl(GSPC)))

set.seed(1234)
rf <- buildModel(data.model,method='randomForest',
                 training.per=c(start(GSPC),index(GSPC["1999-12-31"])),
                 ntree=50,importance=T)

varImpPlot(rf@fitted.model, type=1)

#ex.model <- specifyModel(T.ind(IBM) ~ Delt(Cl(IBM),k=1:3))
#data <- modelData(ex.model,data.window=c("2009-01-01","2009-08-10"))
#m <- myFavouriteModellingTool(ex.model@model.formula,as.data.frame(data))

imp <- importance(rf@fitted.model, type = 1)
rownames(imp)[which(imp>10)]

