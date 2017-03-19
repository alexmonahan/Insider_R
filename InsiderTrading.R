#this code will analyze data for insider trading trends...
library(MASS) #wilcoxon signed rank test

setwd("/Users/Alex")
require(lmtest)
data = read.csv('OilInsider.csv')
#print(lapply(data, class))

X <- cbind(data)
X$oilDelta <- 0
X$oilPriceDelta <- 0
print(head(X))
for (i in 1:(length(X$Date1)-1)){
  X[i+1, "oilDelta"] = X[i+1, "RigCount"] - X [i, "RigCount"]
  X[i, "oilPriceDelta"] = X[i, "oilFri"] - X[i, "oilThursday"]
}
print(head(X$oilDelta[X$oilDelta > 0], 40))
#X <- X[!is.na(X)]
X<- X[-seq(nrow(X),nrow(X)-4),]

#print(head(X$oilPriceDelta[X$oilDelta > 0]))

#X = X[, -which(names(X) %in% c("RigCount","X0", "DATE2", "oilThursdays", "DATE3", "oilFri"))]
#print(head(X))

#mean(X$oilPriceDelta[X$oilDelta > 0])
#mean(X$oilPriceDelta[X$oilDelta < 0])
wilcox.test(X$oilPriceDelta[X$oilDelta > 0], X$oilPriceDelta[X$oilDelta < 0], paired=FALSE) 
