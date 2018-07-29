library(e1071)
library(tidyverse)
library(dplyr)
library(tidyr)
library(readxl)
library(Metrics)
library(ordinalNet)

##SVR middle model and little model

Results <- read_excel("~/Pascal/R/Masterarbeit/ResultsLDT.xls",
                      sheet = "Tabelle1", na = "NA")

Results[Results=='NaN'] <- NA

rowsWithNA <- unique (unlist (lapply (Results, function (x) which (is.na (x)))))

Results <- Results[-rowsWithNA, ]

wholeData <- read_excel("~/Pascal/R/Masterarbeit/allDataWholeData.xls",
                        sheet = "Tabelle1", na = "NA")

frequenciesMW <- read_excel("~/Pascal/R/Masterarbeit/frequencyMW.xlsx",
                            sheet = "Tabelle1", na = "NA")
frequenciesMW <- frequenciesMW[-rowsWithNA, ]

wholeData[wholeData=="-4"] <- 4
wholeData[wholeData=="-3"] <- 3
wholeData[wholeData=="-2"] <- 2
wholeData[wholeData=="-1"] <- 1

meansWholeData <- rowMeans(wholeData[,c(2:99)])
meansWholeData <- as.numeric(meansWholeData)

myX2 <- read_csv("myX2.csv", 
                 col_names = FALSE)
myX2 <- as.matrix(myX2)

testIndex <- seq(351,453,1)

trainSet <- myX2[-testIndex,]
testSet <- myX2[testIndex,]

testSetData <- meansWholeData[testIndex]
testSetData <- as.data.frame(testSetData)

trainSetData <- meansWholeData[-testIndex]
trainSetData <- as.matrix(trainSetData)

##GermaNet and W2V measures

cosine <- frequenciesMW$Typefr.norm.
pathBetween <- Results$Lin
pathRoot <- Results$`Sense Comb`
senseComb <- Results$`Path Root`
li <- Results$`Li, Bandar & McLean`
lee <- Results$Lee

cosine <- as.numeric(cosine)
pathBetween <- as.numeric(pathBetween)
pathRoot <- as.numeric(pathRoot)
senseComb <- as.numeric(senseComb)
li <- as.numeric(li)
lee <- as.numeric(lee)

cosineTrain <- cosine[1:350]
pathBetweenTrain <- pathBetween[1:350]
pathRootTrain <- pathRoot[1:350]
senseCombTrain <- senseComb[1:350]
liTrain <- li[1:350]
leeTrain <- lee[1:350]

cosineTest <- cosine[351:453]
pathBetweenTest <- pathBetween[351:453]
pathRootTest <- pathRoot[351:453]
senseCombTest <- senseComb[351:453]
liTest <- li[351:453]
leeTest <- lee[351:453]

##PCA

prinComp <- prcomp(trainSet, scale. = T)

stdDev <- prinComp$sdev

prVar <- stdDev^2

prVar[1:10]

propVarex <- prVar/sum(prVar)

propVarex[1:20]

plot(propVarex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")

plot(cumsum(propVarex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

newDataTrain<-prinComp$x[,1:175]
newDataTrain <- as.data.frame(newDataTrain)

newDataTest <- predict(prinComp, newdata = testSet)

newDataTest <- newDataTest[,1:175]
newDataTest <- as.data.frame(newDataTest)

newDataTrain$cosineTrain <- cosineTrain
newDataTrain$pathBetweenTrain <- pathBetweenTrain
newDataTrain$pathRootTrain <- pathRootTrain
newDataTrain$senseCombTrain <- senseCombTrain
newDataTrain$liTrain <- liTrain
newDataTrain$leeTrain <- leeTrain

newDataTest$cosineTest <- cosineTest
newDataTest$pathBetweenTest <- pathBetweenTest
newDataTest$pathRootTest <- pathRootTest
newDataTest$senseCombTest <- senseCombTest
newDataTest$liTest <- liTest
newDataTest$leeTest <- leeTest

#Middle Model with prediction

epsilon <- seq(0.02,0.1,0.02)

cost <- seq(1,10,1)

gamma <- seq(0.002,0.02,0.002)

countLoop <- 1

epsilonTable <- 1
costTable <- 1
gammaTable <- 1
nRMSETable <- 1
correlationTable <- 1
supVecTable <- 1

for (i in 1:length(epsilon)) {
  for (j in 1:length(cost)) {
    for (k in 1:length(gamma)) {
      tempEpsilon <- epsilon[i]
      tempCost <- cost[j]
      tempGamma <- gamma[k]
          
      svmPCA = svm(y=trainSetData, x=newDataTrain, kernel="radial", epsilon=tempEpsilon, cost=tempCost, gamma=tempGamma)
      predictedYTune = predict(svmPCA,newdata=newDataTest)
          
      precorMWTune <- cor.test(testSetData$testSetData, predictedYTune)
          
      rMSETune <- rmse(testSetData,predictedYTune)
          
      standardDevTune <- sd(testSetData$testSetData)
          
      nRMSETune=rMSETune/standardDevTune

      nRMSETable[countLoop] <- nRMSETune
      correlationTable[countLoop] <- precorMWTune$estimate
      supVecTable[countLoop] <- svmPCA$tot.nSV
      epsilonTable[countLoop] <- tempEpsilon
      costTable[countLoop] <- tempCost
      gammaTable[countLoop] <- tempGamma
      countLoop <- countLoop +1
      
    }
    
  }
  
}

svmTable <- data.frame(epsilonTable,costTable,gammaTable,nRMSETable,correlationTable,supVecTable)