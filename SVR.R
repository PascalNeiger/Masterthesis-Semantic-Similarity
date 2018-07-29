##SVR Implemmentation  https://docs.id.unibe.ch/ubelix/software/r Infos für R auf Ubelix

library(e1071)
library(tidyverse)
library(dplyr)
library(tidyr)
library(readxl)
library(Metrics)

##SVR middle model and little model

Results <- read_excel("~/Pascal/R/Masterarbeit/ResultsLDT.xls",
                      sheet = "Tabelle1", na = "NA")

Results[Results=='NaN'] <- NA

rowsWithNA <- unique (unlist (lapply (Results, function (x) which (is.na (x)))))

Results <- Results[-rowsWithNA, ]

frequenciesMW <- read_excel("~/Pascal/R/Masterarbeit/frequencyMW.xlsx",
                            sheet = "Tabelle1", na = "NA")
frequenciesMW <- frequenciesMW[-rowsWithNA, ]

wholeData <- read_excel("~/Pascal/R/Masterarbeit/allDataWholeData.xls",
                        sheet = "Tabelle1", na = "NA")


wholeData[wholeData=="-4"] <- 4
wholeData[wholeData=="-3"] <- 3
wholeData[wholeData=="-2"] <- 2
wholeData[wholeData=="-1"] <- 1

meansWholeData <- rowMeans(wholeData[,c(2:99)])
medianWholeData <- apply(wholeData[,c(2:99)],1,mean)
sdWholeData <- apply(wholeData[,c(2:99)],1,sd)
interQuartileRange <- apply(wholeData[,c(2:99)],1,IQR)

descreptiveStatsData <- data.frame(meansWholeData,sdWholeData)

meansWholeData <- as.data.frame(meansWholeData)

LeeData <- Results$Lee
LeeData <- as.numeric(LeeData)

myX2 <- read_csv("myX2.csv", 
                col_names = FALSE)
myX2 <- as.matrix(myX2)

#Middle Model with prediction

cosine <- Results$Lee
pathBetween <- Results$Lin
pathRoot <- Results$`Sense Comb`
senseComb <- Results$`Path Root`
li <- Results$`Li, Bandar & McLean`
lee <- frequenciesMW$Typefr.norm.

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

myXTrain <- data.frame(cosineTrain,pathBetweenTrain,pathRootTrain,senseCombTrain,liTrain,leeTrain)
myXTrain$cosineTrain <- as.numeric(myXTrain$cosineTrain)
myXTrain$pathBetweenTrain <- as.numeric(myXTrain$pathBetweenTrain)
myXTrain$pathRootTrain <- as.numeric(myXTrain$pathRootTrain)
myXTrain$senseCombTrain <- as.numeric(myXTrain$senseCombTrain)
myXTrain$liTrain <- as.numeric(myXTrain$liTrain)

myXTest <- data.frame(cosineTest,pathBetweenTest,pathRootTest,senseCombTest,liTest,leeTest)
myXTest$cosineTest <- as.numeric(myXTest$cosineTest)
myXTest$pathBetweenTest <- as.numeric(myXTest$pathBetweenTest)
myXTest$pathRootTest <- as.numeric(myXTest$pathRootTest)
myXTest$senseCombTest <- as.numeric(myXTest$senseCombTest)
myXTest$liTest <- as.numeric(myXTest$liTest)

testSetData <- meansWholeData[351:453]
testSetData <- as.data.frame(testSetData)

testSetVectors <- myX2[351:453,]
testSetVectors <- as.data.frame(testSetVectors)

trainSetData <- meansWholeData[1:350]
trainSetData <- as.matrix(trainSetData)

trainSetVectors <- myX2[1:350,]
trainSetVectors <- as.data.frame(trainSetVectors)

##Parameter Tuning

epsilon <- seq(0.1,2,0.1)

cost <- seq(1,20,1)

gamma <- seq(0.01,0.2,0.01)

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
      
      model2 = svm(y=trainSetData, x=myXTrain, kernel="radial", epsilon=tempEpsilon, cost=tempCost, gamma=tempGamma)
      
      predictedYTune = predict(model2,newdata=myXTest)
      
      precorMWTune <- cor.test(testSetData$testSetData, predictedYTune)
      
      rMSETune <- rmse(testSetData,predictedYTune)
      
      standardDevTune <- sd(testSetData$testSetData)
      
      nRMSETune=rMSETune/standardDevTune
      
      epsilonTable[countLoop] <- tempEpsilon
      costTable[countLoop] <- tempCost
      gammaTable[countLoop] <- tempGamma
      nRMSETable[countLoop] <- nRMSETune
      correlationTable[countLoop] <- precorMWTune$estimate
      supVecTable[countLoop] <- model2$tot.nSV
      countLoop <- countLoop +1
    }
    
  }
  
}

svmTable <- data.frame(epsilonTable,costTable,gammaTable,nRMSETable,correlationTable,supVecTable)