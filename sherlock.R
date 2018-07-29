library(e1071)
library(tidyverse)
library(dplyr)
library(tidyr)
library(readxl)
library(Metrics)

Results <- read_excel("~/Pascal/R/Masterarbeit/Results.xls",
                      sheet = "Tabelle1", na = "NA")

Results[Results=='NaN'] <- NA

rowsWithNA <- unique (unlist (lapply (Results, function (x) which (is.na (x)))))

Results <- Results[-rowsWithNA, ]

wholeData <- read_excel("~/Pascal/R/Masterarbeit/allDataWholeData.xls",
                        sheet = "Tabelle1", na = "NA")

frequenciesMW <- read_excel("~/Pascal/R/Masterarbeit/frequencyMW.xlsx",
                            sheet = "Tabelle1", na = "NA")
frequenciesMW <- frequenciesMW[-rowsWithNA, ]

frequenciesBig <- read_excel("~/Pascal/R/Masterarbeit/frequencyBig.xlsx",
                             sheet = "Tabelle1", na = "NA")
frequenciesBig <- frequenciesBig[-rowsWithNA, ]


wholeData[wholeData=="-4"] <- 4
wholeData[wholeData=="-3"] <- 3
wholeData[wholeData=="-2"] <- 2
wholeData[wholeData=="-1"] <- 1

meansWholeData <- rowMeans(wholeData[,c(2:99)])
medianWholeData <- apply(wholeData[,c(2:99)],1,median)
sdWholeData <- apply(wholeData[,c(2:99)],1,sd)
interQuartileRange <- apply(wholeData[,c(2:99)],1,IQR)

descreptiveStatsData <- data.frame(meansWholeData,sdWholeData,medianWholeData,interQuartileRange)

meansWholeData <- as.data.frame(meansWholeData)

LeeData <- Results$Lee
LeeData <- as.numeric(LeeData)

typeNormMW <- frequenciesMW$Typefr.norm.


##Item tests

senseCombs <- Results$`Sense Comb`
senseCombs <- as.numeric(senseCombs)
medianSenses <- median(senseCombs)
interQuartileRangeSenseComb <- IQR(senseCombs, na.rm = FALSE, type = 7)

senseCombs[senseCombs>14.5] <- NA
rowsWithNA2 <- which(is.na(senseCombs))
senseCombs <- senseCombs[-rowsWithNA2]

meansData <- as.matrix(meansWholeData)
meansData <- as.numeric(meansData)
meansData <- meansData[-rowsWithNA2]

LeeData <- Results$Lee
LeeData <- as.numeric(LeeData)

Results <- Results[-rowsWithNA2, ]
LeeData <- LeeData[-rowsWithNA2]
descreptiveStatsData <- descreptiveStatsData[-rowsWithNA2,]
descreptiveStats2 <- descreptiveStatsData

# meansData <- medianWholeData
correlationsSubsets <- 1
startPoints <- 1
endPoints <- 1

for (l in 1:35) {
  foldSize <- round(length(meansData)/35)
  restData <- length(meansData)%%35 
  
  if (l==35) {
    startPoint <- ((l-1)*foldSize)+1
    endPoint <- (l*foldSize)+restData
    testIndex <- seq(startPoint,endPoint,1)
  } else {
    startPoint <- ((l-1)*foldSize)+1
    endPoint <- (l*foldSize)
    testIndex <- seq(startPoint,endPoint,1)
  }
  
  testSetLee <- LeeData[testIndex]
  testSetData <- meansData[testIndex]
  tempCor <- cor.test(testSetData,testSetLee)
  correlationsSubsets[l] <- tempCor$estimate
  startPoints[l] <- startPoint
  endPoints[l] <- endPoint
  
}

sherlockFrame2 <- data.frame(correlationsSubsets,startPoints,endPoints)

corOverAll <- cor.test(LeeData,meansData)