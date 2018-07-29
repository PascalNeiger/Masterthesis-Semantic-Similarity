library(tidyverse)
library(dplyr)
library(tidyr)
library(readxl)
library(lme4)

Results <- read_excel("ResultsLDT.xls",
                      sheet = "Tabelle1", na = "NA")

LDTDataWW <- read_excel("LDTDataCleanWW.xlsx",
                      sheet = "Tabelle1", na = "NA")

LDTDataNWW <- read_excel("LDTDataCleanNWW.xlsx",
                        sheet = "Tabelle1", na = "NA")

LDTDataWNW <- read_excel("LDTDataCleanWNW.xlsx",
                        sheet = "Tabelle1", na = "NA")

LDTDataNWNW <- read_excel("LDTDataCleanNWNW.xlsx",
                        sheet = "Tabelle1", na = "NA")

wholeData <- read_excel("wholeDataMatch.xlsx",
                      sheet = "Tabelle1", na = "NA")

wordLengths <- read_excel("wordLengths.xlsx",
                        sheet = "Tabelle1", na = "NA", col_names = FALSE)
wordlengths <- as.numeric(wordLengths)

frequencies <- read_excel("WortlisteLDT.xlsx",
                        sheet = "Tabelle1", na = "NA")

cleanDataLDTWW <- LDTDataWW[4:363,3:31]
cleanDataLDTWW <- as.data.frame(cleanDataLDTWW)
cleanDataLDTWW <- as.data.frame(sapply(cleanDataLDTWW, as.numeric))
WW <- rowMeans(cleanDataLDTWW,na.rm = TRUE)
meansLDTWW <- rowMeans(cleanDataLDTWW,na.rm = TRUE)
shapiro.test(WW)

cleanDataLDTNWW <- LDTDataNWW[4:123,3:31]
cleanDataLDTNWW <- as.data.frame(cleanDataLDTNWW)
cleanDataLDTNWW <- as.data.frame(sapply(cleanDataLDTNWW, as.numeric))
NWW <- rowMeans(cleanDataLDTNWW,na.rm = TRUE)
shapiro.test(NWW)

cleanDataLDTWNW <- LDTDataWNW[4:123,3:31]
cleanDataLDTWNW <- as.data.frame(cleanDataLDTWNW)
cleanDataLDTWNW <- as.data.frame(sapply(cleanDataLDTWNW, as.numeric)) 
WNW <- rowMeans(cleanDataLDTWNW,na.rm = TRUE)
shapiro.test(WNW)

cleanDataLDTNWNW <- LDTDataNWNW[4:123,3:31]
cleanDataLDTNWNW <- as.data.frame(cleanDataLDTNWNW)
cleanDataLDTNWNW <- as.data.frame(sapply(cleanDataLDTNWNW, as.numeric)) 
NWNW <- rowMeans(cleanDataLDTNWNW,na.rm = TRUE)
shapiro.test(NWNW)

meansLDT <- data.frame(WW,NWW,WNW,NWNW)
meansLDTLong <- gather(meansLDT, key = Condition, value = RT)
meansLDTLong$Condition <- as.factor(meansLDTLong$Condition)
meansLDTLong <- meansLDTLong[complete.cases(meansLDTLong),]

resultsAOV <- aov(RT ~ Condition, data = meansLDTLong)
summary(resultsAOV)
TukeyHSD(resultsAOV)
resultsKruskal <- kruskal.test(RT ~ Condition, data = meansLDTLong)
summary(resultsKruskal)

meansPlot <- ggplot(meansLDTLong,mapping = aes(x=Condition,y=RT,color = Condition))
meansPlot + geom_boxplot() + geom_jitter(width = 0.2, size = 1, alpha = 0.5)

wordlengthsMW <- mean(wordlengths)
wordlengthsSD <- sd(wordlengths)
wordlengthsUpper <- wordlengths - (wordlengthsMW - wordlengthsSD)
wordlengthsMiddle <- wordlengths - (wordlengthsMW)
wordlengthsLower <- wordlengths - (wordlengthsMW + wordlengthsSD)

typeNorm <- frequencies$Typefr.norm.__1
typeNorm <- as.numeric(typeNorm)

typeNormMW <- mean(typeNorm)
typeNormSD <- sd(typeNorm)
typeNormUpper <- typeNorm - (typeNormMW - typeNormSD)
typeNormMiddle <- typeNorm - (typeNormMW)
typeNormLower <- typeNorm - (typeNormMW + typeNormSD)


typeLog <- frequencies$`Typefr. log__1`
typeLog <- as.numeric(typeLog)

typeLogMW <- mean(typeLog)
typeLogSD <- sd(typeLog)
typeLogUpper <- typeLog - (typeLogMW - typeLogSD)
typeLogMiddle <- typeLog - (typeLogMW)
typeLogLower <- typeLog - (typeLogMW + typeLogSD)

lemmaNorm <- frequencies$`Lemmafr. norm.__1`
lemmaNorm <- as.numeric(lemmaNorm)

lemmaNormMW <- mean(lemmaNorm)
lemmaNormSD <- sd(lemmaNorm)
lemmaNormUpper <- lemmaNorm - (lemmaNormMW - lemmaNormSD)
lemmaNormMiddle <- lemmaNorm - (lemmaNormMW)
lemmaNormLower <- lemmaNorm - (lemmaNormMW + lemmaNormSD)

lemmaLog <- frequencies$`Lemmafr. Log__1`
lemmaLog <- as.numeric(lemmaLog)

lemmaLogMW <- mean(lemmaLog)
lemmaLogSD <- sd(lemmaLog)
lemmaLogUpper <- lemmaLog - (lemmaLogMW - lemmaLogSD)
lemmaLogMiddle <- lemmaLog - (lemmaLogMW)
lemmaLogLower <- lemmaLog - (lemmaLogMW + lemmaLogSD)

cleanDataOnline <- wholeData[2:99,2:361]
cleanDataOnline <- as.data.frame(cleanDataOnline)
cleanDataOnline <- as.data.frame(sapply(cleanDataOnline, as.numeric)) 

meansOnline <- colMeans(cleanDataOnline)

corLDTOnline <- cor.test(meansLDTWW,meansOnline)

##Correlations with Resnik

dataResnik <- Results$Resnik
dataResnik <- as.numeric(dataResnik)
corResultResnik <- cor.test(meansLDTWW,dataResnik)

##Correlations with Lin

dataLin <- Results$Lin
dataLin <- as.numeric(dataLin)
corResultLin <- cor.test(meansLDTWW,dataLin)

##Correlations with Li, Bandar und McLean

dataLi <- Results$`Li, Bandar & McLean`
dataLi <- as.numeric(dataLi)
corResultLi <- cor.test(meansLDTWW,dataLi)

##Correlations with Cosine Distance

dataCosine <- Results$`Cosine Distance`
dataCosine <- as.numeric(dataCosine)
corResultCosine <- cor.test(meansLDTWW,dataCosine)

##Correlations with Iacobacci

dataIacobacci <- Results$Iacobacci
dataIacobacci <- as.numeric(dataIacobacci)
corResultIacobacci <- cor.test(meansLDTWW,dataIacobacci)

##Correlations with Iacobacci und Li

dataIacobacciLi <- Results$`Iacobacci and Li`
dataIacobacciLi <- as.numeric(dataIacobacciLi)
corResultIacobacciLi <- cor.test(meansLDTWW,dataIacobacciLi)

##Correlations with Cosine und Li

dataCosineLi <- Results$`Cosine and Li`
dataCosineLi <- as.numeric(dataCosineLi)
corResultCosineLi <- cor.test(meansLDTWW,dataCosineLi)

##Correlations with Lee

dataLee <- Results$Lee
dataLee <- as.numeric(dataLee)
corResultLee <- cor.test(meansLDTWW,dataLee)

##Correlations with Iacobacci und Path

dataIacobacciPath <- Results$`Iacobacci and Path`
dataIacobacciPath <- as.numeric(dataIacobacciPath)
corResultIacobacciPath <- cor.test(meansLDTWW,dataIacobacciPath)

##Frequency of Target correlation

corResultTypeNorm <- cor.test(meansLDTWW,typeNorm)

corResultTypeLog <- cor.test(meansLDTWW,typeLog)

corResultLemmaNorm <- cor.test(meansLDTWW,lemmaNorm)

corResultLemmaLog <- cor.test(meansLDTWW,lemmaLog)


##Linear model TypeLog und Lin
dataLee <- Results$Lee
dataLee <- as.numeric(dataLee)

corLengthLDT <- cor.test(meansLDTWW,wordlengths)

fit11 <- lm(meansLDTWW ~ wordlengths)
summary(fit11)

fit12 <- lm(meansLDTWW ~ (dataLee+wordlengths)^2)
summary(fit12)

fit2 <- lm(meansLDTWW ~ (dataLee+meansOnline)^2)
summary(fit2)

fit31 <- lm(meansLDTWW ~ (wordlengths+dataLin)^2)
summary(fit31)

fit321 <- lm(meansLDTWW ~ (wordlengths+typeNorm)^2)
summary(fit321)

fit322 <- lm(meansLDTWW ~ (wordlengths+typeNormUpper)^2)
summary(fit322)

fit323 <- lm(meansLDTWW ~ (wordlengths+typeNormMiddle)^2)
summary(fit323)

fit324 <- lm(meansLDTWW ~ (wordlengths+typeNormLower)^2)
summary(fit324)

fit331 <- lm(meansLDTWW ~ (wordlengths+typeLog)^2)
summary(fit331)

fit3 <- lm(meansLDTWW ~ (dataLin+wordlengths+typeLog)^3)
summary(fit3)

fit3321 <- lm(meansLDTWW ~ (dataLin+typeLogUpper)^2)
summary(fit3321)

fit3331 <- lm(meansLDTWW ~ (dataLin+typeLogMiddle)^2)
summary(fit3331)

fit3341 <- lm(meansLDTWW ~ (dataLin+typeLogLower)^2)
summary(fit3341)

fit332 <- lm(meansLDTWW ~ (dataLin+wordlengths+typeLogUpper)^3)
summary(fit332)

fit333 <- lm(meansLDTWW ~ (dataLin+wordlengths+typeLogMiddle)^3)
summary(fit333)

fit334 <- lm(meansLDTWW ~ (dataLin+wordlengths+typeLogLower)^3)
summary(fit334)

fit3411 <- lm(meansLDTWW ~ (dataLin+wordlengthsUpper)^2)
summary(fit3411)

fit3421 <- lm(meansLDTWW ~ (dataLin+wordlengthsMiddle)^2)
summary(fit3421)

fit3431 <- lm(meansLDTWW ~ (dataLin+wordlengthsLower)^2)
summary(fit3431)

fit341 <- lm(meansLDTWW ~ (dataLin+typeLog+wordlengthsUpper)^3)
summary(fit341)

fit342 <- lm(meansLDTWW ~ (dataLin+typeLog+wordlengthsMiddle)^3)
summary(fit342)

fit343 <- lm(meansLDTWW ~ (dataLin+typeLog+wordlengthsLower)^3)
summary(fit343)

corLengthFreq <- cor.test(wordlengths,dataLin)

plot(meansLDTWW,wordlengths)

plot(meansLDTWW,typeLog)