library(tidyverse)
library(dplyr)
library(tidyr)
library(readxl)
library(lme4)

##Same procedure as semanticSimilarity.R with filtered participants

Results <- read_excel("~/Pascal/R/Masterarbeit/ResultsLDT.xls",
                      sheet = "Tabelle1", na = "NA")

Results <- Results[,c(18,1:17)]

##Searching for rows with NA

Results[Results=='NaN'] <- NA

rowsWithNA <- unique (unlist (lapply (Results, function (x) which (is.na (x)))))

##Delete rows with NA

Results <- Results[-rowsWithNA, ]

ResultsClean <- Results

VpExcludeWholeData <- read_excel("~/Pascal/R/Masterarbeit/VpExcludeWholeData.xls",
                                 sheet = "Tabelle1", na = "NA")


VpExcludeWholeData[VpExcludeWholeData=="-4"] <- 4
VpExcludeWholeData[VpExcludeWholeData=="-3"] <- 3
VpExcludeWholeData[VpExcludeWholeData=="-2"] <- 2
VpExcludeWholeData[VpExcludeWholeData=="-1"] <- 1

meansVpExcludeWholeData <- rowMeans(VpExcludeWholeData[,c(2:89)])
meansVpExcludeWholeData <- t(as.numeric(meansVpExcludeWholeData))

##Correlation of the means with Resnik

dataResnik <- ResultsClean$Resnik
dataResnik <- t(as.numeric(dataResnik))
corResultResnik <- cor.test(meansVpExcludeWholeData,dataResnik)

##Correlation of the means with Lin

dataLin <- ResultsClean$Lin
dataLin <- t(as.numeric(dataLin))
corResultLin <- cor.test(meansVpExcludeWholeData,dataLin)

##Correlation of the means with Li, Bandar und McLean

dataLi <- ResultsClean$`Li, Bandar & McLean`
dataLi <- t(as.numeric(dataLi))
corResultLi <- cor.test(meansVpExcludeWholeData,dataLin)

##Correlation of the means with Cosine Distance

dataCosine <- ResultsClean$`Cosine Distance`
dataCosine <- t(as.numeric(dataCosine))
corResultCosine <- cor.test(meansVpExcludeWholeData,dataCosine)

##Correlation of the means with Iacobacci

dataIacobacci <- ResultsClean$Iacobacci
dataIacobacci <- t(as.numeric(dataIacobacci))
corResultIacobacci <- cor.test(meansVpExcludeWholeData,dataIacobacci)

##Correlation of the means with Iacobacci und Li

dataIacobacciLi <- ResultsClean$`Iacobacci and Li`
dataIacobacciLi <- t(as.numeric(dataIacobacciLi))
corResultIacobacciLi <- cor.test(meansVpExcludeWholeData,dataIacobacciLi)

##Correlation of the means with Cosine und Li

dataCosineLi <- ResultsClean$`Cosine and Li`
dataCosineLi <- t(as.numeric(dataCosineLi))
corResultCosineLi <- cor.test(meansVpExcludeWholeData,dataCosineLi)

##Correlation of the means with Lee

dataLee <- ResultsClean$Lee
dataLee <- t(as.numeric(dataLee))
corResultLee <- cor.test(meansVpExcludeWholeData,dataLee)

##Correlation of the means with Iacobacci und Path

dataIacobacciPath <- ResultsClean$`Iacobacci and Path`
dataIacobacciPath <- t(as.numeric(dataIacobacciPath))
corResultIacobacciPath <- cor.test(meansVpExcludeWholeData,dataIacobacciPath)

##Creation of table of correlations

Resnik <- corResultResnik$estimate
Resnik[2] <- corResultResnik$statistic
Resnik[3] <- corResultResnik$parameter
Resnik[4] <- corResultResnik$p.value

Lin <- corResultLin$estimate
Lin[2] <- corResultLin$statistic
Lin[3] <- corResultLin$parameter
Lin[4] <- corResultLin$p.value

Li <- corResultLi$estimate
Li[2] <- corResultLi$statistic
Li[3] <- corResultLi$parameter
Li[4] <- corResultLi$p.value

Cosine <- corResultCosine$estimate
Cosine[2] <- corResultCosine$statistic
Cosine[3] <- corResultCosine$parameter
Cosine[4] <- corResultCosine$p.value

Iacobacci <- corResultIacobacci$estimate
Iacobacci[2] <- corResultIacobacci$statistic
Iacobacci[3] <- corResultIacobacci$parameter
Iacobacci[4] <- corResultIacobacci$p.value

Lee <- corResultLee$estimate
Lee[2] <- corResultLee$statistic
Lee[3] <- corResultLee$parameter
Lee[4] <- corResultLee$p.value

CosineLi <- corResultCosineLi$estimate
CosineLi[2] <- corResultCosineLi$statistic
CosineLi[3] <- corResultCosineLi$parameter
CosineLi[4] <- corResultCosineLi$p.value

IacobacciLi <- corResultIacobacciLi$estimate
IacobacciLi[2] <- corResultIacobacciLi$statistic
IacobacciLi[3] <- corResultIacobacciLi$parameter
IacobacciLi[4] <- corResultIacobacciLi$p.value

IacobacciPath <- corResultIacobacciPath$estimate
IacobacciPath[2] <- corResultIacobacciPath$statistic
IacobacciPath[3] <- corResultIacobacciPath$parameter
IacobacciPath[4] <- corResultIacobacciPath$p.value

corResultsVpExcludeTable <- data.frame(Resnik = Resnik, Lin = Lin, Li = Li,
                                       Cosine = Cosine, Iacobacci = Iacobacci,
                                       Lee = Lee, CosineLi = CosineLi,
                                       IacobacciLi = IacobacciLi, IacobacciPath = IacobacciPath)

row.names(corResultsVpExcludeTable) <- c("Correlation","T-Value","df","p-value")
corResultsVpExcludeTable <- round(corResultsVpExcludeTable, 3)

##Correlations of each participant with Resnik

VpExcludeWholeData <- VpExcludeWholeData[,2:89]

#Correlations of each participant with Resnik

corSingleResnik <- 1
for(i in 1:88) {
  temp1 <- VpExcludeWholeData[,i]
  temp1 <- t(temp1)
  temp2 <- cor.test(temp1,dataResnik)
  corSingleResnik[i] <- temp2$estimate
}

#Correlations of each participant with Lin

corSingleLin <- 1
for(i in 1:88) {
  temp1 <- VpExcludeWholeData[,i]
  temp1 <- t(temp1)
  temp2 <- cor.test(temp1,dataLin)
  corSingleLin[i] <- temp2$estimate
}

#Correlations of each participant with Li

corSingleLi <- 1
for(i in 1:88) {
  temp1 <- VpExcludeWholeData[,i]
  temp1 <- t(temp1)
  temp2 <- cor.test(temp1,dataLi)
  corSingleLi[i] <- temp2$estimate
}

#Correlations of each participant with Cosine

corSingleCosine <- 1
for(i in 1:88) {
  temp1 <- VpExcludeWholeData[,i]
  temp1 <- t(temp1)
  temp2 <- cor.test(temp1,dataCosine)
  corSingleCosine[i] <- temp2$estimate
}

#Correlations of each participant with Iacobacci

corSingleIacobacci <- 1
for(i in 1:88) {
  temp1 <- VpExcludeWholeData[,i]
  temp1 <- t(temp1)
  temp2 <- cor.test(temp1,dataIacobacci)
  corSingleIacobacci[i] <- temp2$estimate
}

#Correlations of each participant with IacobacciLi

corSingleIacobacciLi <- 1
for(i in 1:88) {
  temp1 <- VpExcludeWholeData[,i]
  temp1 <- t(temp1)
  temp2 <- cor.test(temp1,dataIacobacciLi)
  corSingleIacobacciLi[i] <- temp2$estimate
}

#Correlations of each participant with CosineLi

corSingleCosineLi <- 1
for(i in 1:88) {
  temp1 <- VpExcludeWholeData[,i]
  temp1 <- t(temp1)
  temp2 <- cor.test(temp1,dataCosineLi)
  corSingleCosineLi[i] <- temp2$estimate
}

#Correlations of each participant with Lee

corSingleLee <- 1
for(i in 1:88) {
  temp1 <- VpExcludeWholeData[,i]
  temp1 <- t(temp1)
  temp2 <- cor.test(temp1,dataLee)
  corSingleLee[i] <- temp2$estimate
}

#Correlations of each participant with IacobacciPath

corSingleIacobacciPath <- 1
for(i in 1:88) {
  temp1 <- VpExcludeWholeData[,i]
  temp1 <- t(temp1)
  temp2 <- cor.test(temp1,dataIacobacciPath)
  corSingleIacobacciPath[i] <- temp2$estimate
}

##PLot of single correlations

corSingleVpExclude <- data.frame(Resnik = corSingleResnik,
                                 Lin = corSingleLin,
                                 Li = corSingleLi,
                                 Cosine = corSingleCosineLi,
                                 Iacobacci = corSingleIacobacci,
                                 IacobacciLi = corSingleIacobacciLi,
                                 CosineLi = corSingleCosineLi,
                                 Lee = corSingleLee,
                                 IacobacciPath = corSingleIacobacciPath)

corSingleVpExclude <- round(corSingleVpExclude, 3)

corSingleVpExcludeLong <- gather(corSingleVpExclude, key = Methode, value = Korrelation)
corSingleVpExcludeLong$Methode <- as.factor(corSingleVpExcludeLong$Methode)
plotCorSingleVpExclude <- ggplot(data = corSingleVpExcludeLong,
                                 mapping = aes(x=Methode,
                                               y=Korrelation,
                                               color = Methode))
plotCorSingleVpExclude + geom_boxplot(outlier.alpha = 0) + geom_jitter(width = 0.2, size = 3, alpha = 0.5) +
  labs(title = "Korrelationen der Vpn zu den Methoden") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold")) +
  theme(plot.title = element_text(size = 16))
endPlotCorSingleVpExclude <- plotCorSingleVpExclude + geom_boxplot(outlier.alpha = 0) + geom_jitter(width = 0.2, size = 3, alpha = 0.5) +
  labs(title = "Korrelationen der Vpn zu den Methoden") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"))+
  theme(plot.title = element_text(size = 16))
ggsave(filename = "c:/Users/PDaddy/Documents/Pascal/R/Masterarbeit/plotCorSingleVpExclude.png",endPlotCorSingleVpExclude, scale = 5,
       width = 2.4, height = 2, units = "in", dpi = 900)

##Same procedure with 1/10 of participants and half of the items excluded

ResultsClean <- read_excel("~/Pascal/R/Masterarbeit/ItemExcludeWholeData.xls",
                           sheet = "Tabelle1", na = "NA")

VpItemExcludeWholeData <- read_excel("~/Pascal/R/Masterarbeit/VpItemExcludeWholeData.xls",
                                     sheet = "Tabelle1", na = "NA")


VpItemExcludeWholeData[VpItemExcludeWholeData=="-4"] <- 4
VpItemExcludeWholeData[VpItemExcludeWholeData=="-3"] <- 3
VpItemExcludeWholeData[VpItemExcludeWholeData=="-2"] <- 2
VpItemExcludeWholeData[VpItemExcludeWholeData=="-1"] <- 1

meansVpItemExcludeWholeData <- rowMeans(VpItemExcludeWholeData[,c(2:89)])
meansVpItemExcludeWholeData <- t(as.numeric(meansVpItemExcludeWholeData))

##Mean correlations with Resnik

dataResnik <- ResultsClean$Resnik
dataResnik <- t(as.numeric(dataResnik))
corResultResnik <- cor.test(meansVpItemExcludeWholeData,dataResnik)

##Mean correlations with Lin

dataLin <- ResultsClean$Lin
dataLin <- t(as.numeric(dataLin))
corResultLin <- cor.test(meansVpItemExcludeWholeData,dataLin)

##Mean correlations with Li, Bandar und McLean

dataLi <- ResultsClean$`Li, Bandar & McLean`
dataLi <- t(as.numeric(dataLi))
corResultLi <- cor.test(meansVpItemExcludeWholeData,dataLin)

##Mean correlations with Cosine Distance

dataCosine <- ResultsClean$`Cosine Distance`
dataCosine <- t(as.numeric(dataCosine))
corResultCosine <- cor.test(meansVpItemExcludeWholeData,dataCosine)

##Mean correlations with Iacobacci

dataIacobacci <- ResultsClean$Iacobacci
dataIacobacci <- t(as.numeric(dataIacobacci))
corResultIacobacci <- cor.test(meansVpItemExcludeWholeData,dataIacobacci)

##Mean correlations with Iacobacci und Li

dataIacobacciLi <- ResultsClean$`Iacobacci and Li`
dataIacobacciLi <- t(as.numeric(dataIacobacciLi))
corResultIacobacciLi <- cor.test(meansVpItemExcludeWholeData,dataIacobacciLi)

##Mean correlations with Cosine und Li

dataCosineLi <- ResultsClean$`Cosine and Li`
dataCosineLi <- t(as.numeric(dataCosineLi))
corResultCosineLi <- cor.test(meansVpItemExcludeWholeData,dataCosineLi)

##Mean correlations with Lee

dataLee <- ResultsClean$Lee
dataLee <- t(as.numeric(dataLee))
corResultLee <- cor.test(meansVpItemExcludeWholeData,dataLee)

##Mean correlations with Iacobacci und Path

dataIacobacciPath <- ResultsClean$`Iacobacci and Path`
dataIacobacciPath <- t(as.numeric(dataIacobacciPath))
corResultIacobacciPath <- cor.test(meansVpItemExcludeWholeData,dataIacobacciPath)

##-create correlation table

Resnik <- corResultResnik$estimate
Resnik[2] <- corResultResnik$statistic
Resnik[3] <- corResultResnik$parameter
Resnik[4] <- corResultResnik$p.value

Lin <- corResultLin$estimate
Lin[2] <- corResultLin$statistic
Lin[3] <- corResultLin$parameter
Lin[4] <- corResultLin$p.value

Li <- corResultLi$estimate
Li[2] <- corResultLi$statistic
Li[3] <- corResultLi$parameter
Li[4] <- corResultLi$p.value

Cosine <- corResultCosine$estimate
Cosine[2] <- corResultCosine$statistic
Cosine[3] <- corResultCosine$parameter
Cosine[4] <- corResultCosine$p.value

Iacobacci <- corResultIacobacci$estimate
Iacobacci[2] <- corResultIacobacci$statistic
Iacobacci[3] <- corResultIacobacci$parameter
Iacobacci[4] <- corResultIacobacci$p.value

Lee <- corResultLee$estimate
Lee[2] <- corResultLee$statistic
Lee[3] <- corResultLee$parameter
Lee[4] <- corResultLee$p.value

CosineLi <- corResultCosineLi$estimate
CosineLi[2] <- corResultCosineLi$statistic
CosineLi[3] <- corResultCosineLi$parameter
CosineLi[4] <- corResultCosineLi$p.value

IacobacciLi <- corResultIacobacciLi$estimate
IacobacciLi[2] <- corResultIacobacciLi$statistic
IacobacciLi[3] <- corResultIacobacciLi$parameter
IacobacciLi[4] <- corResultIacobacciLi$p.value

IacobacciPath <- corResultIacobacciPath$estimate
IacobacciPath[2] <- corResultIacobacciPath$statistic
IacobacciPath[3] <- corResultIacobacciPath$parameter
IacobacciPath[4] <- corResultIacobacciPath$p.value

corResultsVpItemExcludeTable <- data.frame(Resnik = Resnik, Lin = Lin, Li = Li,
                                           Cosine = Cosine, Iacobacci = Iacobacci,
                                           Lee = Lee, CosineLi = CosineLi,
                                           IacobacciLi = IacobacciLi, IacobacciPath = IacobacciPath)

row.names(corResultsVpItemExcludeTable) <- c("Correlation","T-Value","df","p-value")
corResultsVpItemExcludeTable <- round(corResultsVpItemExcludeTable, 3)

##Correlations of each participant with Resnik

VpItemExcludeWholeData <- VpItemExcludeWholeData[,2:89]

#Correlations of each participant with Resnik

corSingleResnik <- 1
for(i in 1:88) {
  temp1 <- VpItemExcludeWholeData[,i]
  temp1 <- t(temp1)
  temp2 <- cor.test(temp1,dataResnik)
  corSingleResnik[i] <- temp2$estimate
}

#Correlations of each participant with Lin

corSingleLin <- 1
for(i in 1:88) {
  temp1 <- VpItemExcludeWholeData[,i]
  temp1 <- t(temp1)
  temp2 <- cor.test(temp1,dataLin)
  corSingleLin[i] <- temp2$estimate
}

#Correlations of each participant with Li

corSingleLi <- 1
for(i in 1:88) {
  temp1 <- VpItemExcludeWholeData[,i]
  temp1 <- t(temp1)
  temp2 <- cor.test(temp1,dataLi)
  corSingleLi[i] <- temp2$estimate
}

#Correlations of each participant with Cosine

corSingleCosine <- 1
for(i in 1:88) {
  temp1 <- VpItemExcludeWholeData[,i]
  temp1 <- t(temp1)
  temp2 <- cor.test(temp1,dataCosine)
  corSingleCosine[i] <- temp2$estimate
}

#Correlations of each participant with Iacobacci

corSingleIacobacci <- 1
for(i in 1:88) {
  temp1 <- VpItemExcludeWholeData[,i]
  temp1 <- t(temp1)
  temp2 <- cor.test(temp1,dataIacobacci)
  corSingleIacobacci[i] <- temp2$estimate
}

#Correlations of each participant with IacobacciLi

corSingleIacobacciLi <- 1
for(i in 1:88) {
  temp1 <- VpItemExcludeWholeData[,i]
  temp1 <- t(temp1)
  temp2 <- cor.test(temp1,dataIacobacciLi)
  corSingleIacobacciLi[i] <- temp2$estimate
}

#Correlations of each participant with CosineLi

corSingleCosineLi <- 1
for(i in 1:88) {
  temp1 <- VpItemExcludeWholeData[,i]
  temp1 <- t(temp1)
  temp2 <- cor.test(temp1,dataCosineLi)
  corSingleCosineLi[i] <- temp2$estimate
}

#Correlations of each participant with Lee

corSingleLee <- 1
for(i in 1:88) {
  temp1 <- VpItemExcludeWholeData[,i]
  temp1 <- t(temp1)
  temp2 <- cor.test(temp1,dataLee)
  corSingleLee[i] <- temp2$estimate
}

#Correlations of each participant with IacobacciPath

corSingleIacobacciPath <- 1
for(i in 1:88) {
  temp1 <- VpItemExcludeWholeData[,i]
  temp1 <- t(temp1)
  temp2 <- cor.test(temp1,dataIacobacciPath)
  corSingleIacobacciPath[i] <- temp2$estimate
}

##Creation of plot of single correaltions

corSingleVpItemExclude <- data.frame(Resnik = corSingleResnik,
                                     Lin = corSingleLin,
                                     Li = corSingleLi,
                                     Cosine = corSingleCosineLi,
                                     Iacobacci = corSingleIacobacci,
                                     IacobacciLi = corSingleIacobacciLi,
                                     CosineLi = corSingleCosineLi,
                                     Lee = corSingleLee,
                                     IacobacciPath = corSingleIacobacciPath)

corSingleVpItemExclude <- round(corSingleVpItemExclude, 3)

corSingleVpItemExcludeLong <- gather(corSingleVpItemExclude, key = Methode, value = Korrelation)
corSingleVpItemExcludeLong$Methode <- as.factor(corSingleVpItemExcludeLong$Methode)
plotCorSingleVpItemExclude <- ggplot(data = corSingleVpItemExcludeLong,
                                     mapping = aes(x=Methode,
                                                   y=Korrelation,
                                                   color = Methode))
plotCorSingleVpItemExclude + geom_boxplot(outlier.alpha = 0) + geom_jitter(width = 0.2, size = 3, alpha = 0.5) +
  labs(title = "Korrelationen der Vpn zu den Methoden") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold")) +
  theme(plot.title = element_text(size = 16))
endPlotCorSingleVpItemExclude <- plotCorSingleVpItemExclude + geom_boxplot(outlier.alpha = 0) + geom_jitter(width = 0.2, size = 3, alpha = 0.5) +
  labs(title = "Korrelationen der Vpn zu den Methoden") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"))+
  theme(plot.title = element_text(size = 16))
ggsave(filename = "c:/Users/PDaddy/Documents/Pascal/R/Masterarbeit/plotCorSingleVpItemExclude.png",endPlotCorSingleVpItemExclude, scale = 5,
       width = 2.4, height = 2, units = "in", dpi = 900)

##Homogenity items

ResultsClean <- read_excel("~/Pascal/R/Masterarbeit/homogenityItemsResults.xls",
                           sheet = "Tabelle1", na = "NA")

homogenityItemsData <- read_excel("~/Pascal/R/Masterarbeit/homogenityItemsData.xls",
                                  sheet = "Tabelle1", na = "NA")


Item1 <- homogenityItemsData[1,2:89]
Item1=as.numeric(Item1)
Item2 <- homogenityItemsData[2,2:89]
Item2=as.numeric(Item2)
Item3 <- homogenityItemsData[3,2:89]
Item3=as.numeric(Item3)
Item6 <- homogenityItemsData[4,2:89]
Item6=as.numeric(Item6)
Item17 <- homogenityItemsData[5,2:89]
Item17=as.numeric(Item17)

Prediction15 <- ResultsClean[15,16]
Prediction15 <- as.numeric(Prediction15)
Prediction24 <- ResultsClean[24,16]
Prediction24 <- as.numeric(Prediction24)
Prediction67 <- ResultsClean[67,16]
Prediction67 <- as.numeric(Prediction67)
Prediction108 <- ResultsClean[108,16]
Prediction108 <- as.numeric(Prediction108)
Prediction135 <- ResultsClean[135,16]
Prediction135 <- as.numeric(Prediction135)

plotReady <- data.frame(Item1=Item1, Item2=Item2, Item3=Item3, Item6=Item6, Item17=Item17)
plotReady2 <- gather(plotReady, key = Item, value = Ratings)

plotReady3=data.frame(Prediction108=Prediction108, Prediction135=Prediction135, Prediction15=Prediction15, Prediction24=Prediction24, Prediction67=Prediction67)


plotHomogenityReady2 <- ggplot(data = plotReady2,
                               mapping = aes(x=Item,
                                             y=Ratings,
                                             color = Item),ylim=c(-4, 4))

plotHomogenityReady2 + geom_boxplot(outlier.alpha = 0) + geom_jitter(width = 0.2, size = 3, alpha = 0.5) +
  labs(title = "Ratings der Vpn zu einzelnen Items") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold")) +
  theme(plot.title = element_text(size = 16))
endplotHomogenityReady2 <- plotHomogenityReady2 + geom_boxplot(outlier.alpha = 0) + geom_jitter(width = 0.2, size = 3, alpha = 0.5) +
  labs(title = "Ratings der Vpn zu einzelnen Items") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"))+
  theme(plot.title = element_text(size = 16))
ggsave(filename = "c:/Users/PDaddy/Documents/Pascal/R/Masterarbeit/plotHomogenityReady2.png",endplotHomogenityReady2, scale = 5,
       width = 2.4, height = 2, units = "in", dpi = 900)



homogenityItemsData[homogenityItemsData=="-4"] <- 4
homogenityItemsData[homogenityItemsData=="-3"] <- 3
homogenityItemsData[homogenityItemsData=="-2"] <- 2
homogenityItemsData[homogenityItemsData=="-1"] <- 1

meansHomogenityItemsData <- rowMeans(homogenityItemsData[,c(2:89)])
meansHomogenityItemsData <- t(as.numeric(meansHomogenityItemsData))

##Mean correlations with Resnik

dataResnik <- ResultsClean$Resnik
dataResnik <- t(as.numeric(dataResnik))
corResultResnik <- cor.test(meansHomogenityItemsData,dataResnik)

##Mean correlations with Lin

dataLin <- ResultsClean$Lin
dataLin <- t(as.numeric(dataLin))
corResultLin <- cor.test(meansHomogenityItemsData,dataLin)

##Mean correlations with Li, Bandar und McLean

dataLi <- ResultsClean$`Li, Bandar & McLean`
dataLi <- t(as.numeric(dataLi))
corResultLi <- cor.test(meansHomogenityItemsData,dataLin)

##Mean correlations with Cosine Distance

dataCosine <- ResultsClean$`Cosine Distance`
dataCosine <- t(as.numeric(dataCosine))
corResultCosine <- cor.test(meansHomogenityItemsData,dataCosine)

##Mean correlations with Iacobacci

dataIacobacci <- ResultsClean$Iacobacci
dataIacobacci <- t(as.numeric(dataIacobacci))
corResultIacobacci <- cor.test(meansHomogenityItemsData,dataIacobacci)

##Mean correlations with Iacobacci und Li

dataIacobacciLi <- ResultsClean$`Iacobacci and Li`
dataIacobacciLi <- t(as.numeric(dataIacobacciLi))
corResultIacobacciLi <- cor.test(meansHomogenityItemsData,dataIacobacciLi)

##Mean correlations with Cosine und Li

dataCosineLi <- ResultsClean$`Cosine and Li`
dataCosineLi <- t(as.numeric(dataCosineLi))
corResultCosineLi <- cor.test(meansHomogenityItemsData,dataCosineLi)

##Mean correlations with Lee

dataLee <- ResultsClean$Lee
dataLee <- t(as.numeric(dataLee))
corResultLee <- cor.test(meansHomogenityItemsData,dataLee)

##Mean correlations with Iacobacci und Path

dataIacobacciPath <- ResultsClean$`Iacobacci and Path`
dataIacobacciPath <- t(as.numeric(dataIacobacciPath))
corResultIacobacciPath <- cor.test(meansHomogenityItemsData,dataIacobacciPath)

##Creation of a table of the correlations

Resnik <- corResultResnik$estimate
Resnik[2] <- corResultResnik$statistic
Resnik[3] <- corResultResnik$parameter
Resnik[4] <- corResultResnik$p.value

Lin <- corResultLin$estimate
Lin[2] <- corResultLin$statistic
Lin[3] <- corResultLin$parameter
Lin[4] <- corResultLin$p.value

Li <- corResultLi$estimate
Li[2] <- corResultLi$statistic
Li[3] <- corResultLi$parameter
Li[4] <- corResultLi$p.value

Cosine <- corResultCosine$estimate
Cosine[2] <- corResultCosine$statistic
Cosine[3] <- corResultCosine$parameter
Cosine[4] <- corResultCosine$p.value

Iacobacci <- corResultIacobacci$estimate
Iacobacci[2] <- corResultIacobacci$statistic
Iacobacci[3] <- corResultIacobacci$parameter
Iacobacci[4] <- corResultIacobacci$p.value

Lee <- corResultLee$estimate
Lee[2] <- corResultLee$statistic
Lee[3] <- corResultLee$parameter
Lee[4] <- corResultLee$p.value

CosineLi <- corResultCosineLi$estimate
CosineLi[2] <- corResultCosineLi$statistic
CosineLi[3] <- corResultCosineLi$parameter
CosineLi[4] <- corResultCosineLi$p.value

IacobacciLi <- corResultIacobacciLi$estimate
IacobacciLi[2] <- corResultIacobacciLi$statistic
IacobacciLi[3] <- corResultIacobacciLi$parameter
IacobacciLi[4] <- corResultIacobacciLi$p.value

IacobacciPath <- corResultIacobacciPath$estimate
IacobacciPath[2] <- corResultIacobacciPath$statistic
IacobacciPath[3] <- corResultIacobacciPath$parameter
IacobacciPath[4] <- corResultIacobacciPath$p.value

corResultsHomogenityItemsData <- data.frame(Resnik = Resnik, Lin = Lin, Li = Li,
                                            Cosine = Cosine, Iacobacci = Iacobacci,
                                            Lee = Lee, CosineLi = CosineLi,
                                            IacobacciLi = IacobacciLi, IacobacciPath = IacobacciPath)

row.names(corResultsHomogenityItemsData) <- c("Correlation","T-Value","df","p-value")
corResultsHomogenityItemsData <- round(corResultsHomogenityItemsData, 3)

##Correlations of each participant with Resnik

homogenityItemsData <- homogenityItemsData[,2:89]

#Correlations of each participant with Resnik

corSingleResnik <- 1
for(i in 1:88) {
  temp1 <- homogenityItemsData[,i]
  temp1 <- t(temp1)
  temp2 <- cor.test(temp1,dataResnik)
  corSingleResnik[i] <- temp2$estimate
}

#Correlations of each participant with Lin

corSingleLin <- 1
for(i in 1:88) {
  temp1 <- homogenityItemsData[,i]
  temp1 <- t(temp1)
  temp2 <- cor.test(temp1,dataLin)
  corSingleLin[i] <- temp2$estimate
}

#Correlations of each participant with Li

corSingleLi <- 1
for(i in 1:88) {
  temp1 <- homogenityItemsData[,i]
  temp1 <- t(temp1)
  temp2 <- cor.test(temp1,dataLi)
  corSingleLi[i] <- temp2$estimate
}

#Correlations of each participant with Cosine

corSingleCosine <- 1
for(i in 1:88) {
  temp1 <- homogenityItemsData[,i]
  temp1 <- t(temp1)
  temp2 <- cor.test(temp1,dataCosine)
  corSingleCosine[i] <- temp2$estimate
}

#Correlations of each participant with Iacobacci

corSingleIacobacci <- 1
for(i in 1:88) {
  temp1 <- homogenityItemsData[,i]
  temp1 <- t(temp1)
  temp2 <- cor.test(temp1,dataIacobacci)
  corSingleIacobacci[i] <- temp2$estimate
}

#Correlations of each participant with IacobacciLi

corSingleIacobacciLi <- 1
for(i in 1:88) {
  temp1 <- homogenityItemsData[,i]
  temp1 <- t(temp1)
  temp2 <- cor.test(temp1,dataIacobacciLi)
  corSingleIacobacciLi[i] <- temp2$estimate
}

#Correlations of each participant with CosineLi

corSingleCosineLi <- 1
for(i in 1:88) {
  temp1 <- homogenityItemsData[,i]
  temp1 <- t(temp1)
  temp2 <- cor.test(temp1,dataCosineLi)
  corSingleCosineLi[i] <- temp2$estimate
}

#Correlations of each participant with Lee

corSingleLee <- 1
for(i in 1:88) {
  temp1 <- homogenityItemsData[,i]
  temp1 <- t(temp1)
  temp2 <- cor.test(temp1,dataLee)
  corSingleLee[i] <- temp2$estimate
}

#Correlations of each participant with IacobacciPath

corSingleIacobacciPath <- 1
for(i in 1:88) {
  temp1 <- homogenityItemsData[,i]
  temp1 <- t(temp1)
  temp2 <- cor.test(temp1,dataIacobacciPath)
  corSingleIacobacciPath[i] <- temp2$estimate
}

##Creation of plot of single correlations

corSingleHomogenityItems <- data.frame(Resnik = corSingleResnik,
                                       Lin = corSingleLin,
                                       Li = corSingleLi,
                                       Cosine = corSingleCosineLi,
                                       Iacobacci = corSingleIacobacci,
                                       IacobacciLi = corSingleIacobacciLi,
                                       CosineLi = corSingleCosineLi,
                                       Lee = corSingleLee,
                                       IacobacciPath = corSingleIacobacciPath)

corSingleHomogenityItems <- round(corSingleHomogenityItems, 3)

corSingleHomogenityItemsLong <- gather(corSingleHomogenityItems, key = Methode, value = Korrelation)
corSingleHomogenityItemsLong$Methode <- as.factor(corSingleHomogenityItemsLong$Methode)
plotCorSingleHomogenityItems <- ggplot(data = corSingleHomogenityItemsLong,
                                       mapping = aes(x=Methode,
                                                     y=Korrelation,
                                                     color = Methode))
plotCorSingleHomogenityItems + geom_boxplot(outlier.alpha = 0) + geom_jitter(width = 0.2, size = 3, alpha = 0.5) +
  labs(title = "Korrelationen der Vpn zu den Methoden") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold")) +
  theme(plot.title = element_text(size = 16))
endPlotCorSingleHomogenityItems <- plotCorSingleHomogenityItems + geom_boxplot(outlier.alpha = 0) + geom_jitter(width = 0.2, size = 3, alpha = 0.5) +
  labs(title = "Korrelationen der Vpn zu den Methoden") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"))+
  theme(plot.title = element_text(size = 16))
ggsave(filename = "c:/Users/PDaddy/Documents/Pascal/R/Masterarbeit/plotCorSingleHomogenityItems.png",endPlotCorSingleHomogenityItems, scale = 5,
       width = 2.4, height = 2, units = "in", dpi = 900)

