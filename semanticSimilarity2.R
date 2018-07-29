library(tidyverse)
library(dplyr)
library(tidyr)
library(readxl)
library(lme4)
library(matrixStats)
library(ordinalNet)

##### Test ffor position effect #####
##Inversed study

Verkehrte_Studie <- read_delim("~/Pascal/R/Masterarbeit/Verkehrte Studie For R.csv",
                               ";", escape_double = FALSE, col_types = cols(v_541 = col_skip(),
                                                                            external_lfdn = col_skip(), tester = col_skip(), dispcode = col_skip(),
                                                                            lastpage = col_skip(), quality = col_skip(), duration = col_skip(),
                                                                            browser = col_skip(), referer = col_skip(), participant_browser = col_skip(),
                                                                            participant_browser_version = col_skip(), participant_os = col_skip(),
                                                                            participant_device = col_skip(), participant_brand = col_skip(),
                                                                            participant_model = col_skip(), participant_isbot = col_skip(),
                                                                            participant_continent = col_skip(), participant_country = col_skip(),
                                                                            participant_region = col_skip(), participant_city = col_skip(),
                                                                            participant_latitude = col_skip(), participant_longitude = col_skip(),
                                                                            quota = col_skip(), quota_assignment = col_skip(), page_history = col_skip(),
                                                                            hflip = col_skip(), vflip = col_skip(), output_mode = col_skip(),
                                                                            javascript = col_skip(), flash = col_skip(), session_id = col_skip(),
                                                                            language = col_skip(), cleaned = col_skip(), ats = col_skip(),
                                                                            datetime = col_skip(), date_of_last_access = col_skip(),
                                                                            date_of_first_mail = col_skip()), trim_ws = TRUE)
Verkehrte_Studie$lfdn <- as.factor(Verkehrte_Studie$lfdn)
Verkehrte_Studie$sex <- as.factor(Verkehrte_Studie$v_38)
Verkehrte_Studie$age <- as.factor(Verkehrte_Studie$v_39)
Verkehrte_Studie$education <- as.factor(Verkehrte_Studie$v_40)

##Normal study

Normale_Studie <- read_delim("~/Pascal/R/Masterarbeit/Normale Studie For R.csv",
                             ";", escape_double = FALSE, col_types = cols(v_541 = col_skip(),
                                                                          external_lfdn = col_skip(), tester = col_skip(), dispcode = col_skip(),
                                                                          lastpage = col_skip(), quality = col_skip(), duration = col_skip(),
                                                                          browser = col_skip(), referer = col_skip(), participant_browser = col_skip(),
                                                                          participant_browser_version = col_skip(), participant_os = col_skip(),
                                                                          participant_device = col_skip(), participant_brand = col_skip(),
                                                                          participant_model = col_skip(), participant_isbot = col_skip(),
                                                                          participant_continent = col_skip(), participant_country = col_skip(),
                                                                          participant_region = col_skip(), participant_city = col_skip(),
                                                                          participant_latitude = col_skip(), participant_longitude = col_skip(),
                                                                          quota = col_skip(), quota_assignment = col_skip(), page_history = col_skip(),
                                                                          hflip = col_skip(), vflip = col_skip(), output_mode = col_skip(),
                                                                          javascript = col_skip(), flash = col_skip(), session_id = col_skip(),
                                                                          language = col_skip(), cleaned = col_skip(), ats = col_skip(),
                                                                          datetime = col_skip(), date_of_last_access = col_skip(),
                                                                          date_of_first_mail = col_skip()), trim_ws = TRUE)
Normale_Studie$lfdn <- as.factor(Normale_Studie$lfdn)
Normale_Studie$sex <- as.factor(Normale_Studie$v_38)
Normale_Studie$age <- as.factor(Normale_Studie$v_39)
Normale_Studie$education <- as.factor(Normale_Studie$v_40)

##Results

Results <- read_excel("~/Pascal/R/Masterarbeit/ResultsLDT.xls",
                      sheet = "Tabelle1", na = "NA")

##Insert variable names into Results

columnNames <- colnames(Normale_Studie)

columnNamesClean <- columnNames[-c(1,502:507)]

Results["Variables"] <- columnNamesClean

Results <- Results[,c(18,1:17)]

##Find rows with NA

Results[Results=='NaN'] <- NA

rowsWithNA <- unique (unlist (lapply (Results, function (x) which (is.na (x)))))

##Rows with NA are deleted

Results <- Results[-rowsWithNA, ]

ResultsClean <- Results

##Exclude NA rows from results

columnsWithNA <- rowsWithNA+1 

Verkehrte_Studie <- Verkehrte_Studie[, -columnsWithNA]

Normale_Studie <- Normale_Studie[, -columnsWithNA]

##Recode the data

Verkehrte_Studie[Verkehrte_Studie=="1"] <- -4
Verkehrte_Studie[Verkehrte_Studie=="2"] <- -3
Verkehrte_Studie[Verkehrte_Studie=="3"] <- -2
Verkehrte_Studie[Verkehrte_Studie=="4"] <- -1
Verkehrte_Studie[Verkehrte_Studie=="5"] <- 0
Verkehrte_Studie[Verkehrte_Studie=="6"] <- 1
Verkehrte_Studie[Verkehrte_Studie=="7"] <- 2
Verkehrte_Studie[Verkehrte_Studie=="8"] <- 3
Verkehrte_Studie[Verkehrte_Studie=="9"] <- 4

Normale_Studie[Normale_Studie=="1"] <- -4
Normale_Studie[Normale_Studie=="2"] <- -3
Normale_Studie[Normale_Studie=="3"] <- -2
Normale_Studie[Normale_Studie=="4"] <- -1
Normale_Studie[Normale_Studie=="5"] <- 0
Normale_Studie[Normale_Studie=="6"] <- 1
Normale_Studie[Normale_Studie=="7"] <- 2
Normale_Studie[Normale_Studie=="8"] <- 3
Normale_Studie[Normale_Studie=="9"] <- 4

##Mann-Whitney-Wilcoxon Test

meansNormaleStudie <- colMeans(Normale_Studie[,c(2:454)])
meansNormaleStudie <- t(as.numeric(meansNormaleStudie))

meansVerkehrteStudie <- colMeans(Verkehrte_Studie[,c(2:454)])
meansVerkehrteStudie <- t(as.numeric(meansVerkehrteStudie))

resultWilcox <- wilcox.test(meansNormaleStudie,meansVerkehrteStudie)

#####Data analysis#####
wholeData <- read_excel("~/Pascal/R/Masterarbeit/allDataWholeData.xls",
                      sheet = "Tabelle1", na = "NA")

Item15 <- wholeData[15,2:99]
Item15=as.numeric(Item15)
Item24 <- wholeData[24,2:99]
Item24=as.numeric(Item24)
Item67 <- wholeData[67,2:99]
Item67=as.numeric(Item67)
Item108 <- wholeData[108,2:99]
Item108=as.numeric(Item108)
Item135 <- wholeData[135,2:99]
Item135=as.numeric(Item135)

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

plotReady <- data.frame(Item15=Item15, Item24=Item24, Item67=Item67, Item108=Item108, Item135=Item135)
plotReady2 <- gather(plotReady, key = Item, value = Ratings)

plotReady3=data.frame(Prediction108=Prediction108, Prediction135=Prediction135, Prediction15=Prediction15, Prediction24=Prediction24, Prediction67=Prediction67)


plotplotReady2 <- ggplot(data = plotReady2,
                        mapping = aes(x=Item,
                                      y=Ratings,
                                      color = Item),ylim=c(-4, 4))

plotplotReady2 + geom_boxplot(outlier.alpha = 0) + geom_jitter(width = 0.2, size = 3, alpha = 0.5) +
  labs(title = "Ratings der Vpn zu einzelnen Items") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold")) +
  theme(plot.title = element_text(size = 16))
endplotplotReady2 <- plotplotReady2 + geom_boxplot(outlier.alpha = 0) + geom_jitter(width = 0.2, size = 3, alpha = 0.5) +
  labs(title = "Ratings der Vpn zu einzelnen Items") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"))+
  theme(plot.title = element_text(size = 16))
ggsave(filename = "c:/Users/PDaddy/Documents/Pascal/R/Masterarbeit/plotplotReady2.png",endplotplotReady2, scale = 5,
       width = 2.4, height = 2, units = "in", dpi = 900)


wholeData[wholeData=="-4"] <- 4
wholeData[wholeData=="-3"] <- 3
wholeData[wholeData=="-2"] <- 2
wholeData[wholeData=="-1"] <- 1

matrixWholeData <- wholeData[,c(2:99)]
matrixWholeData <- as.matrix(matrixWholeData)
meansWholeData <- rowMeans(matrixWholeData)

##Correlation with Resnik

dataResnik <- ResultsClean$Resnik
dataResnik <- as.numeric(dataResnik)
corResultResnik <- cor.test(meansWholeData,dataResnik)

##Correlation with Lin

dataLin <- ResultsClean$Lin
dataLin <- as.numeric(dataLin)
corResultLin <- cor.test(meansWholeData,dataLin)

##Correlation with Li, Bandar und McLean

dataLi <- ResultsClean$`Li, Bandar & McLean`
dataLi <- as.numeric(dataLi)
corResultLi <- cor.test(meansWholeData,dataLi)

##MCorrelation with Cosine Distance

dataCosine <- ResultsClean$`Cosine Distance`
dataCosine <- as.numeric(dataCosine)
corResultCosine <- cor.test(meansWholeData,dataCosine)

##Correlation with Iacobacci

dataIacobacci <- ResultsClean$Iacobacci
dataIacobacci <- as.numeric(dataIacobacci)
corResultIacobacci <- cor.test(meansWholeData,dataIacobacci)

##Correlation with Iacobacci und Li

dataIacobacciLi <- ResultsClean$`Iacobacci and Li`
dataIacobacciLi <- as.numeric(dataIacobacciLi)
corResultIacobacciLi <- cor.test(meansWholeData,dataIacobacciLi)

##Correlation with Cosine und Li

dataCosineLi <- ResultsClean$`Cosine and Li`
dataCosineLi <- as.numeric(dataCosineLi)
corResultCosineLi <- cor.test(meansWholeData,dataCosineLi)

##Correlation with Lee

dataLee <- ResultsClean$Lee
dataLee <- as.numeric(dataLee)
corResultLee <- cor.test(meansWholeData,dataLee)

##Correlation with Iacobacci und Path

dataIacobacciPath <- ResultsClean$`Iacobacci and Path`
dataIacobacciPath <- as.numeric(dataIacobacciPath)
corResultIacobacciPath <- cor.test(meansWholeData,dataIacobacciPath)

##Create table of results
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

corResultsTable <- data.frame(Resnik = Resnik, Lin = Lin, Li = Li,
                                    Cosine = Cosine, Iacobacci = Iacobacci,
                                    Lee = Lee, CosineLi = CosineLi,
                                    IacobacciLi = IacobacciLi, IacobacciPath = IacobacciPath)

row.names(corResultsTable) <- c("Correlation","T-Value","df","p-value")
corResultsTable <- round(corResultsTable, 3)

#####Correlation of each person with the various methods#####

vp <- c(1:98)
vpWholeData <- wholeData[,2:99]


#Correlation of each person with Resnik

corSingleResnik <- 1
for(i in 1:98) {
  temp1 <- vpWholeData[,i]
  temp1 <- as.matrix(temp1)
  temp2 <- cor.test(temp1,dataResnik)
  corSingleResnik[i] <- temp2$estimate
}

#Correlation of each person with Lin

corSingleLin <- 1
for(i in 1:98) {
  temp1 <- vpWholeData[,i]
  temp1 <- as.matrix(temp1)
  temp2 <- cor.test(temp1,dataLin)
  corSingleLin[i] <- temp2$estimate
}

#Correlation of each person with Li

corSingleLi <- 1
for(i in 1:98) {
  temp1 <- vpWholeData[,i]
  temp1 <- as.matrix(temp1)
  temp2 <- cor.test(temp1,dataLi)
  corSingleLi[i] <- temp2$estimate
}

#Correlation of each person with Cosine

corSingleCosine <- 1
for(i in 1:98) {
  temp1 <- vpWholeData[,i]
  temp1 <- as.matrix(temp1)
  temp2 <- cor.test(temp1,dataCosine)
  corSingleCosine[i] <- temp2$estimate
}

#Correlation of each person with Iacobacci

corSingleIacobacci <- 1
for(i in 1:98) {
  temp1 <- vpWholeData[,i]
  temp1 <- as.matrix(temp1)
  temp2 <- cor.test(temp1,dataIacobacci)
  corSingleIacobacci[i] <- temp2$estimate
}

#Correlation of each person with IacobacciLi

corSingleIacobacciLi <- 1
for(i in 1:98) {
  temp1 <- vpWholeData[,i]
  temp1 <- as.matrix(temp1)
  temp2 <- cor.test(temp1,dataIacobacciLi)
  corSingleIacobacciLi[i] <- temp2$estimate
}

#Correlation of each person with CosineLi

corSingleCosineLi <- 1
for(i in 1:98) {
  temp1 <- vpWholeData[,i]
  temp1 <- as.matrix(temp1)
  temp2 <- cor.test(temp1,dataCosineLi)
  corSingleCosineLi[i] <- temp2$estimate
}

#Correlation of each person with Lee

corSingleLee <- 1
for(i in 1:98) {
  temp1 <- vpWholeData[,i]
  temp1 <- as.matrix(temp1)
  temp2 <- cor.test(temp1,dataLee)
  corSingleLee[i] <- temp2$estimate
}

#Correlation of each person with IacobacciPath

corSingleIacobacciPath <- 1
for(i in 1:98) {
  temp1 <- vpWholeData[,i]
  temp1 <- as.matrix(temp1)
  temp2 <- cor.test(temp1,dataIacobacciPath)
  corSingleIacobacciPath[i] <- temp2$estimate
}


##Creation of a plot of the single person correlations

corSingle <- data.frame(Resnik = corSingleResnik,
                              Lin = corSingleLin,
                              Li = corSingleLi,
                              Cosine = corSingleCosineLi,
                              Iacobacci = corSingleIacobacci,
                              IacobacciLi = corSingleIacobacciLi,
                              CosineLi = corSingleCosineLi,
                              Lee = corSingleLee,
                              IacobacciPath = corSingleIacobacciPath)

corSingle <- round(corSingle, 3)

corSingleLong <- gather(corSingle, key = Methode, value = Korrelation)
corSingleLong$Methode <- as.factor(corSingleLong$Methode)
plotCorSingle <- ggplot(data = corSingleLong,
                              mapping = aes(x=Methode,
                                            y=Korrelation,
                                            color = Methode))
plotCorSingle + geom_boxplot(outlier.alpha = 0) + geom_jitter(width = 0.2, size = 3, alpha = 0.5) +
  labs(title = "Korrelationen der Vpn zu den Methoden") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold")) +
  theme(plot.title = element_text(size = 16))
endPlotCorSingle <- plotCorSingle + geom_boxplot(outlier.alpha = 0) + geom_jitter(width = 0.2, size = 3, alpha = 0.5) +
  labs(title = "Korrelationen der Vpn zu den Methoden") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"))+
  theme(plot.title = element_text(size = 16))
ggsave(filename = "c:/Users/PDaddy/Documents/Pascal/R/Masterarbeit/plotCorSingle.png",endPlotCorSingle, scale = 5,
       width = 2.4, height = 2, units = "in", dpi = 900)

#####Search for best Lambda#####

ResultsLee <- read_excel("~/Pascal/R/Masterarbeit/ResultsLee.xlsx",
                        sheet = "Tabelle1", na = "NA")

ResultsLee[ResultsLee=='NaN'] <- NA

rowsWithNA <- unique (unlist (lapply (ResultsLee, function (x) which (is.na (x)))))

ResultsLee <- ResultsLee[-rowsWithNA, ]

##Correlation of each Lambda with human ratings

Lamda0 <- ResultsLee$Lamda0
Lamda0 <- as.numeric(Lamda0)
CorLamda0 <- cor.test(meansWholeData,Lamda0)
L0 <- CorLamda0$estimate

Lamda0.05 <- ResultsLee$Lamda0.05
Lamda0.05 <- as.numeric(Lamda0.05)
CorLamda0.05 <- cor.test(meansWholeData,Lamda0.05)
L0.05 <- CorLamda0.05$estimate

Lamda0.1 <- ResultsLee$Lamda0.1
Lamda0.1 <- as.numeric(Lamda0.1)
CorLamda0.1 <- cor.test(meansWholeData,Lamda0.1)
L0.1 <- CorLamda0.1$estimate

Lamda0.15 <- ResultsLee$Lamda0.15
Lamda0.15 <- as.numeric(Lamda0.15)
CorLamda0.15 <- cor.test(meansWholeData,Lamda0.15)
L0.15 <- CorLamda0.15$estimate

Lamda0.2 <- ResultsLee$Lamda0.2
Lamda0.2 <- as.numeric(Lamda0.2)
CorLamda0.2 <- cor.test(meansWholeData,Lamda0.2)
L0.2 <- CorLamda0.2$estimate

Lamda0.25 <- ResultsLee$Lamda0.25
Lamda0.25 <- as.numeric(Lamda0.25)
CorLamda0.25 <- cor.test(meansWholeData,Lamda0.25)
L0.25 <- CorLamda0.25$estimate

Lamda0.3 <- ResultsLee$Lamda0.3
Lamda0.3 <- as.numeric(Lamda0.3)
CorLamda0.3 <- cor.test(meansWholeData,Lamda0.3)
L0.3 <- CorLamda0.3$estimate

Lamda0.35 <- ResultsLee$Lamda0.35
Lamda0.35 <- as.numeric(Lamda0.35)
CorLamda0.35 <- cor.test(meansWholeData,Lamda0.35)
L0.35 <- CorLamda0.35$estimate

Lamda0.4 <- ResultsLee$Lamda0.4
Lamda0.4 <- as.numeric(Lamda0.4)
CorLamda0.4 <- cor.test(meansWholeData,Lamda0.4)
L0.4 <- CorLamda0.4$estimate

Lamda0.45 <- ResultsLee$Lamda0.45
Lamda0.45 <- as.numeric(Lamda0.45)
CorLamda0.45 <- cor.test(meansWholeData,Lamda0.45)
L0.45 <- CorLamda0.45$estimate

Lamda0.5 <- ResultsLee$Lamda0.5
Lamda0.5 <- as.numeric(Lamda0.5)
CorLamda0.5 <- cor.test(meansWholeData,Lamda0.5)
L0.5 <- CorLamda0.5$estimate

Lamda0.55 <- ResultsLee$Lamda0.55
Lamda0.55 <- as.numeric(Lamda0.55)
CorLamda0.55 <- cor.test(meansWholeData,Lamda0.55)
L0.55 <- CorLamda0.55$estimate

Lamda0.6 <- ResultsLee$Lamda0.6
Lamda0.6 <- as.numeric(Lamda0.6)
CorLamda0.6 <- cor.test(meansWholeData,Lamda0.6)
L0.6 <- CorLamda0.6$estimate

Lamda0.65 <- ResultsLee$Lamda0.65
Lamda0.65 <- as.numeric(Lamda0.65)
CorLamda0.65 <- cor.test(meansWholeData,Lamda0.65)
L0.65 <- CorLamda0.65$estimate

Lamda0.7 <- ResultsLee$Lamda0.7
Lamda0.7 <- as.numeric(Lamda0.7)
CorLamda0.7 <- cor.test(meansWholeData,Lamda0.7)
L0.7 <- CorLamda0.7$estimate

Lamda0.75 <- ResultsLee$Lamda0.75
Lamda0.75 <- as.numeric(Lamda0.75)
CorLamda0.75 <- cor.test(meansWholeData,Lamda0.75)
L0.75 <- CorLamda0.75$estimate

Lamda0.8 <- ResultsLee$Lamda0.8
Lamda0.8 <- as.numeric(Lamda0.8)
CorLamda0.8 <- cor.test(meansWholeData,Lamda0.8)
L0.8 <- CorLamda0.8$estimate

Lamda0.85 <- ResultsLee$Lamda0.85
Lamda0.85 <- as.numeric(Lamda0.85)
CorLamda0.85 <- cor.test(meansWholeData,Lamda0.85)
L0.85 <- CorLamda0.85$estimate

Lamda0.9 <- ResultsLee$Lamda0.9
Lamda0.9 <- as.numeric(Lamda0.9)
CorLamda0.9 <- cor.test(meansWholeData,Lamda0.9)
L0.9 <- CorLamda0.9$estimate

Lamda0.95 <- ResultsLee$Lamda0.95
Lamda0.95 <- as.numeric(Lamda0.95)
CorLamda0.95 <- cor.test(meansWholeData,Lamda0.95)
L0.95 <- CorLamda0.95$estimate

Lamda1 <- ResultsLee$Lamda1
Lamda1 <- as.numeric(Lamda1)
CorLamda1 <- cor.test(meansWholeData,Lamda1)
L1 <- CorLamda1$estimate

##Plot creation

CorLamda <- data.frame(L0 = L0,
                       L0.05=L0.05,
                       L0.1=L0.1,
                       L0.15=L0.15,
                       L0.2=L0.2,
                       L0.25=L0.25,
                       L0.3=L0.3,
                       L0.35=L0.35,
                       L0.4=L0.4,
                       L0.45=L0.45,
                       L0.5=L0.5,
                       L0.55=L0.55,
                       L0.6=L0.6,
                       L0.65=L0.65,
                       L0.7=L0.7,
                       L0.75=L0.75,
                       L0.8=L0.8,
                       L0.85=L0.85,
                       L0.9=L0.9,
                       L0.95=L0.95,
                       L1=L1)

CorLamda <- round(CorLamda, 3)

CorLamdaLong <- gather(CorLamda, key = Lamda, value = Correlation)
CorLamdaLong$Methode <- as.factor(CorLamdaLong$Methode)
plotCorLamdaLong <- ggplot(data = CorLamdaLong,
                        mapping = aes(x=Lamda,
                                      y=Correlation,
                                      color = Lamda))
plotCorLamdaLong + geom_boxplot(outlier.alpha = 0)  +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold")) +
  theme(plot.title = element_text(size = 16))
endplotCorLamdaLong <- plotCorLamdaLong + geom_boxplot(outlier.alpha = 0) + 
  theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"))+
  theme(plot.title = element_text(size = 16))
ggsave(filename = "c:/Users/PDaddy/Documents/Pascal/R/Masterarbeit/plotCorLamda.png",endplotCorLamdaLong, scale = 5,
       width = 2.4, height = 2, units = "in", dpi = 900)


#### linear models etc.####

frequenciesMW <- read_excel("~/Pascal/R/Masterarbeit/frequencyMW.xlsx",
                        sheet = "Tabelle1", na = "NA")
frequenciesMW <- frequenciesMW[-rowsWithNA, ]

frequenciesBig <- read_excel("~/Pascal/R/Masterarbeit/frequencyBig.xlsx",
                        sheet = "Tabelle1", na = "NA")
frequenciesBig <- frequenciesBig[-rowsWithNA, ]


##Correlation with MW frequencies
corResultTypeNormMW <- cor.test(frequenciesMW$Typefr.norm.,meansWholeData)

corResultTypeLogMW <- cor.test(frequenciesMW$`Typefr. log`,meansWholeData)

corResultLemmaNormMW <- cor.test(frequenciesMW$`Lemmafr. norm.`,meansWholeData)

corResultLemmaLogMW <- cor.test(frequenciesMW$`Lemmafr. Log`,meansWholeData)

##Correlation with Big frequencies
corResultTypeNormBig <- cor.test(frequenciesBig$Typefr.norm.,meansWholeData)

corResultTypeLogBig <- cor.test(frequenciesBig$`Typefr. log`,meansWholeData)

corResultLemmaNormBig <- cor.test(frequenciesBig$`Lemmafr. norm.`,meansWholeData)

corResultLemmaLogBig <- cor.test(frequenciesBig$`Lemmafr. Log`,meansWholeData)

##best frequency is type norm MW

## lm

pathBetween <- Results$`Path Between`
pathBetween <- as.numeric(pathBetween)
pathRoot <- Results$`Path Root`
pathRoot <- as.numeric(pathRoot)
senseComb <- Results$`Sense Comb`
senseComb <- as.numeric(senseComb)

dataLee <- Results$Lee
dataLee <- as.numeric(dataLee)

dataLi <- Results$`Li, Bandar & McLean`
dataLi <- as.numeric(dataLi)

dataLin <- Results$Lin
dataLin <- as.numeric(dataLin)

fit11 <- lm(meansWholeData ~ dataLee)
summary(fit11)

fit12 <- lm(meansWholeData ~ dataLi)
summary(fit12)

fit21 <- lm(meansWholeData ~ (dataLee+dataLin)^2)
summary(fit21)

fit22 <- lm(meansWholeData ~ (dataLee+frequenciesMW$Typefr.norm.)^2)
summary(fit22)

fit23 <- lm(meansWholeData ~ (dataLee+frequenciesBig$Typefr.norm.)^2)
summary(fit23)

fit31 <- lm(meansWholeData ~ (dataLee+dataLi+frequenciesMW$Typefr.norm.)^3)
summary(fit31)

fit32 <- lm(meansWholeData ~ (dataLee+dataLi+frequenciesMW$`Typefr. log`)^3)
summary(fit32)

fit33 <- lm(meansWholeData ~ (dataLee+dataLi+frequenciesMW$`Lemmafr. norm.`)^3)
summary(fit33)

fit34 <- lm(meansWholeData ~ (dataLee+dataLi+frequenciesMW$`Lemmafr. Log`)^3)
summary(fit34)

fit35 <- lm(meansWholeData ~ (dataLee+dataLi+frequenciesBig$Typefr.norm.)^3)
summary(fit35)

fit36 <- lm(meansWholeData ~ (dataLee+dataLi+frequenciesBig$`Typefr. log`)^3)
summary(fit36)

fit37 <- lm(meansWholeData ~ (dataLee+dataLi+frequenciesBig$`Lemmafr. norm.`)^3)
summary(fit37)

fit38 <- lm(meansWholeData ~ (dataLee+dataLi+dataLin+senseComb+pathRoot)^5)
summary(fit38)

fitTest <- lm(meansWholeData ~ (dataLee+dataLi+dataLin+frequenciesMW$Typefr.norm.+senseComb+pathRoot)^6)
summary(fitTest)

##Best model for low, middle and high frequencies

typeNormFreqMW <- frequenciesMW$Typefr.norm.
typeNormFreqMW <- as.numeric(typeNormFreqMW)

typeNormMW <- mean(typeNormFreqMW)
typeNormSD <- sd(typeNormFreqMW)
typeNormUpper <- typeNormFreqMW - (typeNormMW - typeNormSD)
typeNormMiddle <- typeNormFreqMW - (typeNormMW)
typeNormLower <- typeNormFreqMW - (typeNormMW + typeNormSD)

fit41 <- lm(meansWholeData ~ (dataLee+typeNormUpper)^2)
summary(fit41)

fit42 <- lm(meansWholeData ~ (dataLee+typeNormMiddle)^2)
summary(fit42)

fit43 <- lm(meansWholeData ~ (dataLee+typeNormLower)^2)
summary(fit43)

write.csv(meansWholeData, file = "Medians.csv",row.names=FALSE)