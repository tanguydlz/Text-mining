
#charger le document
setwd("C:/Users/ameli/Desktop/R/TextMining/Text-mining")
library(xlsx)
data<-read.xlsx2("Base_CEDA_nov_2020_anonymisee.xlsx", sheetIndex = 1,header = TRUE)

#Prendre en compte le diagnostic dans une deuxième partie d'analyse :

#Var diagnostic sans données manquantes : 159 obs
data$Diagnostic.[data$Diagnostic.!=""]
length(data$Diagnostic.[data$Diagnostic.!=""])

#Données comportant les enfants qui ont un diagnostic avere d'autisme :
#Les enfants sui ont "TSA" dans leur diagnostic ?
library(stringr)
data2<-data[rownames(data)[str_detect(data$Diagnostic., "TSA")],]
#Plus que 103 obs
dim(data2)

str(data2)
head(data2)
summary(data2)
