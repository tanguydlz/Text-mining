
#charger le document
setwd("C:/Users/ameli/Desktop/R/TextMining/Projet")
library(xlsx)
data<-read.xlsx2("Base_CEDA_nov_2020_anonymisee.xlsx", sheetIndex = 1,header = TRUE)

class(data)
str(data)
head(data)
summary(data)

#Première partie d'analyse sur l'inquiétude : 

#Principale Variable: 

#Var Raison sans données manquantes : 161 obs
VarRaison<-data$Raison[data$Raison!=""]
length(VarRaison)
#stat desc : 
lignes<-unlist(VarRaison)
vec.lignes<-unlist(lignes)
vec.lignes<-tolower(vec.lignes)
#une liste de chaque mot :
mot<-strsplit(vec.lignes,c(" "))
vec.mot<-unlist(mot)
#création du dictionnaire : 
#on garde que les mots uniques
vec.mot<-unique(vec.mot)
vec.mot<-sort(vec.mot)
mondico<-vec.mot
#Creation de la table de frequence des mots :
vec.mot<-unlist(mot)
table.mots<-table(vec.mot)
#Affichage du nuage de mot
library(wordcloud)
wordcloud(words=mondico, freq=table.mots,colors=brewer.pal(8, "Dark2"))

#Ajouté 

#Recodage 1er inquiétude en mois :
#Des la naissance : 0
data$X1eres.inquiétudes..mois.[str_detect(data$X1eres.inquiétudes..mois.,"naissance")]<-0
#Recodage années en mois :
for (i in 1:length(data$X1eres.inquiétudes..mois.)) {
  if (str_detect(data$X1eres.inquiétudes..mois.[i],"ans")) {
    data$X1eres.inquiétudes..mois.[i]<-as.numeric(substr(data$X1eres.inquiétudes..mois.[i],1,str_locate(data$X1eres.inquiétudes..mois.[i], " ans")[1]-1))*12
  }
}
#transforme la variable en numéric pour garder que les valeurs numéric
data$X1eres.inquiétudes..mois.<-as.numeric(data$X1eres.inquiétudes..mois.)
#stat desc : 
summary(data$X1eres.inquiétudes..mois.)
boxplot(data$X1eres.inquiétudes..mois.)
#en moyenne c'est à l'age de 31,5 mois que les parents ont leur première inquiétude

#Complément 

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
=======
  #charger le document
  setwd("C:/Users/ameli/Desktop/R/TextMining/Projet")
library(xlsx)
data<-read.xlsx2("Base_CEDA_nov_2020_anonymisee.xlsx", sheetIndex = 1,header = TRUE)

class(data)
str(data)
head(data)
summary(data)

#Première partie d'analyse sur l'inquiétude : 

#Principale Variable: 

#Var Raison sans données manquantes : 161 obs
data$Raison[data$Raison!=""]
length(data$Raison[data$Raison!=""])

#Ajouté 

#Recodage 1er inquiétude en mois :
#Des la naissance : 0
data$X1eres.inquiétudes..mois.[str_detect(data$X1eres.inquiétudes..mois.,"naissance")]<-0
#Recodage années en mois :
for (i in 1:length(data$X1eres.inquiétudes..mois.)) {
  if (str_detect(data$X1eres.inquiétudes..mois.[i],"ans")) {
    data$X1eres.inquiétudes..mois.[i]<-as.numeric(substr(data$X1eres.inquiétudes..mois.[i],1,str_locate(data$X1eres.inquiétudes..mois.[i], " ans")[1]-1))*12
  }
}

#Complément 

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
