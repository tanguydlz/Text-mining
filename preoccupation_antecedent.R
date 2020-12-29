#charger le document
library(xlsx)
data<-read.xlsx2("BDD_CEDA_dec_2020_complete - ANONYME.xlsx", sheetIndex = 1,header = TRUE)

class(data)
str(data)
head(data)
summary(data)

preoccupation = data$Préoccupations
antecedent = data$Antécédents.familiaux.
#Majuscule pour var diag
diag = str_to_upper(data$Diagnostic.)

X=as.data.frame(cbind(preoccupation,antecedent, diag))


#Recodage des variables, si 0 pour antécédent alors on met non
# Si valeur manquante, on considère NC
X[,2] = recode(X[,2], '0' = "non")
X[X$preoccupation == "",] = "NC"
X[X$antecedent == "",] = "NC"


#On garde seulement les individus avec un TSA
X2_TSA<-X[rownames(X)[str_detect(X$diag, "TSA")],]
X2_TSA = subset(X2_TSA, diag != "PAS DE TSA")
nrow(X2_TSA)

#On stocke ici seulement les individus non TSA
lgn = as.numeric(rownames(X2_TSA))
X2_nonTSA = X[-lgn,]

summary(X2_TSA[,3])

require(quanteda)

corpus_TSA <- corpus(X2_TSA[,1])
docvars(corpus)
token_TSA = tokens(corpus_TSA,remove_punct = T)
print(token_TSA)

toks_nostop <- tokens_select(token_TSA, pattern = stopwords("fr"), selection = "remove")
toks_nostop

dfmat_inaug <- dfm(toks_nostop)
print(dfmat_inaug)
