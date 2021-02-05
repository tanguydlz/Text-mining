

```{r}
#charger le document
setwd("C:/Users/ameli/Desktop/R/TextMining/Text-mining")
library(xlsx)
df<-read.xlsx2("Base_CEDA_nov_2020_anonymisee.xlsx", sheetIndex = 1,header = TRUE)
library(tidytext)
```


Sujet 2 : "Quels inquiétudes et descriptions cliniques concordent avec un diagnostic avéré d'autisme après évaluation complète sur notre centre."

Nous allons prendre en compte la variable diagnostic dans cette deuxième analyse :

```{r}
VarDiag<-new_df$df.Diagnostic.[new_df$df.Diagnostic.!=""]
length(VarDiag)






    
    
    
    
    
    
    
    
```
La variable diagnostic posséde 3 valeurs manquantes. Donc nous enleverons ces individus de la suite de l'analyse. Notre nouvel échantillon posséde 183 individus.

Premièrement, nous allons voir les statistiques descriptives de cette variable.

Analyse sur 183 individus :

```{r}
library(dplyr)
tidytext <- tibble(line = 1:length(VarDiag), text = VarDiag)
#source
n=2
tidytext <- tidytext %>%
  unnest_tokens(ngram, text, token = "ngrams", n = n)  %>% 
  filter(!is.na(ngram))
if (n==1) {tidytext <- tidytext %>% anti_join(stop_words)}
tidytext <- tidytext %>% count(ngram, sort = TRUE) 
library(wordcloud)
wordcloud(tidytext$ngram,tidytext$n,max.words=20,color=c("lightblue3","lightblue4","darkcyan"))


tidytext %>% 
  filter(!is.na(ngram)) %>%
  filter(!word2 %in% stop_words$word) %>% 
  unite(word,word1, word2, sep = " ") %>% 
  count(word, sort = TRUE)
```


```{r}
#TransformMinuscule 
diag_corpus= tm_map(diag_corpus,content_transformer(tolower))
#removePunctuation
diag_corpus = tm_map(diag_corpus,removePunctuation)
#removeStopWord
diag_corpus = tm_map(diag_corpus,removeWords, stopwords('fr'))
#removeWhiteSpace
diag_corpus = tm_map(diag_corpus,stripWhitespace)
```

Matrice contenant les termes deux à deux : 
```{r}
diag_matrix=TermDocumentMatrix(diag_corpus, control = list(tokenize=BigramTokenizer))
```


```{r}

#matrice
M_diag <- as.matrix(diag_matrix)
print(nrow(M_diag))
print(ncol(M_diag))

#liste des termes
#print(colnames(M1))
#fréquence de chaque terme
freq_diag <- apply(M_diag,1,sum)

print(sort(freq_diag,decreasing=TRUE)[1:10])
```


```{r}
wordcloud(names(freq_diag),freq_diag,max.words=20,color=c("lightblue3","lightblue4","darkcyan"))
```

Les pairs de mots qui ressortent le plus dans cette variable sont "tsa retard", "retard language", "trouble language" "tsa tdah" et "tsa di", sur un échantillon de 183 individus.


Matrice contenant terme par terme : 
```{r}
diag_matrix2=TermDocumentMatrix(diag_corpus, control = list(tokenize=OnegramTokenizer))
```



```{r}

#matrice
M_diag2 <- as.matrix(diag_matrix2)
print(nrow(M_diag2))
print(ncol(M_diag2))

#liste des termes
#print(colnames(M1))
#fréquence de chaque terme
freq_diag2 <- apply(M_diag2,1,sum)

print(sort(freq_diag2,decreasing=TRUE)[1:10])
```


```{r}
wordcloud(names(freq_diag2),freq_diag2,max.words=20,color=c("lightblue3","lightblue4","darkcyan"))
```
Figure : Nuage de mots de la variable "diagnostic"

Ce nuage de est moins intéressant que celui comparant deux à deux les termes.


QUESTION : 
Comment reconnait-on un diagnostique d'autisme avérer grâce à cette variable ?

Uniquement ceux qui sont diagnostiqué "TSA" ?

