

library(shiny)

corpus_fct<-function(var){
    source=VectorSource(var)
    corpus=VCorpus(source)
    #modification
    corpus= tm_map(corpus,content_transformer(tolower))
    corpus = tm_map(corpus,removePunctuation)
    corpus = tm_map(corpus,removeWords, stopwords('fr'))
    corpus = tm_map(corpus,stripWhitespace)
    return(corpus)
}

#Fonction nb mots


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    library(tm)
    library(ngram)
    library(wordcloud)
    #setwd("/Users/hoangkhanhle/Desktop/School/Master 2/Text Mining/Projet")
    setwd("C:/Users/ameli/Desktop/R/TextMining/Text-mining")
    library(xlsx)
    df<-read.xlsx2("BDD_CEDA_dec_2020_complete - ANONYME.xlsx", sheetIndex = 1,header = TRUE)
    new_df<-data.frame(df$Préoccupations,df$Motif.de.consult,df$Diagnostic.,df$jeux.et.activités,df$Sommeil,df$Entrée.en.contact,df$Habitudes.de.l.enfant,df$Qui,df$Antécédents,df$X1eres.inquiétudes..mois.)
    
    #Variable
    rvar <- reactive({
        new_df[,input$var][new_df[,input$var]!=""]     })
    
    output$table<- renderTable({
        var<-rvar()
        gramTokenizer =function(x){
            unlist(lapply(ngrams(words(x), input$nb), paste, collapse = " "), use.names = FALSE)
        }
        matrix=TermDocumentMatrix(corpus_fct(var), control = list(tokenize=gramTokenizer))
        
        M <- as.matrix(matrix)
        freq <- apply(M,1,sum)
        tab<-data.frame(names(sort(freq,decreasing=TRUE)[1:10]),sort(freq,decreasing=TRUE)[1:10])
        colnames(tab)<-c("Mots","Frequence")
        tab
    })

    output$distPlot <- renderPlot({
        var<-rvar()
        gramTokenizer =function(x){
            unlist(lapply(ngrams(words(x), input$nb), paste, collapse = " "), use.names = FALSE)
        }
        matrix=TermDocumentMatrix(corpus_fct(var), control = list(tokenize=gramTokenizer))
        
        M <- as.matrix(matrix)
        freq <- apply(M,1,sum)
        wordcloud(names(freq),freq,max.words=20,color=c("lightblue3","lightblue4","darkcyan"))

    })

})
