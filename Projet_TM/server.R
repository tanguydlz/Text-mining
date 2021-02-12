
    #setwd("D:/Téléchargements/M2/Text mining")
#setwd("C:/Users/ameli/Desktop/R/TextMining/Text-mining")
setwd("/Users/hoangkhanhle/Desktop/School/Master 2/Text Mining/Projet")

server <- function(input, output) {
    df<-read.xlsx2("BDD_CEDA_dec_2020_complete - ANONYME.xlsx", sheetIndex = 1,header = TRUE)
    new_df<-data.frame(df$Préoccupations,df$Motif.de.consult,df$Diagnostic.,df$jeux.et.activités,df$Sommeil,
                       df$Entrée.en.contact,df$Habitudes.de.l.enfant,df$Qui,df$Antécédents,df$X1eres.inquiétudes..mois.,
                       df$Marche..mois., df$Place.dans.la.fratrie,df$X1ers.mots, df$Sexe)
    X=new_df[,1:14]
    colnames(X) = c("preoccupations","motif", "diagnostique", "jeux","sommeil","contact","habitute","qui","antecedents",
                    "preminq", "marche", "fraterie", "mot","sexe")
    #donnée sujet 2 :
    X2=X[!str_detect(X$diagnostique, "pas de TSA") & !str_detect(X$diagnostique, "Pas de TSA") & X$diagnostique!="",]
    header=0
    
    FEEL_lex <- read_csv2("http://advanse.lirmm.fr/FEEL.csv")
    
    #Initialisation des variables réactives (df et le titre)
    text_df <- reactiveValues(data = tibble(line = 1:nrow(X), preoccupation = X[,1], motif=X[,2],diagnostique=X[,3], jeux=X[,4], sommeil=X[,5],contact=X[,6],habitute=X[,7],qui=X[,8],antecedents=X[,9])
    )
    
    text_reactive <- reactiveValues(
      text = "Inquiétude des parents"
    )
    
    X3 <- reactiveValues(
      data = X
    )
    
    #evenement lors des clics des boutons pour les sujets
    observeEvent(input$suj1, {
      text_df$data <- tibble(line = 1:nrow(X), preoccupation = X[,1], motif=X[,2],diagnostique=X[,3], jeux=X[,4], sommeil=X[,5],contact=X[,6],habitute=X[,7],qui=X[,8],antecedents=X[,9])
      text_reactive$text="Inquiétude des parents"
      X3$data=X
    })
    
    observeEvent(input$suj2, {
      text_df$data <- tibble(line = 1:nrow(X2), preoccupation = X2[,1], motif=X2[,2],diagnostique=X2[,3], jeux=X2[,4], sommeil=X2[,5],contact=X2[,6],habitute=X2[,7],qui=X2[,8],antecedents=X2[,9])
      text_reactive$text="Inquiétude et description cliniques des enfants étant diagnostiqué autiste (donnée réduite)"
      X3$data=X2
    })  
    
    #Titre selon le sujet choisi
    output$sujet<- renderText({
      text_reactive$text
    })
    
    #
    #X2$diagnostique
    
    #Word plot data
    var <- reactive({
      tidytext = text_df$data %>% unnest_tokens(word, as.numeric(input$selectVar))
      tidytext = tidytext[,-2]
      test_clean_df <- tidytext %>% filter(!word %in% c(stopwords('french'),'a','très','d\'un','qu\'il','nc','avoir','afin'))
      #Fréquence des mots
      freq=test_clean_df %>%
        count(word,sort=T)
      
    })
    
    #Sentiment analysis data
    feel <- reactive({
        tidytext = text_df$data %>% unnest_tokens(word, as.numeric(input$selectVar2))
        test_clean_df <- tidytext %>% filter(!word %in% c(stopwords('french'),'a','très','d\'un','qu\'il','nc','avoir','afin'))
        test_feel_df <- inner_join(test_clean_df, FEEL_lex, by = 'word')
    })
    
    
    #Sentiment analysis data
    feel <- reactive({
      tidytext = text_df$data %>% unnest_tokens(word, as.numeric(input$selectVar2))
      test_clean_df <- tidytext %>% filter(!word %in% c(stopwords('french'),'a','très','d\'un','qu\'il','nc','avoir','afin'))
      test_feel_df <- inner_join(test_clean_df, FEEL_lex, by = 'word')
    })
    
    #Radar Emotion
    emotion_score <- reactive({
      progress <- shiny::Progress$new()
      progress$set(message="Radar Emotion", value = 0.4)
      on.exit(progress$close())
      
      tidytext = text_df$data %>% unnest_tokens(word, as.numeric(input$selectVar2))
      tidytext = tidytext[,-2]
      test_clean_df <- tidytext %>% filter(!word %in% c(stopwords('french'),'a','très','d\'un','qu\'il','nc','avoir','afin'))
      
      progress$set(value = 0.6, detail = paste("Chercher l'emotion pour chaque mot ..."))
      nrc<- get_nrc_sentiment(test_clean_df$word,language="french", lowercase=TRUE)
      
      progress$set(value = 0.6, detail = paste("Calculer le total de chaque emotion.."))
      test_feel_df1<-as.data.frame(t(colSums(nrc[,1:8])))
      test_feel_df1 <- rbind(rep(max(test_feel_df1),8) , rep(0,8) , test_feel_df1)
      colnames(test_feel_df1)<-c("colère","anticipation","dégoût","peur","joie","tristesse","surprise","confiance")
      
      progress$set(value = 0.8, detail = paste("Sortir le résultat.."))
      test_feel_df1
    })
    
    afinn<-read.delim("/Users/hoangkhanhle/Desktop/School/Master 2/Text Mining/Projet/Khanh/AFINN.txt",header=0)
    colnames(afinn)<-c("word","score")
    
    afinn_score <- reactive({
      progress <- shiny::Progress$new()
      progress$set(message="AFINN score", value = 0.4)
      on.exit(progress$close())
      
      tidytext = text_df$data %>% unnest_tokens(word, as.numeric(input$selectVar2))
      test_clean_df <- tidytext %>% filter(!word %in% c(stopwords('french'),'a','très','d\'un','qu\'il','nc','avoir','afin'))
      progress$set(value = 0.8, detail = paste("Calculer le score pour chaque mot..."))
      
      afinn_df <- inner_join(test_clean_df, afinn, by = 'word')
    })
    
    
    output$radar <- renderPlot({
      
      if(input$radioSentiment==1){
        
        afinn_df <- afinn_score()
        #Compte le nombre d'occurence et son sentiment (négatif ou positif)
        occur_sentiment = afinn_df %>%
          count(word,score,sort=TRUE)
        
        occur_sentiment%>%
          mutate(contribution = n * score) %>%
          arrange(desc(abs(contribution))) %>%
          head(input$numWords2) %>%
          mutate(word = reorder(word, contribution)) %>%
          ggplot(aes(word, contribution, fill = contribution > 0)) +
          geom_col(show.legend = FALSE) +
          xlab("Les n mots les plus contributifs au sentiment") +
          ylab("Sentiment score * nombre d'occurrences")+
          coord_flip()
      }
      else if (input$radioSentiment==2){
        
        test_feel_df1 <- emotion_score()
        radarchart( test_feel_df1  , axistype=1 , 
                    
                    #custom polygon
                    pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
                    
                    #custom the grid
                    cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,300,50), cglwd=0.8,
                    
                    #custom labels
                    vlcex=0.8 
        )}
      
    })
    
    #n-gram visualization data
    ngram <- reactive ({
      tidytext <- text_df$data %>% unnest_tokens(bigram, as.numeric(input$selectVar3),token = "ngrams", n = 2)
      tidytext=tidytext[,-2]
      
      bigrams_separated <- tidytext %>%
        separate(bigram, c("word1", "word2"), sep = " ")
      
      bigrams_filtered <- bigrams_separated %>%
        filter(!word1 %in% c(stopwords('french'),'a','très','d\'un','qu\'il','nc','avoir','afin')) %>%
        filter(!word2 %in% c(stopwords('french'),'a','très','d\'un','qu\'il','nc','avoir','afin')) %>%
        filter(!is.na(word1)) %>%
        filter(!is.na(word2))
      
      bigram_counts <- bigrams_filtered %>% 
        count(word1, word2, sort = TRUE)
    })
    
    
    
    #Corrplot data
    corr <- reactive ({
      tidytext = text_df$data %>% unnest_tokens(word, as.numeric(input$selectVar4))
      tidytext = tidytext[,-2]
      test_clean_df <- tidytext %>% filter(!word %in% c(stopwords('french'),'a','très','d\'un','qu\'il','nc','avoir','afin'))
      word_cors <- test_clean_df %>%
        group_by(word) %>%
        filter(n() >= 5) %>%
        pairwise_cor(word, line, sort = TRUE)
    })
    
    #Comparaison
    #fonction 
    
    trfct<-function(age) {
      tranche<-age
      if (is.na(age)) {
        tranche<-""
      } else if (age<18) {
        tranche<-"inf18"
      } else if (age<24) {
        tranche<-"entre18et24"
      } else if (age<36) {
        tranche<-"entre24et36"
      } else {
        tranche<-"sup36"
      }
    }
    
    trfct2<-function(frat) {
      tranche<-frat
      if (is.na(frat)) {
        tranche<-""
      } else if (frat==1) {
        tranche<-"un"
      } else {
        tranche<-"plus"
      }
    }
    
    trfct3<-function(mar) {
      tranche<-mar
      if (is.na(mar)) {
        tranche<-""
      } else if (mar<=12.75) {
        tranche<-"petit"
      } else {
        tranche<-"grand"
      }
    }
    
    trfct4<-function(mot) {
      tranche<-mot
      if (is.na(mot)) {
        tranche<-""
      } else if (mot<=12) {
        tranche<-"inf12"
      } else if (mot<=16) {
        tranche<-"entre12et16"
      } else if (mot<=24) {
        tranche<-"entre16et24"
      } else {
        tranche<-"sup24"
      }
    }
    
    trfct5<-function(som) {
      tranche<-som
      if (is.na(som)) {
        tranche<-""
      } else if (!str_detect(som, "non") & !str_detect(som, "Non")) {
        tranche<-"oui"
      } else {
        tranche<-"non"
      }
    }
    
    trfct6<-function(dia) {
      tranche<-dia
      if (is.na(dia)) {
        tranche<-""
      } else if (!str_detect(dia, "pas de TSA") & !str_detect(dia, "Pas de TSA")) {
        tranche<-"TSA"
      } else {
        tranche<-"Pas de TSA"
      }
    }
    
    wc<-function(X3,vartext){
      test <- tibble(line = 1:nrow(X3), preoccupation = X3[,1], motif=X3[,2])
      if (vartext=="2") {
        tidytext = test %>% unnest_tokens(word, as.numeric(preoccupation))
      } else {
        tidytext = test %>% unnest_tokens(word, as.numeric(motif))
      }
      tidytext = tidytext[,-2]
      test_clean_df <- tidytext %>% filter(!word %in% c(stopwords('french'),'a','très','d\'un','qu\'il','nc','avoir','afin'))
      #Fréquence des mots
      freq=test_clean_df %>%
        count(word,sort=T)
    }
    
    compdata<-reactive({
      comp<-X3$data
      comp$tranche<-unlist(lapply(comp$preminq, trfct))
      comp$mar<-unlist(lapply(as.numeric(comp$marche), trfct3))
      comp$frat<-unlist(lapply(as.numeric(comp$fraterie), trfct2))
      comp$mots<-unlist(lapply(as.numeric(comp$mot), trfct4))
      comp$som<-unlist(lapply(comp$sommeil, trfct5))
      comp$sex<-comp$sexe
      comp$dia<-unlist(lapply(comp$diagnostique, trfct6))
      comp
    })
    
    mod<-reactive({
      if (input$selectVar6=="2") {
        mod=c("inf18","entre18et24","entre24et36","sup36")
      } else if (input$selectVar6=="4") {
        mod=c("un","plus")
      } else if (input$selectVar6=="3") {
        mod=c("petit","grand")
      } else if (input$selectVar6=="5") {
        mod=c("inf12","entre12et16","entre16et24","sup24")
      } else if (input$selectVar6=="6") {
        mod=c("oui","non")
      } else if (input$selectVar6=="7") {
        mod=c("F","M")
      } else if (input$selectVar6=="8") {
        mod=c("TSA","Pas de TSA")
      }
      
      mod
    })
    
    #A faire: sujet 2
    
    compfct<- reactive({
      mod<-mod()
      comp<-compdata()
      tranche1<-comp[comp[,(as.numeric(input$selectVar6)+13)]==mod[1],]
      graph1<-wc(tranche1,input$selectVar5)
    })
    
    compfct2<- reactive({
      mod<-mod()
      comp<-compdata()
      tranche2<-comp[comp[,(as.numeric(input$selectVar6)+13)]==mod[2],]
      graph2<-wc(tranche2,input$selectVar5)
    })
    
    compfct3<- reactive({
      mod<-mod()
      comp<-compdata()
      tranche3<-comp[comp[,(as.numeric(input$selectVar6)+13)]==mod[3],]
      graph3<-wc(tranche3,input$selectVar5)
    })
    
    compfct4<- reactive({
      mod<-mod()
      comp<-compdata()
      tranche4<-comp[comp[,(as.numeric(input$selectVar6)+13)]==mod[4],]
      graph4<-wc(tranche4,input$selectVar5)
    })

    hues <- c(60:330)
    
    output$mod1 <- renderText({
      res=mod()
      res[1]
    })
    
    output$mod2 <- renderText({
      res=mod()
      res[2]
    })
    
    output$mod3 <- renderText({
      res=mod()
      res[3]
    })
    
    output$mod4 <- renderText({
      res=mod()
      res[4]
    })
    
    output$comp<- renderWordcloud2({
      freq<-compfct()
      new_df <- freq[1:input$max4,]
      wordcloud2(new_df, color="random-light", size = .3, shuffle=T, rotateRatio = sample(c(1:100) / 100))
    })
    
    output$comp2<- renderWordcloud2({
      freq<-compfct2()
      new_df <- freq[1:input$max4,]
      wordcloud2(new_df, color="random-light", size = .3, shuffle=T, rotateRatio = sample(c(1:100) / 100))
    })
    
    output$comp3<- renderWordcloud2({
      freq<-compfct3()
      new_df <- freq[1:input$max4,]
      wordcloud2(new_df, color="random-light", size = .3, shuffle=T, rotateRatio = sample(c(1:100) / 100))
    })
    
    output$comp4<- renderWordcloud2({
      freq<-compfct4()
      new_df <- freq[1:input$max4,]
      wordcloud2(new_df, color="random-light", size = .3, shuffle=T, rotateRatio = sample(c(1:100) / 100))
    })
    
        #word frequency barplot
        output$freqPlot <- renderPlot({
            freq <- var()
            new_df <- freq[1:input$numWords,]
            ggplot(new_df, aes(x=reorder(word, n), y=n, fill = as.factor(word))) +
                geom_bar(stat = "identity", position = "dodge", col= "black") + xlab("Mots") + ylab("Nombre") +
                coord_flip() + scale_fill_hue(c = sample(hues, 1)) + guides(fill=FALSE)
        })

        
        #wordcloud world plot
        output$wordPlot <- renderWordcloud2({
            freq <- var()
            validate(
                need(input$max <= nrow(freq), "Selected size greater than number of elements in data")
            )
            new_df <- freq[1:input$max,]
            wordcloud2(new_df, color="random-light", size = .3, shuffle=T, rotateRatio = sample(c(1:100) / 100))
        })
        
        #wordcloud generation
        observeEvent(input$newCloud, handlerExpr = {
            output$wordPlot <- renderWordcloud2({
                freq <- var()
                validate(
                    need(input$max <= nrow(freq), "Selected size greater than number of elements in data")
                )
                new_df <- freq[1:input$max,]
                wordcloud2(new_df, color = "random-light", shuffle=T, size = .3, rotateRatio = sample(c(1:100) / 100))
            })
        })
        
        #occurence sentiment plot
        output$sentiment <- renderPlot({
            
            if(input$radioSentiment==1){
            test_feel_df <- feel()
            #Compte le nombre d'occurence et son sentiment (négatif ou positif)
            occur_sentiment = test_feel_df %>%
                count(word, polarity, sort = TRUE)
            
            #graphique occurence par sentiment
            occur_sentiment %>%
                group_by(polarity) %>%
                top_n(input$numWords2) %>%
                ungroup() %>%
                mutate(word = reorder(word, n)) %>%
                ggplot(aes(n, word, fill = polarity)) +
                geom_col(show.legend = FALSE) +
                facet_wrap(~polarity, scales = "free_y") +
                labs(x = "Contribution to sentiment",
                     y = NULL)
            
            } else if(input$radioSentiment==2){
                
                test_feel_df <- feel()
                test_feel_df = as.data.frame(test_feel_df)
                test_feel_df[,11] = NA
                for(i in 1:nrow(test_feel_df)){
                    for(j in 5:10){
                        if (test_feel_df[i,j]==1){
                            test_feel_df[i,11] = colnames(test_feel_df)[j]
                        }
                    }
                }
                
                colnames(test_feel_df)[11]="sentiment"
                
                #Compte le nombre d'occurence et son sentiment (joy, anger...)
                occur_sentiment2 = test_feel_df %>%
                    count(word, sentiment, sort = TRUE)
                
                occur_sentiment2=occur_sentiment2 %>% filter(!is.na(sentiment))
                
                #graphique occurence par sentiment
                occur_sentiment2 %>%
                    group_by(sentiment) %>%
                    top_n(input$numWords2) %>%
                    ungroup() %>%
                    mutate(word = reorder(word, n)) %>%
                    ggplot(aes(n, word, fill = sentiment)) +
                    geom_col(show.legend = FALSE) +
                    facet_wrap(~sentiment, scales = "free_y") +
                    labs(x = "Contribution to sentiment",
                         y = NULL)
                
            }


        })
    
        output$wordCloudSentiment <- renderPlot({
            if(input$radioSentiment==1){
            
            test_feel_df <- feel()
            par(mar = rep(0, 4))
            #La taille du texte d'un mot dans la figure 2.6 est proportionnelle Ã  sa fréquence au sein de son sentiment.
            #Nous pouvons utiliser cette visualisation pour voir les mots positifs et négatifs les plus importants,
            #mais la taille des mots n'est pas comparable d'un sentiment Ã  l'autre
            test_feel_df %>%
                count(word, polarity, sort = TRUE) %>%
                acast(word ~ polarity, value.var = "n", fill = 0) %>%
                comparison.cloud(colors = c("darkred", "darkgreen"),
                                     max.words = input$max2)
            
            }else if(input$radioSentiment==2){
                
                test_feel_df <- feel()
                par(mar = rep(0, 4))
                
                test_feel_df <- feel()
                test_feel_df = as.data.frame(test_feel_df)
                test_feel_df[,11] = NA
                for(i in 1:nrow(test_feel_df)){
                    for(j in 5:10){
                        if (test_feel_df[i,j]==1){
                            test_feel_df[i,11] = colnames(test_feel_df)[j]
                        }
                    }
                }
                
                colnames(test_feel_df)[11]="sentiment"
                
                test_feel_df %>%
                    count(word, sentiment, sort = TRUE) %>%
                    acast(word ~ sentiment, value.var = "n", fill = 0) %>%
                    comparison.cloud(colors = c("darkred", "darkgreen","purple","darkblue", "darkgray","magenta"),
                                     max.words = input$max2)
            }
        })
        
        output$ngramPlot <- renderPlot({
          
          bigram_counts <- ngram()
          bigram_graph <- bigram_counts %>%
            filter(n > 1) %>%
            graph_from_data_frame()
          
          set.seed(2017)
          a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
          
          ggraph(bigram_graph, layout = "fr") +
            geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                           arrow = a, end_cap = circle(.07, 'inches')) +
            geom_node_point(color = "lightblue", size = 5) +
            geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
            theme_void()
        })
        
        output$bigramWordCloud2 <- renderWordcloud2({
          
          bigram_counts <- ngram()
          
          
          bigrams_united <- bigram_counts %>%
            unite(bigram, word1, word2, sep = " ")
          
          validate(
            need(input$max <= nrow(bigrams_united), "Selected size greater than number of elements in data")
          )
          new_df <- bigrams_united[1:input$max3,]
          wordcloud2(new_df, size = .3, shuffle=T, rotateRatio = sample(c(1:100) / 100))
          
        })
        
        output$bigramBarplot <- renderPlot({
          
          bigram_counts <- ngram()
          
          
          bigrams_united <- bigram_counts %>%
            unite(bigram, word1, word2, sep = " ")
          
          new_df <- bigrams_united[1:input$numWords3,]
          
          ggplot(new_df, aes(x=reorder(bigram, n), y=n, fill = as.factor(bigram))) +
            geom_bar(stat = "identity", position = "dodge", col= "black") + xlab("Bigram") + ylab("Frequency") +
            coord_flip() + scale_fill_hue(c = sample(hues, 1)) + guides(fill=FALSE) + ggtitle("Frequency of each Bigram") + 
            theme(plot.title = element_text(color = "red", size = 12, face = "bold", hjust = 0.5))
          
        })
        
        #wordcloud generation
        observeEvent(input$newCloud2, handlerExpr = {
          output$bigramWordCloud2 <- renderWordcloud2({
            
            bigram_counts <- ngram()
            
            
            bigrams_united <- bigram_counts %>%
              unite(bigram, word1, word2, sep = " ")
            
            validate(
              need(input$max <= nrow(bigrams_united), "Selected size greater than number of elements in data")
            )
            new_df <- bigrams_united[1:input$max3,]
            wordcloud2(new_df, size = .3, shuffle=T, rotateRatio = sample(c(1:100) / 100))
            
          })
        })
        
        output$corrPlot <- renderPlot({
          
          word_cors <- corr()
          
          word_cors %>%
            filter(correlation > input$phiNum) %>%
            graph_from_data_frame() %>%
            ggraph(layout = "fr") +
            geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
            geom_node_point(color = "lightblue", size = 5) +
            geom_node_text(aes(label = name), repel = TRUE) +
            theme_void()
          
          
        })
        
        
        
        output$corrTable <- renderTable({
          
          word_cors <- corr()
          
          as.data.frame(word_cors %>%
                          filter(correlation > input$phiTable) %>%
                          filter(item1 == input$word))
        })
        
        observeEvent(input$randomWord, handlerExpr = {
          word_cors <- corr()
          ls=unlist(word_cors[,1])
          ls=unname(ls)
          word=sample(ls,size=1,replace=F)
          updateTextInput(session, "word", value=word)
        })
        
        
        output$corrPlotTable <- renderPlot({
          
          word_cors <- corr()
          
          word_cors %>%
            filter(item1 == input$word) %>%
            group_by(item1) %>%
            top_n(input$corrNum) %>%
            ungroup() %>%
            mutate(item2 = reorder(item2, correlation)) %>%
            ggplot(aes(item2, correlation, fill=as.factor(item2))) +
            geom_bar(stat = "identity") +
            facet_wrap(~ item1, scales = "free") +
            coord_flip() + guides(fill=FALSE)
          
        })



        
    

}#server
