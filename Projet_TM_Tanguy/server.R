
    setwd("D:/Téléchargements/M2/Text mining")


server <- function(input, output, session) {
    df<-read.xlsx2("BDD_CEDA_dec_2020_complete - ANONYME.xlsx", sheetIndex = 1,header = TRUE)
    X<-data.frame(df$Préoccupations,df$Motif.de.consult,df$Diagnostic.,df$jeux.et.activités,df$Sommeil,
                  df$Entrée.en.contact,df$Habitudes.de.l.enfant,df$Qui,df$Antécédents,
                  df$Sexe,df$Raison, df$Profession.mère, df$Profession.père,
                  df$Code.postal, df$X1eres.inquiétudes..mois.,
                  df$Marche..mois., df$Place.dans.la.fratrie,df$X1ers.mots)
    
    load(file="FEEL_lex.Rdata")
    
    
    #nettoyage des données
    for(i in 1:13){
      for(j in 1:nrow(X)){
        #suppression de caractères spéciaux
        X[j,i] <- gsub("['`^~\"]", " ", X[j,i])
        #Suppression des accents
        X[j,i] <- iconv(X[j,i],from="UTF-8",to="ASCII//TRANSLIT")
        X[j,i] <- gsub("['`^~\"]", "", X[j,i])
        #suppression des chiffres
        X[j,i] <- gsub('\\d+', '', X[j,i])
        #temp <- lapply(temp, function(x) sub("(.*)s$", '\\1', x))
      }
    }
    
    colnames(X) = c("preoccupations","motif", "diagnostique", "jeux","sommeil","contact","habitute","qui","antecedents",
                    "sexe", "raison", "professionM","professionP", "cp", "inquietudeM", "marche","place","mot")
    
    X[,15] = replace(as.numeric(X[,15]), is.na(as.numeric(X[,15])),0)
    X[,16] = replace(as.numeric(X[,16]), is.na(as.numeric(X[,16])),0)
    X[,18] = replace(as.numeric(X[,18]), is.na(as.numeric(X[,18])),0)
    
    
    X2=X[!str_detect(X$diagnostique, "pas de TSA") & !str_detect(X$diagnostique, "Pas de TSA") & X$diagnostique!="" & str_detect(X$diagnostique, "TSA"),]
    
    suj <- reactive({
      if(input$sujet == 1){
        text_df <- tibble(line = 1:nrow(X), preoccupation = X[,1], motif=X[,2],diagnostique=X[,3], jeux=X[,4], sommeil=X[,5],
                          contact=X[,6],habitute=X[,7],qui=X[,8],antecedents=X[,9],sexe = X[,10], raison = X[,11], professionM = X[,12],
                          professionP = X[,13], cp= X[,14], inquietudeM = X[,15], marche = X[,16], place = X[,17], mot = X[,18])
      } else if(input$sujet== 2){
        text_df <- tibble(line = 1:nrow(X2), preoccupation = X2[,1], motif=X2[,2],diagnostique=X2[,3], jeux=X2[,4], sommeil=X2[,5],contact=X2[,6],habitute=X2[,7],qui=X2[,8],antecedents=X2[,9],
                          sexe = X2[,10], raison = X2[,11], professionM = X2[,12],
                          professionP = X2[,13], cp= X2[,14], inquietudeM = X2[,15], marche = X2[,16], place = X2[,17], mot = X2[,18])
      }
    })
    
    #text_df <- tibble(line = 1:nrow(X), preoccupation = X[,1], motif=X[,2],diagnostique=X[,3], jeux=X[,4], sommeil=X[,5],contact=X[,6],habitute=X[,7],qui=X[,8],antecedents=X[,9])
    
    #Titre selon le sujet choisi
      output$sujet<- renderText({
        if(input$sujet == 1){
          "Inquiétude des parents"
        } else if(input$sujet== 2){
          "Inquiétudes et descriptions cliniques concordant avec un diagnostic avéré d’autisme"
        }
      })



    #Word plot data
    var <- reactive({
        text_df <- suj()
        tidytext = text_df %>% unnest_tokens(word, as.numeric(input$selectVar)) %>% select(line,word)
        test_clean_df <- tidytext %>% filter(!word %in% c(stopwords('french'),'a','tres','d\'un','qu\'il','nc','afin','ans'))
        #Fréquence des mots
        freq=test_clean_df %>%
        count(word,sort=T)
    
    })
    
    #Sentiment analysis data
    feel <- reactive({
        text_df <- suj()
        tidytext = text_df %>% unnest_tokens(word, as.numeric(input$selectVar2)) %>% select(line,word)
        test_clean_df <- tidytext %>% filter(!word %in% c(stopwords('french'),'a','tres','d\'un','qu\'il','nc','avoir','afin','ans'))
        test_feel_df <- inner_join(test_clean_df, FEEL_lex, by = 'word')
    })
    
    #bigram visualization data
    ngram <- reactive({
      text_df <- suj()
      tidytext <- text_df %>% unnest_tokens(bigram, as.numeric(input$selectVar3),token = "ngrams", n = 2) %>% select(line, bigram)
      
      bigrams_separated <- tidytext %>%
        separate(bigram, c("word1", "word2"), sep = " ")
      
      bigrams_filtered <- bigrams_separated %>%
          filter(!word1 %in% c(stopwords('french'),'a','tres','d\'un','qu\'il','nc','avoir','afin','ans')) %>%
          filter(!word2 %in% c(stopwords('french'),'a','tres','d\'un','qu\'il','nc','avoir','afin','ans')) %>%
          filter(!is.na(word1)) %>%
          filter(!is.na(word2))
        
      bigram_counts <- bigrams_filtered %>% 
            count(word1, word2, sort = TRUE)
    })
    

    
    #Corrplot data
    corr <- reactive ({
      text_df <- suj()
      tidytext = text_df %>% unnest_tokens(word, as.numeric(input$selectVar4)) %>% select(line,word)
      test_clean_df <- tidytext %>% filter(!word %in% c(stopwords('french'),'a','tres','d\'un','qu\'il','nc','avoir','afin','ans'))
      word_cors <- test_clean_df %>%
        group_by(word) %>%
        filter(n() >= 5) %>%
        pairwise_cor(word, line, sort = TRUE)
    })
    
    
    #Comparison data
    comp <- reactive({
      text_df <- suj()
      if(input$selectOtherVar==11){
        text_df = text_df %>% filter(sexe %in% input$sex)
      }else if(input$selectOtherVar==13){
        text_df = text_df %>% filter(professionM %in% input$professionM)
      }else if(input$selectOtherVar==14){
        text_df = text_df %>% filter(professionP %in% input$professionP)
      }else if(input$selectOtherVar==15){
        text_df = text_df %>% filter(cp %in% input$cp)
      }else if(input$selectOtherVar==16){
        text_df = text_df %>% filter(inquietudeM %in% input$inquietudeM[1]:input$inquietudeM[2])
      }else if(input$selectOtherVar==17){
        text_df = text_df %>% filter(marche %in% input$marche[1]:input$marche[2])
      }else if(input$selectOtherVar==18){
        text_df = text_df %>% filter(place %in% input$fraterie)
      }else if(input$selectOtherVar==19){
        text_df = text_df %>% filter(mot %in% input$mots[1]:input$mots[2])
      }
      tidytext = text_df %>% unnest_tokens(word, as.numeric(input$selectVar5)) %>% select(line,word)
      test_clean_df <- tidytext %>% filter(!word %in% c(stopwords('french'),'a','tres','d\'un','qu\'il','nc','afin','ans'))
      #Fréquence des mots
      freq=test_clean_df %>%
        count(word,sort=T)
      
    })
    
    #Distribution comparison data
    distri <- reactive({
      text_df <- suj()
      if(input$selectOtherVar==11){
        text_df = text_df %>% filter(sexe %in% input$sex) %>% count(sexe)
      }else if(input$selectOtherVar==13){
        text_df = text_df %>% filter(professionM %in% input$professionM) %>% count(professionM)
      }else if(input$selectOtherVar==14){
        text_df = text_df %>% filter(professionP %in% input$professionP) %>% count(professionP)
      }else if(input$selectOtherVar==15){
        text_df = text_df %>% filter(cp %in% input$cp) %>% count(cp)
      }else if(input$selectOtherVar==16){
        text_df = text_df %>% filter(inquietudeM %in% input$inquietudeM[1]:input$inquietudeM[2]) %>% count(inquietudeM)
      }else if(input$selectOtherVar==17){
        text_df = text_df %>% filter(marche %in% input$marche[1]:input$marche[2]) %>% count(marche)
      }else if(input$selectOtherVar==18){
        text_df = text_df %>% filter(place %in% input$fraterie) %>% count(place)
      }else if(input$selectOtherVar==19){
        text_df = text_df %>% filter(mot %in% input$mots[1]:input$mots[2]) %>% count(mot)
      }
    })

    #bigram comparison data
    bigram <- reactive({
      text_df <- suj()
      
      if(input$selectOtherVar==11){
        text_df = text_df %>% filter(sexe %in% input$sex)
      }else if(input$selectOtherVar==13){
        text_df = text_df %>% filter(professionM %in% input$professionM)
      }else if(input$selectOtherVar==14){
        text_df = text_df %>% filter(professionP %in% input$professionP)
      }else if(input$selectOtherVar==15){
        text_df = text_df %>% filter(cp %in% input$cp)
      }else if(input$selectOtherVar==16){
        text_df = text_df %>% filter(inquietudeM %in% input$inquietudeM[1]:input$inquietudeM[2])
      }else if(input$selectOtherVar==17){
        text_df = text_df %>% filter(marche %in% input$marche[1]:input$marche[2])
      }else if(input$selectOtherVar==18){
        text_df = text_df %>% filter(place %in% input$fraterie)
      }else if(input$selectOtherVar==19){
        text_df = text_df %>% filter(mot %in% input$mots[1]:input$mots[2])
      }
      
      tidytext <- text_df %>% unnest_tokens(bigram, as.numeric(input$selectVar5),token = "ngrams", n = 2) %>% select(line, bigram)
      
      bigrams_separated <- tidytext %>%
        separate(bigram, c("word1", "word2"), sep = " ")
      
      bigrams_filtered <- bigrams_separated %>%
        filter(!word1 %in% c(stopwords('french'),'a','tres','d\'un','qu\'il','nc','avoir','afin','ans')) %>%
        filter(!word2 %in% c(stopwords('french'),'a','tres','d\'un','qu\'il','nc','avoir','afin','ans')) %>%
        filter(!is.na(word1)) %>%
        filter(!is.na(word2))
      
      bigram_counts <- bigrams_filtered %>% 
        count(word1, word2, sort = TRUE)
    })
    
    
    
    
    
    hues <- c(60:330)

#########################################################
#########################################################
#########################################################
    
    
      ###Word plots
        #word frequency barplot
        output$freqPlot <- renderPlot({
            freq <- var()
            new_df <- freq[1:input$numWords,]
            ggplot(new_df, aes(x=reorder(word, n), y=n, fill = as.factor(word))) +
                geom_bar(stat = "identity", position = "dodge", col= "black") + xlab("Mots") + ylab("Nombre") +
                coord_flip() + scale_fill_hue(c = sample(hues, 1)) + guides(fill=FALSE)
        })

        
        #wordcloud word plot
        output$wordPlot <- renderWordcloud2({
            freq <- var()
            validate(
                need(input$max <= nrow(freq), "Selected size greater than number of elements in data")
            )
            new_df <- freq[1:input$max,]
            wordcloud2(new_df, color="random-light", size = .4, shuffle=T, rotateRatio = sample(c(1:100) / 100))
        })
        
        #wordcloud generation
        observeEvent(input$newCloud, handlerExpr = {
            output$wordPlot <- renderWordcloud2({
                freq <- var()
                validate(
                    need(input$max <= nrow(freq), "Selected size greater than number of elements in data")
                )
                new_df <- freq[1:input$max,]
                wordcloud2(new_df, color = "random-light", shuffle=T, size = .4, rotateRatio = sample(c(1:100) / 100))
            })
        })
    ###Sentiment Analysis 
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
           
            #Sentiment détaillé 
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
      
        #Wordcloud sentiment
        output$wordCloudSentiment <- renderPlot({
            if(input$radioSentiment==1){
            
            test_feel_df <- feel()
            par(mar = rep(0, 4))
            #La taille du texte d'un mot est proportionnelle à sa fréquence au sein de son sentiment.
            #Nous pouvons utiliser cette visualisation pour voir les mots positifs et négatifs les plus importants,
            #mais la taille des mots n'est pas comparable d'un sentiment à l'autre
            test_feel_df %>%
                count(word, polarity, sort = TRUE) %>%
                acast(word ~ polarity, value.var = "n", fill = 0) %>%
                comparison.cloud(colors = c("darkred", "darkgreen"),
                                     max.words = input$max2)
            #Sentiment détaillé
            }else if(input$radioSentiment==2){
                
                test_feel_df <- feel()
                par(mar = rep(0, 4))

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
                    filter(!is.na(sentiment)) %>%
                    acast(word ~ sentiment, value.var = "n", fill = 0) %>%
                    comparison.cloud(colors = c("darkred", "darkgreen","purple","darkblue", "darkgray","magenta"),
                                     max.words = input$max2)
            }
        })
        
        output$radarChartSentiment <- renderPlot({
          if(input$radioSentiment==2){
            test_feel_df <- feel()
            par(mar = rep(0, 4))
            
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
            test_feel_df2 = test_feel_df %>%
              count(word, sentiment, sort = TRUE) %>%
              filter(!is.na(sentiment)) %>% select(sentiment,n)
            test_feel_df2 = aggregate(test_feel_df2$n, list(test_feel_df2$sentiment), FUN="sum")
            test_feel_df2 = t(test_feel_df2)
            colnames(test_feel_df2) = test_feel_df2[1,]
            test_feel_df2[1,] = as.numeric(max(test_feel_df2[2,])) + 5
            test_feel_df2 = rbind(test_feel_df2,test_feel_df2[2,])
            test_feel_df2[2,] = 0
            test_feel_df2 = apply(test_feel_df2,2, as.numeric)
            test_feel_df2 = as.data.frame(test_feel_df2)
            
            radarchart(test_feel_df2  , axistype=1 , 
                        
                       #custom polygon
                       pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
                       
                       #custom the grid
                       cglcol="blue", cglty=1, axislabcol="red", caxislabels=seq(0,max(test_feel_df2[,3]),20), cglwd=0.8,
                       
                       #custom labels
                       vlcex=0.8 
            )
          }
        })
        
        
        
        
      
        ###Bigram Visualization
        #ggraph
        output$ngramPlot <- renderPlot({
          
          bigram_counts <- ngram()
          
          validate(
            need(input$sizeLink <= max(bigram_counts["n"]), "Size is greater than the max frequency")
          )
          
          bigram_graph <- bigram_counts %>%
            filter(n > input$sizeLink) %>%
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
        
        #Wordcloud bigram
        output$bigramWordCloud2 <- renderPlot({

          bigram_counts <- ngram()
          par(mar = rep(0, 4))

          bigrams_united <- bigram_counts %>%
            unite(bigram, word1, word2, sep = " ")

          validate(
            need(input$max3 <= nrow(bigrams_united), "Selected size greater than number of elements in data")
          )
          new_df <- bigrams_united[1:input$max3,]
          wordcloud(words = new_df$bigram, freq = new_df$n, min.freq = 1, max.words=200, random.order=T, rot.per=0.2, colors=brewer.pal(8, "Dark2"))
        })

        #wordcloud bigram generation
        observeEvent(input$newCloud2, handlerExpr = {
          output$bigramWordCloud2 <- renderPlot({

            bigram_counts <- ngram()
            par(mar = rep(0, 4))

            bigrams_united <- bigram_counts %>%
              unite(bigram, word1, word2, sep = " ")

            validate(
              need(input$max3 <= nrow(bigrams_united), "Selected size greater than number of elements in data")
            )
            new_df <- bigrams_united[1:input$max3,]
            wordcloud(words = new_df$bigram, freq = new_df$n, min.freq = 1, max.words=200, random.order=T, rot.per=0.2, colors=brewer.pal(8, "Dark2"))
          })
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
        



      
      ###Word correlation
        
        #Plot correlation
        output$corrPlot <- renderPlot({
          
          word_cors <- corr()
          
          word_cors %>%
            filter(correlation > input$phiNum) %>%
            graph_from_data_frame() %>%
            ggraph(layout = "fr") +
            geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation), edge_colour = "royalblue", show.legend = F) +
            geom_node_point(size = 5) +
            geom_node_text(aes(label = name), repel = TRUE,
                           point.padding = unit(0.2, "lines")) +
            theme_void()

          
        })
        

        #Table correlation
        output$corrTable <- renderTable({
          
          word_cors <- corr()
          
          as.data.frame(word_cors %>%
            filter(correlation > input$phiTable) %>%
            filter(item1 == input$word))
        })
        #Randomize word
        #Change le mot en cas de changement de variable
        observeEvent(input$selectVar4, handlerExpr = {
          word_cors <- corr()
          word_cors = word_cors %>% filter(correlation > 0.05)
          ls = unlist(word_cors[,1])
          ls=unname(ls)
          word=sample(ls,size=1,replace=F)
          updateTextInput(session, "word", value=word)
        })  
        
        #Randomizer word
        observeEvent(input$randomWord, handlerExpr = {
            word_cors <- corr()
            word_cors = word_cors %>% filter(correlation > 0.05)
            ls = unlist(word_cors[,1])
            ls=unname(ls)
            word=sample(ls,size=1,replace=F)
            updateTextInput(session, "word", value=word)
        })
        
        #Barplot correlation
        output$corrPlotTable <- renderPlot({
          
          word_cors <- corr()
          
          validate(
            need( nrow(word_cors %>% filter(correlation > input$phiTable) %>% filter(item1 == input$word)) >= 1, "This word is not present in this variable or the correlation limit is too higher")
          )
          
          word_cors %>%
            filter(correlation > input$phiTable) %>%
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
        
        
        
        ###Comparison
        #word frequency barplot comparison
        output$compBarplot <- renderPlot({
          freq <- comp()
          new_df <- freq[1:input$numWords4,]
          ggplot(new_df, aes(x=reorder(word, n), y=n, fill = as.factor(word))) +
            geom_bar(stat = "identity", position = "dodge", col= "black") + xlab("Words") + ylab("Frequency") +
            coord_flip() + scale_fill_hue(c = sample(hues, 1)) + guides(fill=FALSE)
        })
        
        #distribution barplot comparison
        output$compBarDistri <- renderPlot({
          text_df <- distri()
          colnames(text_df) = c("distribution","n")
          ggplot(text_df, aes(x=distribution, y=n, fill = as.factor(distribution))) +
            geom_bar(stat = "identity", position = "dodge", col= "black") + xlab("Attributs") + ylab("Distribution") +
            coord_flip() + scale_fill_hue(c = sample(hues, 1)) + guides(fill=FALSE)
        })
        
        #wordcloud comparison
        output$wordCloudComp <- renderPlot({
          freq <- comp()
          par(mar = rep(0, 4))
          validate(
            need(input$max4 <= nrow(freq), "Selected size greater than number of elements in data")
          )
          new_df <- freq[1:input$max4,]
          wordcloud(words = new_df$word, freq = new_df$n, min.freq = 1, max.words=200, random.order=T, rot.per=0.2, colors=brewer.pal(8, "Dark2"))
        })
        
        
        #wordcloud bigram comparison
        output$bigramComparison <- renderPlot({
          
          bigram_counts <- bigram()
          par(mar = rep(0, 4))
          
          bigrams_united <- bigram_counts %>%
            unite(bigram, word1, word2, sep = " ")
          
          validate(
            need(input$max3 <= nrow(bigrams_united), "Selected size greater than number of elements in data")
          )
          new_df <- bigrams_united[1:input$max4,]
          wordcloud(words = new_df$bigram, freq = new_df$n, min.freq = 1, max.words=200, random.order=T, rot.per=0.2, colors=brewer.pal(8, "Dark2"))
        })

}
