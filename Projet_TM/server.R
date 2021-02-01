
    setwd("D:/Téléchargements/M2/Text mining")


server <- function(input, output, session) {
    df<-read.xlsx2("BDD_CEDA_dec_2020_complete - ANONYME.xlsx", sheetIndex = 1,header = TRUE)
    new_df<-df[,15:16]
    
    
    FEEL_lex <- read_csv2("http://advanse.lirmm.fr/FEEL.csv")
    
    X=new_df[,1:2]
    
    colnames(X) = c("preoccupations","motif")
    text_df <- tibble(line = 1:nrow(X), preoccupation = X[,1], motif=X[,2])
    


    #Word plot data
    var <- reactive({
        tidytext = text_df %>% unnest_tokens(word, as.numeric(input$selectVar))
        tidytext = tidytext[,-2]
        test_clean_df <- tidytext %>% filter(!word %in% c(stopwords('french'),'a','très','d\'un','qu\'il','nc','avoir','afin'))
        #Fréquence des mots
        freq=test_clean_df %>%
        count(word,sort=T)
    
    })
    
    #Sentiment analysis data
    feel <- reactive({
        tidytext = text_df %>% unnest_tokens(word, as.numeric(input$selectVar2))
        tidytext = tidytext[,-2]
        test_clean_df <- tidytext %>% filter(!word %in% c(stopwords('french'),'a','très','d\'un','qu\'il','nc','avoir','afin'))
        test_feel_df <- inner_join(test_clean_df, FEEL_lex, by = 'word')
    })
    
    #n-gram visualization data
    ngram <- reactive ({
      tidytext <- text_df %>% unnest_tokens(bigram, as.numeric(input$selectVar3),token = "ngrams", n = 2)
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
      tidytext = text_df %>% unnest_tokens(word, as.numeric(input$selectVar4))
      tidytext = tidytext[,-2]
      test_clean_df <- tidytext %>% filter(!word %in% c(stopwords('french'),'a','très','d\'un','qu\'il','nc','avoir','afin'))
      word_cors <- test_clean_df %>%
        group_by(word) %>%
        filter(n() >= 5) %>%
        pairwise_cor(word, line, sort = TRUE)
    })

    hues <- c(60:330)
    
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
                test_feel_df[,12] = NA
                for(i in 1:nrow(test_feel_df)){
                    for(j in 6:11){
                        if (test_feel_df[i,j]==1){
                            test_feel_df[i,12] = colnames(test_feel_df)[j]
                        }
                    }
                }
                
                
                colnames(test_feel_df)[12]="sentiment"
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
            #La taille du texte d'un mot dans la figure 2.6 est proportionnelle à sa fréquence au sein de son sentiment.
            #Nous pouvons utiliser cette visualisation pour voir les mots positifs et négatifs les plus importants,
            #mais la taille des mots n'est pas comparable d'un sentiment à l'autre
            test_feel_df %>%
                count(word, polarity, sort = TRUE) %>%
                acast(word ~ polarity, value.var = "n", fill = 0) %>%
                comparison.cloud(colors = c("darkred", "darkgreen"),
                                     max.words = input$max2)
            
            }else if(input$radioSentiment==2){
                
                test_feel_df <- feel()
                par(mar = rep(0, 4))
                
                test_feel_df = as.data.frame(test_feel_df)
                test_feel_df[,12] = NA
                for(i in 1:nrow(test_feel_df)){
                  for(j in 6:11){
                    if (test_feel_df[i,j]==1){
                      test_feel_df[i,12] = colnames(test_feel_df)[j]
                    }
                  }
                }
                
                
                colnames(test_feel_df)[12]="sentiment"
                
                test_feel_df %>%
                    count(word, sentiment, sort = TRUE) %>%
                    filter(!is.na(sentiment)) %>%
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

}
