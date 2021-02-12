
#setwd("D:/Téléchargements/M2/Text mining")
#setwd("C:/Users/ameli/Desktop/R/TextMining/Text-mining")
setwd("/Users/hoangkhanhle/Desktop/School/Master 2/Text Mining/Projet")
server <- function(input, output) {
    df<-read.xlsx2("BDD_CEDA_dec_2020_complete - ANONYME.xlsx", sheetIndex = 1,header = TRUE)
    new_df<-data.frame(df$Préoccupations,df$Motif.de.consult,df$Diagnostic.,df$jeux.et.activités,df$Sommeil,df$Entrée.en.contact,df$Habitudes.de.l.enfant,df$Qui,df$Antécédents,df$X1eres.inquiétudes..mois.)
    
    
    FEEL_lex <- read_csv2("http://advanse.lirmm.fr/FEEL.csv")
    
    X=new_df[,1:9]
    colnames(X) = c("preoccupations","motif", "diagnostique", "jeux","sommeil","contact","habitute","qui","antecedents")
    text_df <- tibble(line = 1:nrow(X), preoccupation = X[,1], motif=X[,2],diagnostique=X[,3], jeux=X[,4], sommeil=X[,5],contact=X[,6],habitute=X[,7],qui=X[,8],antecedents=X[,9])
    
    
    var <- reactive({
        tidytext = text_df %>% unnest_tokens(word, as.numeric(input$selectVar))
        test_clean_df <- tidytext %>% filter(!word %in% c(stopwords('french'),'a','très','d\'un','qu\'il','nc','avoir','afin'))
        #Fréquence des mots
        freq=test_clean_df %>%
            count(word,sort=T)
        
    })
    
    feel <- reactive({
        tidytext = text_df %>% unnest_tokens(word, as.numeric(input$selectVar2))
        test_clean_df <- tidytext %>% filter(!word %in% c(stopwords('french'),'a','très','d\'un','qu\'il','nc','avoir','afin'))
        test_feel_df <- inner_join(test_clean_df, FEEL_lex, by = 'word')
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
    
    
    #wordcloud
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
    
    
    
    
    
    
    
}
