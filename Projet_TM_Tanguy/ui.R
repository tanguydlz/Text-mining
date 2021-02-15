library(xlsx)
library(tidytext)
library(dplyr)
library(ggplot2)
library(tm)
library(readr)
library(tidyr)
library(wordcloud)
library(wordcloud2)
library(reshape2)
library(plotly)
library(igraph)
library(ggraph)
library(widyr)
library(stringr)
library(fmsb)
library(syuzhet)
library(shiny)
library(shinyWidgets)
library(shinydashboard)



#####################################################################################################
#Ajouter le chargement des fichiers ici également car les pickerInput et Slider de l'onglet comparison 
#prend leur valeur directement des variables
#####################################################################################################
#setwd("D:/Téléchargements/M2/Text mining")
setwd("/Users/hoangkhanhle/Desktop/School/Master 2/Text Mining/Projet/Projet_TM_Tanguy")


df<-read.xlsx2("BDD_CEDA_dec_2020_complete - ANONYME.xlsx", sheetIndex = 1,header = TRUE)
X<-data.frame(df$Préoccupations,df$Motif.de.consult,df$Diagnostic.,df$jeux.et.activités,df$Sommeil,
              df$Entrée.en.contact,df$Habitudes.de.l.enfant,df$Qui,df$Antécédents,
              df$Sexe,df$Raison, df$Profession.mère, df$Profession.père,
              df$Code.postal, df$X1eres.inquiétudes..mois.,
              df$Marche..mois., df$Place.dans.la.fratrie,df$X1ers.mots)


colnames(X) = c("preoccupations","motif", "diagnostique", "jeux","sommeil","contact","habitute","qui","antecedents",
                "sexe", "raison", "professionM","professionP", "cp", "inquietudeM", "marche","place","mot")

X[,15] = replace(as.numeric(X[,15]), is.na(as.numeric(X[,15])),0)
X[,16] = replace(as.numeric(X[,16]), is.na(as.numeric(X[,16])),0)
X[,18] = replace(as.numeric(X[,18]), is.na(as.numeric(X[,18])),0)

#####################################################################################################

ui <- fluidPage(
    titlePanel("Projet Text Mining"),
    
    
    sidebarLayout(
        
        #loading message
        sidebarPanel(
            tags$head(tags$style(type="text/css", "
             #loadmessage {
               position: fixed;
               top: 0px;
               left: 0px;
               width: 100%;
               padding: 5px 0px 5px 0px;
               text-align: center;
               font-weight: bold;
               font-size: 100%;
               color: #000000;
               background-color: #CCFF66;
               z-index: 105;
             }
          ")),
            conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                             tags$div("Loading...",id="loadmessage")),
            #side panel 1
            conditionalPanel(condition="input.tabselected==1",
                             helpText("This application makes it possible to represent data in a simple way to answer the question 
                                        \"What are the concerns of parents when their child is suspected of having autism?\".
                                        Here, we have a global representation of the words most present on one of the variables."),
                             hr(),
                             radioButtons("sujet", label = h3("Sujet to select"), 
                                          choices = list("1" = 1,"2" = 2), inline = TRUE, selected = 1),
                             textOutput("sujet"),
                             hr(),
                             #varSelectInput("variable", "Variable:", text_df),
                             selectInput("selectVar", label = h3("Variable to select"),
                                         choices = list("Preoccupation" = 2, "Motif de consultation" = 3, "Diagnostic"=4, "Jeux et activités"=5, "Sommeil"=6 ,"Entrée en contact"=7,"Habitute de l'enfant"=8,"Qui"=9,"Antécédents"=10,"Raison"=12),
                                         selected = 2),
                             sliderInput("numWords", "Words in bar graph:", 
                                         min=1, max= 35, value=10, step = 1),
                             sliderInput("max",
                                         "Size of wordcloud:",
                                         min = 5,  max = 100, value = 50, step = 5),
                             actionButton("newCloud", "Generate new wordcloud")),
            

            #side panel 2
            conditionalPanel(condition="input.tabselected==2",
                             
                             helpText("Every word is associated with a feeling. 
                                      This allows us to try to understand the parents' feelings during a consultation 
                                      or to understand the feeling of their concern."),
                             hr(),
                             selectInput("selectVar2", label = h3("Variable to select"),
                                         choices = list("Preoccupation" = 2, "Motif de consultation" = 3, "Diagnostic"=4, "Jeux et activités"=5, "Sommeil"=6 ,"Entrée en contact"=7,"Habitute de l'enfant"=8,"Qui"=9,"Antécédents"=10,"Raison"=12),
                                         selected = 2),
                             radioButtons("radioSentiment", label = h3("Type of feelings"),
                                          choices = list("Positive/Negative" = 1, "Detailed Feelings" = 2), 
                                          selected = 1),
                            sliderInput("numWords2", "Words in bar graph:", 
                                        min=1, max= 35, value=10, step = 1),
                            sliderInput("max2",
                                        "Size of wordcloud:",
                                        min = 5,  max = 100, value = 50, step = 5)),
            #side panel 3
            conditionalPanel(condition="input.tabselected==3",
                         
                         helpText("We may be interested in visualizing all of the relationships among words simultaneously.
                                    We apply this visualization to bigrams (two terms).
                                    To make the visualization interpretable, we chose to show only the most common word to word connections"),
                         hr(),
                         selectInput("selectVar3", label = h3("Variable to select"),
                                     choices = list("Preoccupation" = 2, "Motif de consultation" = 3, "Diagnostic"=4, "Jeux et activités"=5, "Sommeil"=6 ,"Entrée en contact"=7,"Habitute de l'enfant"=8,"Qui"=9,"Antécédents"=10,"Raison"=12),
                                     selected = 2),
                         sliderInput("sizeLink", "Size of links between words :", 
                                     min=1, max= 5, value=1, step = 1),
                         sliderInput("numWords3", "Words in barplot :", 
                                     min=1, max= 35, value=5, step = 1),
                         sliderInput("max3",
                                     "Size of wordcloud :",
                                     min = 5,  max = 100, value = 30, step = 5),
                         actionButton("newCloud2", "Generate new wordcloud")),
            
            #side panel 4
            conditionalPanel(condition="input.tabselected==4",
                             
                             helpText("This chapter showed how to analyze individual words, but also for exploring the relationships and connections between words. 
                                        Such relationships  enable us to see what words tend to appear after others, or co-occurences and correlations, for words that appear in proximity to each other."),
                             hr(),
                             selectInput("selectVar4", label = h3("Variable to select"),
                                         choices = list("Preoccupation" = 2, "Motif de consultation" = 3, "Diagnostic"=4, "Jeux et activités"=5, "Sommeil"=6 ,"Entrée en contact"=7,"Habitute de l'enfant"=8,"Qui"=9,"Antécédents"=10,"Raison"=12),
                                         selected = 2),
                             sliderInput("phiNum", "Minimum correlation :", 
                                         min=0.05, max= 0.9, value=0.4, step = 0.05),
                             textInput(inputId = "word",
                                       label = "Find word associations among data:", value = "retard"),
                             actionButton("randomWord", "Generate word from corpus"),
                             sliderInput("phiTable", "Correlation Limit for table :", 
                                         min=0.05, max= 0.9, value=0.05, step = 0.05),
                             sliderInput("corrNum", "Words in bar graph:", 
                                         min=1, max= 35, value=5, step = 1)),
        
        #side panel 5
        conditionalPanel(condition="input.tabselected==5",
                         
                         helpText("Compare the three main variables with other variables that may be related to parental concerns."),
                         hr(),
                         selectInput("selectVar5", label = h3("Main variable to select"),
                                     choices = list("Preoccupation" = 2, "Motif de consultation" = 3, "Raison"=4),
                                     selected = 2),
                         sliderInput("numWords4", "Words in barplot :", 
                                     min=1, max= 35, value=5, step = 1),
                         sliderInput("max4",
                                     "Size of wordcloud :",
                                     min = 5,  max = 100, value = 30, step = 5),
                         selectInput("selectOtherVar", label = h3("Cutting variable to select"),
                                     choices = list("Sexe" = 11, "Profession mère" = 13, "Profession père"=14, "Code postal"=15,
                                                    "1ère inquiétude (en mois)"=16, "Marche (mois)"=17, "Place dans la fraterie"=18,
                                                     "1er mots en mois"=19),
                                     selected = 2),
                         conditionalPanel(condition="input.selectOtherVar==11",
                                          checkboxGroupInput("sex", label = h3("Sex"), 
                                                             choices = unique(X$sex),
                                                             selected="M")),
                         conditionalPanel(condition="input.selectOtherVar==13",
                                          pickerInput("professionM","Profession Mère", choices=sort(unique(X$professionM)), 
                                                      options = list('actions-box' = TRUE),multiple = T, selected = unique(X$professionM))),
                         conditionalPanel(condition="input.selectOtherVar==14",
                                          pickerInput("professionP","Profession Père", choices=sort(unique(X$professionP)), 
                                                      options = list('actions-box' = TRUE),multiple = T, selected = unique(X$professionP))),
                         conditionalPanel(condition="input.selectOtherVar==15",
                                          pickerInput("cp","Code Postal", choices=sort(unique(X$cp)), 
                                                      options = list('actions-box' = TRUE),multiple = T,selected = unique(X$cp))),
                         conditionalPanel(condition="input.selectOtherVar==16",
                                          sliderInput("inquietudeM", label = h3("1ère inquiétude (en mois)"), 
                                                      min=min(X$inquietudeM), max= max(X$inquietudeM), value=c(50,100), step = 5)),
                         conditionalPanel(condition="input.selectOtherVar==17",
                                          sliderInput("marche", label = h3("Marche (en mois)"), 
                                                      min=min(X$marche), max= max(X$marche), value=c(5,15), step = 1)),
                         conditionalPanel(condition="input.selectOtherVar==18",
                                          pickerInput("fraterie","Place dans la fraterie", choices=sort(unique(X$place)), 
                                                      options = list('actions-box' = TRUE),multiple = T,selected = unique(X$place))),
                         conditionalPanel(condition="input.selectOtherVar==19",
                                          sliderInput("mots", label = h3("1er mots en mois"), 
                                                      min=min(X$mot), max= max(X$mot), value=c(10,20), step = 1)))),
        


     
        #conditional main panel
        mainPanel(
            
            headerPanel(
                fluidPage(
                    setBackgroundColor(color = "ghostwhite"),
                    useShinydashboard(),
                    fluidRow(
                        # Nombre d'observation 
                        valueBoxOutput("progressBox"),
                        # Proportion des patients TSA 
                        valueBoxOutput("approvalBox"),
                        valueBox(
                            uiOutput("orderNum"), "Nombre de mots ", icon = icon("book"),width=4,color = "green"
                        )
                    ))
            )
            ,
            tabsetPanel(
                tabPanel("Word Plots", plotOutput("freqPlot"), wordcloud2Output("wordPlot"), value = 1,  
                         conditionalPanel(condition="input.tabselected==1")),
                tabPanel("Sentiments Analysis", value = 2, conditionalPanel(condition="input.tabselected==2"), 
                         plotOutput("sentiment"), plotOutput("wordCloudSentiment"),plotOutput("radarChartSentiment")),
                tabPanel("BiGram Visualization",plotOutput("ngramPlot"),
                         value = 3, conditionalPanel(condition="input.tabselected==3"),
                         fluidRow(
                             column(6,plotOutput("bigramBarplot")),
                             column(6,plotOutput("bigramWordCloud2"))
                             )),
                
                tabPanel("Word Correlation", value = 4, conditionalPanel(condition="input.tabselected==4"),
                         plotOutput("corrPlot"),fluidRow(
                             column(6,tableOutput("corrTable")),
                             column(6,plotOutput("corrPlotTable")))),
                tabPanel("Comparison", value = 5, conditionalPanel(condition="input.tabselected==5"),
                         fluidRow(
                             column(6,plotOutput("compBarplot")),
                             column(6,plotOutput("compBarDistri"))),
                         fluidRow(
                             column(6,plotOutput("wordCloudComp")),
                             column(6,plotOutput("bigramComparison")))),
                id = "tabselected")
        )
        
    )
    
)