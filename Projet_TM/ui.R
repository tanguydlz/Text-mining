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
library(shiny)
library(stringr)
library(shinydashboard)
library(shinyWidgets)
library(syuzhet)
library(fmsb)

ui <- shinyUI(
        
        
  fluidPage(
    headerPanel("Projet Text Mining"),
            actionButton("suj1", "Sujet 1",style = "color: white; 
                       background-color: #7A83EE;
                       width: 150px"),
            actionButton("suj2", "Sujet 2",style = "color: white; 
                       background-color: #454CAD;
                       width: 150px"), 
            
            headerPanel(textOutput("sujet")),
            
                    tabsetPanel(
                      tabPanel(title="Word Plot" ,
                            
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
                                 } ")),
                                
                                 #side panel 1
                                 helpText("Ici, il est possible d'analyser les mots les plus féquents concernant les variables en lien avec l'inquiétude des parents :"),
                                 hr(),
                                 #varSelectInput("variable", "Variable:", text_df),
                                 selectInput("selectVar", label = h3("Variable to select"),
                                             choices = list("Preoccupation" = 2, "Motif de consultation" = 3, "Diagnostiques"=4, "Jeux et activités"=5, "Sommeil"=6 ,"Entrée en contact"=7,"Habitute de l'enfant"=8,"Qui"=9,"Antécédents"=10),
                                             selected = 2),
                                 sliderInput("numWords", "Words in bar graph:", 
                                             min=1, max= 35, value=10, step = 1),
                                 sliderInput("max",
                                             "Size of wordcloud:",
                                             min = 5,  max = 100, value = 50, step = 5),
                                 actionButton("newCloud", "Generate new wordcloud")
                            ), #sidebar
                        
                            mainPanel(
                                plotOutput("freqPlot"), 
                                wordcloud2Output("wordPlot") 
                            )
                    ), 
                    tabPanel(title = "Sentiments analysis",
                             
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
                                     }")),
                                #side panel 2
                                      hr(),
                                      helpText("Ici, il est possible d'analyser les sentiments ressortant des mots les plus féquents concernant l'inquiétude des parents :"),
                                      selectInput("selectVar2", label = h3("Variable to select"),
                                                  choices = list("Preoccupation" = 2, "Motif de consultation" = 3, "Diagnostiques"=4, "Jeux et activités"=5, "Sommeil"=6 ,"Entrée en contact"=7,"Habitute de l'enfant"=8,"Qui"=9,"Antécédents"=10),
                                                  selected = 2),
                                      radioButtons("radioSentiment", label = h3("Type of feelings"),
                                                   choices = list("Positive/Negative" = 1, "Detailed Feelings" = 2), 
                                                   selected = 1),
                                      sliderInput("numWords2", "Words in bar graph:", 
                                                  min=1, max= 35, value=10, step = 1),
                                      sliderInput("max2",
                                                  "Size of wordcloud:",
                                                  min = 5,  max = 100, value = 50, step = 5)
                                ),#sidebarPanel
                             
                             #conditional main panel
                             mainPanel(
                                 plotOutput("sentiment"),
                                 plotOutput("wordCloudSentiment"),
                                 plotOutput("radar")
                             )#mainPanel
                         )#tabPanel(onglet)
                    ,
                    
                    tabPanel(title="BiGram Visualization" ,
                             
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
                                 } ")),
                                 
                                 #side panel 3
                                 hr(),
                                 helpText("Ici nous observons les mots deux par deux"),
                                 selectInput("selectVar3", label = h3("Variable to select :"),
                                             choices = list("Motif de consultation" = 2, "Preoccupations" = 3),
                                             selected = 2),
                                 sliderInput("numWords3", "Words in barplot :", 
                                             min=1, max= 35, value=6, step = 1),
                                 sliderInput("max3",
                                             "Size of wordcloud :",
                                             min = 5,  max = 100, value = 50, step = 5),
                                 actionButton("newCloud2", "Generate new wordcloud")
                             ),#sidebar
                             mainPanel(
                                 plotOutput("ngramPlot"),
                                 fluidRow(
                                     column(6,plotOutput("bigramBarplot")),
                                     column(6,wordcloud2Output("bigramWordCloud2")))
                             )#mainPanel
                        ),#tabPanel(onglet)
                    tabPanel(title="Word Correlation" ,
                             
                             #
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
                                     } ")),
                                 hr(),
                                 helpText("Ici nous observons la corrélation des termes"),
                                 selectInput("selectVar4", label = h3("Variable to select :"),
                                             choices = list("Motif de consultation" = 2, "Preoccupations" = 3),
                                             selected = 2),
                                 sliderInput("phiNum", "Minimum correlation :", 
                                             min=0.05, max= 0.9, value=0.4, step = 0.05),
                                 textInput(inputId = "word",
                                           label = "Find word associations among data:", value = "retard"),
                                 actionButton("randomWord", "Generate word from corpus"),
                                 sliderInput("phiTable", "Correlation Limit for table :", 
                                             min=0.05, max= 0.9, value=0.2, step = 0.05),
                                 sliderInput("corrNum", "Words in bar graph:", 
                                             min=1, max= 35, value=5, step = 1)
                             ),#sidebar
                             mainPanel(
                                 plotOutput("corrPlot"),
                                 fluidRow(
                                     column(6,tableOutput("corrTable")),
                                     column(6,plotOutput("corrPlotTable")))   
                             )#mainPanel
                    ),#tabPanel(onglet)
                    tabPanel(title="Comparaison" ,
                             
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
                                 } ")),
                               
                               #side panel 1
                               helpText("Ici, il est possible d'analyser les mots les plus féquents concernant les variables en lien avec l'inquiétude des parents :"),
                               hr(),
                               #varSelectInput("variable", "Variable:", text_df),
                               selectInput("selectVar5", label = h3("Text variable to select"),
                                           choices = list("Preoccupation" = 2, "Motif de consultation" = 3),
                                           selected = 2),
                               selectInput("selectVar6", label = h3("Cutting variable to select"),
                                           choices = list("Age de l'enfants lors des 1ère inquiétude" = 2, "Sexe" = 7, "Place dans la fraterie"=4, "Age premier pas"=3, "Sommeil"=6, "Age premier mots"=5, "Diagnostique"=8),
                                           selected = 2),
                               sliderInput("max4",
                                           "Size of wordcloud:",
                                           min = 5,  max = 100, value = 50, step = 5),
                               actionButton("newCloud", "Generate new wordcloud")
                             ), #sidebar
                             
                             mainPanel(
                              tags$h4("Découpage des données selon les modalités : "),
                              tabsetPanel(
                                 tabPanel(textOutput("mod1"),wordcloud2Output("comp")),
                                 tabPanel(textOutput("mod2"),wordcloud2Output("comp2")),
                                 tabPanel(textOutput("mod3"),wordcloud2Output("comp3")),
                                 tabPanel(textOutput("mod4"),wordcloud2Output("comp4"))
                                 
                              )
                               
                             )
                             
                          
                             
                    )
            )
    )#navbar
)#Fin
                
                
               
        