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
library(shiny)

ui <- shinyUI(
    pageWithSidebar(
        headerPanel("Projet Text Mining"),
        
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
                             actionButton("newCloud", "Generate new wordcloud")),
            

            #side panel 2
            conditionalPanel(condition="input.tabselected==2",
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
                                        min = 5,  max = 100, value = 50, step = 5))),

        
        #conditional main panel
        mainPanel(
            tabsetPanel(
                tabPanel("Word Plots", plotOutput("freqPlot"), wordcloud2Output("wordPlot"), value = 1,  
                         conditionalPanel(condition="input.tabselected==1")),
                tabPanel("Sentiments analysis", value = 2, conditionalPanel(condition="input.tabselected==2"), 
                         plotOutput("sentiment"), plotOutput("wordCloudSentiment")),
                id = "tabselected")
        )
        
    )
    
)