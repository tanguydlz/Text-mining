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
                             helpText("blablabla"),
                             hr(),
                             #varSelectInput("variable", "Variable:", text_df),
                             selectInput("selectVar", label = h3("Variable to select :"),
                                         choices = list("Motif de consultation" = 2, "Preoccupations" = 3),
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
                             helpText("blablabla"),
                             selectInput("selectVar2", label = h3("Variable to select :"),
                                         choices = list("Motif de consultation" = 2, "Preoccupations" = 3),
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
                         hr(),
                         helpText("blablabla"),
                         selectInput("selectVar3", label = h3("Variable to select :"),
                                     choices = list("Motif de consultation" = 2, "Preoccupations" = 3),
                                     selected = 2),
                         sliderInput("numWords3", "Words in barplot :", 
                                     min=1, max= 35, value=6, step = 1),
                         sliderInput("max3",
                                     "Size of wordcloud :",
                                     min = 5,  max = 100, value = 50, step = 5),
                         actionButton("newCloud2", "Generate new wordcloud")),
            
            #side panel 4
            conditionalPanel(condition="input.tabselected==4",
                             hr(),
                             helpText("blablabla"),
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
                                         min=1, max= 35, value=5, step = 1))),

        
        #conditional main panel
        mainPanel(
            tabsetPanel(
                tabPanel("Word Plots", plotOutput("freqPlot"), wordcloud2Output("wordPlot"), value = 1,  
                         conditionalPanel(condition="input.tabselected==1")),
                tabPanel("Sentiments Analysis", value = 2, conditionalPanel(condition="input.tabselected==2"), 
                         plotOutput("sentiment"), plotOutput("wordCloudSentiment")),
                tabPanel("BiGram Visualization", value = 3,conditionalPanel(condition="input.tabselected==3"),
                         plotOutput("ngramPlot"),fluidRow(
                         column(6,plotOutput("bigramBarplot")),
                         column(6,wordcloud2Output("bigramWordCloud2")))),
                tabPanel("Word Correlation", value = 4, conditionalPanel(condition="input.tabselected==4"),
                         plotOutput("corrPlot"),fluidRow(
                             column(6,tableOutput("corrTable")),
                             column(6,plotOutput("corrPlotTable")))),
                id = "tabselected")
        )
        
    )
    
)