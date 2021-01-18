

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Etude des inquietudes des parents"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("nb","Nombre de mots", min=1, max=4, value=1),
            selectInput("var","Choix d'une variable", choices = colnames(new_df))
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("table des exprexions",tableOutput("table")),
                tabPanel("wordcloud",plotOutput("distPlot"))
            )
            
        )
    )
))
