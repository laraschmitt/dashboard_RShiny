library(shiny)
library(ggplot2)
library(dplyr)

bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
        titlePanel("BC Liquor Store prices", "BC Liquor"),
        sidebarLayout(
                sidebarPanel(
                        h4("select your products"),
                        
                        # price range input
                        sliderInput("priceInput", "Price", min = 0, max = 100, value = c(25, 40), pre = "$"),
                        
                        # product category input
                        radioButtons("typeInput", "Product type",
                                     choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                                     selected = "WINE"),
                        
                        # country import
                        selectInput("countryInput", "Country",
                                    choices = c("CANADA", "FRANCE", "ITALY"))
                ),
                      mainPanel(
                              "Count found products:",
                              
                              # reactive text output
                              textOutput("countResults"),
                              
                              # plot
                              plotOutput("coolplot"),
                              
                              #linbreak
                              br(),
                              
                              # table
                              tableOutput("results"))
                )
  
)

server <- function(input, output, session) {
        
        # use reactive variable to reduce code duplication
        filtered  <- reactive({
                bcl %>%
                filter(Price >= input$priceInput[1],
                       Price <= input$priceInput[2],
                       Type == input$typeInput,
                       Country == input$countryInput
                       )
        })
        
        # count
        output$countResults <- renderText({
                
                nrow(filtered())
                })
        
        # plot
        output$coolplot <- renderPlot({
        
                # plot using ggplot library 
                ggplot(filtered(), aes(Alcohol_Content)) +
                        geom_histogram()
        })
        
        #table
        output$results <- renderTable({
                filtered()
        })
        
        # observe prints to the console
        # useful if you want to check what value a reactive variable holds
        #observe({print(input$priceInput)}) # or cat() instead of print()
        
        # create my own reactive variable
        priceDiff <- reactive(diff(input$priceInput))
        observe({ print(priceDiff()) })
        
        
        
  
}

shinyApp(ui, server)