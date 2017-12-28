library(shiny)

df <- url(description = "https://raw.githubusercontent.com/alegione/Vet_Help/master/data/cat_food_ME.tsv")
foodTable <- read.table(file = df, header = TRUE, sep = "\t")
foodList <- as.list(foodTable$kcal.kg)
names(foodList) <- foodTable$Cat_Food

# Define UI for application that calculates resting energy requirement and feed
ui <- fluidPage(
   
   # Application title
   titlePanel("Cat recovery feeding"),
   
   # Sidebar with a slider input for weight 
   sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "foodType", label = "Food: ", choices = foodList),
          numericInput(inputId = "weight", label = "Weight (kg):", value = 5, min = 0, max = 20, step = 0.05),
          verbatimTextOutput(outputId = "value")
          ),

      
      
      # Show a plot of the generated distribution
      mainPanel(
        textOutput(outputId = "RER"),
        textOutput(outputId = "chosenFood"),
        textOutput(outputId = "foodQuantityDay1"),
        textOutput(outputId = "foodQuantityDay2"),
        textOutput(outputId = "foodQuantity")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$chosenFood <- renderText({
       paste("The energy value of the selected food type is: ", input$foodType, " kcal/kilogram")
    })
    #RER <- 70*(input$weight)^0.75
    output$RER <- renderText({
      paste("The resting energy requirement of a recovering cat weighing ", input$weight, " kg is: ", round(x = 70*(input$weight)^0.75, 2), " kcal/day")
    })
    output$foodQuantityDay1 <- renderText({
      paste("The quantity of food required on day 1 of recovery is ", round(x = 1000/(as.numeric(input$foodType)/(70*(input$weight)^0.75))*(1/3), 2), " grams")
    })
    output$foodQuantityDay2 <- renderText({
      paste("The quantity of food required on day 2 of recovery is ", round(x = 1000/(as.numeric(input$foodType)/(70*(input$weight)^0.75))*(2/3), 2), " grams")
    })
    output$foodQuantity <- renderText({
      paste("The quantity of food required on subsequent recovery days is ", round(x = 1000/(as.numeric(input$foodType)/(70*(input$weight)^0.75)), 2), " grams")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
