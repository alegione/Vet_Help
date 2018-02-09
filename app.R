library(shiny)

df <- url(description = "https://raw.githubusercontent.com/alegione/Vet_Help/master/data/cat_food_ME.tsv")
foodTable <- read.table(file = df, header = TRUE, sep = "\t")
foodList <- as.list(foodTable$kcal.kg)
names(foodList) <- foodTable$Cat_Food

# Define UI for application that calculates resting energy requirement and feed
ui <- fluidPage(
   
   # Application title
   titlePanel(title = "Cat recovery feeding", windowTitle = "Vet help - Cat recovery feeding"),
   
   # Sidebar with a slider input for weight 
   sidebarLayout(
        sidebarPanel(
          helpText("This app calculates the quantity of recovery feed required for cats"),
          br(),
          selectInput(inputId = "foodType",
                      label = "Food type: ",
                      choices = foodList),
          br(),
          numericInput(inputId = "weight",
                       label = "Weight of cat (kg):",
                       value = 5, min = 0, max = 20, step = 0.05),
          br(),
          verbatimTextOutput(outputId = "value")
          ),

      
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel(title = "Output",
                   br(),
                   textOutput(outputId = "RER"),
                   br(),
                   textOutput(outputId = "chosenFood"),
                   br(),
                   textOutput(outputId = "foodQuantityDay1"),
                   br(),
                   textOutput(outputId = "foodQuantityDay2"),
                   br(),
                   textOutput(outputId = "foodQuantity")
          ),
          tabPanel(title = "Documentation",
                   p(h4("Cat recovery food calculator")),
                   helpText("This app calculates the quantity of recovery feed required for cats"),
                   helpText("The formula is based on the resting energy requirement of a recovering cat of a given weight"),
                   helpText("Suggested feed is 1/3 calculated quantity for day 1, 2/3 for day 2, and the full amount on the subsequent days"),
                   HTML("<u><b>Equation for calculation: </b></u>
                        <br> <br>
                        <b> Food required (grams) = 1000 / (Energy / ( 70 * (Weight) ^ 0.75 ) </b>
                        <br>
                        where: <br>
                        Food required = weight of selected recovery food to provide in grams <br>
                        Energy = Metabolisable energy in selected food type <br>
                        Weight = input weight of cat in kilograms <br>
                        You can report errors or request other food types to be added at <a href = https://github.com/Alegione>github.com/Alegione</a>"
                   )
          )
        )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    values <- reactiveValues()
    
    observe({
      input$weight
      values$RER <- 70 * (input$weight) ^ 0.75
      values$foodRequired <- 1000 / (as.numeric(input$foodType) / values$RER)
    })
    output$chosenFood <- renderText({
       paste("The energy value of the selected food type is: ", input$foodType, " kcal/kilogram")
    })
    #RER <- 70*(input$weight)^0.75
    output$RER <- renderText({
      paste("The resting energy requirement of a recovering cat weighing ", input$weight, " kg is: ", round(x = values$RER, 2), " kcal/day")
    })
    output$foodQuantityDay1 <- renderText({
      paste("The quantity of food required on day 1 of recovery is ", round(x = values$foodRequired * (1/3), 2), " grams")
    })
    output$foodQuantityDay2 <- renderText({
      paste("The quantity of food required on day 2 of recovery is ", round(x = values$foodRequired * (2/3), 2), " grams")
    })
    output$foodQuantity <- renderText({
      paste("The quantity of food required on subsequent recovery days is ", round(x = values$foodRequired, 2), " grams")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
