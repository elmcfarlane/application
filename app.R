library(shiny)

# Define UI for the app
ui <- fluidPage(
  
  # Application title
  titlePanel("Estimating species richness and diversity"),
  
  # Sidebar with input for number of individuals of each species
  sidebarLayout(
    sidebarPanel(
      numericInput("species1",
                   "Species 1: number of individuals",
                   min = 0,
                   max = 50,
                   value = 0),
      numericInput("species2",
                   "Species 2: number of individuals",
                   min = 0,
                   max = 50,
                   value = 0),
      numericInput("species3",
                   "Species 3: number of individuals",
                   min = 0,
                   max = 50,
                   value = 0),
      numericInput("species4",
                   "Species 4: number of individuals",
                   min = 0,
                   max = 50,
                   value = 0),
      numericInput("species5",
                   "Species 5: number of individuals",
                   min = 0,
                   max = 50,
                   value = 0),
      submitButton("Submit")
    ),
    
    # Show calculated values
    mainPanel(
      h3("Chao 1 Estimator"),
      textOutput("chao"),
      h3("Simpson's Diversity Index"),
      textOutput("simps")
    )
  )
)

# Define server logic required to calculate two values
server <- function(input, output) {
  
  output$chao <- renderText({
    
    # convert inputs to vector
    x <- c(input$species1, input$species2, input$species3, input$species4, input$species5)
    
    # calculate inputs to chao
    Sn <- length(which(x != 0))
    an <- sum(which(x == 1))
    bn <- sum(which(x == 2))
    
    # calculate chao
    Sn + ((an^2)/(2*bn))
    
  })
  
  output$simps <- renderText({
    
    # calculate inputs to simpsons
    
    z <- input$species1+input$species2+input$species3+input$species4+input$species5
    
    # calculate simpsons
    
    1-((input$species1/z)^2+(input$species2/z)^2+(input$species3/z)^2+(input$species4/z)^2+(input$species5/z)^2)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
