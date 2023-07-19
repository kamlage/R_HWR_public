library(dplyr)
library(shiny)
library(plotly)

# Define UI for application that draws a reactive line plot
ui <- fluidPage(
  
  # Application title
  titlePanel(h2("Simulations of a random walk process", 
              h5("Based on Heiss, F. ",
                      em("Using R for Introductory Econometrics,")," pp. 200-202. ",tags$a(href = "https://www.urfie.net/","https://www.urfie.net/"),
              h5("R Shiny by Beate Kamlage")))),
  # Sidebar definition with random walk parameters
  # Periods: Length of walk. 
  # Scenarios: Number of walks.
  # Initial value: Starting points. 
  # Drift: Skewness of direction of walk. 
  # SD: Standard deviation as a measure of variability of shocks
  sidebarLayout(
    sidebarPanel(
      sliderInput("periods",
                  "Number of periods",
                  min = 1,
                  max = 100,
                  value = 50),
      sliderInput("scenarios",
                "Number of scenarios",
                min = 1,
                max = 100,
                value = 50),
      sliderInput("initial_value",
              "Initial value",
              min = -5,
              max = 5,
              value = 0),
      sliderInput("drift",
            "Drift",
            min = -5,
            max = 5,
            value = 0),
      sliderInput("sdev",
                  "Standard deviation",
                  min = 0,
                  max = 5,
                  value = 1)),
    
    # Show a plot of the generated random walk. CAVE! Notice it has to be named "plotlyOutput" and not "plotOutput" in case plot_ly is used
    mainPanel(
      plotlyOutput("plot", height = "500px")
    )
  )
)

# Define server logic. CAVE! Notice it has to be named "renderplotly" and not "renderplot" in case plot_ly is used
server <- function(input, output) {
  output$plot <- renderPlotly({
    
    scenarios <- seq(0, 100,length.out = input$scenarios)
    set.seed(4077)
    
    p <- plot_ly() %>%
      add_trace(x = c(0,input$periods),
                y = c(input$initial_value,input$drift*input$periods+input$initial_value),
                type = 'scatter', 
                mode = "lines",
                line = list(color = "black",
                            width = 2.5),
                showlegend = FALSE)
    
    for(r in scenarios) {
      e <- rnorm(n = input$periods, mean = 0, sd = input$sdev)
      y <- cumsum(input$drift + e) + input$initial_value
      p<-  add_trace(p,
                      y=y,
                      type = 'scatter', 
                      mode = 'lines',
                      line = list(color = "grey",
                                  width = 0.6),
                      showlegend = FALSE)
    }
    p
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

