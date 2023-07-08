rm(list = ls())


library(tidyr)
library(shiny)
library(dplyr)
library(plotly)
library(gapminder)
library(scales)

#download and check data for plot

data_wide <- gapminder %>% 
  group_by(year, continent) %>% 
  summarise(lifeExp = mean(lifeExp), 
            pop = mean(pop),
            gdpPercap = mean(gdpPercap))

data_wide
str(data_wide)

# Unpivot data: from wide to long format. In addition, rename variables to desired name for plot

data_long <- data_wide %>% 
 pivot_longer(cols=c('lifeExp', 'pop', 'gdpPercap'), names_to = "variable", values_to = "value") %>% 
  # Renaming with case_match() within mutate
  mutate(variable = case_match(
    variable,
    "lifeExp" ~ "Life expectancy at birth", 
    "pop" ~ "Population",
    "gdpPercap" ~ "GDP per capita (in dollar)"))

data_long

#define vectors comprising unique elements to filter

continents <- unique(gapminder$continent)
continents

variables <- unique(data_long$variable)
variables

# Start of user interface
ui <- fluidPage(

    # Title
    titlePanel("The gapminder dataset"),

    # Select a continent
    sidebarLayout(
        sidebarPanel(
          selectInput("continent", 
                      "Select a continent", 
                      choices = continents,
                      selected = "Asia"),
          selectInput("variable", 
                      "Select a variable*", 
                      choices = variables,
                      selected = "Population"),
        "*Mean value."),

        # Plot
        mainPanel(
           plotlyOutput("plot"),
        )
    )
)

# Define server logic
server <- function(input, output) {
  
  # Object reacting to users selection. Reactive objects have to be always within {}
  df_plot_final <- reactive({
    data_long %>% 
      filter(continent == input$continent) %>%
      filter(variable == input$variable) %>%
      select(year, value) 
  })
  
  # Plot
  output$plot <- renderPlotly({
    
    
    p1 <- ggplot(df_plot_final()) +
      geom_line(aes(x = year, 
                    y = value), color = "blue") +
      theme_bw() +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45,
                                       hjust = 1),
            axis.title.x = element_blank(),
            axis.title.y = element_blank()) +
            scale_x_continuous(breaks =seq(1955,2005,5)) +
            scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()))
    
    p_plotly <- ggplotly(p1) %>% 
      config(displayModeBar = F)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)