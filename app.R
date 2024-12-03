
ui <- fluidPage(
  titlePanel("Extreme Weather Analysis"),
  
  tabsetPanel(
    tabPanel("Extreme Events", 
             sliderInput("yearRange", "Select Year Range", min = 1920, max = 2020, value = c(1920, 2020)),
             plotOutput("extremePlot")
    ),
    tabPanel("Temperature Trends",
             sliderInput("yearRangeTemp", "Select Year Range", min = 1920, max = 2020, value = c(1920, 2020)),
             plotOutput("maxTempPlot"),
             plotOutput("minTempPlot")
    )
  )
)

server <- function(input, output) {
  
  # Filter data based on the selected year range
  filtered_extreme_events <- reactive({
    extreme_events %>%
      filter(Year >= input$yearRange[1] & Year <= input$yearRange[2])
  })
  
  filtered_cleaned_data <- reactive({
    cleanedData %>%
      filter(Year >= input$yearRangeTemp[1] & Year <= input$yearRangeTemp[2])
  })
  
  # Plot for extreme events
  output$extremePlot <- renderPlot({
    ggplot(filtered_extreme_events(), aes(x = Year)) +
      geom_line(aes(y = ExtremeHeatDays, color = "Extreme Heat"), size = 1) +
      geom_line(aes(y = ExtremeColdDays, color = "Extreme Cold"), size = 1) +
      labs(title = "Extreme Temperature Events Over the Years",
           x = "Year",
           y = "Number of Extreme Temperature Days") +
      scale_color_manual(values = c("Extreme Heat" = "orange", "Extreme Cold" = "turquoise")) +
      theme_minimal()
  })
  
  # Plot for maximum temperature trends
  output$maxTempPlot <- renderPlot({
    ggplot(filtered_cleaned_data(), aes(x = as.numeric(Year), y = Mean_Max_Temperature)) + 
      geom_point() +
      geom_smooth(method = 'lm') +
      labs(title = "Mean Maximum Temperature Growth Over the Years",
           x = "Year",
           y = "Mean Maximum Temperature")
  })
  
  # Plot for minimum temperature trends
  output$minTempPlot <- renderPlot({
    ggplot(filtered_cleaned_data(), aes(x = as.numeric(Year), y = Mean_Min_Temperature)) + 
      geom_point() +
      geom_smooth(method = 'lm') +
      labs(title = "Mean Minimum Temperature Growth Over the Years",
           x = "Year",
           y = "Mean Minimum Temperature")
  })
}

shinyApp(ui = ui, server = server)
