#' 
#' Shiny app to display line plots of each state to note trends
#' in both ggplot and base R
#'
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
#
tuesdata <- tidytuesdayR::tt_load('2018-04-02')
us_avg_tuition <- tuesdata$us_avg_tuition
#
ui <- fluidPage(
  titlePanel('Average Tuition USA'),
  selectInput(inputId = 'us_state', label = 'State', 
              choices = unique(us_avg_tuition$State)),
  tableOutput(outputId = 'state_data'),
  splitLayout(cellWidths = c(450,450), style = 'border: 1px solid silver:',
              plotOutput(outputId = 'state_plot_base'),
              plotOutput(outputId = 'state_plot_ggplot2')
  ))

server <- function(input, output, session) {
  # Reactive state to capture input state data
  us_data <- reactive({
    subset(x = us_avg_tuition, subset = State == input$us_state, 
           select = -State)
  }) 
  # Output data per state
  output$state_data <- renderTable({
    us_data()
  }, striped = TRUE, bordered = TRUE)
  # Plot output
  output$state_plot_base <- renderPlot({
    x_val = 1:ncol(us_data())
    y_val = t(us_data())
    col_names_data = colnames(us_data())
    plot(x = x_val, y = y_val,
         ylab = 'Average Tuition', xlab = 'Year',
         col = 'blue', type = 'b', pch = 19,
         col.lab = 'black', cex.lab = 1.2, las = 1,
         xaxt = 'n', 
         main = paste(
           'United States of America:', input$us_state, 'Average Tuition',
           sep = ' '))
    axis(1, at = x_val, labels = col_names_data)
    grid(nx = NA, ny = NULL, col = "gray", lty = "dashed")
  })
  output$state_plot_ggplot2 <- renderPlot({
    us_data() %>% 
      pivot_longer(
        cols = everything(), 
        names_to = 'year', 
        values_to = 'average_tuition') %>% 
      mutate(year = factor(year, ordered = T)) %>% 
      ggplot(aes(x = year, y = average_tuition, group = 1)) + 
      geom_line() + geom_point(color = 'blue') +  
      labs(y = 'Average Tuition', x = 'Year',
           title = paste(
             'United States of America:', input$us_state, 'Average Tuition',
             sep = ' ')) + 
      theme_bw() + 
      theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
  })
  
}

shinyApp(ui, server)