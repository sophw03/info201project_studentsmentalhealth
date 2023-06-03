# Shiny Code for Final Project

library(shiny)
library(ggplot2)

# Source the final data frame
source("final.R")

# UI
ui <- fluidPage(
  titlePanel("Insert Title"),
  
  # Introduction page
  tabPanel("Introduction",
           h1("Introduction"),
           p(" our intro stuff ")),
  
  # Graph page
  tabPanel("Graph",
           sidebarLayout(
             sidebarPanel(
               h4("Navigation"),
               br(),
               h5("Select Graph:"),
               radioButtons("graph_type", label = NULL, choices = c("Depression", "Anxiety", "Panic Attacks"), selected = "Depression")
             ),
             mainPanel(
               plotOutput("selected_plot")
             )
           )
  )
)

# Server
server <- function(input, output) {
  
  # Render the selected plot based on the user's choice
  output$selected_plot <- renderPlot({
    if (input$graph_type == "Depression") {
      # Visualization for depression by major
      ggplot(summary_data_major, aes(x = Major, y = Percentage_Depression)) +
        geom_bar(stat = "identity", fill = "lightpink") +
        labs(title = "Percentage of People with Depression by Major",
             x = "Major",
             y = "Percentage of People with Depression")
    } else if (input$graph_type == "Anxiety") {
      # Visualization for anxiety by major
      ggplot(summary_data_major, aes(x = Major, y = Percentage_Anxiety)) +
        geom_bar(stat = "identity", fill = "lightblue") +
        labs(title = "Percentage of People with Anxiety by Major",
             x = "Major",
             y = "Percentage of People with Anxiety")
    } else if (input$graph_type == "Panic Attacks") {
      # Visualization for panic attacks by major
      ggplot(summary_data_major, aes(x = Major, y = Percentage_Panic_Attacks)) +
        geom_bar(stat = "identity", fill = "lightgreen") +
        labs(title = "Percentage of People with Panic Attacks by Major",
             x = "Major",
             y = "Percentage of People with Panic Attacks")
    }
  })
} 

# Run the application
shinyApp(ui = ui, server = server)
