# Shiny Code for Final Project

# Libraries
library(shiny)
library(ggplot2)


# Source the final data frame
source("final.R")

# UI
ui <- fluidPage(
  titlePanel("Mental Health in the College System"),
  
  # Tab panels
  tabsetPanel(
    # Introduction page
    tabPanel("Introduction",
             h1("Introduction"),
             p("Talking about the importance of Mental Health and the problems in college, discussing why it's interesting and why we should care.")
    ),
    
    # Story 1
    tabPanel("The Percentage of College Students with Depression",
             sidebarLayout(
               sidebarPanel(
                 h4("Navigation"),
                 br(),
                 h5("Select Plot Type:"),
                 radioButtons("plot_type", label = NULL, choices = c("Depression", "Anxiety", "Panic Attacks"), selected = "Depression")
               ),
               mainPanel(
                 plotOutput("depression_plot")
               )
             )
    ),
    # Story 2
    tabPanel("The Impact of Mental Health on College Students Academic Performance",
             h3("Story 2"),
             p("Describe the limitations and problems of the study.")
    ),
    
    # Story 3
    tabPanel("How your Major could affect Mental Health",
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
    ),
    
    # Summary page
    tabPanel("Summary",
             h2("Summary"),
             plotOutput("summary_plot"),
             p("Provide a summary of the key findings and takeaways from the study.")
    ),
    
    # About Me page
    tabPanel("About Me",
             h2("About Me"),
             p("Introduce yourself and provide some information about your background and expertise in the field of mental health.")
    )
  )
)
# Server
server <- function(input, output) {
  
  # Filter data based on selected age range
  filtered_data <- reactive({
    if (is.null(input$age_range) || input$age_range == "All") {
      summary_data_age
    } else {
      subset(summary_data_age, Age_Group == input$age_range)
    }
  })
  
  # Render the selected plot based on the user's choice
  output$depression_plot <- renderPlot({
    if (input$plot_type == "Depression") {
      # Visualization for depression for the selected age range
      ggplot(filtered_data(), aes(x = Age_Group, y = Percentage_Depression)) +
        geom_bar(stat = "identity", fill = "lavender") +
        labs(title = paste("Percentage of People with Depression for Age Group:", input$age_range),
             x = "Age",
             y = "Percentage of People with Depression")
    } else if (input$plot_type == "Panic Attacks") {
      # Visualization for panic attacks for the selected age range
      ggplot(filtered_data(), aes(x = Age_Group, y = Percentage_Panic_Attacks)) +
        geom_bar(stat = "identity", fill = "lightgreen") +
        labs(title = paste("Percentage of People with Panic Attacks for Age Group:", input$age_range),
             x = "Age",
             y = "Percentage of People with Panic Attacks")
    } else if (input$plot_type == "Anxiety") {
      # Visualization for anxiety for the selected age range
      ggplot(filtered_data(), aes(x = Age_Group, y = Percentage_Anxiety)) +
        geom_bar(stat = "identity", fill = "skyblue") +
        labs(title = paste("Percentage of People with Anxiety for Age Group:", input$age_range),
             x = "Age",
             y = "Percentage of People with Anxiety")
    }
  })
  
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
  
  # Render the summary plot
  output$summary_plot <- renderPlot({
    # Create a matrix for the bar plot
    bar_data <- t(as.matrix(summary_data_major[, -1]))
    
    # Set the color palette
    colors <- c("lightpink", "lightblue", "lightgreen")
    
    # Create the bar plot
    barplot(bar_data, beside = TRUE, col = colors, ylim = c(0, 50), 
            xlab = "Major", ylab = "Percentage",
            main = "Percentage of People with Depression, Anxiety, and Panic Attacks by Major",
            legend.text = c("Depression", "Anxiety", "Panic Attacks"),
            args.legend = list(x = "topright", bty = "n"),
            names.arg = summary_data_major$Major)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
