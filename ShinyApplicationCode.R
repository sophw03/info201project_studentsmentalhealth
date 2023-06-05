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
             p("Talking about the importance of Mental Health and the problems in college, discussing why it's interesting and why we should care."),
             br(),
             h3("Story 1: The Percentage of College Students with Mental Health Conditions"),
             p("Add in brief description and how we apporached it "),
             br(),
             h3("Story 2: The Impact of Mental Health on College Students Academic Performance"),
             p("Add in brief description and how we apporached it "),
             br(),
             h3("Story 3: How your Major could affect your Mental Health"),
             p("Add in brief description and how we apporached it "),
             p(""),
             br(),
    ),
    
    # Story 1 Page 
    tabPanel("The Percentage of College Students with Mental Health Conditions",
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
    
    # Story 2 Page
    tabPanel("The Impact of Mental Health on College Students Academic Performance",
             sidebarLayout(
               sidebarPanel(
                 h4("Navigation"),
                 br(),
                 h5("Select Graph Type:"),
                 radioButtons("story2_graph_type", label = NULL, choices = c("Depression", "Panic Attacks", "Anxiety"), selected = "Depression")
               ),
               mainPanel(
                 plotOutput("story2_plot")
               )
             )
    ),
    
    # Story 3 Page
    tabPanel("How your Major could affect your Mental Health",
             sidebarLayout(
               sidebarPanel(
                 h4("Navigation"),
                 br(),
                 h5("Select Graph:"),
                 radioButtons("story3_graph_type", label = NULL, choices = c("Depression", "Anxiety", "Panic Attacks"), selected = "Depression")
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
             h1("About Me"),
             h3("Authors and Affiliations"),
             p(" Authors: Leilani Flores, Sophie, Cung"),
             p("Affiliations: Info 201 - Technical Foundations of Informatics"),
             h3("Data Sets"),
             p("insert data sets"),
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
      # Graph for depression for the selected age range
      ggplot(filtered_data(), aes(x = Age_Group, y = Percentage_Depression)) +
        geom_bar(stat = "identity", fill = "lavender") +
        labs(title = paste("Percentage of People with Depression for Age Group:", input$age_range),
             x = "Age",
             y = "Percentage of People with Depression")
    } else if (input$plot_type == "Panic Attacks") {
      # Graph for panic attacks for the selected age range
      ggplot(filtered_data(), aes(x = Age_Group, y = Percentage_Panic_Attacks)) +
        geom_bar(stat = "identity", fill = "lightgreen") +
        labs(title = paste("Percentage of People with Panic Attacks for Age Group:", input$age_range),
             x = "Age",
             y = "Percentage of People with Panic Attacks")
    } else if (input$plot_type == "Anxiety") {
      # Graph for anxiety for the selected age range
      ggplot(filtered_data(), aes(x = Age_Group, y = Percentage_Anxiety)) +
        geom_bar(stat = "identity", fill = "skyblue") +
        labs(title = paste("Percentage of People with Anxiety for Age Group:", input$age_range),
             x = "Age",
             y = "Percentage of People with Anxiety")
    }
  })
  
  # Graph for anxiety by age 
  output$story2_plot <- renderPlot({
    if (input$story2_graph_type == "Depression") {
      # Graph for data for GPA
      ggplot(summary_data_gpa, aes(x = GPA_Range, y = Percentage_Depression)) +
        geom_bar(stat = "identity", fill = "darkgreen") +
        labs(title = "Percentage of People with Depression by GPA Range",
             x = "GPA Range",
             y = "Percentage of People with Depression")
    } else if (input$story2_graph_type == "Panic Attacks") {
      # Graph for panic attacks by GPA
      ggplot(summary_data_gpa, aes(x = GPA_Range, y = Percentage_Panic_Attacks)) +
        geom_bar(stat = "identity", fill = "darkorange") +
        labs(title = "Percentage of People with Panic Attacks by GPA Range",
             x = "GPA Range",
             y = "Percentage of People with Panic Attacks")
    } else if (input$story2_graph_type == "Anxiety") {
      # Graph for anxiety by GPA
      ggplot(summary_data_gpa, aes(x = GPA_Range, y = Percentage_Anxiety)) +
        geom_bar(stat = "identity", fill = "darkblue") +
        labs(title = "Percentage of People with Anxiety by GPA Range",
             x = "GPA Range",
             y = "Percentage of People with Anxiety")
    }
  })
  
  #  User's choice in Story 3
  output$selected_plot <- renderPlot({
    if (input$story3_graph_type == "Depression") {
      # Graph for depression by major
      ggplot(summary_data_major, aes(x = Major, y = Percentage_Depression)) +
        geom_bar(stat = "identity", fill = "lightpink") +
        labs(title = "Percentage of People with Depression by Major",
             x = "Major",
             y = "Percentage of People with Depression")
    } else if (input$story3_graph_type == "Anxiety") {
      # Graph for anxiety by major
      ggplot(summary_data_major, aes(x = Major, y = Percentage_Anxiety)) +
        geom_bar(stat = "identity", fill = "lightblue") +
        labs(title = "Percentage of People with Anxiety by Major",
             x = "Major",
             y = "Percentage of People with Anxiety")
    } else if (input$story3_graph_type == "Panic Attacks") {
      # Graph for panic attacks by major
      ggplot(summary_data_major, aes(x = Major, y = Percentage_Panic_Attacks)) +
        geom_bar(stat = "identity", fill = "lightgreen") +
        labs(title = "Percentage of People with Panic Attacks by Major",
             x = "Major",
             y = "Percentage of People with Panic Attacks")
    }
  })
  
  # summary plot
  output$summary_plot <- renderPlot({
    # Create a matrix for the bar plot
    bar_data <- t(as.matrix(summary_data_major[, -1]))
    
    # Set color palette
    colors <- c("lightpink", "lightblue", "lightgreen")
    
    # Create bar plot
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
