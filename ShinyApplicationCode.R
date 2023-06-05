# Shiny Code for Final Project

# Libraries
library(shiny)
library(ggplot2)


# Source the final data frame
source("final.R")

# UI
ui <- fluidPage(
  titlePanel(h1("Mental Health in the College System")),
  
  # Custom HTML styling 
  tags$style(HTML("
    h2 {
            background-color: #B49FCC;
            color: Black;
            }"),
             HTML("
    h3 {
            background-color: #EAD7D7;
            color: Black;
            }")),
  
  # Tab panels
  tabsetPanel(
    # Introduction page
    tabPanel("Introduction",
             h2("Introduction"),
             p("Due to social expectations, financial stress, and academic constraints, the college experience often leads to 
             anxiety and mental health issues. Due to the major impact this has on anxiety and depression diagnoses, 
             many students have to drop out before getting their degrees. Our project aims to explore the complex interaction 
             between mental health and college students, shedding light on the difficulties they encounter and the effects on 
             their personal and academic life. By analyzing two datasets related to mental health diagnoses, we seek to emphasize the 
             importance of addressing mental health concerns and advocating for more resources. We will examine factors such as gender, 
             ethnicity, major-specific pathway, and the academic and social environment to identify relationships with mental health outcomes. 
             We aim to provide useful information on mental health using data analytics and visualization tools. We want to bring attention 
             to the importance of mental health. We hope to highlight the critical but often overlooked issue of mental health disparities 
             among college students, allowing for a better understanding of how college living affects mental health."),
             p("Our research focuses mostly on data surronding depression, anxiety, and panic attacks, but these are not the only form
               of mental health struggles. Mental health affects many different people in many different way. We also hope that our project 
               will help those struggling to not feel alone and help others begin to understand the impact mental health can have on our lives."),
             h3("Data Stories"),
             h4("The Percentage of College Students with Mental Health Conditions"),
             p("Add in brief description and how we apporached it "),
             br(),
             h4("The Impact of Mental Health on College Students Academic Performance"),
             p("Add in brief description and how we apporached it "),
             br(),
             h4("How your Major could Affect your Mental Health"),
             p("Add in brief description and how we apporached it "),
             br(),
    ),
    
    # Story 1 Page 
    tabPanel("The Percentage of College Students with Mental Health Conditions",
             h2("The Percentage of College Students with Mental Health Conditions"),
             sidebarLayout(
               sidebarPanel(
                 h4("Conditions:"),
                 br(),
                 radioButtons("story1_graph_type", label = NULL, choices = c("Depression", "Anxiety", "Panic Attacks"), selected = "Depression")
               ),
               mainPanel(
                 plotOutput("story1_plot")
               )
             )
    ),
    ( the data story focuses on examining the percentage of college students with mental health condition,
     it explores the prevalence anf trends of mental health issue among the college students. Using a combination
     of survey, and data analysis to provide a better understanding. 
    
    # Story 2 Page
    tabPanel("The Impact of Mental Health on College Students Academic Performance",
             h2("The Impact of Mental Health on College Students Academic Performance"),
             sidebarLayout(
               sidebarPanel(
                 h4("Conditions:"),
                 br(),
                 radioButtons("story2_graph_type", label = NULL, choices = c("Depression", "Anxiety", "Panic Attacks"), selected = "Depression")
               ),
               mainPanel(
                 plotOutput("story2_plot")
               )
             )
    ),
    
    # Story 3 Page
    tabPanel("How your Major could Affect your Mental Health",
             h2("How your Major could Affect your Mental Health"),
             sidebarLayout(
               sidebarPanel(
                 h4("Conditions:"),
                 br(),
                 radioButtons("story3_graph_type", label = NULL, choices = c("Depression", "Anxiety", "Panic Attacks"), selected = "Depression")
               ),
               mainPanel(
                 plotOutput("story3_plot")
               )
             ),
             p("Shown above are four bar graph showing the percentage of people with depression, anxiety, and panic attacks grouped
                by major. As shown by the differences in percentages, majors such as busniess and engineering have a higher affiliation
                with students experiencing mental health concerns. While compared to the other three degree catagories, the other and 
                humanaities catagory shown much lower percentages. While all shown percentages are under 50%, there is no denying the
                relationship between mental health and univeristy.")
    ),
    
    # Summary page
    #tabPanel("Summary",

    #),
    
    # About Me page
    tabPanel("Summary & About Me",
             h2("About Me"),
             p("The goal of our project and reasearch is to provide useful information on mental health using data analytics and visualization tools. 
             We want to bring attention to the importance of mental health among college studenst because not only can it affect yourself, but other you know."),
             h3("Authors and Affiliations"),
             p(" Authors: Leilani Flores, Sophie Wetzel, Cung Tran"),
             p("Affiliations: Info 201 - Technical Foundations of Informatics"),
             h3("Data Sets"),
             h4("Found Data Sets:"),
             p("StudentMentalHealth.csv, https://www.kaggle.com/datasets/shariful07/student-mental-health"),
             p("Mental_Health_Care_in_the_Last_4_Weeks.csv, https://www.cdc.gov/nchs/covid19/pulse/mental-health-care.htm"),
             h4("Created Data Sets:"),
             p("FinalDataSet.csv"),
             h3("Github Respitory"),
             p("https://github.com/sophw03/info201project_studentsmentalhealth"),
             br()
    ),
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
  output$story1_plot <- renderPlot({
    if (input$story1_graph_type == "Depression") {
      # Graph for depression for the selected age range
      ggplot(filtered_data(), aes(x = Age_Group, y = Percentage_Depression)) +
        geom_bar(stat = "identity", fill = "lightpink") +
        labs(title = paste("Percentage of People with Depression for Age Group:", input$age_range),
             x = "Age",
             y = "Percentage of People with Depression")
    } else if (input$story1_graph_type == "Panic Attacks") {
      # Graph for panic attacks for the selected age range
      ggplot(filtered_data(), aes(x = Age_Group, y = Percentage_Panic_Attacks)) +
        geom_bar(stat = "identity", fill = "lightblue") +
        labs(title = paste("Percentage of People with Panic Attacks for Age Group:", input$age_range),
             x = "Age",
             y = "Percentage of People with Panic Attacks")
    } else if (input$story1_graph_type == "Anxiety") {
      # Graph for anxiety for the selected age range
      ggplot(filtered_data(), aes(x = Age_Group, y = Percentage_Anxiety)) +
        geom_bar(stat = "identity", fill = "lightgreen") +
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
        geom_bar(stat = "identity", fill = "lightpink") +
        labs(title = "Percentage of People with Depression by GPA Range",
             x = "GPA Range",
             y = "Percentage of People with Depression")
    } else if (input$story2_graph_type == "Anxiety") {
      # Graph for anxiety by GPA
      ggplot(summary_data_gpa, aes(x = GPA_Range, y = Percentage_Anxiety)) +
        geom_bar(stat = "identity", fill = "lightblue") +
        labs(title = "Percentage of People with Anxiety by GPA Range",
             x = "GPA Range",
             y = "Percentage of People with Anxiety")
    } else if (input$story2_graph_type == "Panic Attacks") {
      # Graph for panic attacks by GPA
      ggplot(summary_data_gpa, aes(x = GPA_Range, y = Percentage_Panic_Attacks)) +
        geom_bar(stat = "identity", fill = "lightgreen") +
        labs(title = "Percentage of People with Panic Attacks by GPA Range",
             x = "GPA Range",
             y = "Percentage of People with Panic Attacks")
    } 
  })
  
  #  User's choice in Story 3
  output$story3_plot <- renderPlot({
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
  
}

# Run the application
shinyApp(ui = ui, server = server)
