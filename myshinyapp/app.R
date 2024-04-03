#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)



# Define UI for application

ui <- fluidPage(
  
  # Application title
  
  titlePanel("Characteristics of Taylor Swift's Music"),
  
# Tabbed layout with input and output definitions
tabsetPanel(
  # Tab for bar plots
  tabPanel("Bar Plots", 
           fluidRow( 
             column(width = 4, 
                    selectInput(inputId = "feature_select", 
                                label = "Select Feature:", 
                                choices = c("Danceability", "Acousticness", "Tempo")), 
                    plotOutput(outputId = "barplot")) 
           ) 
  ),
  
  # Tab for analysis
  tabPanel("Analysis",
           fluidRow(
             column(width = 12,
                    p("This is some analysis text. You can write your analysis content here.")
             )
           )
  )
)

# Define server logic required to generate the bar plots and custom text

server <- function(input, output) {
  
  # Render bar plot based on selected characteristic
  output$barplot <- renderPlot({
    # Import dataset
    
    data <- read.csv("tsfeatures.csv", header = TRUE, sep = ",")
    
    # Define albums to exclude
    
    albums_to_exclude <- c("folklore: the long pond studio sessions (from the Disney+ special) [deluxe edition]", "reputation Stadium Tour Surprise Song Playlist", "Speak Now World Tour Live", "Live From Clear Channel Stripped 2008")
    
    # Filter out excluded albums
    
    filtered_data <- subset(data, !album_name %in% albums_to_exclude)
    
    feature <- switch(input$feature_select,
                      "Danceability" = "danceability",
                      "Acousticness" = "acousticness",
                      "Tempo" = "tempo")
    # Adjust plot size for readability
    options(repr.plot.width = 100, repr.plot.height = 10) # Adjust the width and height as needed
    ggplot(filtered_data, aes_string(x = "album_name", y = feature, fill = "album_name")) +
    geom_bar(stat = "identity") +
      labs(title = paste(input$feature_select, "of Taylor Swift Albums"),
           x = "Album",
           y = input$feature_select) +
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
    text = element_text(size = 12)) # Adjust text size as needed
  })
}

shinyApp(ui = ui, server = server)
