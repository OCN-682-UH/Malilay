library(shiny)
library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)

#Read in data
#worldcup22 <- read_csv(here("Week_11","Data","Fifa_world_cup_matches.csv"))
worldcup22 <- read_csv("Fifa_world_cup_matches.csv")

ui<- fluidPage(    
  titlePanel("Goals scored and team possession by country"), #title for the panel
  sidebarLayout(  #use sidebar layout for dynamic output and choose a panel    
    sidebarPanel(
      selectInput(inputId = "TeamNames", #choose the inputs that will show up on the panel
                  label = "Countries", 
                  choices = unique(worldcup22$team1)), #select the team1 column
      hr(), #horizontal line for visual separator
      helpText("Data from the World Cup of 2022.") #text at bottom of panel
    ),
     #Create a spot for the barplot
    mainPanel(
      plotOutput("countryPlot"),
      hr(), # Add a separator for clarity
      h3("Match Summary Statistics"), #heading for the stats summary
      verbatimTextOutput("stats") 
    )
    
  ))

server<-function(input, output) {
  filtered_data <- reactive({worldcup22 %>% #reactive so object will respond to every reactive value in the code
      mutate(across(where(~any(str_detect(.,"%"))), parse_number)) %>% #remove % and convert chr to num
      filter(team1 == input$TeamNames) %>%
      rename('Team 1 Possession (%)' = `possession team1`, #rename for clarity in the output
             'Team 2 Possession (%)' = `possession team2`,
             'Contested Possession (%)' = `possession in contest`,
             'Goals Scored' = `number of goals team1`)}) 
   #Fill in the spot we created for a plot
  output$countryPlot <- renderPlot({
    req(input$TeamNames)
     #Render a geom point
    ggplot(filtered_data(), aes(x = `Team 1 Possession (%)`, y = `Goals Scored`)) +
             geom_point(na.rm = TRUE) +
      labs(x = "Ball Possession (%)",
           y = "Goals Scored")
  })
  output$stats <- renderPrint({
    req(input$TeamNames)
    summary(filtered_data() %>% #calculate summary statistics
              select(`Team 1 Possession (%)`, `Team 2 Possession (%)`, `Contested Possession (%)`)) #compare possession between teams
  })
}
shinyApp(ui = ui, server = server)
