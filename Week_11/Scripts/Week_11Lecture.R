
##-------------this is the basic structure to set up shiny
library(shiny)

ui <-fluidPage()
server <- function(input, output) {}
shinyapp(ui = ui, server = server)

##-------------try making a hello world app
ui <- fluidPage('Hello World')
server <- function(input, output){}
shinyApp(ui, server)

##----------------
ui<-fluidPage(
  sliderInput(inputId = "num", # ID name for the input
              label = "Choose a number", # Label above the input
              value = 25, min = 1, max = 100 # values for the slider, 25 is the default
  )
)
server<-function(input,output){}
shinyApp(ui = ui, server = server)



library(shiny)
library(tidyverse)
ui<-fluidPage(
  sliderInput(inputId = "num", # ID name for the input
              label = "Choose a number", # Label above the input
              value = 25, min = 1, max = 100 # values for the slider
  ),
  plotOutput("hist") #creates space for a plot called hist
)

server<-function(input,output){
  output$hist <- renderPlot({ #R code to make the histogram
    data<-tibble(x = rnorm(input$num)) # 100 random normal points
    ggplot(data, aes(x = x))+ # make a histogram
      geom_histogram()
  })
} #this is where the meat of the app is located
shinyApp(ui = ui, server = server)


##------------------- Let's add two inputs: -----------------------
#we want to write an input where the user can create whatever title they want by typing it into a box.
#date our ui to have a textInput.

ui<-fluidPage(
  sliderInput(inputId = "num", # ID name for the input 
              label = "Choose a number", # Label above the input
              value = 25, min = 1, max = 100 # values for the slider
  ),
  textInput(inputId = "title", # new Id is title
            label = "Write a title",
            value = "Histogram of Random Normal Values"), # starting title
  plotOutput("hist") #creates space for a plot called hist  
)
server<-function(input,output){
  output$hist <- renderPlot({
    # {} allows us to put all our R code in one nice chunck
    data<-tibble(x = rnorm(input$num)) # 100 random normal points 
    ggplot(data, aes(x = x))+ # make a histogram 
      geom_histogram() +
      labs(title = input$title) #Add a new title
  })
}
shinyApp(ui = ui, server = server)

##-------------Create two outputs:
##Let's say we want to add a data table below the histogram that has all the summary statistics. 
#We need to add another ui that creates a place to put the table. We will use verbatimTextOutput()

ui<-fluidPage(
  sliderInput(inputId = "num", # ID name for the input 
              label = "Choose a number", # Label above the input
              value = 25, min = 1, max = 100 # values for the slider
  ),
  textInput(inputId = "title", # new Id is title 
            label = "Write a title",
            value = "Histogram of Random Normal Values"), # starting title 
  plotOutput("hist"), #creates space for a plot called hist
  verbatimTextOutput("stats") # create a space for stats
)
server<-function(input,output){
  output$hist <- renderPlot({
    # {} allows us to put all our R code in one nice chunck
    data<-tibble(x = rnorm(input$num)) # 100 random normal points 
    ggplot(data, aes(x = x))+ # make a histogram 
      geom_histogram() +
      labs(title = input$title) #Add a new title
  })
  output$stats <- renderPrint({
    summary(rnorm(input$num)) # calculate summary stats based on the numbers
  })
}

#----------------------------
library(shiny)
library(tidyverse)
ui<-fluidPage(
  sliderInput(inputId = "num", # ID name for the input
              label = "Choose a number", # Label above the input
              value = 25, min = 1, max = 100 # values for the slider
  ), 
  textInput(inputId = "title", # new Id is title
            label = "Write a title", #
            value = "Histogram of Random Normal Values"), # starting title
  plotOutput("hist"), #creates space for a plot called hist  
  verbatimTextOutput("stats") # create a space for stats
)
server<-function(input,output){
  data<-reactive({
    tibble(x = rnorm(input$num)) # 100 random normal points
  })
  output$hist <- renderPlot({
    ggplot(data, aes(x = x))+ # make a histogram
      geom_histogram()+
      labs(title = input$title) #Add a new title
    server<-function(input,output){
      data<-reactive({ 
        tibble(x = rnorm(input$num)) # 100 random normal points
      }) 
      output$hist <- renderPlot({
        ggplot(data(), aes(x = x))+ # make a histogram
          geom_histogram()+
          labs(title = input$title) #Add a new title
      })
      output$stats <- renderPrint({
        summary(data()) # calculate summary stats based on the numbers
      })
    }
    shinyApp(ui = ui, server = server)
    
    
install.packages('rsconnect')
    
install.packages("rsconnect")
