

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Next Word prediction App using Katz back off" ),
  
 
  sidebarLayout(
    sidebarPanel(
      # Text input
      textInput("text", label = ('Please enter your sentence'), value = 'the default value'),
      # Submit button for making the prediction
      submitButton("Submit")
    ),
    
    # Show a table of the generated output
    mainPanel(
      h3("Predicted output with probabilities"),
      dataTableOutput('table'))
    )
  )
)
