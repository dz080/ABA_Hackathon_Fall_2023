
library(shiny)


# model <- lm(mpg ~ wt + qsec + am, data = mtcars)







# Define UI for application that draws a histogram
ui <- 
  fluidPage(

  
  
    pageWithSidebar(
      headerPanel('Get a Quote'),
      sidebarPanel(
    
        #age input
        numericInput("age", label = h3("Age"), value = "empty"),
        
      
        #gender input
        selectInput("selectGender", label = h3("Gender"), 
                    choices = c("Please select one" = "", "Female" = 1, "Male" = 0),
                    selected = "",
                    selectize = TRUE),
        
        #BMI input
        numericInput("bmi", label = h3("BMI"),  value = "empty"),
       
        
        #children input
        numericInput("children", label = h4("How many children do you have?"),  value = "empty"),
        
        
        # Smoker
        selectInput("selectSmoker", label = h4("Do you smoke?"), 
                    choices = list("Please select one" = "","Yes" = 1, "No" = 0),
                    selected = "",
                    selectize = TRUE),
        
        
        # Region
        selectInput("selectRegion", label = h4("What region of the U.S. do you reside in?"), 
                           choices = list("Please select one" = "", "SouthEast" = "SE", "SouthWest" = "SW", 
                                          "NorthEast"="NE", "NorthWest"="NW"),
                    selected = "",
                    selectize = TRUE),
      
        
        # Add an action button to trigger the prediction
        actionButton("predict", "Predict"),
      ),
      
      
      mainPanel(
      
        # Display the predicted value
        textOutput("prediction")
      
      )
      
     
      
      
      
    )
)  


server <- function(input, output) {

   
    
    output$female <- renderPrint({ input$selectGender })
    output$smoker <- renderPrint({ input$selectSmoker })
    output$region <- renderPrint({ input$selectRegion })
    
    output$age <- renderPrint({ input$age })
    output$bmi <- renderPrint({ input$bmi })
    output$children <- renderPrint({ input$children })
    
    # Observe the event of the 'Predict' button being clicked
    observeEvent(input$predict, {
      # Collect input values
      data <- data.frame(
        age = as.numeric(input$age),
        gender = as.numeric(input$selectGender),
        bmi = as.numeric(input$bmi),
        children = as.numeric(input$children),
        smoker = as.numeric(input$selectSmoker),
        region = input$selectRegion
        # Add other input values as needed
      )
      
      # Handle missing values or perform other data cleaning as necessary
      
      # Make predictions using the pre-fitted model
      prediction <- predict(model, newdata = data)
      
      # Store the predicted value for rendering
      output$prediction <- renderText({
        paste("Predicted value: ", prediction)
      })
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
