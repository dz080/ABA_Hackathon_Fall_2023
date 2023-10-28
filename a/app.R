
library(shiny)
library(tidyverse)
library(caret)
library(lme4)
library(forecast)

set.seed(28)

df <- read.csv('cleaned_df.csv')

# Load the lme4 package
df$region <- ifelse(df$region == "NE", 1, 
                    ifelse(df$region == "NW", 2, 
                           ifelse(df$region == "SE", 3, 4)))


# Multi-Level Varying Slopes & Varying Intercept Model
model_1 <- lmer(charges ~ age*bmi + region + children + female + (1 | smoker) + (0 + bmi|smoker) , data = df)


ui <- fluidPage(
  
    pageWithSidebar(
      headerPanel('Get a Quote'),
      sidebarPanel(
    
        #age input
        numericInput("age", label = h3("Age"), value = NULL),
        
      
        #gender input
        selectInput("selectGender", label = h3("Gender"), 
                    choices = c( "Female" = 1, "Male" = 0)),
        
        #BMI input
        numericInput("bmi", label = h3("BMI"),  value = NULL),
       
        
        #children input
        numericInput("children", label = h4("How many children do you have?"),  value = NULL),
        
        
        # Smoker
        selectInput("selectSmoker", label = h4("Do you smoke?"), 
                    choices = list("Yes" = 1, "No" = 0)),
        
        
        # Region
        selectInput("selectRegion", label = h4("What region of the U.S. do you reside in?"), 
                           choices = list( "SouthEast" = "SE", "SouthWest" = "SW", 
                                          "NorthEast"="NE", "NorthWest"="NW")),
      
        
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
        age = input$age,
        female =input$selectGender,
        bmi = input$bmi,
        children = input$children,
        smoker = input$selectSmoker,
        region = input$selectRegion
        # Add other input values as needed
      )
       
       data$region <- ifelse(data$region == "NE", 1, 
                           ifelse(data$region == "NW", 2, 
                                  ifelse(data$region == "SE", 3, 4)))
       

     # Make predictions using the pre-fitted model
      prediction <- predict(model_1, newdata = data)

      # Store the predicted value for rendering
      output$prediction <- renderText({
         paste("Predicted value: ", prediction)
       })
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
