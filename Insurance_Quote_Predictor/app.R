
library(shiny)
library(tidyverse)
library(caret)
library(lme4)
library(forecast)
library(htmltools)

set.seed(28)

df <- read.csv('cleaned_df.csv')

# Load the lme4 package
df$region <- ifelse(df$region == "NE", 1, 
                    ifelse(df$region == "NW", 2, 
                           ifelse(df$region == "SE", 3, 4)))


# Multi-Level Varying Slopes & Varying Intercept Model
model_1 <- lmer(charges ~ age*bmi + region + children + female + (1 | smoker) + (0 + bmi|smoker) , data = df)


ui <- fluidPage(
    
  tags$style(HTML("
  .well {
    background-color: #AED6F1;
  }
  .navbar-default .navbar-header > .navbar-brand {
    font-size: 36px;
    background-color: #AED6F1;
  }
  h1 {
    font-size: 45px;
  }
")),
  
    
    pageWithSidebar(
      headerPanel('Get a Quote!'),
      sidebarPanel(
    
        #age input
        numericInput("age", label = h3("Age"), value = NULL),
        
      
        #gender input
        selectInput("selectGender", label = h3("Gender"), choices = c("Female" = 1, "Male" = 0)),
        
        #BMI input
        numericInput("bmi", label = h3("BMI"),  value = NULL),
       
        
        #children input
        numericInput("children", label = h4("How many children do you have?"),  value = NULL),
        
        
        # Smoker
        selectInput("selectSmoker", label = h4("Do you smoke?"), choices = c("Yes" = 1, "No" = 0)),
        
        
        # Region
        selectInput("selectRegion", label = h4("What region of the U.S. do you reside in?"), 
                           choices = list( "SouthEast" = "SE", "SouthWest" = "SW", 
                                          "NorthEast"="NE", "NorthWest"="NW")),
      
        
        # Add an action button to trigger the prediction
        actionButton("predict", "Predict"),
       ),
      
      
      mainPanel(
      
        # Display the predicted value
        uiOutput("prediction"),
        br(),
        # Add an image below the output
        img(src = "logo.jpeg", alt = "Logo", width = "80%", height = "80%")
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
        female =as.numeric(input$selectGender),
        bmi = input$bmi,
        children = input$children,
        smoker = as.numeric(input$selectSmoker),
        region = as.numeric(input$selectRegion)
        # Add other input values as needed
      )
       region_mapping <- c("NE" = 1, "NW" = 2, "SE" = 3, "SW" = 4)
       
       data$region <- region_mapping[input$selectRegion]
       

     # Make predictions using the pre-fitted model
      prediction <- predict(model_1, newdata = data)
      
      
      output$prediction <- renderText({
        HTML(paste0("<div style='font-size: 54px; font-family: Roboto, sans-serif; color: #3498db; display: flex; justify-content: center; align-items: center; height: 100px; border: 2px solid black; padding: 10px; margin-top: 30px; margin-bottom: 10px; margin-left: 10px; margin-right: 10px;'>Estimated Quote: $", round(prediction, 0), "</div>"))
      })
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
