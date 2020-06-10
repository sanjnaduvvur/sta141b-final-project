#
# This Shiny application is built to make finding cocktail recipes easier
# 
# The user can prompt the application to choose a random cocktail recipe by selecting "Surpise Me!"
# Alternately, the user can select ingredients they like and/or have available to them 
# and the application will display possible recipes the user can create
#
# TheCocktailDB API documentation: https://www.thecocktaildb.com/api.php
#

library(shiny)
library(tidyverse)
library(jsonlite)
library(lubridate)

# Store all ingredients from API
ingredients <- fromJSON("https://www.thecocktaildb.com/api/json/v1/1/list.php?i=list")$drinks$strIngredient1

# Function returns all ingredients needed for a particular drink
# @param drink: parsed JSON of specific drink
getIngredients <- function(drink){
  ingredients <- c(drink$drinks$strIngredient1,drink$drinks$strIngredient2,drink$drinks$strIngredient3,drink$drinks$strIngredient4,
                   drink$drinks$strIngredient5,drink$drinks$strIngredient6,drink$drinks$strIngredient7,drink$drinks$strIngredient8,
                   drink$drinks$strIngredient9,drink$drinks$strIngredient10,drink$drinks$strIngredient11,drink$drinks$strIngredient12,
                   drink$drinks$strIngredient13,drink$drinks$strIngredient14,drink$drinks$strIngredient15)
  ingredients <- ingredients[!is.na(ingredients)]
  return(ingredients)
}

# Function returns all measurements of ingredients needed for a particular drink
# @param drink: parsed JSON of specific drink
getMeasures <- function(drink){
  measures <- c(drink$drinks$strMeasure1,drink$drinks$strMeasure2,drink$drinks$strMeasure3,drink$drinks$strMeasure4,drink$drinks$strMeasure5,
                drink$drinks$strMeasure6,drink$drinks$strMeasure7,drink$drinks$strMeasure8,drink$drinks$strMeasure9,drink$drinks$strMeasure10,
                drink$drinks$strMeasure11,drink$drinks$strMeasure12,drink$drinks$strMeasure13,drink$drinks$strMeasure14,drink$drinks$strMeasure15)
  measures <- measures[!is.na(measures)]
  return(measures)
}

ui <- fluidPage(

   titlePanel("Cocktail Maker"),
   
   sidebarLayout(
      sidebarPanel(
       
        actionButton("random_input", "Surprise Me!"),
        br(),
        fluidRow(checkboxGroupInput("ingredients", h3("Select Ingredients"), choices = ingredients, inline = TRUE)),
        br(),
        actionButton("manual_input", "Make a Cocktail"),
        width = 4
      ),
      
      mainPanel(
       
        h2(textOutput("name")),
        br(),
        uiOutput("drinkImage"),
        h3(textOutput("ingTitle")),
        tableOutput("ingredients"),
        h3(textOutput("instrTitle")),
        textOutput("instructions"),
        br(),
        uiOutput("nextButton")
      )
      
   ) 
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Function returns list of cocktail ids that contain all ingredients selected by user
  drink_ids <- reactive({
    all_recipes <- fromJSON(str_glue("https://www.thecocktaildb.com/api/json/v1/1/filter.php?i={ingredient}", ingredient = URLencode(input$ingredients[1])))$drinks$idDrink
    
    if(length(input$ingredients) > 1){
      for(val in input$ingredients) {
        recipes <- fromJSON(str_glue("https://www.thecocktaildb.com/api/json/v1/1/filter.php?i={ingredient}", ingredient = URLencode(val)))$drinks$idDrink
        
        # Compare cocktail ids and only keep drinks in common with existing list
        all_recipes <- intersect(all_recipes,recipes)
       }
    }
    all_recipes
  })
  
  counter <- reactiveVal(0)
  
  # Called when user selects "Make a Cocktail" button
  observeEvent(input$manual_input, {
    
    # Clear existing UI
    output$name <- renderText({})
    output$drinkImage <- renderUI({})
    output$ingTitle <- renderText({})
    output$ingredients <- renderTable({})
    output$instrTitle <- renderText({})
    output$instructions <- renderText({})
    output$nextButton <- renderUI({})
    
    # Initialize counter
    counter(0)
    
    # If no ingredients have been selected, show warning
    if(is.null(input$ingredients)){
      output$ingTitle <- renderText({
        print("Please enter at least one ingredient")
      })
    } else{ # User has selected ingredients
      drink_ids <- drink_ids()
      # If there are no cocktails with selected ingredients, show warning
      if(length(drink_ids) == 0){
        output$ingTitle <- renderText({
          print("There are no cocktail recipes with all of these ingredients")
        })
      } else{ # One or more cocktail recipes exist
        
        newCount <- counter() + 1
        counter(newCount)
        
        drink <- fromJSON(str_glue("https://www.thecocktaildb.com/api/json/v1/1/lookup.php?i={id}", id = drink_ids[newCount]))
        imgurl <- drink$drinks$strDrinkThumb
        ingredient <- getIngredients(drink)
        amount <- getMeasures(drink)
        ingredients <- cbind(amount, ingredient)  
        
        output$name <- renderText({
          drink$drinks$strDrink
        })
        
        output$drinkImage <- renderUI({
          tags$img(src=imgurl, width = 400, height = 400)
        })
        
        output$ingTitle <- renderText({
          print("Ingredients:")
        })
        
        output$ingredients <- renderTable({
          ingredients
        })
        
        output$instrTitle <- renderText({
          print("Instructions:")
        })
        
        output$instructions <- renderText({
          drink$drinks$strInstructions
        })
        
        # If there is more than 1 available recipe, provide next button
        if(length(drink_ids) > 1){
          output$nextButton <- renderUI({
            actionButton("next_drink", "next")
          })
        }
      }
    }
  })
  
  # Called when user selects "next" button, iterates through all remaining cockatil recipes
  observeEvent(input$next_drink, {   
    
    newCount <-counter() + 1
    counter(newCount)
    
    drink_ids <- drink_ids()
    drink <- fromJSON(str_glue("https://www.thecocktaildb.com/api/json/v1/1/lookup.php?i={id}", id = drink_ids[newCount]))
    imgurl <- drink$drinks$strDrinkThumb
    ingredient <- getIngredients(drink)
    amount <- getMeasures(drink)
    ingredients <- cbind(amount, ingredient)  
    
    output$name <- renderText({
      drink$drinks$strDrink
    })
    
    output$drinkImage <- renderUI({
      tags$img(src=imgurl, width = 400, height = 400)
    })
    
    output$ingTitle <- renderText({
      print("Ingredients:")
    })
    
    output$ingredients <- renderTable({
      ingredients
    })
    
    output$instrTitle <- renderText({
      print("Instructions:")
    })
    
    output$instructions <- renderText({
      drink$drinks$strInstructions
    })
    
    if(counter() == length(drink_ids)){
      output$nextButton <- renderUI({})
    }
  })
  
  # Called when user selects "Surprise Me!"; Displays randomly chosen cocktail
  observeEvent(input$random_input, {
    
    # Generate random cocktail
    random <- fromJSON("https://www.thecocktaildb.com/api/json/v1/1/random.php")
    
    imgurl <- random$drinks$strDrinkThumb
    ingredient <- getIngredients(random)
    amount <- getMeasures(random)
    ingredients <- cbind(amount, ingredient)
    
    # Clear existing UI
    output$nextButton <- renderUI({})
    
    output$name <- renderText({
      random$drinks$strDrink
    })
    
    output$drinkImage <- renderUI({
      tags$img(src=imgurl, width = 400, height = 400)
    })
    
    output$ingTitle <- renderText({
      print("Ingredients:")
    })
    
   output$ingredients <- renderTable({
      ingredients
    })
      
    output$instrTitle <- renderText({
      print("Instructions:")
    })
    
    output$instructions <- renderText({
      random$drinks$strInstructions
    })
    
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

