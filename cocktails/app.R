#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(jsonlite)
library(lubridate)

ingredients <- fromJSON("https://www.thecocktaildb.com/api/json/v1/1/list.php?i=list")$drinks$strIngredient1

getIngredients <- function(drink){
  ingredients <- c(drink$drinks$strIngredient1,drink$drinks$strIngredient2,drink$drinks$strIngredient3,drink$drinks$strIngredient4,
                   drink$drinks$strIngredient5,drink$drinks$strIngredient6,drink$drinks$strIngredient7,drink$drinks$strIngredient8,
                   drink$drinks$strIngredient9,drink$drinks$strIngredient10,drink$drinks$strIngredient11,drink$drinks$strIngredient12,
                   drink$drinks$strIngredient13,drink$drinks$strIngredient14,drink$drinks$strIngredient15)
  ingredients <- ingredients[!is.na(ingredients)]
  return(ingredients)
}

getMeasures <- function(drink){
  measures <- c(drink$drinks$strMeasure1,drink$drinks$strMeasure2,drink$drinks$strMeasure3,drink$drinks$strMeasure4,drink$drinks$strMeasure5,
                drink$drinks$strMeasure6,drink$drinks$strMeasure7,drink$drinks$strMeasure8,drink$drinks$strMeasure9,drink$drinks$strMeasure10,
                drink$drinks$strMeasure11,drink$drinks$strMeasure12,drink$drinks$strMeasure13,drink$drinks$strMeasure14,drink$drinks$strMeasure15)
  measures <- measures[!is.na(measures)]
  return(measures)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
   # Application title
   titlePanel("Cocktail Maker"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
       
        actionButton("random_input", "Surprise Me!"),
        br(),
        fluidRow(checkboxGroupInput("ingredients", h3("Select Ingredients"), choices = ingredients, inline = TRUE)),
        br(),
        actionButton("manual_input", "Make a Cocktail"),
        width = 4
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
       
        h3(textOutput("selected_ingredients")),
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
  
  drink_ids <- reactive({
    all_recipes <- fromJSON(str_glue("https://www.thecocktaildb.com/api/json/v1/1/filter.php?i={ingredient}", ingredient = URLencode(input$ingredients[1])))$drinks$idDrink
    if(length(input$ingredients) > 1){
      for(val in input$ingredients) {
        recipes <- fromJSON(str_glue("https://www.thecocktaildb.com/api/json/v1/1/filter.php?i={ingredient}", ingredient = URLencode(val)))$drinks$idDrink
        all_recipes <- intersect(all_recipes,recipes)
       }
    }
    all_recipes
  })
  
  random_drink_details <- reactive({
    random <- fromJSON("https://www.thecocktaildb.com/api/json/v1/1/random.php")
    random
  })
  
  counter <- reactiveVal(0)
  
  observeEvent(input$manual_input, {
    #clear existing UI
    output$name <- renderText({})
    output$drinkImage <- renderUI({})
    output$ingTitle <- renderText({})
    output$ingredients <- renderTable({})
    output$instrTitle <- renderText({})
    output$instructions <- renderText({})
    output$nextButton <- renderUI({})
    
    #initialize count
    counter(0)
    
    if(is.null(input$ingredients)){
      output$ingTitle <- renderText({
        print("Please enter at least one ingredient")
      })
    } else{
      drink_ids <- drink_ids()
      if(length(drink_ids) == 0){
        output$ingTitle <- renderText({
          print("There are no cocktail recipes with all of these ingredients")
        })
      } else{
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
        
        if(length(drink_ids) > 1){
          output$nextButton <- renderUI({
            actionButton("next_drink", "next")
          })
        }
      }
    }
  })
  
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
  
  #display randomly chosen drink
  observeEvent(input$random_input, {
    random <- fromJSON("https://www.thecocktaildb.com/api/json/v1/1/random.php")
    imgurl <- random$drinks$strDrinkThumb
    ingredient <- getIngredients(random)
    amount <- getMeasures(random)
    ingredients <- cbind(amount, ingredient)
    
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

