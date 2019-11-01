########
# Shiny app to create a dynamically-filterable visualization of the diamonds app
########

# These bits get run before any of the rest of the code
# Note: contrary to what I told you on Monday, the use of the global.R file is no longer recommended.
# At present, I'm not sure why.
library(shiny)
library(tidyverse)

# We'll limit the range of selectable carats to teh actual range of carats
min.carat <- min(diamonds$carat)
max.carat <- max(diamonds$carat)

# Need a vector of axis variables as characters
axis_vars <- names(diamonds)

# Create a character vector of those columns of diamonds that are 
factor.indices <- vapply(diamonds, is.factor, TRUE) # vapply is a base R function like sapply & lapply that we haven't talked about
# It applies a function (is.factor) to every element of a list (diamonds, remember that a data frame is a list)
# The 'TRUE' is at the end to convey that we want vapply to return the results as a logical vector:
#   I could just as well have used FALSE or c(TRUE, FALSE, TRUE) or any other logical vector
factor.columns <- axis_vars[factor.indices]


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Diamonds viewer"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      # This is a range slider (i.e. there's a max and min). It is set that way by "value" (the starting value), which is a 2-element vector
      sliderInput("caratrange",
                  "Range of carats",
                  min = min.carat,
                  max = max.carat,
                  value = c(min.carat, max.carat)),
      
      
      # Select x and y variables
      selectInput(inputId = "xvar",
                  label = "X axis",
                  choices = axis_vars,
                  selected = "x"),
      
      selectInput(inputId = "yvar",
                  label = "Y axis",
                  choices = axis_vars,
                  selected = "y"),
      
      # I can't stop, I want to add a splash of color
      selectInput(inputId = "color",
                  label = "Color", 
                  choices = factor.columns,
                  selected = "carat"),
      
      actionButton("go", 
                   "Go!",
                   icon = icon("thumbs-up")) # see available icons at http://fontawesome.io/icons/ and http://getbootstrap.com/components/#glyphicons
    ),
    
    # Show a plot of diamonds data frame. This output doesn't care what that plot is, only that it will be associated with output$diamonds_plot
    mainPanel(
      plotOutput("diamonds_plot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  d_filt <- reactive({
  #filter diamonds plot so it only contains specified range
    low.carat <- input$carat.adjuster[1]
    hi.carat <- input$carat.adjuster[2]
  
    diamonds %>%
      filter(carat >= low.carat) %>%
      filter(carat <= hi.carat)
  })
  
  output$diamonds_plot <- renderPlot({
    #filter diamonds plot so it only contians specified range
    ggplot(d_filt(), aes_string(x="carat", y="y", color = "clarity")) +
                                geom_point()
  })
  

}


# Run the application 
shinyApp(ui = ui, server = server)