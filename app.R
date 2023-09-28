
# installing required packages if necessary
if(!"shiny" %in% installed.packages()){ install.packages("shiny") }
if(!"shinythemes" %in% installed.packages()){ install.packages("shinythemes") }
if(!"ggplot2" %in% installed.packages()){ install.packages("ggplot2") }

library(shiny)
library(shinythemes)
library(ggplot2)


ui <- fluidPage(theme= shinytheme("flatly"),
  navbarPage(
    "Item Response Theory",
    tabPanel("Rasch Model",
       sidebarPanel(
           sliderInput(
             inputId = "rasch_diff",
             label = "item difficulty",
             min = -5,
             max = 5,
             step = .1,
             value = 0),

         ),

        mainPanel(

        plotOutput(outputId = "curve_rasch")

        )
    ),
    tabPanel("2PL",
       sidebarLayout(

         # Sidebar panels for inputs
         sidebarPanel(

           sliderInput(inputId = "diff",
                       label = "item difficulty",
                       min = -5,
                       max = 5,
                       step = .1,
                       value = 0),

           sliderInput(inputId = "discr",
                       label = "item discrimination",
                       min = -5,
                       max = 5,
                       step = .1,
                       value = 1)
         ),

         mainPanel(

           plotOutput(outputId = "curve_2PL")

         )
       )
    ),
    tabPanel("3PL",
      sidebarLayout(

        # Sidebar panels for inputs
        sidebarPanel(

          sliderInput(inputId = "3pl_diff",
                      label = "item difficulty",
                      min = -5,
                      max = 5,
                      step = .1,
                      value = 0),

          sliderInput(inputId = "3pl_discr",
                      label = "item discrimination",
                      min = -5,
                      max = 5,
                      step = .1,
                      value = 1),
          sliderInput(inputId = "guess",
                      label = "guess score",
                      min = 0,
                      max = 1,
                      step = .1,
                      value = 0)
        ),

        mainPanel(

          plotOutput(outputId = "curve_3PL")

        )
      )
    )
  )
)
# functions for rendering the plots for each model
server <- function(input, output) {

  # assigning euler's number to variable e
  e <- exp(1)

  #assigning the base attributes of the plot
  base <-
    ggplot() +
    xlim(-5, 5) +
    ylim(0,1) +
    ylab("P(X=1)") +
    xlab("trait level") +
    scale_color_brewer(palette="Set1")



  output$curve_rasch <- renderPlot({
    rasch <- function(x){
      y <- e ** (x - input$rasch_diff) / (1 + e ** (x - input$rasch_diff))
    }
    base + geom_function(fun = rasch)
  })

  output$curve_2PL <- renderPlot({
    two_pl <- function(x){
      y <- e ** (input$discr * (x - input$diff)) / (1 + e ** (input$discr * (x - input$diff)))
    }
    base + geom_function(fun = two_pl)
  })

  output$curve_3PL <- renderPlot({
    three_pl <- function(x){
      y <- input$guess + (1-input$guess)* e ** (input$"3pl_discr" * (x - input$"3pl_diff")) / (1 + e ** (input$"3pl_discr" * (x - input$"3pl_diff")))
    }
    base + geom_function(fun = three_pl)
  })
}

shinyApp(ui = ui, server = server)
