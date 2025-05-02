library(shiny)
library(shinythemes)
library(ggplot2)
library(latex2exp)

ui <- fluidPage(theme= shinytheme("flatly"),
  navbarPage(
    "Item Response Theory",
    tabPanel("Rasch Model",
       sidebarPanel(
           sliderInput(
             inputId = "rasch_diff",
             label = withMathJax("$$\\text{item difficulty } \\color{red}{b_i}$$"),
             min = -5,
             max = 5,
             step = .1,
             value = 0),

         ),

        mainPanel(
          withMathJax(
            "$$\\text{Probability of a correct answer from participant } p \\text{ on item } i: P(X_{pi}=1|
            \\theta_p)=\\frac{e^{\\theta_p-\\color{red}{b_i}}}{1+e^{\\theta_p-\\color{red}{b_i}}}$$"),
          plotOutput(outputId = "curve_rasch")

        )
    ),
    tabPanel("2PL",
       sidebarLayout(

         # Sidebar panels for inputs
         sidebarPanel(

           sliderInput(inputId = "diff",
                       label = withMathJax("$$\\text{item difficulty } \\color{red}{b_i}$$"),
                       min = -5,
                       max = 5,
                       step = .1,
                       value = 0),

           sliderInput(inputId = "discr",
                       label = withMathJax("$$\\text{item discrimination } \\color{blue}{a_i}$$"),
                       min = -5,
                       max = 5,
                       step = .1,
                       value = 1)
         ),

         mainPanel(
           withMathJax(
             "$$\\text{Probability of a correct answer from participant } p \\text{ on item } i: P(X_{pi}=1|
            \\theta_p)=\\frac{e^{\\color{blue}{a_i}(\\theta_p-\\color{red}{b_i})}}{1+e^{\\color{blue}{a_i}(\\theta_p-\\color{red}{b_i})}}$$"),
           plotOutput(outputId = "curve_2PL")

         )
       )
    ),
    tabPanel("3PL",
      sidebarLayout(
        # Sidebar panels for inputs
        sidebarPanel(

          sliderInput(inputId = "3pl_diff",
                      label = withMathJax("$$\\text{item difficulty } \\color{red}{b_i}$$"),
                      min = -5,
                      max = 5,
                      step = .1,
                      value = 0),

          sliderInput(inputId = "3pl_discr",
                      label = withMathJax("$$\\text{item discrimination } \\color{blue}{a_i}$$"),
                      min = -5,
                      max = 5,
                      step = .1,
                      value = 1),
          sliderInput(inputId = "guess",
                      label = withMathJax("$$\\text{guess score } \\color{orange}{c_i}$$"),
                      min = 0,
                      max = 0.5,
                      step = .01,
                      value = 0),
        ),

        mainPanel(
          withMathJax(
            "$$\\text{Probability of a correct answer from participant } p \\text{ on item } i: P(X_{pi}=1|
            \\theta_p)=\\color{orange}{c_i} + (1-\\color{orange}{c_i})\\frac{e^{\\color{blue}{a_i}(\\theta_p-\\color{red}{b_i})}}{1+e^{\\color{blue}{a_i}(\\theta_p-\\color{red}{b_i})}}$$"),
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
    ylab(TeX("$P(X=1|\\theta_p)$")) +
    xlab(TeX("trait $\\theta_p$")) +
    scale_color_brewer(palette="Set1") +
    theme_classic() +
    theme(axis.title=element_text(size=16))

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
