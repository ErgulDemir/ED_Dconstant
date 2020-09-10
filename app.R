##ShinyApp_D constatnt for IRT (10.09.2020)

library(shiny)

ui <- fluidPage(
  titlePanel("D constant for IRT models"),
  numericInput("D", "Enter a D constant value:", 1, 1, 2, step = 0.001),
  br(),
  mainPanel(
    plotOutput("plot"),
    br(),
    textOutput("text")
  )
)

server <- function(input, output){ 

  output$plot <- renderPlot({ 

    #Standard normal distribution density-probability function
    f1 <- function(z) 1/sqrt(2*pi) * exp(-1/2 * z^2)

    #IRT 1PL logistic function (b=0)
    f2 <- function(t, D) 1 / (1 + exp(-D*t))
    
    xv <- seq(-3.5, 3.5, .01)
    yv1 <- f1(xv)
    yv2 <- cumsum(yv1)
    yv3 <- yv2 / max(yv2)
    yv4 <- f2(xv, input$D)

    plot(xv, yv3, type="l", ylab = "Probability", 
      xlab = "Theta", col = "red", lwd = 2)
    lines(xv, yv4, type ="l", lwd = 2, col = "blue") 
    legend("right", c("Normal-Ogive", "1PL IRT"), col = c("red", "blue"),
      lty = 1, pch = 15, cex = 1.2)
  })
  
  output$text <- renderText({
    f1 <- function(z) 1/sqrt(2*pi) * exp(-1/2 * z^2)
    f2 <- function(t, D) 1 / (1 + exp(-D*t))
    
    xv <- seq(-3.5, 3.5, .01)
    yv1 <- f1(xv)
    yv2 <- cumsum(yv1)
    yv3 <- yv2 / max(yv2)
    yv4 <- f2(xv, input$D)
    
    diff <- yv4 - yv3

    std.err <- sd(diff^2) / sqrt(length(xv))
    
    options(scipen=999)		#for not reverse to the scientific notation

    paste("For D =", input$D, ", mean of square difference is", 
      round(mean(diff^2), 5), "(with", round(std.err, 5), "standard error)." 
      )
  })
}

shinyApp(ui, server)
