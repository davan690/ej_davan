library(shiny)
#sample distribution plot
m <- 35.4
s <- 4.2
n <- 4
a <- 37
b <- s/25

x <- seq(from=m-3*s/sqrt(n), to=m+3*s/sqrt(n),by=b) ## X values to be plotted
y <- dnorm(x,mean=m,sd=s) ## Normal density
plot(x, y,ylim =c(0.035, 0.1),type="l",col="darkgreen",lwd=2,xaxt="n", main="Standard normal density function",las=1) 
axis(1,
     c(m-3*s/sqrt(n),
       m-2*s/sqrt(n),
       m-1*s/sqrt(n),
       m,
       m+1*s/sqrt(n),
       m+2*s/sqrt(n),
       m+3*s/sqrt(n)))
plot(x,y, type = "l")
polygon(c(x[x <= a],a),c(y[x<=a], 0), col = "lightgreen")
polygon(c(x[x <= 32],32),c(y[x<=32], 0), col = "white")

legend(min(x),max(y), legend=paste("mean=",m,"; sd=",s,sep=""))
grid(col="black")
abline(v=m,col="purple", lwd=2)
abline(v=m-s/sqrt(n),col="green", lwd=2)
abline(v=m+s/sqrt(n),col="green", lwd=2)
abline(v=m-2*s/sqrt(n),col="blue", lwd=2)
abline(v=m+2*s/sqrt(n),col="blue", lwd=2)
abline(v=m-3*s/sqrt(n),col="red", lwd=2)
abline(v=m+3*s/sqrt(n),col="red", lwd=2)

#------------------- shiny --------------------------#
# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Mean Sampling Distribution!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      #input: varibles
      numericInput(inputId = "m",
                   label = "mean:",
                   value = 35.2), 
      numericInput(inputId = "s",
                   label = "standard deviation:",
                   value = 4.2),
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "n",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 1),
      selectInput("var", 
                  label = "Percentage of the area:",
                  choices = c("below","over","between"),
                  selected = "below"),
  
      conditionalPanel(condition = "input.var == 'between'",
         numericInput(inputId = "aaa",
                      label = "quantiles:",
                      value = 0), 
        numericInput(inputId = "aa",
                     label = "quantiles:",
                     value = 0) 
      ),#first condition
      conditionalPanel(condition = "input.var != 'between'",
        numericInput(inputId = "a",
                     label = "quantiles:",
                     value = 0)  
      )
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot"),
      htmlOutput("selected_model")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  output$selected_model <- renderUI({ 
    myModel <- paste("ybar ~ N (", input$m,",", input$s, "/ sqrt(" ,input$n ,")")
    lessThan <- paste("percentage below", input$a, ":",
                      round(pnorm(input$a, mean = input$m, sd = input$s, lower.tail = TRUE),4))
    
    HTML(paste(myModel, lessThan, sep="<br/>"))
  })  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    
    b <- s/25

    
    x <- seq(from=input$m-3*input$s/sqrt(input$n), to=input$m+3*input$s/sqrt(input$n),by=b) ## X values to be plotted
    y <- dnorm(x,mean=input$m,sd=input$s) ## Normal density
    plot(x, y, type="l",col="darkgreen",lwd=2,xaxt="n", 
         main="Standard normal density function",las=1) 
    axis(1,
         round(c(input$m-3*input$s/sqrt(input$n),
           input$m-2*input$s/sqrt(input$n),
           input$m-1*input$s/sqrt(input$n),
           input$m,
           input$m+1*input$s/sqrt(input$n),
           input$m+2*input$s/sqrt(input$n),
           input$m+3*input$s/sqrt(input$n)),2))
    abline(v=input$m,col="purple", lwd=2)
    abline(v=input$m-1*input$s/sqrt(input$n),col="green", lwd=2)
    abline(v=input$m+1*input$s/sqrt(input$n),col="green", lwd=2)
    abline(v=input$m-2*input$s/sqrt(input$n),col="blue", lwd=2)
    abline(v=input$m+2*input$s/sqrt(input$n),col="blue", lwd=2)
    abline(v=input$m-3*input$s/sqrt(input$n),col="red", lwd=2)
    abline(v=input$m+3*input$s/sqrt(input$n),col="red", lwd=2)
    
    
      if(input$var == "below"){
    polygon(c(x[x <= input$a],input$a),c(y[x<=input$a], 0), col = "lightgreen")
      }
    else if(input$var == "over"){
      polygon(c(x[x >= input$a],input$a),c(y[x>=input$a], 0), col = "lightgreen")
    }
    else{
      if(input$aaa > input$aa){
      polygon(c(x[x <= input$aaa],input$aaa),c(y[x<=input$aaa], 0), col = "lightgreen")
      polygon(c(x[x <= input$aa],input$aa),c(y[x<=input$aa], 0), col = "white")
      }
      else{
        polygon(c(x[x <= input$aa],input$aa),c(y[x<=input$aa], 0), col = "lightgreen")
        polygon(c(x[x <= input$aaa],input$aaa),c(y[x<=input$aaa], 0), col = "white")
      }
    }
    # x    <- faithful$waiting
    # bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    # hist(x, breaks = bins, col = "#75AADB", border = "white",
    #      xlab = "Waiting time to next eruption (in mins)",
    #      main = "Histogram of waiting times")
  
     
  })
 
}

shinyApp(ui, server)
