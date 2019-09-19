#dnaExtract

#Extractions using the Salting out method
#----------- libraries ------------
library(tidyverse)
library(shiny)
#------------ create dataframe ---------------
step <- c(2, 2, 2, 2, 4, 8, 12, 18)
needfull <- c("1.5ml tubes", "tissue extraction buffer", "10%SDS",
              "Proteinase K (10-20 mg/ml)", "7.5M NH4 Acetate",
              "100% ethanol / isopropienol", "70% ethanol", "MQ H2O")
need <- c("1.5ml tube", "ExtractBuffer", "10%SDS",
          "Proteinase K", "7.5M NH4 Acetate",
          "100% ethanol", "70% ethanol", " MQ H2O")
volume <- c(1, 300, 20, 15, 150, 1000, 600, 50)
units <- c("", "µl", "µl","µl","µl", "µl", "µl","µl")
method <- rep("Salting Out", length(units))
steps <- c(1:length(units))

dnaExtractions <- data.frame(cbind(method, steps, step, needfull,need, volume, units))

summary(dnaExtractions)
dnaExtract <- dnaExtractions %>%
  mutate(volume = as.numeric(as.character(volume)),
         step = as.numeric(as.character(step)))
summary(dnaExtract)
rm(dnaExtractions)


#--------------------- shiny ----------------------
ui <- fluidPage(
  titlePanel("DNA Extraction from Tissue"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Salting Out protocol: quantities of tubes and volumes of solutions"),
      
      selectInput("var", 
                  label = "Choose volume unit",
                  choices = c("µl","ml"),
                  selected = "ml"),
      
      sliderInput("range", 
                  label = "Number of samples:",
                  min = 1, max = 500, value = 50)
    ),
    
    mainPanel(
      textOutput("selected_var"),
      htmlOutput("text2")
    )
  )
)

server <- function(input, output) {
  
  output$selected_var <- renderText({ 
    
  
  })
  output$text2 <- renderUI({
      
    tubesNo <- paste0(dnaExtract$needfull, ":  ", (dnaExtract$volume*input$range))
    
    if (input$var == "ml") {
      
      jsamp <- paste0(dnaExtract$needfull, ":  ", (dnaExtract$volume*input$range)/1000, input$var)
      HTML(paste("You will need:", tubesNo[1], "", jsamp[2], "",
                 jsamp[3], "",jsamp[4], "",jsamp[5],"",jsamp[6],"",
                 jsamp[7], "",jsamp[8], sep="<br/>"))
    }
    else {
      jsamp <- paste0(dnaExtract$needfull, ":  ", dnaExtract$volume*input$range, input$var)
    HTML(paste("You will need:", jsamp[1], "", jsamp[2], "",
               jsamp[3], "",jsamp[4], "",jsamp[5],"",jsamp[6],"",
               jsamp[7], "",jsamp[8], sep="<br/>"))
    }
    
  })
}

shinyApp(ui = ui, server = server)

