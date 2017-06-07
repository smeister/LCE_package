library(shiny)
library(shinysky)
library(shinydashboard)
library(ggplot2)

ui <- fluidPage(
  fluidRow(
    column(2, selectInput(inputId = "virusCT", label = "Virus CT", choices = c("Virus CT" = "CT_test"), selected = "CT_test")),
    column(2, selectInput(inputId = "viruschoice", label = "According to:", choices = c("Genus" = "Genus", "Family" = "Family", "Species" = "Species", "Virus Name" = "Name", "Genome type" = "GenomeT"), selected = "Genus")),
    column(3, selectInput(inputId = "virusdisinf", label = "Disinfectant", choices = c("UV" = "UV","Sunlight" = "Sunlight","Chloramine" = "Chloramine", "Chlorine" = "Chlorine", "Chlorine dioxide" = "Chlorine dioxide", "Ozone" = "Ozone"), selected = "UV")),
    column(3, selectInput(inputId = "viruslog", label = "Log inactivation", choices = c("2-log"=99, "3-log"=99.9, "4-log"=99.99), selected = 99)),
    column(2, numericInput(inputId = "CTth", label = "CT threslhold", value = 10))
  ),
  fluidRow(
    plotOutput(outputId = "VirusGraph", width = "100%", height = "500px")
  ),
  fluidRow(
    column(width = 12, align = "center", sliderInput(inputId = "tempslider", label = "Temperature range", min = 0, max = 40, value = c(0, 40), width = "85%"))
  )
)



server <- function (input, output) {
  #######################################################
  ##################### Virus Database ##################

  ## Graphs Display ########
  df_virus<-data.frame(read.csv2("./Data/Virus_CT_list2.csv", header = TRUE, dec = "."))
  observe({
    df_virus2 <- df_virus[df_virus$Disinfectant == input$virusdisinf,] # OKAY
    if (input$virusdisinf == "UV" | input$virusdisinf == "Sunlight") {
      df_virus3 <- df_virus2
    } else {
      df_virus3 <- df_virus2[df_virus2$Temp_test >= input$tempslider[1] & df_virus2$Temp_test <= input$tempslider[2], ]
    }
    df_virus4 <- df_virus3[df_virus3$Inactivation == input$viruslog,]
    if (input$virusdisinf == "UV" | input$virusdisinf == "Sunlight") {
      yLabel <- "mWs/cm2"
    } else {
      yLabel <- "mg*min/L"
    }
    output$VirusGraph <- renderPlot({
      ggplot(data = df_virus4, aes_string(x = input$viruschoice, y = input$virusCT)) +
        geom_boxplot() +
        geom_hline(yintercept = input$CTth, color = "red", size = 2) +
        theme(axis.text.x = element_text(angle = 45, size = 14, face = "bold", hjust = 1))+
        labs(y = yLabel)
    })
  })

  ##################### Virus Database ##################
  #######################################################
}

shinyApp(server = server, ui = ui)


