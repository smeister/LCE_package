library(shiny)
library(shinysky)
library(shinydashboard)
library(LCE.Package)
library(bbmle)
library(ggplot2)

ui <- navbarPage("LCE",
                 navbarMenu("Matt's Functions",
                            tabPanel("getMPN",
                                     fluidRow(
                                       column(width = 2, numericInput(inputId = "data.nb", label = "Nb. of datapoints", value = 12)),
                                       column(width = 2, numericInput(inputId = "rep.nb", label = "Nb. of replicates", value = 5)),
                                       column(width = 2, numericInput(inputId = "dil.nb", label = "Nb. of dilutions", value = 8)),
                                       column(width = 2, textInput(inputId = "start.conc", label = "Starting volume", value = 0.01)),
                                       column(width = 2, numericInput(inputId = "dil.fact", label = "Dilution factor", value = 10))
                                     ),
                                     fluidRow(
                                       column(hotable("importData"), width = 12, offset = 0.1)
                                     ),
                                     fluidRow(
                                       br(),
                                       column(actionButton(inputId = "clickMPN", label = "Calculate MPN"), width = 12, offset = 0.1),
                                       column(p(strong("Results:")), width = 12, offset = 0.1),
                                       column(hotable("results"), width = 12, offset = 0.1)
                                     ),
                                     fluidRow(
                                       br(),
                                       column(downloadButton("savegetMPN", "Save"), width = 12)
                                     )
                            ),
                            tabPanel("getK",
                                     fixedRow(
                                       column(width = 4, selectInput(inputId = "biorep", label = "Number of biological replicates", choices = c(1, 2, 3))),
                                       br(),
                                       column(width = 4, actionButton(inputId = "clickK", label = "Calculate K"), offset = 1)
                                     ),
                                     fluidRow(
                                       column(width = 12,
                                              # 1st biological replicate
                                              conditionalPanel(condition = "input.biorep == 1 || input.biorep == 2 || input.biorep == 3",
                                                               fluidRow(
                                                                 column(12, style = "border-radius: 10px; border-width: 2px; border-style: solid;border-color: #000000;height:350px",
                                                                        column(8,
                                                                               br(),
                                                                               fluidRow(
                                                                                 column(width = 2, numericInput(inputId = "dataA1", label = "Data nb", value = 3)),
                                                                                 column(width = 2, numericInput(inputId = "repA1", label = "Rep. nb", value = 5)),
                                                                                 column(width = 2, numericInput(inputId = "dilA1", label = "Dil. nb", value = 3)),
                                                                                 column(width = 2, textInput(inputId = "startA1", label = "Start [ ]", value = 0.01)),
                                                                                 column(width = 2, numericInput(inputId = "dilfactA1", label = "Dil. fact.", value = 10))
                                                                               ),
                                                                               fixedRow(
                                                                                 box(width = 12,
                                                                                     div(style = 'height:60px; width:100%', hotable("getKTimeA1")),
                                                                                     div(style = 'overflow-y: scroll; height:190px; width:100%', hotable("getKDataA1"))
                                                                                 )
                                                                               )
                                                                        ),
                                                                        br(),
                                                                        column(4, style = "border-radius: 10px; border-width: 2px; border-style: dotted;border-color: #000000; height: 300px",
                                                                               fluidRow(
                                                                                 column(12, style = "height:150px;",
                                                                                        plotOutput(outputId = "graph1", height = "150px")
                                                                                 )
                                                                               ),
                                                                               br(),
                                                                               fluidRow(
                                                                                 column(12, align = "center",
                                                                                        tableOutput("getKDataA2")
                                                                                 )
                                                                               )
                                                                        )
                                                                 )

                                                               )
                                              ),
                                              br(),
                                              # 2nd biological replicate
                                              conditionalPanel(condition = "input.biorep == 2 || input.biorep == 3",
                                                               fluidRow(
                                                                 column(12, style = "border-radius: 10px; border-width: 2px; border-style: solid;border-color: #000000;height:350px",
                                                                        column(8,
                                                                               br(),
                                                                               fluidRow(
                                                                                 column(width = 2, numericInput(inputId = "dataB1", label = "Data nb", value = 3)),
                                                                                 column(width = 2, numericInput(inputId = "repB1", label = "Rep. nb", value = 5)),
                                                                                 column(width = 2, numericInput(inputId = "dilB1", label = "Dil. nb", value = 3)),
                                                                                 column(width = 2, textInput(inputId = "startB1", label = "Start [ ]", value = 0.01)),
                                                                                 column(width = 2, numericInput(inputId = "dilfactB1", label = "Dil. fact.", value = 10))
                                                                               ),
                                                                               fixedRow(
                                                                                 box(width = 12,
                                                                                     div(style = 'height:60px; width:100%', hotable("getKTimeB1")),
                                                                                     div(style = 'overflow-y: scroll; height:190px; width:100%', hotable("getKDataB1"))
                                                                                 )
                                                                               )
                                                                        ),
                                                                        br(),
                                                                        column(4, style = "border-radius: 10px; border-width: 2px; border-style: dotted;border-color: #000000; height: 300px",
                                                                               fluidRow(
                                                                                 column(12, style = "height:150px;",
                                                                                        plotOutput(outputId = "graph2", height = "150px")
                                                                                 )
                                                                               ),
                                                                               br(),
                                                                               fluidRow(
                                                                                 column(12, align = "center",
                                                                                        tableOutput("getKDataB2")
                                                                                 )
                                                                               )
                                                                        )
                                                                 )
                                                               )
                                              ),
                                              br(),
                                              # 3rd biological replicate
                                              conditionalPanel(condition = "input.biorep == 3",
                                                               fluidRow(
                                                                 column(12, style = "border-radius: 10px; border-width: 2px; border-style: solid;border-color: #000000;height:350px",
                                                                        column(8,
                                                                               br(),
                                                                               fluidRow(
                                                                                 column(width = 2, numericInput(inputId = "dataC1", label = "Data nb", value = 3)),
                                                                                 column(width = 2, numericInput(inputId = "repC1", label = "Rep. nb", value = 5)),
                                                                                 column(width = 2, numericInput(inputId = "dilC1", label = "Dil. nb", value = 3)),
                                                                                 column(width = 2, textInput(inputId = "startC1", label = "Start [ ]", value = 0.01)),
                                                                                 column(width = 2, numericInput(inputId = "dilfactC1", label = "Dil. fact.", value = 10))
                                                                               ),
                                                                               fixedRow(
                                                                                 box(width = 12,
                                                                                     div(style = 'height:60px; width:100%', hotable("getKTimeC1")),
                                                                                     div(style = 'overflow-y: scroll; height:190px; width:100%', hotable("getKDataC1"))
                                                                                 )
                                                                               )
                                                                        ),
                                                                        br(),
                                                                        column(4, style = "border-radius: 10px; border-width: 2px; border-style: dotted;border-color: #000000; height: 300px",
                                                                               fluidRow(
                                                                                 column(12, style = "height:150px;",
                                                                                        plotOutput(outputId = "graph3", height = "150px")
                                                                                 )
                                                                               ),
                                                                               br(),
                                                                               fluidRow(
                                                                                 column(12, align = "center",
                                                                                        tableOutput("getKDataC2")
                                                                                 )
                                                                               )
                                                                        )
                                                                 )


                                                               )
                                              ),
                                              br(),
                                              conditionalPanel(condition = "input.biorep == 2 | input.biorep == 3",
                                                               fluidRow(
                                                                 column(12, align = "center",
                                                                        h3("Results")
                                                                 )
                                                               ),
                                                               fluidRow(
                                                                 column(6,
                                                                        plotOutput(outputId = "graph0")
                                                                 ),
                                                                 column(6, align = "center",
                                                                        tableOutput("getKData0")
                                                                 )
                                                               )
                                              )
                                       )
                                     )
                            ),
                            tabPanel("getCT",
                              em("blabla")
                            )
                 ),
                 navbarMenu("Fluence/Dose Calculation",
                            tabPanel("UV",
                                     sidebarLayout(
                                       sidebarPanel(
                                         em("Input values:"),
                                         br(),
                                         numericInput(inputId = "UVtemp", label = "Temperature °C", value = 22),
                                         numericInput(inputId = "UViodide", label = "iodide [M]", value = 1.054),
                                         numericInput(inputId = "UVvol", label = "Volume [L]", value = 0.002),
                                         numericInput(inputId = "UVcm2", label = "Area [cm2]", value = 4.91),

                                         br(),
                                         em("Initial OD values:"),
                                         numericInput(inputId = "OD300", label = "300 nm", value = 1.1181),
                                         numericInput(inputId = "OD352", label = "352 nm", value = 0.0245),
                                         br(),
                                         fluidRow(
                                           column(6, h4("Quantum Yield:")),
                                           column(6, h4(textOutput(outputId = "QuantumY"), style = "color:red"))
                                         ),
                                         fluidRow(
                                           column(6, h4("Fluence [W/m2]:")),
                                           column(6, h4(textOutput(outputId = "UVfluence"), style = "color:red"))
                                         )
                                       ),
                                       mainPanel(
                                         fluidRow(
                                           column(12, style = "border-radius: 10px; border-width: 2px; border-style: solid;border-color: #000000; height: 380px",
                                                  column(12,
                                                         br(),
                                                         column(3, selectInput(inputId = "UVtimes", label = "Time Unit", choices = c("Hours"=3600, "Min."=60, "Sec."=1), selected = 60)),
                                                         column(3, numericInput(inputId = "UVtimesNB", label = "Points nb", value = 9)),
                                                         br(),
                                                         column(3, actionButton(inputId = "clickUV", label = "Calculate Dose/Time"))

                                                  ),
                                                  column(12,
                                                         hotable("UVtimestable"),
                                                         br(),
                                                         hotable("UVODtable"),
                                                         br(),
                                                         column(12, h4("Results in [ mWs/cm2 ] or [ mJ/cm2 ]")),
                                                         hotable("resultUV")

                                                  )

                                           )
                                         ),
                                         fluidRow(
                                           plotOutput("UVplotresults")
                                         )
                                       )
                                     )

                            ),
                            tabPanel("Sunlight",
                                     fluidRow(
                                       column(12, style = "border-radius: 10px; border-width: 2px; border-style: solid;border-color: #000000; height: 580px",
                                              fluidRow(
                                                column(12, h3("Absolute Irradiance (Atmospheric Filter)"), align = "center")
                                              ),
                                              fluidRow(
                                                column(2, selectInput(inputId = "sundata", label = p(em("Select profile:")), choices = c("new lamp", "old lamp"))),
                                                column(8, em("Sun 2000, Abet Technologies, 1000W Xenon lamp"), align = "center")
                                              ),
                                              conditionalPanel(condition = "input.sundata == 'new lamp'",
                                                               fluidRow(
                                                                 column(width = 12,
                                                                        plotOutput(outputId = "sungraph2", width = "100%", height = "300px")
                                                                 )
                                                               )
                                              ),
                                              conditionalPanel(condition = "input.sundata == 'old lamp'",
                                                               fluidRow(
                                                                 column(width = 12,
                                                                        plotOutput(outputId = "sungraph1", width = "100%", height = "300px")
                                                                 )
                                                               )
                                              ),
                                              fluidRow(
                                                column(width = 12, align = "center", sliderInput(inputId = "sunslider", label = NULL, min = 250, max = 950, value = c(280, 320), width = "85%")
                                                )
                                              ),
                                              fluidRow(
                                                column(width = 12,
                                                       column(5, h3("Fluence [ W/m2 ] :"), offset = 1),
                                                       column(3, h3(textOutput(outputId = "sunfluence"), style = "color:red"))
                                                )
                                              )

                                       )
                                     ),
                                     br(),
                                     fluidRow(
                                       column(12, style = "border-radius: 10px; border-width: 2px; border-style: solid;border-color: #000000; height: 280px",
                                              column(12,
                                                     br(),
                                                     column(2, selectInput(inputId = "suntimes", label = "Time Unit", choices = c("Hours"=3600, "Min."=60, "Sec."=1), selected = 3600)),
                                                     column(2, numericInput(inputId = "suntimesNB", label = "Points nb", value = 8)),
                                                     br(),
                                                     column(2, actionButton(inputId = "clickSUN", label = "Calculate Dose/Time"))

                                              ),
                                              box(width = 10,
                                                  div(style = 'overflow-y: scroll; height:60px; width:900px', hotable("suntimestable"))
                                              ),
                                              column(12, h4("Results in [ mWs/cm2 ] or [ mJ/cm2 ]")),
                                              box(width = 10,
                                                  div(style = 'overflow-y: scroll; height:190px; width:900px', hotable("resultsun"))
                                              )
                                       )

                                     )
                            ),
                            tabPanel("Free Chlorine",
                                     # Insert Free Chlorine
                                     h4("FC stuff")
                            ),
                            tabPanel("ClO2",
                                     # Insert ClO2
                                     h4("ClO2 stuff")
                            ),
                            tabPanel("Ozone",
                                     # Insert Ozone
                                     h4("Ozone stuff")
                            )
                 ),
                 navbarMenu("Database",
                            tabPanel("Virus Database",
                                     fluidRow(
                                       column(12,
                                              box(width = 12,
                                                  div(style = 'overflow-y: scroll; height:500px;font-size:80%', dataTableOutput(outputId = "viruses"))
                                              )

                                       ),
                                       column(12,
                                              br(),
                                              downloadButton("saveDatabase", "Download Database")
                                       )
                                     )
                            ),
                            tabPanel("Graphs Display",
                                     fluidPage(
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
                            )
                 )

)

##########
# SERVER #
##########

server <- function (input, output) {
  ###################################################
  #################### getMPN #######################
  # Render: Create the data table
  output$importData <- renderHotable({
    df<-data.frame(dil.=repDIL(as.numeric(input$start.conc), input$dil.nb, input$dil.fact), rep.=rep(input$rep.nb, input$dil.nb))
    data.frame(df, matrix("", ncol = input$data.nb, nrow = input$dil.nb))
  }, readOnly = FALSE)

  # Observe
  observeEvent(input$clickMPN, {
    df <- hot.to.df(input$importData)
    output$results <- renderHotable({
      calcMPN(df)
    })
  })

  # Save Data
  output$savegetMPN <- downloadHandler(
    filename = function () {
      paste("data", ".csv", sep="")
    },
    content = function (file) {
      df <- hot.to.df(input$importData)
      resultsDF <- data.frame(lapply(calcMPN(df), as.character), stringsAsFactors=FALSE)
      df <- rbind(df, rep("", length(df)))
      df <- rbind(df, c("RESULTS:", rep("", length(df)-1)))
      df <- rbind(df, names(resultsDF))
      df <- rbind(df, unlist(resultsDF[1,], use.names=FALSE), unlist(resultsDF[2,], use.names=FALSE), unlist(resultsDF[3,], use.names=FALSE))
      df <- rbind(df, rep("", length(df)))
      print(df)
      write.table(df, file, sep=";", dec=".", row.names = FALSE)
    }
  )
  #################### getMPN #######################
  ###################################################

  #######################################################
  #################### getK #############################

  ### 1st biological replicate ###
  # Data table
  output$getKDataA1 <- renderHotable({
    df<-data.frame(dil.=repDIL(as.numeric(input$startA1), input$dilA1, input$dilfactA1), rep.=rep(input$repA1, input$dilA1))
    data.frame(df, matrix("", ncol = input$dataA1, nrow = input$dilA1))
  }, readOnly = FALSE)
  # Time/Dose table
  output$getKTimeA1 <- renderHotable({
    data.frame("[Time/Dose]", matrix("", ncol = input$dataA1))
  }, readOnly = FALSE)

  ### 2nd biological replicate ###
  # Data table
  output$getKDataB1 <- renderHotable({
    df<-data.frame(dil.=repDIL(as.numeric(input$startB1), input$dilB1, input$dilfactB1), rep.=rep(input$repB1, input$dilB1))
    data.frame(df, matrix("", ncol = input$dataB1, nrow = input$dilB1))
  }, readOnly = FALSE)
  # Time/Dose table
  output$getKTimeB1 <- renderHotable({
    data.frame("[Time/Dose]", matrix("", ncol = input$dataB1))
  }, readOnly = FALSE)

  ### 3rd biological replicate ###
  # Data table
  output$getKDataC1 <- renderHotable({
    df<-data.frame(dil.=repDIL(as.numeric(input$startC1), input$dilC1, input$dilfactC1), rep.=rep(input$repC1, input$dilC1))
    data.frame(df, matrix("", ncol = input$dataC1, nrow = input$dilC1))
  }, readOnly = FALSE)
  # Time/Dose table
  output$getKTimeC1 <- renderHotable({
    data.frame("[Time/Dose]", matrix("", ncol = input$dataC1))
  }, readOnly = FALSE)

  # Observe
  observeEvent(input$clickK, {
    dataLIST <- list(hot.to.df(input$getKDataA1), hot.to.df(input$getKDataB1), hot.to.df(input$getKDataC1))
    timeLIST <- list(hot.to.df(input$getKTimeA1), hot.to.df(input$getKTimeB1), hot.to.df(input$getKTimeC1))

    # Results output: k value table separated
    output$getKDataA2 <- renderTable({
      calcK(dataLIST, timeLIST, replicate = 1)
    }, digits = 3, rownames = TRUE)
    output$getKDataB2 <- renderTable({
      calcK(dataLIST, timeLIST, replicate = 2)
    }, digits = 3, rownames = TRUE)
    output$getKDataC2 <- renderTable({
      calcK(dataLIST, timeLIST, replicate = 3)
    }, digits = 3, rownames = TRUE)
    # Result output: graph separated
    output$graph1 <- renderPlot({
      calcK(dataLIST, timeLIST, replicate = 1, graph = TRUE)
    })
    output$graph2 <- renderPlot({
      calcK(dataLIST, timeLIST, replicate = 2, graph = TRUE)
    })
    output$graph3 <- renderPlot({
      calcK(dataLIST, timeLIST, replicate = 3, graph = TRUE)
    })
    # All values computing
    output$getKData0 <- renderTable({
      calcK(dataLIST, timeLIST)
    }, digits = 3, rownames = TRUE)
    output$graph0 <- renderPlot({
      calcK(dataLIST, timeLIST, graph = TRUE)
    })
  })



  # Save Data



  #################### getK #############################
  #######################################################

  #######################################################
  ###################### Dose/Fluence ###################
  ## UV ##############
  output$QuantumY <- renderText({
    QY <- 0.75*(1 + 0.02*(input$UVtemp - 20.7))*(1 + 0.23*(input$UViodide - 0.557))
  })

  output$UVtimestable <- renderHotable({
    UVtimestable <- data.frame("NA", matrix("", ncol = input$UVtimesNB))
    colnames(UVtimestable)[1] <- "Time"
    UVtimestable
  }, readOnly = FALSE)
  output$UVODtable <- renderHotable({
    UVODtable <- data.frame("NA", matrix("", ncol = input$UVtimesNB))
    colnames(UVODtable)[1] <- "OD"
    UVODtable
  }, readOnly = FALSE)

  # Observe
  observeEvent(input$clickUV, {
    UVtimestable2 <- hot.to.df(input$UVtimestable)
    UVdosetable <- hot.to.df(input$UVODtable)
    UVdosetable2 <- hot.to.df(input$UVODtable)
    QY <- 0.75*(1 + 0.02*(input$UVtemp - 20.7))*(1 + 0.23*(input$UViodide - 0.557))
    w <-2
    while (w <= length(UVdosetable[1,])) {
      ODdiff <- as.numeric(as.character(UVdosetable[1,w])) - as.numeric(as.character(UVdosetable[1,2]))
      UVdosetable2[1, w] <- (((ODdiff/26400)*input$UVvol*(1/QY))*472000000)/input$UVcm2
      w = w+1
    }

    set_x <- as.numeric(as.vector(UVtimestable2[1,2:length(UVtimestable2)]))
    set_y <- as.numeric(as.vector(UVdosetable2[1,2:length(UVdosetable2)]))
    fitUV <- lm(set_y~set_x)
    intercept <- coef(fitUV)[1] #intercept
    slope <- coef(fitUV)[2] # slope
    R2 <- summary(fitUV)$r.squared # R2

    w <-2
    while (w <= length(UVtimestable2[1,])) {
      UVtimestable2[1, w] <- round(as.numeric(as.character(UVtimestable2[1, w]))*slope, digits = 3)
      w = w+1
    }

    output$resultUV <- renderHotable({
      resultsUV <- data.frame(UVtimestable2)
      colnames(resultsUV)[1] <- "Dose"
      resultsUV
    })

    output$UVplotresults <- renderPlot({
      plot(set_x, set_y, xlab = "Time", ylab = "[mJ/cm2]")
      abline(lm(fitUV),col="red")
      legend("bottomright", legend = c(paste("Intercept =", round(intercept, digits = 3)), paste("Slope =", round(slope, digits = 3)), paste("r2 =", round(R2, digits = 3))), bty = "n")
    })

    # Fluence value in W/m2 * UVfluence
    UVfluence <- slope*10/as.numeric(as.character(input$UVtimes))
    output$UVfluence <- renderText({
      UVfluence
    })
  })




  ## Sunlight ########
  plotplot2 <- function (a, b) {
    a
    b
  }
  output$sungraph1 <- renderPlot({
    plotplot2(par(mar=c(4, 4, 0, 1)), plot(sunlamp1$WL_nm, sunlamp1$Abs_Irradiance, type = "l", ylab = "Absolute Irradiance", xlab = "Wavelength [nm]"))
  })
  output$sungraph2 <- renderPlot({
    plotplot2(par(mar=c(4, 4, 0, 1)), plot(sunlamp2$WL_nm, sunlamp2$Abs_Irradiance, type = "l", ylab = "Absolute Irradiance", xlab = "Wavelength [nm]"))
  })

  # Observe
  observe({
    if (input$sundata == "new lamp") {
      output$sunfluence <- renderText({
        ((sum(sunlamp2$corr2[sunlamp2$WL_nm >= input$sunslider[1] & sunlamp2$WL_nm <= input$sunslider[2]]))/100)
      })
    } else {
      if (input$sundata == "old lamp") {
        output$sunfluence <- renderText({
          ((sum(sunlamp1$corr2[sunlamp1$WL_nm >= input$sunslider[1] & sunlamp1$WL_nm <= input$sunslider[2]]))/100)
        })
      }
    }
  })

  # Table for fluence/dose calculation
  output$suntimestable <- renderHotable({
    suntimestable3 <- data.frame("NA", matrix("", ncol = input$suntimesNB))
    colnames(suntimestable3)[1] <- "Time"
    suntimestable3
  }, readOnly = FALSE)

  observeEvent(input$clickSUN, {
    if (input$sundata == "new lamp") {
      Fluence <- ((sum(sunlamp2$corr2[sunlamp2$WL_nm >= input$sunslider[1] & sunlamp2$WL_nm <= input$sunslider[2]]))/100)
      suntimestable2 <- hot.to.df(input$suntimestable)
      colnames(suntimestable2)[1] <- "Dose"
      w <-2
      while (w <= length(suntimestable2[1,])) {
        suntimestable2[1, w] <- round(((as.numeric(as.character(suntimestable2[1,w]))*Fluence)/10)*as.numeric(as.character(input$suntimes)), digits = 3)
        w = w+1
      }
      output$resultsun <- renderHotable({
        data.frame(suntimestable2)
      })
    } else {
      if (input$sundata == "old lamp") {
        Fluence <- ((sum(sunlamp1$corr2[sunlamp1$WL_nm >= input$sunslider[1] & sunlamp1$WL_nm <= input$sunslider[2]]))/100)
        suntimestable2 <- hot.to.df(input$suntimestable)
        colnames(suntimestable2)[1] <- "Dose"
        w <-2
        while (w <= length(suntimestable2[1,])) {
          suntimestable2[1, w] <- round(((as.numeric(as.character(suntimestable2[1,w]))*Fluence)/10)*as.numeric(as.character(input$suntimes)), digits = 3)
          w = w+1
        }
        output$resultsun <- renderHotable({
          data.frame(suntimestable2)
        })
      }
    }
  })

  ###################### Dose/Fluence ###################
  #######################################################

  #######################################################
  ##################### Virus Database ##################

  ## Database ########
  output$viruses <- renderDataTable(
    Virus_CT_list2
    #read.csv2("./virus/data/Virus_CT_list2.csv", header = TRUE)
  )
  output$saveDatabase <- downloadHandler(
    filename = function () {
      paste("data", ".csv", sep="")
    },
    content <- function(file) {
      write.table(Virus_CT_list2, file, sep=";", dec=".", row.names = FALSE)
    }
  )

  ## Graphs Display ######## "./virus/data/virus_CT_list2.csv"
  #df_virus<-data.frame(read.csv2("./virus/data/Virus_CT_list2.csv", header = TRUE, dec = "."))
  df_virus<-Virus_CT_list2
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



