#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plyr)
library(dplyr)
library(flextable)
library(officer)
library(mrgsolve)
library(plotly)
library(RCurl)
library(readr)
library(shinydashboard)
library(png)

shinyServer(function(input, output, session) {
  values <-
    reactiveValues(maindata = NULL,     # the data frame used for plotting results versus simulations
                   data2comp = NULL)
  observeEvent(input$datafile, {
    file <- input$datafile$datapath
    values$maindata <- read.csv(file, na.strings = c("NA", "."))
  })

  observeEvent(input$datafile2comp, {
    file <- input$datafile2comp$datapath
    values$data2comp <- read.csv(file, na.strings = c("NA", "."))
  })

  output$ycol <- renderUI({
    df <- values$maindata
    validate(need(!is.null(df), "Please select a data set"))
    items = names(df)
    names(items) = items
    selectInput("y", "y variable:", items, selected = items[1])
  })

  output$xcol <- renderUI({
    df <- values$maindata
    validate(need(!is.null(df), "Please select a data set"))
    items = names(df)
    names(items) = items
    selectInput("x", "x variable:", items, selected = items[2])

  })

  output$ycol2comp <- renderUI({
    df <- values$data2comp
    validate(need(!is.null(df), "Please select a data set"))
    items = names(df)
    names(items) = items
    selectInput("y2comp", "y variable:", items, selected = items[1])
  })

  output$xcol2comp <- renderUI({
    df <- values$data2comp
    validate(need(!is.null(df), "Please select a data set"))
    items = names(df)
    names(items) = items
    selectInput("x2comp", "x variable:", items, selected = items[2])

  })

  output$groupcol2comp <- renderUI({
    df <- values$data2comp
    validate(need(!is.null(df), "Please select a data set"))
    items = names(df)
    names(items) = items
    selectInput("DrugColumn", "Drug variable:", items, selected = items[3])

  })

  output$DrugAvalue <- renderUI({
    df <- values$data2comp
    validate(need(!is.null(df), "Please select a data set"))
    items = unique(df[, input$DrugColumn])
    names(items) = items
    selectInput("drugAvaluegraph", "Drug A value:", items, selected = items[1])

  })

  output$DrugBvalue <- renderUI({
    df <- values$data2comp
    validate(need(!is.null(df), "Please select a data set"))
    items = unique(df[, input$DrugColumn])
    names(items) = items
    selectInput("drugBvaluegraph", "Drug B value:", items, selected = items[2])

  })

  output$uiHalfLife1Comp <- renderUI({
    selectInput("halfLifehours1Comp",
                "Half life of drug (hours)",
                possibleHalfLifeValues1Comp())
  })
  output$uiHalfLifeA2Comp <- renderUI({
    selectInput("halfLifehours_A",
                "Half life of drug A (hours)",
                possibleHalfLifeValues2Comp())
  })
  output$uiHalfLifeB2Comp <- renderUI({
    selectInput("halfLifehours_B",
                "Half life of drug B (hours)",
                possibleHalfLifeValues2Comp())
  })

  possibleHalfLifeValues1Comp <- reactive({
    calculatePossibleHalfLives(
      input$minPumpFlow1Comp,
      input$maxPumpFlow1Comp,
      input$stepPumpFlow1Comp,
      input$Vcentral1Comp,
      input$Vcartridge1Comp,
      roundingDigits = 2
    )
  })

  possibleHalfLifeValues2Comp <- reactive({
    calculatePossibleHalfLives(
      input$minPumpFlow2Comp,
      input$maxPumpFlow2Comp,
      input$stepPumpFlow2Comp,
      input$Vcentral2Comp,
      input$Vcartridge2Comp,
      roundingDigits = 2
    )
  })

  model1Comp <- mread_cache("models/HollowFiber1Comp-Vadd.cpp")

  HF1CompAll <- reactive ({
    if (is.null(input$simulateButton1Comp))
      return()
    input$simulateButton1Comp
    isolate(
      HF1Comp(
        model = model1Comp,
        halfLifeHours = as.numeric(input$halfLifehours1Comp),
        Vcentral = input$Vcentral1Comp,
        Vcartridge = input$Vcartridge1Comp,
        initial_concentration = input$initial_concentration1Comp,
        lastTimePointHours = input$lastTimePointhours1Comp,
        drugName = input$drugName1Comp,
        VinjectBolus =  input$VinjectBolus1Comp,
        dosingIntervalHoursBolus = input$dosingIntervalHoursBolus1Comp,
        numberOfDosesBolus = input$numberOfDosesBolus1Comp,
        tinfuseHours = input$tinfuseHours1Comp,
        VinjectInf =  input$VinjectInf1Comp,
        dosingIntervalHoursInf =  input$dosingIntervalHoursInf1Comp,
        numberOfDosesInf = input$numberOfDosesInf1Comp,
        adm.type = input$admtype1Comp,
        constantVolume = input$constantVolume1Comp,
        Css = input$css1comp
      )
    )
  })


  PK1CompSimuPlot <- reactive({
    df <- values$maindata
    ifelse(is.null(df),
           return(HF1CompAll()[[2]]),
           {
             df <- df %>%
               type.convert()
             return(
               HF1CompAll()[[2]] %>%
                 add_trace(
                   data = df,
                   type = 'scatter',
                   mode = 'markers',
                   x = ~ get(input$x),
                   y = ~ get(input$y),
                   name = input$drugName1Comp,
                   hovertemplate = paste(
                     '<b>Concentration</b>: %{y:.2f} mg/L',
                     '<br><b>Time</b>: %{x:.2f} h'
                   ),
                   color = I("red")
                 )
             )
           })
  })

  output$simuPK1Comp <- renderPlotly({
    PK1CompSimuPlot()%>%
      config(toImageButtonOptions=list(format='svg'))
  })



  flexTable1Comp <- reactive({
    HF1CompAll()[[1]][, c("Group", "Parameter", "Value")] %>%
      as_grouped_data(x = ., groups = c("Group")) %>%
      as_flextable() %>%
      flextable::compose(
        i = ~ !is.na(Group),
        # when Group not NA
        j = "Parameter",
        # on column "parameter"
        # create a paragraph containing a chunk containing value of `var_group`
        value = as_paragraph(as_chunk(Group))
      ) %>%
      bold(
        j = 1,
        i = ~ !is.na(Group),
        bold = TRUE,
        part = "body"
      ) %>%
      bold(part = "header", bold = TRUE) %>%
      autofit()
  })

  observeEvent(input$simulateButton1Comp,
               output$HFParameters1Comp <- renderUI({
                 isolate(flexTable1Comp() %>%
                           htmltools_value())
               }))

  output$downloadtable <- downloadHandler(
    filename = function() {
      "example.docx"
    },
    content = function(file) {
      ft <- flexTable1Comp()
      doc <- read_docx()
      doc <- body_add_flextable(doc, value = ft)
      print(doc, target = file)
    }
  )

  output$downloadData1Comp <- downloadHandler(
    filename = function() {
      "simulated_data.csv"
    },
    content = function(file) {
      data <- HF1CompAll()[[4]]
      write_csv(data,file)
    }
  )

  observeEvent(input$simulateButton1Comp, {
    output$HFDiagram1Comp <- renderPlotly({
      image_file <- "images/1CompApp.png"
      txt <-
        RCurl::base64Encode(readBin(image_file, "raw", file.info(image_file)[1, "size"]), "txt")
      return(
        ggplotly(HF1CompAll()[[3]], height = 800) %>%
          layout(images = list(
            list(
              source = paste('data:image/png;base64', txt, sep = ','),
              xref = "x",
              yref = "y",
              x = 0,
              y = 0,
              sizex = 1700,
              sizey = 1264,
              opacity = 1,
              layer = "below",
              sizing = "stretch"
            )
          ),
          hovermode = FALSE) %>%
          config(toImageButtonOptions = list(format = 'svg'))
      )
    })
  })

  model2Comp <- mread_cache("models/HollowFiber2Comp-Vadd.cpp")
  HF2CompAll <- reactive({
    if (is.null(input$simulateButton2Comp))
      return()
    input$simulateButton2Comp
    isolate(
      HF2Comp(
        model = model2Comp,
        halfLifeHours_A = as.numeric(input$halfLifehours_A),
        halfLifeHours_B = as.numeric(input$halfLifehours_B),
        Vcentral = input$Vcentral2Comp,
        Vcartridge = input$Vcartridge2Comp,
        initial_concentration_A = input$initial_concentration_A,
        initial_concentration_B = input$initial_concentration_B,
        lastTimePointHours = input$lastTimePointhours2Comp,
        Vinject_ABolus = input$VinjectABolus ,
        Vinject_B_centralBolus = input$VinjectB_centralBolus,
        Vinject_B_extraBolus = input$VinjectB_extraBolus,
        dosingIntervalHours_ABolus = input$dosingIntervalHours_ABolus,
        dosingIntervalHours_BBolus = input$dosingIntervalHours_BBolus,
        numberOfDosesABolus = input$numberOfDosesABolus,
        numberOfDosesBBolus = input$numberOfDosesBBolus,
        drugNameA = input$drugNameA,
        drugNameB = input$drugNameB,
        adm.type = input$admtype2Comp,
        tinfuseHours_A = input$tinfuseHours_A,
        tinfuseHours_B = input$tinfuseHours_B,
        Vinject_AInf =  input$VinjectAInf,
        Vinject_B_centralInf = input$VinjectB_centralInf,
        Vinject_B_extraInf = input$VinjectB_extraInf,
        dosingIntervalHours_AInf = input$dosingIntervalHours_AInf,
        dosingIntervalHours_BInf = input$dosingIntervalHours_BInf,
        numberOfDosesAInf = input$numberOfDosesAInf,
        numberOfDosesBInf = input$numberOfDosesBInf,
        constantVolume = input$constantVolume2Comp
      )
    )
  })

  PK2CompSimuPlot <- reactive({
    df <- values$data2comp
    df[, input$DrugColumn] <- as.factor(df[, input$DrugColumn])
    ifelse(is.null(df),
           return(HF2CompAll()[[2]]),
           {
             dfA <-
               filter(df,!!sym(input$DrugColumn) == input$drugAvaluegraph) %>%
               type.convert()
             dfB <-
               filter(df,!!sym(input$DrugColumn) == input$drugBvaluegraph) %>%
               type.convert()

             return(
               HF2CompAll()[[2]] %>%
                 add_trace(
                   data = dfA,
                   type = 'scatter',
                   mode = 'markers',
                   x = ~ get(input$x2comp),
                   y = ~ get(input$y2comp),
                   name = input$drugNameA,
                   hovertemplate = paste(
                     '<b>Concentration</b>: %{y:.2f} mg/L',
                     '<br><b>Time</b>: %{x:.2f} h'
                   ),
                   color = I(c("red"))
                 ) %>%
                 add_trace(
                   data = dfB,
                   type = 'scatter',
                   mode = 'markers',
                   x = ~ get(input$x2comp),
                   y = ~ get(input$y2comp),
                   name = input$drugNameB,
                   hovertemplate = paste(
                     '<b>Concentration</b>: %{y:.2f} mg/L',
                     '<br><b>Time</b>: %{x:.2f} h'
                   ),
                   color = I(c("blue"))
                 )
             )
           })
  })

  output$simuPK2Comp <- renderPlotly({
    PK2CompSimuPlot()%>%
      config(toImageButtonOptions=list(format='svg'))
  })



  output$downloadData2Comp <- downloadHandler(
    filename = function() {
      "simulated_data.csv"
    },
    content = function(file) {
      data <- HF2CompAll()[[4]]
             write_csv(data,file)
    }
  )


  flexTable2Comp <- reactive({
    HF2CompAll()[[1]][, c("Group", "Parameter", "Value")] %>%
      as_grouped_data(x = ., groups = c("Group")) %>%
      as_flextable() %>%
      flextable::compose(
        i = ~ !is.na(Group),
        # when Group not NA
        j = "Parameter",
        # on column "parameter"
        # create a paragraph containing a chunk containing value of `var_group`
        value = as_paragraph(as_chunk(Group))
      ) %>%
      bold(
        j = 1,
        i = ~ !is.na(Group),
        bold = TRUE,
        part = "body"
      ) %>%
      bold(part = "header", bold = TRUE) %>%
      autofit()
  })

  observeEvent(input$simulateButton2Comp,
               output$HFParameters2Comp <- renderUI({
                 isolate(flexTable2Comp() %>%
                           htmltools_value())
               }))

  output$downloadtable2Comp <- downloadHandler(
    filename = function() {
      "example.docx"
    },
    content = function(file) {
      ft <- flexTable2Comp()
      doc <- read_docx()
      doc <- body_add_flextable(doc, value = ft)
      # fileout <- "test.docx" # uncomment to write in your working directory
      print(doc, target = file)
    }
  )
  observeEvent(input$simulateButton2Comp, {
    output$HFDiagram2Comp <- renderPlotly({
      image_file <- "images/2CompApp.png"
      txt <-
        RCurl::base64Encode(readBin(image_file, "raw", file.info(image_file)[1, "size"]), "txt")
      ggplotly(HF2CompAll()[[3]],
               height = 800) %>%
        layout(images = list(
          list(
            source = paste('data:image/png;base64', txt, sep = ','),
            xref = "x",
            yref = "y",
            x = 0,
            y = 0,
            sizex = 2655,
            sizey = 1972,
            opacity = 1,
            layer = "below",
            sizing = "stretch"
          )
        ),
        hovermode = FALSE) %>%
        config(toImageButtonOptions=list(format='svg'))
    })
  })

})
