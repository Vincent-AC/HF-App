columnChooserUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("controls"))
}

columnChooserServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session, datafile = reactive(NULL)) {
      
      output$xcol <- renderUI({
        df <- datafile()
        validate(need(!is.null(df), "Please select a data set"))
        items = names(df)
        names(items) = items
        selectInput(session$ns("x"), "x variable:", items, selected = items[1])
        
      })
      
      output$ycol <- renderUI({
        df <- datafile()
        validate(need(!is.null(df), "Please select a data set"))
        items = names(df)
        names(items) = items
        selectInput(session$ns("y"), "y variable:", items, selected = items[2])
      })
      
      output$groupcol2comp <- renderUI({
        df <- datafile()
        validate(need(!is.null(df), "Please select a data set"))
        items = names(df)
        names(items) = items
        selectInput(session$ns("DrugColumn"), "Drug variable:", items, selected = items[3])
        
      })
      
      output$DrugAvalue <- renderUI({
        df <- datafile()
        validate(need(!is.null(df), "Please select a data set"))
        items = unique(df[, input$DrugColumn])
        names(items) = items
        selectInput(session$ns("drugAvaluegraph"), "Drug A value:", items, selected = items[1])
        
      })
      
      output$DrugBvalue <- renderUI({
        df <- df <- datafile()
        validate(need(!is.null(df), "Please select a data set"))
        items = unique(df[, input$DrugColumn])
        names(items) = items
        selectInput(session$ns("drugBvaluegraph"), "Drug B value:", items, selected = items[2])
        
      })
    }
  )
}