#' input_dosing_charac UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_input_dosing_charac_1comp_abs_ui <- function(id){
  ns <- NS(id)
  tagList(
    box(
      title = "Dosing characteristics",
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      width = 3,
      numericInput(
        ns("nInfusions"),
        "Number of infusion steps desired",
        min = 1,
        step = 1,
        value = 12
      ),
      numericInput(
        ns("dosingIntervalHoursAbs"),
        "Dosing interval (hours)",
        12,
        min = 0,
        step = 0.01
      ),
      numericInput(
        ns("nDoses"),
        "Total number of doses",
        1,
        min = 1,
        step = 1
      ),
      numericInput(
        ns("CinfusionMin"),
        "Minimal concentration in the infusion bag that can be prepared (mg/L)",
        1,
        min = 0,
        step = 1
      ),
      numericInput(
        ns("CinfusionMax"),
        "Maximal concentration in the infusion bag that can be prepared (mg/L)",
        1000,
        min = 0,
        step = 1
      ),
      numericInput(
        ns("CinfusionStep"),
        "Step of infusion bag concentrations that can be prepared (mg/L)",
        1,
        min = 0,
        step = 0.1
      )
    )
  )
}

#' input_dosing_charac Server Functions
#'
#' @return list with the following components
#' \describe{
#'  \item{nInfusions} reactive number of infusions used for each dosing interval
#'  \item{dosingIntervalHoursAbs} reactive number indicating the duration of the dosing interval in hours
#'  }
#' @noRd
mod_input_dosing_charac_1comp_abs_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    return(list(
      nInfusions = reactive({
        input$nInfusions
      }),
      dosingIntervalHoursAbs = reactive({
        input$dosingIntervalHoursAbs
      }),
      nDoses = reactive({
        input$nDoses
      }),
      CinfusionMin = reactive({
        input$CinfusionMin
      }),
      CinfusionMax = reactive({
        input$CinfusionMax
      }),
      CinfusionStep = reactive({
        input$CinfusionStep
      })
    ))

  })
}

## To be copied in the UI
# mod_input_dosing_charac_1comp_abs_ui("input_dosing_charac_1")

## To be copied in the server
# mod_input_dosing_charac_1comp_abs_server("input_dosing_charac_1")
