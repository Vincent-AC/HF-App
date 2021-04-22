tab1Comp <-  fluidPage(tabsetPanel(
  id = "tabset1comp",
  type = "pills",
  tabPanel(
    "HF-Setup",
    fluidRow(
      box(
        title = "Pump characteristics",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 3,
        numericInput(
          "minPumpFlow1Comp",
          "Minimum Pump Flow (mL/min)",
          0.4,
          min = 0,
          step = 0.1
        ),
        numericInput(
          "maxPumpFlow1Comp",
          "Maximum Pump Flow (mL/min)",
          24.3,
          min = 0,
          step = 0.1
        ),
        numericInput(
          "stepPumpFlow1Comp",
          "Step of pump (mL/min)",
          0.1,
          min = 0,
          step = 0.1
        )
      ),
      box(
        title = "Experimental system characteristics",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 3,

        numericInput(
          "Vcentral1Comp",
          "Central volume (mL)",
          200,
          min = 0,
          step = 0.01
        ),
        numericInput(
          "Vcartridge1Comp",
          "Cartridge volume (mL)",
          50,
          min = 0,
          step = 0.01
        ),
        checkboxInput("constantVolume1Comp",
                      "Is volume constant ?",
                      value = T)
      ),
      box(
        title = "Drug charateristics",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 3,
        textInput("drugName1Comp", "Name of the drug", "Drug"),
        htmlOutput("uiHalfLife1Comp"),
        numericInput(
          "initial_concentration1Comp",
          "Peak concentration after 1st dose (mg/L)",
          2
          ,
          min = 0,
          step = 0.01
        ),
        numericInput(
          "lastTimePointhours1Comp",
          "Experiment duration (hours)",
          24,
          min = 0,
          step = 0.01
        )
      ),
      box(
        title = "Dosing characteristics",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 3,
        radioButtons(
          "admtype1Comp",
          "Type of injection",
          choices = c("Bolus", "Infusion"),
          selected = "Bolus"
        ),
        conditionalPanel(
          condition = "input.admtype1Comp== 'Bolus'",

          numericInput(
            "VinjectBolus1Comp",
            "Bolus volume (mL)",
            10,
            min = 0,
            step = 0.01
          ),
          numericInput(
            "dosingIntervalHoursBolus1Comp",
            "Dosing interval (hours)",
            12,
            min = 0,
            step = 0.01
          ),
          numericInput(
            "numberOfDosesBolus1Comp",
            "Total number of doses",
            2,
            min = 1,
            step = 1
          )
        ),
        conditionalPanel(
          condition = "input.admtype1Comp== 'Infusion'",

          numericInput(
            "tinfuseHours1Comp",
            "Infusion duration (hours)",
            1,
            min = 0,
            step = 0.01
          ),
          numericInput(
            "VinjectInf1Comp",
            "Infusion volume (mL)",
            10,
            min = 0,
            step = 0.01
          ),
          numericInput(
            "dosingIntervalHoursInf1Comp",
            "Dosing interval (hours)",
            12,
            min = 0,
            step = 0.01
          ),
          numericInput(
            "numberOfDosesInf1Comp",
            "Total number of doses",
            2,
            min = 1,
            step = 1
          )
        )
      )
    ),
    fluidRow(
      column(
        12,
        align = "center",
        style = "padding-bottom:10px",
        actionButton("simulateButton1Comp", "Simulate HF", style = 'font-size:150%')
      )
    ),
    fluidRow(
      box(
        title = "Parameters",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 4,
        div(style = 'overflow-x: scroll', uiOutput("HFParameters1Comp")),
        downloadButton('downloadtable', 'Download table')
      ),
      box(
        title = "Diagram",
        status = "primary",
        width = 8,
        solidHeader = TRUE,
        collapsible = TRUE,
        plotlyOutput("HFDiagram1Comp", height =
                     "800px")
      )
    )
  ),
  #tabPanel
  tabPanel("Simulation plotting",
           fluidRow(
             box(
               title = "SimuPK",
               status = "primary",
               width = 12,
               solidHeader = TRUE,
               collapsible = TRUE,
               plotlyOutput("simuPK1Comp")
             ),
             fileInput("datafile", "Add data to graph (.csv)",
                       multiple = FALSE, accept = c("csv")),
             uiOutput("ycol"),
             uiOutput("xcol")
           )),
  tabPanel("Simulation data",
           fluidRow(
             box(
               title = "Download data",
               status = "primary",
               width = 12,
               solidHeader = TRUE,
               collapsible = TRUE,
               downloadButton('downloadData1Comp', 'Download simulated data')
             )
           ))
))