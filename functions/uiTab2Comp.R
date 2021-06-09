tab2Comp <-
  fluidPage(tabsetPanel(
    id = "tabset2comp",
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
        "minPumpFlow2Comp",
        "Minimum Pump Flow (mL/min)",
        0.4,
        min = 0,
        step = 0.1
      ),
      numericInput(
        "maxPumpFlow2Comp",
        "Maximum Pump Flow (mL/min)",
        24.3,
        min = 0,
        step = 0.1
      ),
      numericInput(
        "stepPumpFlow2Comp",
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
        "Vcentral2Comp",
        "Central volume (mL)",
        200,
        min = 0,
        step = 0.01
      ),
      numericInput(
        "debit_central_cartridge2Comp",
        "Flow rate central to cartridge (mL/min)",
        120,
        min = 0,
        step = 0.01
      ),
      numericInput(
        "Vcartridge2Comp",
        "Cartridge volume (mL)",
        50,
        min = 0,
        step = 0.01
      ),
      checkboxInput("constantVolume2Comp",
                    "Is volume constant ?",
                    value = T)
    ),
    box(title = "Drug characteristics",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
        width = 3,
      textInput("drugNameA", "Name of drug A", "DrugA"),
      htmlOutput("uiHalfLifeA2Comp"),
      numericInput(
        "initial_concentration_A",
        "Peak concentration after 1st dose of drug A (mg/L)",
        2,
        min = 0,
        step = 0.01
      ),
      textInput("drugNameB", "Name of drug B", "DrugB"),
      htmlOutput("uiHalfLifeB2Comp"),
      numericInput(
        "initial_concentration_B",
        "Peak concentration after 1st dose of drug B (mg/L)",
        0.8,
        min = 0,
        step = 0.01
      ),
      numericInput(
        "lastTimePointhours2Comp",
        "Experiment duration (hours)",
        24,
        min = 0,
        step = 0.01
      )
      ),
    box(title = "Dosing characteristics",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 3,
      radioButtons(
        "admtype2Comp",
        "Type of injection",
        choices = c("Bolus", "Infusion"),
        selected = "Bolus"
      ),
      conditionalPanel(
        condition = "input.admtype2Comp== 'Bolus'",

        numericInput(
          "VinjectABolus",
          "Bolus volume of drug A into central (mL)",
          10,
          min = 0,
          step = 0.01
        ),
        numericInput(
          "dosingIntervalHours_ABolus",
          "Dosing interval for drug A (hours)",
          12,
          min = 0,
          step = 0.01
        ),
        numericInput(
          "numberOfDosesABolus",
          "Total number of doses of drug A",
          2,
          min = 1,
          step = 1
        ),
        numericInput(
          "VinjectB_centralBolus",
          "Bolus volume of drug B into central (mL)",
          10,
          min = 0,
          step = 0.01
        ),
        numericInput(
          "VinjectB_extraBolus",
          "Bolus volume of drug B into extra (mL)",
          10,
          min = 0,
          step = 0.01
        ),
        numericInput(
          "dosingIntervalHours_BBolus",
          "Dosing interval for drug B (hours)",
          12,
          min = 0,
          step = 0.01
        ),
        numericInput(
          "numberOfDosesBBolus",
          "Total number of doses of drug B",
          2,
          min = 1,
          step = 1
        )
      ),
      conditionalPanel(
        condition = "input.admtype2Comp== 'Infusion'",

        numericInput(
          "tinfuseHours_A",
          "Infusion duration of drug A in central compartment (hours)",
          1,
          min = 0,
          step = 0.01
        ),
        numericInput(
          "tinfuseHours_B",
          "Infusion duration of drug B in central and extra compartment(hours)",
          1,
          min = 0,
          step = 0.01
        ),
        numericInput(
          "VinjectAInf",
          "Infusion volume of drug A in central compartment (mL)",
          10,
          min = 0,
          step = 0.01
        ),
        numericInput(
          "VinjectB_centralInf",
          "Infusion volume of drug B in central compartment (mL)",
          10,
          min = 0,
          step = 0.01
        ),
        numericInput(
          "VinjectB_extraInf",
          "Infusion volume of drug B in extra compartment (mL)",
          10,
          min = 0,
          step = 0.01
        ),
        numericInput(
          "numberOfDosesAInf",
          "Total number of doses of drug A",
          2,
          min = 1,
          step = 1
        ),
        numericInput(
          "numberOfDosesBInf",
          "Total number of doses of drug B",
          2,
          min = 1,
          step = 1
        ),
        numericInput(
          "dosingIntervalHours_AInf",
          "Dosing interval for drug A (hours)",
          12,
          min = 0,
          step = 0.01
        ),
        numericInput(
          "dosingIntervalHours_BInf",
          "Dosing interval for drug B (hours)",
          12,
          min = 0,
          step = 0.01
        )
      )
    )),
    fluidRow(
      column(
        12,
        align = "center",
        style = "padding-bottom:10px",
        actionButton("simulateButton2Comp", "Simulate HF", style = 'font-size:150%')
      )
    ),
fluidRow(
    box(
      title = "Parameters",
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      width = 4,
      div(style = 'overflow-x: scroll', uiOutput("HFParameters2Comp")),
      downloadButton('downloadtable2Comp', 'Download table')
    ),
    box(
      title = "Diagram",
      status = "primary",
      width = 8,
      solidHeader = TRUE,
      collapsible = TRUE,
      plotlyOutput("HFDiagram2Comp",
                   height ="800px")
    )
  )
),
  tabPanel("Simulation plotting",
  fluidRow(
    box(
      title = "SimuPK",
      status = "primary",
      width = 12,
      solidHeader = TRUE,
      collapsible = TRUE,
      plotlyOutput("simuPK2Comp")
    ),
    fileInput("datafile2comp", "Add data to graph (.csv)",
              multiple = FALSE, accept = c("csv")),
    uiOutput("ycol2comp"),
    uiOutput("xcol2comp"),
    uiOutput("groupcol2comp"),
    uiOutput("DrugAvalue"),
    uiOutput("DrugBvalue")
  )),
tabPanel("Simulation data",
         fluidRow(
           box(
             title = "Download data",
             status = "primary",
             width = 12,
             solidHeader = TRUE,
             collapsible = TRUE,
             downloadButton('downloadData2Comp', 'Download simulated data')
           )
         ))
  ))
