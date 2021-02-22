#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(dplyr)
library(shinydashboard)
library(plotly)
source("functions/uiTab1Comp.R")
source("functions/uiTab2Comp.R")
dashboardPage(
        title = "HF-App",
        dashboardHeader(title = "HF-App"),
        dashboardSidebar(sidebarMenu(
                menuItem("1 compartment Hollow Fibre",
                         tabName = "1comp"),
                menuItem("2 compartment Hollow Fibre",
                         tabName = "2comp")
        )),
        dashboardBody(tabItems(
                tabItem(tabName = "1comp",
                        tab1Comp
),
                tabItem(tabName = "2comp",
tab2Comp)
        ))
)
