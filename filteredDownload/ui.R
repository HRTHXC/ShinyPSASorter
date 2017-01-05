#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

fluidPage(
  title = 'DataTables Information',
  h1('ShinyPSASorter'),
  h6('If you don;t have a data file yet, plaese go to here (Family Consolidation):'),
  tags$a(href="http://shiny-dev.powerplant.pfr.co.nz/PsaBlockSummary/", "Click here!"),
  fluidRow(
    column(6, DT::dataTableOutput('x1'))
  ),
  fluidRow(
    p(class = 'text-center', downloadButton('x4', 'Download Data (Parents)'))
  ),
  fluidRow(
    column(6, DT::dataTableOutput('x2'))
  ),
  fluidRow(
    p(class = 'text-center', downloadButton('x5', 'Download Data (Mothers)'))
  ),
  fluidRow(
    column(6, DT::dataTableOutput('x3'))
  ),
  fluidRow(
    p(class = 'text-center', downloadButton('x6', 'Download Data (Fathers)'))
  )
)