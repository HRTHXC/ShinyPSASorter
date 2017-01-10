library(shiny)
## User interface
ui <- dashboardPage(skin=skin_colour,
                    dashboardHeader(title = "ShinyPSASorter"),
                    dashboardSidebar(
                      sidebarMenu(
                        ## ---- Add Form fields here ---- #
                        hr(),
                        fluidRow(
                          actionButton('contents', "Run script"),
                          column(4, h6(" "))
                          
                        ),
                        hr(),
                        ## ------------------------------ #

                        ## Dynamic side bar menu
                        sidebarMenuOutput("menu")
                      )
                    ),
                    dashboardBody(
                      tabItems(

                        # Fourth tab content
                        tabItem(tabName = "specs",
                                fluidRow(
                                  box(title="Shiny resources", status = "primary", width=6, solidHeader = TRUE,
                                      collapsible = TRUE,
                                      a("Shiny website", href="http://shiny.rstudio.com/"), br(),
                                      a("Cheatsheet", href="http://shiny.rstudio.com/images/shiny-cheatsheet.pdf"), br(),
                                      a("Shiny examples", href="https://github.com/rstudio/shiny-examples")
                                  )
                                )
                        ),

                        # Fifth tab content
                        tabItem(tabName = "session",
                                box(title = "reactivevalues input$... variables", status = "primary", width=6, solidHeader = TRUE,
                                    collapsible = TRUE, tableOutput("input_variables")),
                                box(title = "sessionInfo()", status = "primary", width=12, solidHeader = TRUE,
                                    collapsible = TRUE, verbatimTextOutput("sessionInfo"))
                        )
                      ),
                      
                      selectInput("dataset", "Choose a dataset (saves to Downloads folder):", 
                                  choices = c("parents", "mothers", "fathers")),
                      downloadButton('downloadData', 'Download full'),
                      
                      mainPanel(
                        #panel clips outside of lighter box designated for it?
                        dataTableOutput('contents'),
                        tabsetPanel(
                          tabPanel('parents',
                                   dataTableOutput("testParents")),
                          tabPanel('mothers',
                                   dataTableOutput("testMothers")),
                          tabPanel('fathers',
                                   dataTableOutput("testFathers"))
                        )
                      ))#,
                      
                      #fluidRow(  downloadButton('downloadSection', 'Download section')))
                      
                    )