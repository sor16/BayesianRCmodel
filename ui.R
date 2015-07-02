library(shinydashboard)

dashboardPage(
    dashboardHeader(title='Bayesian Rating Curve'),
    dashboardSidebar(
        sidebarMenu(
        menuItem("Instructions", tabName = "instructions", icon = icon("info-circle")),
        menuItem("App", icon = icon("line-chart"), tabName = "app",
                 badgeLabel = "new", badgeColor = "green"),
        menuItem("Theory", tabName = "theory", icon = icon("book")),
        menuItem("Code", tabName = "code", icon = icon("code"))
        )
    ),
    dashboardBody(
       
        tabItems(
            tabItem(tabName="instructions",
                    includeMarkdown("instructions.md")
                    
                    ),
            tabItem(tabName="app",
                fluidRow(
                    column(width=8,
                           tabBox(
                              # title = tagList(shiny::icon("gear"), "tabBox status"),
                               # The id lets us use input$tabset1 on the server to find the current tab
                               id = "tabset1",width=NULL,
                        
                          #  textOutput('callreactive'),
            
                                
                                tabPanel('Plots',uiOutput('plots')),
                                tabPanel('Numeric summary', uiOutput('tafla')),
                                tabPanel('Plots2',  uiOutput('plots2')),
                                tabPanel('Numeric summary 2', uiOutput('tafla2'))
                           ),
                          
            
                            tagList(
                                tags$head(
                                    tags$link(rel="stylesheet", type="text/css",href="style.css"),
                                    tags$script(type="text/javascript", src = "busy.js")
                                )
                            ),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            div(class = "busy",
                                img(src="progress.GIF")
                            )
                        
                        
                    ),
                    column(width=4,
                        box(status="primary", width = NULL,
                            #background="light-blue",
                            title = "Controls",
                            selectInput("select", label = "Choose country", choices = list("Iceland" = 'Iceland'), 
                                        selected = 'Iceland'),
                            textInput("name","Name of river"),
                            fileInput('file1', 'Choose file'),
                            checkboxGroupInput("checkbox", label = "Output",
                                                choices=list("Real scale"="raun","Logarithmic scale"="log",
                                                "Real scale residuals"="leifraun","Standardized residuals"="leiflog") ,selected = NULL),
                            checkboxInput("checkboxA", label="Advanced settings", value=FALSE),
                            conditionalPanel(condition="input.checkboxA == true",   sliderInput("slider", label = "Date Range", min = 1950, max = as.numeric(format(Sys.Date(), "%Y")), 
                                                                                                value=c(1950,as.numeric(format(Sys.Date(), "%Y"))))),
                            checkboxGroupInput("checkbox2", label = "Models",choices=list("Model1"='mdl1', "Model2"='mdl2'), inline=TRUE),
                            actionButton("go", label="Submit"),
                            br(),br(),br(),
                            downloadButton('downloadReport',label="Download as PDF")
                        )
                    )
                )
            ),
            
            tabItem(tabName="theory",
                    includeMarkdown("Theory.md")
            ),
             #       includeHTML("Theory.html")),
            tabItem(tabName="code",
                    includeMarkdown("Code.md")
            )
        )    
    )
)



