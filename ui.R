suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(googleVis))
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
                    #includeMarkdown("instructions.md")
                    includeHTML("instructions.html")
                    
                    ),
            tabItem(tabName="app",
                fluidRow(
                    column(width=8,
                           tabBox(
                               id = "tabset1",width=NULL,
                                tabPanel('Plots 1',uiOutput('plots1')),
                                tabPanel('Numeric summary 1',
                                         h4("Data fitted"),
                                         htmlOutput('tafla1'),
                                         h4("Fit for unobserved stages"),
                                         htmlOutput('fitrctafla1'),
                                         h4("95% posterior predictive lower for unobserved stages"),
                                         htmlOutput('lowerrctafla1'),
                                         h4("95% posterior predictive upper for unobserved stages"),
                                         htmlOutput("upperrctafla1"),
                                         textOutput('hakk')
                                ),
                                tabPanel('Plots 2',uiOutput('plots2')),
                                tabPanel('Numeric summary 2',
                                         h4("Data fitted"),
                                         htmlOutput('tafla2'),
                                         h4("Fit for unobserved stages"),
                                         htmlOutput('fitrctafla2'),
                                         h4("95% posterior predictive lower for unobserved stages"),
                                         htmlOutput('lowerrctafla2'),
                                         h4("95% posterior predictive upper for unobserved stages"),
                                         htmlOutput('upperrctafla2')
                                         
                                )
                            ),
                          
            
                            tagList(
                                tags$head(
                                    tags$link(rel="stylesheet", type="text/css",href="style.css"),
                                    tags$script(type="text/javascript", src = "busy.js")
                                )
                            ),
                            br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                            div(class = "busy",
                                img(src="progress.GIF")
                            )
                        
                        
                    ),
                    column(width=4,
                        box(status="primary", width = NULL,
                            #background="light-blue",
                            title = "Controls",
                             tags$a(href = 'V316.txt', class = "btn", icon("download"), 'Download test file'),
                            br(),
                            selectInput("select", label = "Choose Country", choices = list("Iceland" = 'Iceland'), 
                                        selected = 'Iceland'),
                            textInput("name","Name of River"),
                            fileInput('file', 'Choose File'),
                            checkboxGroupInput("checkbox", label = "Output",
                                                choices=list("Real scale"="raun","Logarithmic scale"="log",
                                                "Real scale residuals"="leifraun","Standardized residuals"="leiflog") ,selected = "raun"),
                            checkboxInput("checkboxA", label="Advanced Settings", value=FALSE),
                            conditionalPanel(condition="input.checkboxA == true", 
                                             radioButtons('clickopts',label='Use click to:',choices=list('Zoom'='zoom','Add dummypoint'='dummy','Add forcepoint'='force','Exclude point'='exclude'),selected='zoom'),
                                             sliderInput("slider", label = "Date Range", min = 1950, max = as.numeric(format(Sys.Date(), "%Y")), 
                                                        value=c(1950,as.numeric(format(Sys.Date(), "%Y"))),sep=""),
                                             checkboxInput("checkboxY", label="Exclude years from a certain period", value=FALSE),
                                             conditionalPanel(condition="input.checkboxY == true", 
                                             dateRangeInput("dates", label = "Date Range",start=Sys.Date()-1,end=Sys.Date()-1)),
                                             textInput("Wmax",label="Maximum Stage (m)")
                                             
                            ),
                            checkboxGroupInput("checkbox2", label = "Models",choices=list("Model1"='mdl1', "Model2"='mdl2'),selected="mdl1", inline=TRUE),
                            actionButton('reset',label='Reset'),
                            actionButton("go", label="Submit"),
                            br(),br(),br(),
                            downloadButton('downloadReport',label="Download Report"),
                            br(),br(),
                            downloadButton('downloadImages',label='Download Images'),
                            br(),br(),
                            downloadButton('xlsxexport',label='Export Tables as xlsx')
                        )
                    )
                )
            ),
            
            tabItem(tabName="theory",
                    #includeMarkdown("Theory.md")
                    includeHTML("Theory.html")
            ),
             #       includeHTML("Theory.html")),
            tabItem(tabName="code",
                    # includeMarkdown("Code.md")
                    includeHTML("Code.html")
                    
            )
        )    
    )
)



