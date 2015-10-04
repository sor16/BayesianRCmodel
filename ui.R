suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(googleVis))
dashboardPage(
    dashboardHeader(title='Bayesian Rating Curve'),
    dashboardSidebar(
        sidebarMenu(
        menuItem("Instructions", tabName = "instructions", icon = icon("info-circle")),
        menuItem("App", icon = icon("line-chart"), tabName = "app"),
        menuItem("Theory", tabName = "theory", icon = icon("book")),
        menuItem("Code", tabName = "code", icon = icon("code"))
        )
    ),
    dashboardBody(
       
        tabItems(
            tabItem(tabName="instructions",
                    h1("Instructions"),
                    tags$a(href="https://www.youtube.com/watch?v=NOyQywTcXaQ",
                      target="_blank",tags$img(src="instructions.png",height="356px",width="640px"))
                    ),
            tabItem(tabName="app",
                fluidRow(
                    column(width=8,
                           tabBox(
                               id = "tabset1",width=NULL,
                                tabPanel('Plots Model 1',uiOutput('plots1')),
                                tabPanel('Tables Model 1',
                                         h4("Data fitted"),
                                         htmlOutput('TableOfData1'),
                                         h4("Fit for unobserved stages"),
                                         htmlOutput('FitTable1'),
                                         h4("95% posterior predictive lower for unobserved stages"),
                                         htmlOutput('LowerTable1'),
                                         h4("95% posterior predictive upper for unobserved stages"),
                                         htmlOutput("UpperTable1")
                                         
                                        
                                ),
                                tabPanel('Plots Model 2',uiOutput('plots2')),
                                tabPanel('Tables Model 2',
                                         h4("Data fitted"),
                                         htmlOutput('TableOfData2'),
                                         h4("Fit for unobserved stages"),
                                         htmlOutput('FitTable2'),
                                         h4("95% posterior predictive lower for unobserved stages"),
                                         htmlOutput('LowerTable2'),
                                         h4("95% posterior predictive upper for unobserved stages"),
                                         htmlOutput('UpperTable2')
                                         
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
                            tags$a(href = 'V316.txt', target="_blank",class = "btn", icon("download"), 'Download txt test file'),
                            br(),
                            tags$a(href = 'exceldata.xlsx', class = "btn", icon("download"), 'Download xlsx test file'),
                            br(),
                            br(),
                            selectInput("select", label = "Choose Country", choices = list("Iceland" = 'Iceland'), 
                                        selected = 'Iceland'),
                            textInput("name","Name of River"),
                            fileInput('file', 'Choose File'),
                            checkboxGroupInput("checkbox", label = "Output",
                                                choices=list("Real scale"="raun","Logarithmic scale"="log",
                                                "Real scale residuals"="leifraun","Standardized residuals"="leiflog") ,selected = "raun"),
                            checkboxInput("advanced", label="Advanced Settings", value=FALSE),
                            conditionalPanel(condition="input.advanced == true", 
                                             radioButtons('clickopts',label='Use click to:',choices=list('Zoom'='zoom','Add dummypoint'='dummy','Add forcepoint'='force','Exclude point'='exclude'),selected='zoom'),
                                             sliderInput("includeDates", label = "Date Range", min = 1950, max = as.numeric(format(Sys.Date(), "%Y")), 
                                                        value=c(1950,as.numeric(format(Sys.Date(), "%Y"))),sep=""),
                                             checkboxInput("exclude", label="Exclude years from a certain period", value=FALSE),
                                             conditionalPanel(condition="input.exclude == true", 
                                             dateRangeInput("excludeDates", label = "Date Range",start=Sys.Date()-1,end=Sys.Date()-1)),
                                             textInput("Wmax",label="Maximum Stage (m)")
                                             
                            ),
                            checkboxGroupInput("checkbox2", label = "Models",choices=list("Model1"='mdl1', "Model2"='mdl2'),selected="mdl1", inline=TRUE),
                            actionButton('reset',label='Reset'),
                            actionButton("go", label="Submit"),
#                             actionButton("back", label="Back"),
#                             actionButton("forward", label="Forward"),
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
                    includeMarkdown("Theory-2.md")
                
            ),
            tabItem(tabName="code",
                    includeMarkdown("Code.md")
            )
        )    
    )
)



