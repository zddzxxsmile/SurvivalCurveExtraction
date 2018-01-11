### User interface for the Survival Curve Extraction Dashboard ###

## ui.R ##
library(shinydashboard)
library(DT)
library(shinyAce)
library(formattable)
dashboardPage(
  
  # Header
  dashboardHeader(title = "Survival Curve Extraction Dashboard"),
  
  
  # Sidebar Menus
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Extraction and Error Check", tabName = "extractData", icon = icon("th")),
      menuItem("Data Input and Summary", tabName = "data1", icon = icon("th"),
               collapsesible = 
                 menuSubItem('random1', tabName = 'random1'),
               menuSubItem('Data Input', tabName = 'data'),
               menuSubItem('Summary', tabName = 'datasum')
               ),
      menuItem('Curve Extraction', tabName = 'results', icon = icon('th'), 
                 collapsible = 
                 menuSubItem('random', tabName = 'random'),
               menuSubItem('Modeling Options', tabName = 'modelopts'),
               menuSubItem('Plots', tabName = 'plot'),
                   menuSubItem('Parameter Estimates', tabName = 'regresult'),
                   menuSubItem('Fit Statistics', tabName = 'diag')
        )
      
      
    )
  ),
  
  # Body of the Dashboard
  dashboardBody(
    tabItems(
      
      # First tab content DashBoard
      tabItem(tabName = "dashboard",
              
              h2("Introduction"),
              fluidRow(
                box(
                  h4('This is a dashboard for generating survival curve distribution parameters, fit statistic, and figures'),
                  h4("There are several important key points to remember"),
                  h4(strong("Number 1:"),"User needs to have 2 .csv files to input which is described in Guyot et al. 2012: Enhanced secondary analysis of survival data: reconstructing the data from published Kaplan-Meier survival curves"),
                  h4(strong("If Number 1 is not satisfied:"),"User needs to extract the survival curve data points by using a figure digitizer such as Webplotdigitizer (e.g. https://apps.automeris.io/wpd/) and extract the curve data and relevant risk and censoring information as described in Guyot et al."),
                  h4(strong("Number 2:"),"Before trying to determine a survival distribution that best fits the data, user needs to check that the extracted curve is as accurate as possible. Human error is highly possible to be introduced during the process of survival curve extraction thus this step is crucial in terms of preserving the integrity of the original study survival information"),
                  h4(strong("Number 3:"),"The outputs that are generated in the 'Result' tab (Bottom tab on the left) needs the user to be comfortable with the 'survival' package output. Additional understanding of packages such as 'flexsurv' and 'spline' will be useful as additional distributions may be specified using those packages"),
                  h4(strong("Below:"),"are some examples of the steps outlined above. If you have any questions regarding how to go about each step please consult Guyot et al. first and if still difficult to proceed e-mail: chong.kim@ucdenver.edu")  
                  
                ,width = 12)
              )
          
              ),
      
      # Second Tab content: Information for gathering csv files
    tabItem(tabName = "extractData",
             
             fluidRow(
               box(
                 h2("Extraction and Error Checking"),
                 
                 h4('If the required survival curve and number at risk csv files are not extracted beforehand, please follow the', a("Tutorial", href="https://github.com/ck2136/SurvivalCurveExtraction/blob/master/SurvivalExtractionforCEA.ipynb"),' so that the user can extract the curve according to Guyot et al. then be able to generate survival distribution parameter estimates')
                 ,width = 12
               )
             ),
             
             fluidRow(
               box(
                 h4(strong("Preview of the tutorial")),
                 
                 img(src='step1.gif', align = 'center')
                 , width = 12 , align = 'center'
               )
            ),
            
            fluidRow(
              column(8,
                     box(
                       h4(strong("Preview of Number 2")),
                       
                       img(src='step2.gif', align = 'center'),
                       
                       h4("Currently the most important aspect with regards to checking whether or not the survival curve is appropriate is to determine if it is monotone decreasing (i.e. not increasing). Due to approximation errors in the webplotdigitizer, there may be points that have higher S(t) than the previous time point. The tutorial in the above Tutorial link has python code to remedy non-monotonic S(t) but there are also ways to do this in excel.")
                       , width = 8, align = 'center'
                     ), offset = 3
              )
             
             )
    ),
      
      # Second Tab content: Data and SUmmary
      tabItem(tabName = "data",
              
              h2("Please Enter the CSV Data"),
              
              fileInput('datafile1', 'Please Input the Survival Curve CSV file',
                        accept=c('text/csv', 'text/comma-separated-values,text/plain')),
              
              
              h3("Select the Appropriate Variables for Parameter Estimation"),
              uiOutput("Variables1"),
              
              uiOutput("Variables2"),
              
              fileInput('datafile2', 'Please Input the Number at Risk CSV file',
                        accept=c('text/csv', 'text/comma-separated-values,text/plain')),
              
              h3("Enter total number of events"),
              numericInput('totevent',"Total Number of Events",value =  NA, min = 0),
              
              h3("Control or Treatment Indicator. Control = 0; Treatment = 1"),
              numericInput('arm.id',"Arm ID", 1, min = 0, max = 1),
              
              strong('R session info'),
              verbatimTextOutput("info")
              
      ),
      
      # Data SUmmary tab content
      
      tabItem(tabName = 'datasum',
              
              fluidRow(
                box(
                  h2(strong("Data Summary")),
                  
                  # View quick summary statistics
                  # Create row to check the data
                  h3("Data View of Curve Data"),
                  fluidRow( dataTableOutput("filetable1")),
                  
                  h3("Data View of Number at Risk Data"),
                  fluidRow( dataTableOutput("filetable2")),
                  
                  h3("Individual Patient Data (IPD) View"),
                  fluidRow( dataTableOutput("IPDtable")),
                  
                  h3("Survival Probability Table"),
                  verbatimTextOutput("KMsum")
                  , width = 12
                )
              )
              
            ),

    
      # Model Options tab content
      tabItem(tabName = "modelopts",
              
              fluidRow(
                box(
                  h2("Modeling Options"),
                  
                  radioButtons('splineyn','Splines?',
                               choices =  c(
                                 "Yes" = 'yes',
                                 "No" = 'no'), selected = 'no')
                  ,width = 12
                )
              ),
              
              # add conditional panel if the splineyn == 'yes'
              
              conditionalPanel("input.splineyn == 'yes'",
                               sliderInput('numspline', "Number of splines:",
                                           min = 1, max = 100, value = 1, step = 1)
                               )
      ),
      
      
      # Plots tab: Plot submenu of result
      tabItem(tabName = "plot",
              
              fluidRow(
                box(
                  h3("Plotting Options"),
                  radioButtons('confint','Confidence Intervals?',
                               choices =  c(
                                 "Yes" = 'yes',
                                 "No" = 'no'), selected = 'no'),
                  radioButtons('wpdcurve','Add Digitized Curve?',
                               choices = c(
                                 "Yes" = 'yes',
                                 "No" = 'no'), selected = 'no'
                               )
                  ,width = 4),
                box(
                  # Copy the line below to make a text input box
                  textInput("titletxt", label = h3("Input Title"), value = "Enter text...")
                  ,width = 4
                ),
                box(
                  # Copy the line below to make a text input box
                  textInput("xlab", label = h3("X Label"), value = "Time in Months")
                  ,width = 4
                ),
                box(
                  # Copy the line below to make a text input box
                  textInput("ylab", label = h3("Y Label"), value = "S(t)")
                  ,width = 4
                )
              ),
            
              fluidRow(
                width = 10,
                      box(
                        plotOutput("survplot", width = 700), width = 10
                      )
        )
      ),
    
    
      # Parameter Estimates tab content
      tabItem(tabName = "regresult",
              
              h2("Parameter Estimates"),
              
              fluidRow(
                box(
                  h3("Survival Distribution Parameters"),
                  fluidRow( dataTableOutput("parest"))
                  ,width = 12
                )
                

              )
              
              
      ),
    
      # Sixth tab: Fit statistics/Diagnostic
      tabItem(tabName = "diag",
              h2("Fit Statistics"),
              
              fluidRow(
                box(
                  h3("Fit Statistics"),
                  fluidRow( dataTableOutput("fitstat"))
                  ,width = 12
                )
              )
              
              
      )
      
      
      
      
    )
  )
)