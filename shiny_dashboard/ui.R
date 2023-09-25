dashboardPage(
  dashboardHeader(title = "cran.rstudio.com"),
  dashboardSidebar(
    sliderInput("rateThreshold", "Warn when rate exceeds",
      min = 0, max = 50, value = 3, step = 0.1
    ),
    sidebarMenu(
      menuItem("Forecast", tabName = "forecast"), #dashboard
      menuItem("Historical", tabName = "historical") #raw data
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("forecast",
        fluidRow(
          column(3, 
            #Upcoming peak in rain
            valueBoxOutput("max_rain"),
            #Maximum peak in forecast
            valueBoxOutput("max_peak"),
            #Prediction accuracy 
            valueBoxOutput("pred_acc")
        ),

           column(9, 
            plotOutput("flash_fore")
        )

        ),
        fluidRow(
          # box(
          #   width = 8, status = "info", solidHeader = TRUE,
          #   title = "Popularity by package (last 5 min)",
          #   bubblesOutput("packagePlot", width = "100%", height = 600)
          # ),
          # box(
          #   width = 4, status = "info",
          #   title = "Top packages (last 5 min)",
          #   tableOutput("packageTable")
          # )
        )
      ),
      tabItem("historical",
        # numericInput("maxrows", "Rows to show", 25),
        # verbatimTextOutput("rawtable"),
        # downloadButton("downloadCsv", "Download as CSV")
      )
    )
  )
)

