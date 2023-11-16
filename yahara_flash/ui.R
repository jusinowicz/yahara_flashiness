
ui <- dashboardPage(
  dashboardHeader(title = "Yahara Flashiness"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Mendota", tabName = "Mendota"),
      menuItem("Monona", tabName = "Monona")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("Mendota",
        fluidRow(
          column(width = 3,
            # fluidRow(
            # #Upcoming peak in rain
            # infoBoxOutput("max_rain"),
            # #Maximum peak in forecast
            # infoBoxOutput("max_peak_men"),
            # #Confidence 
            # infoBoxOutput("pred_con_men"))
            fluidRow(
            #Upcoming peak in rain
            infoBoxOutput("max_rain",width=12) ),
            fluidRow(
            #Maximum peak in forecast
            infoBoxOutput("max_peak_men",width=12)),
            fluidRow(
            #Confidence 
            infoBoxOutput("pred_con_men",width=12))
          ),

          column(width=9,
             fluidRow(
             # Plot the predicted values with CIs 
              #title=h3("Forecasted lake level", 
              #style = 'font-size:42px;'), 
             box(
              width = 12, 
              plotOutput("pred_plot1") )
            )
          )


        ),
        fluidRow(
          column(width = 3,
            box(
              width = 12, 
              title = "About the graphs",
              p("The 7-day forecast of lake level projected by ",
              span("GAMM", style = "color:red"),
              " and ",
              span("Machine Learning (RNN)", style = "color:blue"),
              "models. "),
              p("The 7-day forecast can also be seen compared to the
                historical time series of lake level."),
              p("The boxes above highlight peak upcoming rain events and 
                lake levels. Colors indicate whether lake 
                levels are within",
                span("normal", style = "color:aqua"), 
                " levels or will exceed the ",
                span("10-year", style = "color:orange"),
                " or the ",
                span("100-year", style = "color:red"),
                " flood levels." )

            )
            # Give a more historical perspective on the data:
          ), 
          column(width = 9,
            actionButton("mo3", "3 Mo"),
            actionButton("yr1", "1 Yr"),
            actionButton("yr3", "3 Yr"),
            actionButton("yr10", "10 Yr"), 
            box(
              width = 12,
              plotOutput("full_plot1")
            )
            # Give a more historical perspective on the data:
          ), 

        )
      ),
      tabItem("Monona",
        fluidRow(
          column(width = 3,
            # fluidRow(
            # #Upcoming peak in rain
            # infoBoxOutput("max_rain"),
            # #Maximum peak in forecast
            # infoBoxOutput("max_peak_men"),
            # #Confidence 
            # infoBoxOutput("pred_con_men"))
            fluidRow(
            #Upcoming peak in rain
            infoBoxOutput("max_rain2",width=12) ),
            fluidRow(
            #Maximum peak in forecast
            infoBoxOutput("max_peak_mon",width=12)),
            fluidRow(
            #Confidence 
            infoBoxOutput("pred_con_mon",width=12))
          ),

          column(width=9,
             fluidRow(
             # Plot the predicted values with CIs 
             box(width = 12, plotOutput("pred_plot2") )
            )
          )


        ),
        fluidRow(
          column(width = 3,
            box(
              width = 12, 
              title = "About the graphs",
              p("The 7-day forecast of lake level projected by ",
              span("GAMM", style = "color:red"),
              " and ",
              span("Machine Learning (RNN)", style = "color:blue"),
              "models. "),
              p("The 7-day forecast can also be seen compared to the
              historical time series of lake level."),
              p("The boxes above highlight peak upcoming rain events and 
                lake levels. Colors indicate whether lake 
                levels are within",
                span("normal", style = "color:aqua"), 
                " levels or will exceed the ",
                span("10-year", style = "color:orange"),
                " or the ",
                span("100-year", style = "color:red"),
                " flood levels." )

            )
            # Give a more historical perspective on the data:
          ), 
          column(width = 9,
            actionButton("mo32", "3 Mo"),
            actionButton("yr12", "1 Yr"),
            actionButton("yr32", "3 Yr"),
            actionButton("yr102", "10 Yr"), 
            box(
              width = 12,
              plotOutput("full_plot2")
            )
            # Give a more historical perspective on the data:
          ), 

        )
      )

    )
  )

)



