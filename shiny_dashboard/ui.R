
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
              title = "About this graph"

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
          # Clicking this will increment the progress amount
          #box(width = 4, actionButton("count", "Increment progress"))
        )
      )

    )
  )

)



