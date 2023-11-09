function(input, output) {


  ##############################################################
  #PART 1: Data processing
  ##############################################################
  #Check the historic data set and update it to include data up to 
  #the present if it hasn't been been already.
  updateHistoric()

  #Preallocate the important historical data tables 
  lake_table = vector("list", n_lakes)
  daily_precip = vector("list", n_lakes)
  #Final data set
  lake_data = vector("list", n_lakes)

  #Load the historic data sets
  lake_table[[1]] = read.csv(file = "./../data/men_hist.csv")
  lake_table[[1]][,"time"] = ymd(lake_table[[1]][,"time"])
  lake_table[[2]] = read.csv(file = "./../data/mon_hist.csv")
  lake_table[[2]][,"time"] = ymd(lake_table[[2]][,"time"])
  daily_precip[[1]] = read.csv(file = "./../data/rain_hist.csv")
  daily_precip[[1]][,"time"] = ymd(daily_precip[[1]][,"time"])
  daily_precip[[2]] = daily_precip[[1]]

  #Truncate the data 
  for (n in 1:n_lakes){ 
    lake_table[[n]] = lake_table[[n]][lake_table[[n]][,"time"] > real_start[[n]], ]

  }
  
  #Final processing steps of the raw data which joins lake
  #and precip and truncates to desired start date. 
  for (n in 1: n_lakes){

    #Join the lake and rain data to match up dates
    lake_data[[n]] = lake_table[[n]] %>%
          inner_join(daily_precip[[n]], by = "time" )

    #Truncate the data set so that we only have from real_start
    #onwards. 
    lake_data[[n]] = lake_data[[n]][lake_data[[n]][,"time"] 
                      >= real_start[[n]], ]

    #Do some processing to remove ice-on days (approximately). This 
    #function automatically removes winter days and converts data 
    #table to a timeseries (ts) object 
    lake.tmp = remove.days(lake_data[[n]]$level, year(real_start[[n]] ) )
    colnames(lake.tmp) = "level"
    rn.tmp = remove.days(lake_data[[n]]$rn, year(real_start[[n]] ) )
    colnames(rn.tmp) = "rn"

    #This final step creates the full data object, with lags of 
    #lake level for autocorrelation and lags of rain for delayed
    #rain input. 
    lake_data[[n]] = make.flashiness.object(lake.tmp, rn.tmp, lags)
  }

  #Get the rain forecast data:
  fut_precip = as.data.frame(weather_forecast(location =  
    c(43.0930, -89.3727), daily="precipitation_sum") )
  colnames(fut_precip) = c("time", "rn")
  fut_precip$rn = fut_precip$rn / 25.4 #Convert mm to inches


  ##############################################################
  #PART 2: Forecasting
  ##############################################################
  #Check to see if the GAMs have already been fitted and saved in 
  #a *.var file, or if we need to fit them. 
  updateModel(lake_data,model_form)

  #Predict the future lake-level response from the saved GAMs
  predictFlashGAM(lake_data, fut_precip)

  ##############################################################
  #PART 3: Build out the UI
  ##############################################################

  #Section 1: Value Boxes
 
  #The maximum rainfall in the upcoming days 
  output$max_rain = renderValueBox({
    mr = max(fut_precip$rain)

    valueBox(
      value = formatC(mr, digits = 1, format = "f"),
      subtitle = "Peak 7-day rainfall (in)",
      icon = icon("area-chart"),
      color = aqua #if (mr >= year10) "yellow" else "aqua"
    )
  })  

    #The peak lake-level 
    output$max_peak_men = renderValueBox({
      mpm = max(pred_lakes[[1]]$level)

      valueBox(
        value = formatC(mpm, digits = 1, format = "f"),
        subtitle = "Peak 7-day lake (ft)",
        icon = icon("area-chart"),
        color = aqua #if (mr >= year10) "yellow" else "aqua"
      )
    })  
 
    #The confidence interval 
    output$pred_con_men = renderValueBox({
      #Get the max from the time-series
      mpm = max(pred_lakes[[1]]$level)
      #Get its se
      se_men  = pred_lakes[[1]]$se[which(pred_lakes[[1]]$level == mpm)]
      #95% CIs 
      pcmen = 1.96*se_men
      
      valueBox(
        value = formatC(pcmen, digits = 1, format = "f"),
        subtitle = "95% CI",
        icon = icon("area-chart"),
        color = aqua #if (mr >= year10) "yellow" else "aqua"
      )
    })  
 

  # #Section 2: Forecast
  # output$max_rain <- renderValueBox({
  #   # max_rain is the highest forecast rainfall for a single day
  #   elapsed <- as.numeric(Sys.time()) - startTime
  #   downloadRate <- nrow(pkgData()) / min(maxAgeSecs, elapsed)

  #   valueBox(
  #     value = formatC(downloadRate, digits = 1, format = "f"),
  #     subtitle = "Downloads per sec (last 5 min)",
  #     icon = icon("area-chart"),
  #     color = if (downloadRate >= input$rateThreshold) "yellow" else "aqua"
  #   )
  # })

  # output$count <- renderValueBox({
  #   valueBox(
  #     value = dlCount(),
  #     subtitle = "Total downloads",
  #     icon = icon("download")
  #   )
  # })

  # output$users <- renderValueBox({
  #   valueBox(
  #     usrCount(),
  #     "Unique users",
  #     icon = icon("users")
  #   )
  # })

  # output$packagePlot <- renderBubbles({
  #   if (nrow(pkgData()) == 0)
  #     return()

  #   order <- unique(pkgData()$package)
  #   df <- pkgData() %>%
  #     group_by(package) %>%
  #     tally() %>%
  #     arrange(desc(n), tolower(package)) %>%
  #     # Just show the top 60, otherwise it gets hard to see
  #     head(60)

  #   bubbles(df$n, df$package, key = df$package)
  # })

  # output$packageTable <- renderTable({
  #   pkgData() %>%
  #     group_by(package) %>%
  #     tally() %>%
  #     arrange(desc(n), tolower(package)) %>%
  #     mutate(percentage = n / nrow(pkgData()) * 100) %>%
  #     select("Package name" = package, "% of downloads" = percentage) %>%
  #     as.data.frame() %>%
  #     head(15)
  # }, digits = 1)

  # output$downloadCsv <- downloadHandler(
  #   filename = "cranlog.csv",
  #   content = function(file) {
  #     write.csv(pkgData(), file)
  #   },
  #   contentType = "text/csv"
  # )

  # output$rawtable <- renderPrint({
  #   orig <- options(width = 1000)
  #   print(tail(pkgData(), input$maxrows), row.names = FALSE)
  #   options(orig)
  # })
  
}


