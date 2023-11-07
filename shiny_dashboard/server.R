server <- function(input, output) {

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
  #Readable dates for plotting
  lake_dates = vector("list", n_lakes)

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
    lake.tmp = remove.days(lake_data[[n]][,c(1,2)], year(real_start[[n]] ) )
    #colnames(lake.tmp) = "level"
    rn.tmp = remove.days(lake_data[[n]][,c(1,3)], year(real_start[[n]] ) )
    #colnames(rn.tmp) = "rn"

    #Keep the dates
    lake_dates[[n]] = lake.tmp$time 

    #as_date(date_decimal(as.numeric(time(lake.tmp))))

    #This final step creates the full data object, with lags of 
    #lake level for autocorrelation and lags of rain for delayed
    #rain input. 
    lake_data[[n]] = make.flashiness.object(data.frame(level= lake.tmp$level), 
      data.frame(rn=rn.tmp$rn), lags)
  }

  #Get the rain forecast data:
  fut_precip = as.data.frame(weather_forecast(location =  
    c(43.0930, -89.3727), daily="precipitation_sum") )
  colnames(fut_precip) = c("time", "rn")
  fut_precip$rn = fut_precip$rn / 25.4 #Convert mm to inches

  ##############################################################
  #PART 2: Forecasting with GAMs
  ##############################################################
  #Check to see if the GAMs have already been fitted and saved in 
  #a *.var file, or if we need to fit them. 
  updateModel(lake_data,model_form)

  #Predict the future lake-level response from the saved GAMs
  pred_lakes = predictFlashGAM(lake_data, fut_precip)

  ##############################################################
  #PART 3: Forecasting with DNN 
  ##############################################################
  #Check to see if the DNNs have already been fitted and saved in 
  #a *.keras file, or if we need to fit them.
  updateModelDNN(lake_data)

  #Predict the future lake-level response from the saved DNN
  pred_lakes_dnn = predictFlashDNN(lake_data, fut_precip )
  ##############################################################
  #PART 3: Build out the UI
  ##############################################################
  #Add the readable dates back on for plotting
  for (n in 1:n_lakes){
    lake_data[[n]]$dates = lake_dates[[n]] 
  }

  ##############################################################
  #Mendota
  ##############################################################

   #The maximum rainfall in the upcoming days 
    output$max_rain = renderInfoBox({
    mr = max(fut_precip$rn)

    infoBox(
     "Peak 7-day rainfall (in)",
      value = formatC(mr, digits = 1, format = "f"),
      icon = icon("cloud-rain"),
      color = "aqua" #if (mr >= year10) "yellow" else "aqua"
    )
  })  

    #The peak lake-level 
    output$max_peak_men = renderInfoBox({
      mpm = max(pred_lakes[[1]]$level)

      infoBox(
        "Peak 7-day lake (ft)",
        value = formatC(mpm, digits = 1, format = "f"),
        icon = icon("water"),
        color = "aqua" #if (mr >= year10) "yellow" else "aqua"
      )
    })  
 
    #The confidence interval 
    output$pred_con_men = renderInfoBox({
      #Get the max from the time-series
      mpm = max(pred_lakes[[1]]$level)
      #Get its se
      se_men  = pred_lakes[[1]]$se[which(pred_lakes[[1]]$level == mpm)]
      #95% CIs 
      pcmen = 1.96*se_men
      
      infoBox(
        "95% CI",
        value = formatC(pcmen, digits = 1, format = "f"),
        icon = icon("chart-line"),
        color = "aqua" #if (mr >= year10) "yellow" else "aqua"
      )
    })  

  #Plot the time series of predictions

  output$pred_plot1=renderPlot({

      ggplot( data = pred_lakes[[1]]) + 
      geom_line(aes(x = time, y=level)) +
      geom_ribbon(aes(x = time, ymin = level-se, ymax = level+se), alpha = 0.2)+
      ylim(max(pred_lakes[[1]]$level)-max(pred_lakes[[1]]$level)*.2, 
        max(pred_lakes[[1]]$level)+max(pred_lakes[[1]]$level)*.2)+
      theme_minimal() + theme(text=element_text(size=21)) +
      ggtitle("Forecasted lake level") + xlab("Date")+
      ylab("Lake level (ft) ") 
   } )

 #Plot the historical and prediction time series

  low_limx = reactiveValues(limx = current_date - months(3) )

  observeEvent(input$m03, {
    low_limx$limx = current_date - months(3)
  })

  observeEvent(input$yr1, {
    low_limx$limx = current_date - years(1)
  })  

  observeEvent(input$yr3, {
    low_limx$limx = current_date - years(3)
  })

  observeEvent(input$yr10, {
    low_limx$limx = current_date - years(10)
  })  


  output$full_plot1=renderPlot({

      ggplot( ) +
      geom_line(data = lake_data[[1]], aes(x = dates, y=level) ) +
      geom_line(data = pred_lakes[[1]], aes(x = time, y=level), col="red") +
      geom_ribbon(data = pred_lakes[[1]], 
        aes(x = time, ymin = level-se, ymax = level+se), alpha = 0.2)+
      ylim(4, max(lake_data[[1]]$level)+max(lake_data[[1]]$level)*.2 )+
      theme_minimal() + theme(text=element_text(size=21)) +
      ggtitle("Forecasted lake level") + xlab("Date")+
      ylab("Lake level (ft) ") +
      xlim(low_limx$limx, current_date+8)

   } )

##############################################################
#Monona
##############################################################

 #The maximum rainfall in the upcoming days 
    output$max_rain2 = renderInfoBox({
    mr = max(fut_precip$rn)

    infoBox(
     "Peak 7-day rainfall (in)",
      value = formatC(mr, digits = 1, format = "f"),
      icon = icon("cloud-rain"),
      color = "aqua" #if (mr >= year10) "yellow" else "aqua"
    )
  })  

    #The peak lake-level 
    output$max_peak_mon = renderInfoBox({
      mpm = max(pred_lakes[[2]]$level)

      infoBox(
        "Peak 7-day lake (ft)",
        value = formatC(mpm, digits = 1, format = "f"),
        icon = icon("water"),
        color = "aqua" #if (mr >= year10) "yellow" else "aqua"
      )
    })  
 
    #The confidence interval 
    output$pred_con_mon = renderInfoBox({
      #Get the max from the time-series
      mpm = max(pred_lakes[[2]]$level)
      #Get its se
      se_mon  = pred_lakes[[2]]$se[which(pred_lakes[[2]]$level == mpm)]
      #95% CIs 
      pcmon = 1.96*se_mon
      
      infoBox(
        "95% CI",
        value = formatC(pcmon, digits = 1, format = "f"),
        icon = icon("chart-line"),
        color = "aqua" #if (mr >= year10) "yellow" else "aqua"
      )
    })  

      #Plot the time series of predictions

  output$pred_plot2=renderPlot({
      ggplot( data = pred_lakes[[2]]) + 
      geom_line(aes(x = time, y=level)) +
      geom_ribbon(aes(x = time, ymin = level-se, ymax = level+se), alpha = 0.2)+
        ylim(max(pred_lakes[[2]]$level)-max(pred_lakes[[2]]$level)*.2, 
        max(pred_lakes[[2]]$level)+max(pred_lakes[[2]]$level)*.2)+
      theme_minimal() + theme(text=element_text(size=21)) +
      ggtitle("Forecasted lake level") + xlab("Date")+
      ylab("Lake level (ft) ") 
   } )

#Plot the historical and prediction time series

  low_limx2 = reactiveValues(limx2 = current_date%m-%months(3) )

  observeEvent(input$m032, {
    low_limx2$limx2 = current_date%m-%months(3)
  })

  observeEvent(input$yr12, {
    low_limx2$limx2 = current_date - years(1)
  })  

  observeEvent(input$yr32, {
    low_limx2$limx2 = current_date - years(3)
  })

  observeEvent(input$yr102, {
    low_limx2$limx2 = current_date - years(10)
  })  


  output$full_plot2=renderPlot({

      ggplot( ) +
      geom_line(data = lake_data[[2]], aes(x = dates, y=level) ) +
      geom_line(data = pred_lakes[[2]], aes(x = time, y=level), col="red") +
      geom_ribbon(data = pred_lakes[[2]], 
        aes(x = time, ymin = level-se, ymax = level+se), alpha = 0.2)+
      ylim(1, max(lake_data[[2]]$level)+max(lake_data[[2]]$level)*.1 )+
      theme_minimal() + theme(text=element_text(size=21)) +
      ggtitle("Forecasted lake level") + xlab("Date")+
      ylab("Lake level (ft) ") +
      xlim(low_limx2$limx2, current_date+8)

   } )



} ##Server function end

