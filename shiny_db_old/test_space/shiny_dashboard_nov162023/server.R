server <- function(input, output) {

  withProgress(message = 'Retrieving data', value = 0, {
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
  scale_ll = vector("list", n_lakes)

  incProgress(1/4, detail="Getting forecast")
  #Get the rain forecast data:
  fut_precip = as.data.frame(weather_forecast(location =  
    c(43.0930, -89.3727), daily="precipitation_sum") )
  colnames(fut_precip) = c("time", "rn")
  fut_precip$rn = fut_precip$rn / 25.4 #Convert mm to inches

  lagsp = dim(fut_precip)[1]

  incProgress(2/4, detail="Loading historic data")
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
    #Check for and remove NAs: 
    rwNA = rowSums(is.na(cbind(lake.tmp,rn.tmp))) 
    lake.tmp = lake.tmp[rwNA<1,]
    rn.tmp = rn.tmp[rwNA<1,]

    #Keep the dates
    lake_dates[[n]] = lake.tmp$time 

    #as_date(date_decimal(as.numeric(time(lake.tmp))))

    #This final step creates the full data object, with lags of 
    #lake level for autocorrelation and lags of rain for delayed
    #rain input. 
    lake_data[[n]] = make.flashiness.object(data.frame(level= lake.tmp$level), 
      data.frame(rn=rn.tmp$rn), lags)

    #For the RNN. The lags are the number of days into the future we 
    #wish to forecast.
    scale_ll[[n]] = c ( mean(lake.tmp$level,na.rm = T), sqrt(var(lake.tmp$level,na.rm = T)) )
    scale_rn = c ( mean(rn.tmp$rn,na.rm = T), sqrt(var(rn.tmp$rn,na.rm = T)) )
    lake_data_lstm[[n]] = make.flashiness.object(data.frame(level= 
      (lake.tmp$level - scale_ll[[n]] [1])/scale_ll[[n]] [2] ),
      data.frame(rn= (rn.tmp$rn - scale_rn[1])/scale_rn[2] ), lagsp-1, auto=F, orders=lagsp-1)
  
    incProgress((n+2)/4, detail="Final melding")

  }
})
  ##############################################################
  #PART 2: Forecasting with GAMs
  ##############################################################
  withProgress(message = 'Forecasting GAMMs', value = 0, {
  #Check to see if the GAMs have already been fitted and saved in 
  #a *.var file, or if we need to fit them. 
  incProgress(1/2, detail="Update models")
  updateModel(lake_data,model_form)
  
  #Predict the future lake-level response from the saved GAMs
  incProgress(2/2, detail="Get forecast")
  pred_lakes = predictFlashGAM(lake_data, fut_precip)
  })
  ##############################################################
  #PART 3: Forecasting with RNN (LSTM) 
  ##############################################################
  #Check to see if the RNN  exists, and whether it has already 
  #been updated and predictions made:
  withProgress(message = 'Forecasting RNN (might take awhile)', value = 0, {
 
  updateModelLSTM(lake_data_lstm)

  load(file = "todays_forecast.var")
  for(n in 1:n_lakes){ 

    lake_models_forecast[[n]] = data.frame(time = fut_precip$time, 
              level = 
              unlist(lake_models_forecast[[n]])*scale_ll[[n]][2]+
              scale_ll[[n]][1] )

  }
})
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
  #Max lake level
  mpm = max(pred_lakes[[1]]$level)
  col_use  = "aqua"
  #Check against the flood levels: 
  if( mpm < thresh_10[1] ) { col_use = flash_col[1]}
  if( mpm >= thresh_10[1] && mr < thresh_100[1] ){ 
    col_use = flash_col[2]}
  if( mpm >= thresh_100[1] ){ col_use = flash_col[3]}


   #The maximum rainfall in the upcoming days 
    output$max_rain = renderInfoBox({
    mr = max(fut_precip$rn)

    infoBox(
     "Peak 7-day rainfall (in)",
      value = formatC(mr, digits = 1, format = "f"),
      icon = icon("cloud-rain"),
      color = col_use #if (mr >= year10) "yellow" else "aqua"
    )
  })  

    #The peak lake-level 
    output$max_peak_men = renderInfoBox({

      infoBox(
        "Peak 7-day lake (ft)",
        value = formatC(mpm, digits = 1, format = "f"),
        icon = icon("water"),
        color = col_use #if (mr >= year10) "yellow" else "aqua"
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
        color = col_use #if (mr >= year10) "yellow" else "aqua"
      )
    })  

  #Plot the time series of predictions

  output$pred_plot1=renderPlot({

      ggplot( ) + 
      geom_line(data = pred_lakes[[1]], aes(x = time, y=level), 
        linetype = "dotted", col = "red") +
      geom_point(data = pred_lakes[[1]], aes(x = time, y=level), 
        col = "red") +
      geom_ribbon(data = pred_lakes[[1]], 
        aes(x = time, ymin = level-se, ymax = level+se), alpha = 0.2)+
      geom_line(data = lake_models_forecast[[1]], 
        aes(x = time, y=level),col = "blue",linetype = "dotted")+
      geom_point(data = lake_models_forecast[[1]], 
        aes(x = time, y=level),col = "blue")+
      ylim(max(pred_lakes[[1]]$level)-max(pred_lakes[[1]]$level)*.2, 
        max(pred_lakes[[1]]$level)+max(pred_lakes[[1]]$level)*.2)+
      theme_minimal() + theme(text=element_text(size=21)) +
      ggtitle("Forecasted lake level") + xlab("Date")+
      ylab("Lake level (ft) ")+
      geom_hline(yintercept=thresh_10[1], col="orange", linetype = "dotted")+
      geom_hline(yintercept=thresh_100[1], col="red", linetype = "dotted")
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
      geom_line(data = lake_models_forecast[[1]], 
        aes(x = time, y=level),col = "blue")+
      ylim(4, max(lake_data[[1]]$level)+max(lake_data[[1]]$level)*.2 )+
      theme_minimal() + theme(text=element_text(size=21)) +
      xlab("Date")+
      ylab("Lake level (ft) ") +
      xlim(low_limx$limx, current_date+8)+
      geom_hline(yintercept=thresh_10[1], col="orange", linetype = "dotted")+
      geom_hline(yintercept=thresh_100[1], col="red", linetype = "dotted")

   } )

##############################################################
#Monona
##############################################################
#Max lake level
mpm2 = max(pred_lakes[[2]]$level)
col_use2  = "aqua"
#Check against the flood levels: 
if( mpm2 < thresh_10[1] ) { col_use2 = flash_col[1]}
if( mpm2 >= thresh_10[1] && mpm2 < thresh_100[1] ){ 
  col_use2 = flash_col[2]}
if( mpm2 >= thresh_100[1] ){ col_use2 = flash_col[3]}

 #The maximum rainfall in the upcoming days 
    output$max_rain2 = renderInfoBox({
    mr = max(fut_precip$rn)

    infoBox(
     "Peak 7-day rainfall (in)",
      value = formatC(mr, digits = 1, format = "f"),
      icon = icon("cloud-rain"),
      color = col_use2 #if (mr >= year10) "yellow" else "aqua"
    )
  })  

    #The peak lake-level 
    output$max_peak_mon = renderInfoBox({

      infoBox(
        "Peak 7-day lake (ft)",
        value = formatC(mpm2, digits = 1, format = "f"),
        icon = icon("water"),
        color = col_use2 #if (mr >= year10) "yellow" else "aqua"
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
        color = col_use2 #if (mr >= year10) "yellow" else "aqua"
      )
    })  

      #Plot the time series of predictions

  output$pred_plot2=renderPlot({
      ggplot( ) + 
      geom_line(data = pred_lakes[[2]], aes(x = time, y=level),
        col="red", linetype = "dotted") +
      geom_point(data = pred_lakes[[2]], aes(x = time, y=level),
        col="red") +
      geom_line(data = lake_models_forecast[[2]], 
        aes(x = time, y=level),col = "blue", linetype = "dotted") +
      geom_point(data = lake_models_forecast[[2]], 
        aes(x = time, y=level),col = "blue") +
      geom_ribbon(data = pred_lakes[[2]], 
        aes(x = time, ymin = level-se, ymax = level+se), alpha = 0.2)+
      ylim(max(pred_lakes[[2]]$level)-max(pred_lakes[[2]]$level)*.2, 
        max(pred_lakes[[2]]$level)+max(pred_lakes[[2]]$level)*.2)+
      theme_minimal() + theme(text=element_text(size=21)) +
      ggtitle("Forecasted lake level") + xlab("Date")+
      ylab("Lake level (ft) ")+
      geom_hline(yintercept=thresh_10[2], col="orange", linetype = "dotted")+
      geom_hline(yintercept=thresh_100[2], col="red", linetype = "dotted")
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
      geom_line(data = lake_models_forecast[[2]], 
        aes(x = time, y=level),col = "blue")+
      geom_ribbon(data = pred_lakes[[2]], 
        aes(x = time, ymin = level-se, ymax = level+se), alpha = 0.2)+
      ylim(1, max(lake_data[[2]]$level)+max(lake_data[[2]]$level)*.1 )+
      theme_minimal() + theme(text=element_text(size=21)) +
      xlab("Date")+
      ylab("Lake level (ft) ") +
      xlim(low_limx2$limx2, current_date+8)+
      geom_hline(yintercept=thresh_10[2], col="orange", linetype = "dotted")+
      geom_hline(yintercept=thresh_100[2], col="red", linetype = "dotted")

   } )



} ##Server function end

