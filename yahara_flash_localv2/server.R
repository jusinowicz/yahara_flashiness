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
  #Readable dates for plotting
  lake_dates = vector("list", n_lakes)
  #Scales of data
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
  daily_precip[[1]] = read.csv(file = "./data/rain_hist.csv")
  daily_precip[[1]][,"time"] = ymd(daily_precip[[1]][,"time"])

  for (n in 1:n_lakes){
    #file name
    fn = paste("./data/",lake_pre[n], "_hist.csv",sep="")

    #Load the current data files
    lake_table[[n]] = read.csv(file = paste(fn))
    lake_table[[n]][,"time"] = ymd(lake_table[[n]][,"time"])

    #Truncate the data  
    lake_table[[n]] = lake_table[[n]][lake_table[[n]][,"time"] > real_start[[n]], ]

    daily_precip[[n]] = daily_precip[[1]]
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
    lake.tmp = remove.days(lake_data[[n]][,c(1,2)], w.yes=F, year(real_start[[n]] ) )
    #colnames(lake.tmp) = "level"
    rn.tmp = remove.days(lake_data[[n]][,c(1,3)], w.yes=F, year(real_start[[n]] ) )
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

    lake_data[[n]]$time = lake_dates[[n]]
    years.tmp = data.frame( Year=format(lake_dates[[n]], "%Y") )
    months.tmp = data.frame(Month = format(lake_dates[[n]], "%m") )

    # #Include the years and the months as columns
    # lake_data[[n]] = cbind(lake_data[[n]],years.tmp, months.tmp )

    #For the RNN. The lags are the number of days into the future we 
    #wish to forecast.
    # scale_ll[[n]] = c ( mean(lake.tmp$level,na.rm = T), sqrt(var(lake.tmp$level,na.rm = T)) )
    # scale_rn = c ( mean(rn.tmp$rn,na.rm = T), sqrt(var(rn.tmp$rn,na.rm = T)) )
    scale_ll[[n]] = c ( mean( lake.tmp$level,na.rm = T), sqrt(var( lake.tmp$level,na.rm = T)) )
    scale_rn = c ( mean(rn.tmp$rn,na.rm = T), sqrt(var(rn.tmp$rn,na.rm = T)) )

    lake_data_temp[[n]] = data.frame(
      time = lake.tmp$time ,
      level= (lake.tmp$level - scale_ll[[n]] [1])/scale_ll[[n]] [2],
      rn= (rn.tmp$rn - scale_rn[1])/scale_rn[2] )
    
    fut_precip_scaled = fut_precip
    fut_precip_scaled$rn = (fut_precip$rn- scale_rn[1])/scale_rn[2]

    #incProgress((n+2)/4, detail="Final melding")

  }

  #Build out the final data sets with lags of other lake levels
  #Join the lake and rain data to match up dates
  lake_data_all = lake_data_temp[[1]][,1:2]
  lake_data_allG = lake_data[[1]][,1:2]

  for (n in 2:n_lakes){
    lake_data_all = lake_data_all %>%
                        inner_join ( lake_data_temp[[n]][,1:2],by = "time")
    lake_data_allG = lake_data_allG %>%
                        inner_join ( lake_data[[n]][,1:2],by = "time")
  }

  #names
  colnames(lake_data_all) = c("time", lake_pre)
  colnames(lake_data_allG) = c("time", lake_pre)
  
  #Add the rain
  lake_data_all = lake_data_all %>%
                        inner_join ( lake_data_temp[[n]][,c(1,3)],by = "time")

  ld_tmp = data.frame(lake_data[[n]]$time, lake_data[[n]]$rn) 
  colnames(ld_tmp) = c("time","rn")

  lake_data_allG = lake_data_allG %>%
                        inner_join ( ld_tmp, by = "time")

  #Now make the data sets for each lake, with lags of all lakes
  for (n in 1:n_lakes){
    l_others = 1:n_lakes
    l_others = l_others[-n]

    #Keep the dates
    lake_dates[[n]] = lake_data_allG$time 

    #Make the lake specific data frame 
    lake_data_temp[[n]] = data.frame(lake_data_all$time, 
                          level = lake_data_all[,(n+1)],
                          lake_data_all[1+l_others],
                          rn = lake_data_all$rn)

    lake_data_tempG[[n]] = data.frame(lake_data_allG$time, 
                          level = lake_data_allG[,(n+1)],
                          lake_data_allG[1+l_others],
                          rn = lake_data_allG$rn)

    #Now feed it to the function to add the lags
    lake_data_lstm[[n]] = make.flashiness.object(
      data.frame(level = lake_data_temp[[n]]$level), as.data.frame(lake_data_temp[[n]][,3:(n_lakes+2)]),
      matrix(lagsp-1,1,(n_lakes) ), 
      auto=F, orders=lagsp-1)

    lake_data[[n]] = make.flashiness.object(
      data.frame(level = lake_data_tempG[[n]]$level), as.data.frame(lake_data_tempG[[n]][,3:(n_lakes+2)]),
      matrix(lagsp-1,1,(n_lakes) ), 
      auto=F, orders=lagsp-1)

   # lake_data[[n]]$time = lake_dates[[n]]

    # One-hot encode the years and months:      
    years.tmp = data.frame( Year= as.integer(format(lake_data_all$time, "%Y") ))
    months.tmp = data.frame(Month = as.integer(format(lake_data_all$time, "%m") ))
    
    #Get the year alphabet
    yrss = unique(years.tmp$Year)
    nyrs = length(yrss)
    myrs = min(yrss)

    #Use this function from Keras
    month_encoded =  to_categorical(months.tmp$Month-1, num_classes = 12)
    colnames(month_encoded) = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
              "Aug", "Sep", "Oct", "Nov", "Dec")
    year_encoded = to_categorical(years.tmp$Year-myrs, num_classes = nyrs)
    yr_names = vector("character",nyrs)
    for(t in 1:nyrs){
        yr_names[t] = paste("Y",t,sep="")
    }
    colnames(year_encoded) = yr_names 

    lake_data_lstm[[n]] = cbind(lake_data_lstm[[n]],month_encoded, year_encoded )


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
  updateGAM_pred(lake_data, fut_precip)

  for(n in 1:n_lakes){

    #This is the github URL for the data. 
    giturl = paste("./data/gam_",n,"_forecast.csv", sep="")
    pred_lakes[[n]] = tail(read.csv((giturl)),lagsp )
    pred_lakes[[n]]$time = as.Date(ymd(pred_lakes[[n]]$time)) 
    # print(lagsp)
    print((pred_lakes[[n]]))
  
  }
  })

  #Add the readable dates back on for plotting
  for (n in 1:n_lakes){
    lake_data[[n]]$dates = lake_dates[[n]] 
  }

  ##############################################################
  #PART 3: Forecasting with RNN (LSTM) 
  ##############################################################
  #Check to see if the RNN  exists, and whether it has already 
  #been updated and predictions made:
  withProgress(message = 'Forecasting RNN (might take awhile)', value = 0, {
 
  #For now, choose one: 
  #updateModelLSTM(lake_data_lstm)
  #m_use_type = "LSTM"
 
  # updateModelDNN(lake_data_lstm,  fut_precip_scaled)
  # m_use_type = "DNN"
  
  updateModelCNNLSTM(lake_data_lstm, fut_precip_scaled)
  m_use_type = "CNNLSTM"

  #load(file = "todays_forecast.var")
  
  for(n in 1:n_lakes){
    giturl = paste("./data/lakemodel_",n,"_",m_use_type,"forecast.csv",sep ="")
    lake_models_forecast[[n]] = tail(read.csv(giturl),lagsp+1)  

    #Get the last date from the historical data. 
    last_day = tail(lake_data[[n]],1)

    #Because the forecasts are generated as a kind of posterior 
    #draw, get the average and the SE.  
    lm_tmp = lake_models_forecast[[n]]*scale_ll[[n]][2]+
              scale_ll[[n]][1] 
    lm_m = rowMeans(lm_tmp)
    lm_se = sqrt( apply((lm_tmp),1,var) )*1E2

    lake_models_forecast[[n]] = data.frame(time = c(last_day$dates, fut_precip$time), 
              level = lm_m, se = lm_se )

    #Align the last day of data with the prediciton using an extra
    #scaling factor: 
    sf = last_day$level/lake_models_forecast[[n]]$level[1]
    lake_models_forecast[[n]]$level = lake_models_forecast[[n]]$level*sf
    lake_models_forecast[[n]] = lake_models_forecast[[n]][c(-1),]

    print(lake_models_forecast[[n]])

  }
})
  ##############################################################
  #PART 3: Build out the UI
  ##############################################################

  ########### ###################################################
  #Mendota
  ##############################################################
  #Max lake level
  mpm = max(pred_lakes[[1]]$level)
  mr = max(fut_precip$rn)
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
        aes(x = time, ymin = level-se*1.96, ymax = level+se*1.96), fill="red", alpha = 0.2)+
      geom_line(data = lake_models_forecast[[1]], 
        aes(x = time, y=level),col = "blue",linetype = "dotted")+
      geom_point(data = lake_models_forecast[[1]], 
        aes(x = time, y=level),col = "blue")+
      geom_ribbon(data =lake_models_forecast[[1]], 
        aes(x = time, ymin = level-se*1.96, ymax = level+se*1.96), fill="blue", alpha = 0.2)+
      ylim(max(pred_lakes[[1]]$level)-max(pred_lakes[[1]]$level)*.01, 
        max(pred_lakes[[1]]$level)+max(pred_lakes[[1]]$level)*.01)+
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
        aes(x = time, ymin = level-se*1.96, ymax = level+se*1.96), fill = "red", alpha = 0.2)+
      geom_line(data = lake_models_forecast[[1]], 
        aes(x = time, y=level),col = "blue")+
      geom_ribbon(data =lake_models_forecast[[1]], 
        aes(x = time, ymin = level-se*1.96, ymax = level+se*1.96), fill ="blue", alpha = 0.2)+
      ylim( min(lake_data[[1]]$level ), thresh_100[1] +1 )+
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
if( mpm2 < thresh_10[2] ) { col_use2 = flash_col[1]}
if( mpm2 >= thresh_10[2] && mpm2 < thresh_100[2] ){ 
  col_use2 = flash_col[2]}
if( mpm2 >= thresh_100[2] ){ col_use2 = flash_col[3]}

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
      geom_ribbon(data = pred_lakes[[2]], 
        aes(x = time, ymin = level-se*1.96, ymax = level+se*1.96), fill = "red", alpha = 0.2)+
      geom_line(data = lake_models_forecast[[2]], 
        aes(x = time, y=level),col = "blue", linetype = "dotted") +
      geom_point(data = lake_models_forecast[[2]], 
        aes(x = time, y=level),col = "blue") +
      geom_ribbon(data =lake_models_forecast[[2]], 
        aes(x = time, ymin = level-se*1.96, ymax = level+se*1.96), fill="blue", alpha = 0.2)+
      ylim(max(pred_lakes[[2]]$level)-max(pred_lakes[[2]]$level)*0.01, 
        max(pred_lakes[[2]]$level)+max(pred_lakes[[2]]$level)*0.01)+
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
        aes(x = time, ymin = level-se*1.96, ymax = level+se*1.96), fill= "red", alpha = 0.2)+
      geom_ribbon(data = lake_models_forecast[[2]], 
        aes(x = time, ymin = level-se*1.96, ymax = level+se*1.96),fill ="blue", alpha = 0.2)+
      ylim(min(lake_data[[2]]$level ), thresh_100[2] +1 )+      
      theme_minimal() + theme(text=element_text(size=21)) +
      xlab("Date")+
      ylab("Lake level (ft) ") +
      xlim(low_limx2$limx2, current_date+8)+
      geom_hline(yintercept=thresh_10[2], col="orange", linetype = "dotted")+
      geom_hline(yintercept=thresh_100[2], col="red", linetype = "dotted")

   } )

##############################################################
#Waubesa
##############################################################
#Max lake level
mpm3 = max(pred_lakes[[3]]$level)
col_use3  = "aqua"
#Check against the flood levels: 
if( mpm3 < thresh_10[1] ) { col_use2 = flash_col[1]}
if( mpm3 >= thresh_10[1] && mpm3 < thresh_100[1] ){ 
  col_use2 = flash_col[2]}
if( mpm3 >= thresh_100[1] ){ col_use2 = flash_col[3]}

 #The maximum rainfall in the upcoming days 
    output$max_rain3 = renderInfoBox({
    mr = max(fut_precip$rn)

    infoBox(
     "Peak 7-day rainfall (in)",
      value = formatC(mr, digits = 1, format = "f"),
      icon = icon("cloud-rain"),
      color = col_use3#if (mr >= year10) "yellow" else "aqua"
    )
  })  

    #The peak lake-level 
    output$max_peak_wau = renderInfoBox({

      infoBox(
        "Peak 7-day lake (ft)",
        value = formatC(mpm3, digits = 1, format = "f"),
        icon = icon("water"),
        color = col_use3 #if (mr >= year10) "yellow" else "aqua"
      )
    })  
 
    #The confidence interval 
    output$pred_con_wau= renderInfoBox({
      #Get the max from the time-series
      mpm = max(pred_lakes[[3]]$level)
      #Get its se
      se_wau  = pred_lakes[[3]]$se[which(pred_lakes[[3]]$level == mpm)]
      #95% CIs 
      pcwau = 1.96*se_wau
      
      infoBox(
        "95% CI",
        value = formatC(pcwau, digits = 1, format = "f"),
        icon = icon("chart-line"),
        color = col_use3 #if (mr >= year10) "yellow" else "aqua"
      )
    })  

      #Plot the time series of predictions

  output$pred_plot3=renderPlot({
      ggplot( ) + 
      geom_line(data = pred_lakes[[3]], aes(x = time, y=level),
        col="red", linetype = "dotted") +
      geom_point(data = pred_lakes[[3]], aes(x = time, y=level),
        col="red") +
      geom_ribbon(data = pred_lakes[[3]], 
        aes(x = time, ymin = level-se*1.96, ymax = level+se*1.96), fill = "red", alpha = 0.2)+
      geom_line(data = lake_models_forecast[[3]], 
        aes(x = time, y=level),col = "blue", linetype = "dotted") +
      geom_point(data = lake_models_forecast[[3]], 
        aes(x = time, y=level),col = "blue") +
      geom_ribbon(data =lake_models_forecast[[3]], 
        aes(x = time, ymin = level-se*1.96, ymax = level+se*1.96), fill="blue", alpha = 0.2)+
      ylim(max(pred_lakes[[3]]$level)-max(pred_lakes[[3]]$level)*0.01, 
        max(pred_lakes[[3]]$level)+max(pred_lakes[[3]]$level)*0.01)+
      theme_minimal() + theme(text=element_text(size=21)) +
      ggtitle("Forecasted lake level") + xlab("Date")+
      ylab("Lake level (ft) ")+
      geom_hline(yintercept=thresh_10[1], col="orange", linetype = "dotted")+
      geom_hline(yintercept=thresh_100[1], col="red", linetype = "dotted")
   } )

#Plot the historical and prediction time series

  low_limx3 = reactiveValues(limx3 = current_date%m-%months(3) )

  observeEvent(input$m032, {
    low_limx3$limx3 = current_date%m-%months(3)
  })

  observeEvent(input$yr12, {
    low_limx3$limx3 = current_date - years(1)
  })  

  observeEvent(input$yr32, {
    low_limx3$limx3 = current_date - years(3)
  })

  observeEvent(input$yr102, {
    low_limx3$limx3 = current_date - years(10)
  })  


  output$full_plot3=renderPlot({

      ggplot( ) +
      geom_line(data = lake_data[[3]], aes(x = dates, y=level) ) +
      geom_line(data = pred_lakes[[3]], aes(x = time, y=level), col="red") +
      geom_line(data = lake_models_forecast[[3]], 
        aes(x = time, y=level),col = "blue")+
      geom_ribbon(data = pred_lakes[[3]], 
        aes(x = time, ymin = level-se*1.96, ymax = level+se*1.96), fill= "red", alpha = 0.2)+
      geom_ribbon(data = lake_models_forecast[[3]], 
        aes(x = time, ymin = level-se*1.96, ymax = level+se*1.96),fill ="blue", alpha = 0.2)+
      ylim(min(lake_data[[3]]$level ), thresh_100[1] +1 )+      
      theme_minimal() + theme(text=element_text(size=21)) +
      xlab("Date")+
      ylab("Lake level (ft) ") +
      xlim(low_limx3$limx3, current_date+8)+
      geom_hline(yintercept=thresh_10[1], col="orange", linetype = "dotted")+
      geom_hline(yintercept=thresh_100[1], col="red", linetype = "dotted")

   } )

##############################################################
#Kegonsa
##############################################################
#Max lake level
mpm4 = max(pred_lakes[[4]]$level)
col_use4  = "aqua"
#Check against the flood levels: 
if( mpm4 < thresh_10[1] ) { col_use4 = flash_col[1]}
if( mpm4 >= thresh_10[1] && mpm4 < thresh_100[1] ){ 
  col_use4 = flash_col[2]}
if( mpm4 >= thresh_100[1] ){ col_use4 = flash_col[3]}

 #The maximum rainfall in the upcoming days 
    output$max_rain4 = renderInfoBox({
    mr = max(fut_precip$rn)

    infoBox(
     "Peak 7-day rainfall (in)",
      value = formatC(mr, digits = 1, format = "f"),
      icon = icon("cloud-rain"),
      color = col_use4 #if (mr >= year10) "yellow" else "aqua"
    )
  })  

    #The peak lake-level 
    output$max_peak_keg = renderInfoBox({

      infoBox(
        "Peak 7-day lake (ft)",
        value = formatC(mpm4, digits = 1, format = "f"),
        icon = icon("water"),
        color = col_use4 #if (mr >= year10) "yellow" else "aqua"
      )
    })  
 
    #The confidence interval 
    output$pred_con_keg = renderInfoBox({
      #Get the max from the time-series
      mpm = max(pred_lakes[[4]]$level)
      #Get its se
      se_keg  = pred_lakes[[4]]$se[which(pred_lakes[[4]]$level == mpm)]
      #95% CIs 
      pckeg = 1.96*se_keg
      
      infoBox(
        "95% CI",
        value = formatC(pckeg, digits = 1, format = "f"),
        icon = icon("chart-line"),
        color = col_use4 #if (mr >= year10) "yellow" else "aqua"
      )
    })  

      #Plot the time series of predictions

  output$pred_plot4=renderPlot({
      ggplot( ) + 
      geom_line(data = pred_lakes[[4]], aes(x = time, y=level),
        col="red", linetype = "dotted") +
      geom_point(data = pred_lakes[[4]], aes(x = time, y=level),
        col="red") +
      geom_ribbon(data = pred_lakes[[4]], 
        aes(x = time, ymin = level-se*1.96, ymax = level+se*1.96), fill = "red", alpha = 0.2)+
      geom_line(data = lake_models_forecast[[4]], 
        aes(x = time, y=level),col = "blue", linetype = "dotted") +
      geom_point(data = lake_models_forecast[[4]], 
        aes(x = time, y=level),col = "blue") +
      geom_ribbon(data =lake_models_forecast[[4]], 
        aes(x = time, ymin = level-se*1.96, ymax = level+se*1.96), fill="blue", alpha = 0.2)+
      ylim(max(pred_lakes[[4]]$level)-max(pred_lakes[[4]]$level)*0.01, 
        max(pred_lakes[[4]]$level)+max(pred_lakes[[4]]$level)*0.01)+
      theme_minimal() + theme(text=element_text(size=21)) +
      ggtitle("Forecasted lake level") + xlab("Date")+
      ylab("Lake level (ft) ")+
      geom_hline(yintercept=thresh_10[1], col="orange", linetype = "dotted")+
      geom_hline(yintercept=thresh_100[1], col="red", linetype = "dotted")
   } )

#Plot the historical and prediction time series

  low_limx4 = reactiveValues(limx4 = current_date%m-%months(3) )

  observeEvent(input$m032, {
    low_limx4$limx4 = current_date%m-%months(3)
  })

  observeEvent(input$yr12, {
    low_limx4$limx4 = current_date - years(1)
  })  

  observeEvent(input$yr32, {
    low_limx4$limx4 = current_date - years(3)
  })

  observeEvent(input$yr102, {
    low_limx4$limx4 = current_date - years(10)
  })  


  output$full_plot4=renderPlot({

      ggplot( ) +
      geom_line(data = lake_data[[4]], aes(x = dates, y=level) ) +
      geom_line(data = pred_lakes[[4]], aes(x = time, y=level), col="red") +
      geom_line(data = lake_models_forecast[[4]], 
        aes(x = time, y=level),col = "blue")+
      geom_ribbon(data = pred_lakes[[4]], 
        aes(x = time, ymin = level-se*1.96, ymax = level+se*1.96), fill= "red", alpha = 0.2)+
      geom_ribbon(data = lake_models_forecast[[4]], 
        aes(x = time, ymin = level-se*1.96, ymax = level+se*1.96),fill ="blue", alpha = 0.2)+
      ylim(min(lake_data[[4]]$level ), thresh_100[1] +1 )+      
      theme_minimal() + theme(text=element_text(size=21)) +
      xlab("Date")+
      ylab("Lake level (ft) ") +
      xlim(low_limx4$limx4, current_date+8)+
      geom_hline(yintercept=thresh_10[1], col="orange", linetype = "dotted")+
      geom_hline(yintercept=thresh_100[1], col="red", linetype = "dotted")

   } )





} ##Server function end

