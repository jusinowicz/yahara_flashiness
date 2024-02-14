library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(curl)
#Data processing
library(nasapower) #API for NASA data, for precipitation
library(openmeteo)
#"todays_forecast.var"
#misc data processing and stats
source("./functions/flash_functions.R")

##############################################################
#Global variables
##############################################################
#USGS site keys. Currently includes Mendota, Monona, Waubesa, 
#Kegonsa, (not Wingra "425715089164700")
site_keys = c("05428000", "05429000","425715089164700",  "05429485")
n_lakes = length(site_keys)

#The base urls used by the USGS for lake data
url_base = c("https://waterservices.usgs.gov/nwis/iv/?format=rdb&sites=")

#Parameters from the NASA POWER data collection
nasa_pars = c("PRECTOTCORR")

#Lake prefixes
lake_pre = c("men","mon","wau","keg")
#Gauge heights
g_h = c(839.96, 839.86, 840.01, 839.91)

#Where the historical data should start
#How long of a data set? Currently 10 years
#real_start = list( ymd("1980-1-1"), ymd("1980-1-1")) #Too big to save
real_start = list( ymd("2013-1-1"), ymd("2013-1-1"), ymd("2013-1-1"),
              ymd("2013-1-1") )

#Today
current_date = ymd( Sys.Date() )
  
#Where the lake stage and rain data live: 
lake_data = vector("list", n_lakes) #GAM formatted
lake_data_temp = vector("list", n_lakes) #LSTM formatted
lake_data_tempG = vector("list", n_lakes) #LSTM formatted
lake_data_lstm = vector("list", n_lakes) #LSTM formatted

#Models:
lake_models = vector("list", n_lakes) 

#Forecasts:
lake_models_forecast = vector("list", n_lakes) 
lake_forecast_dnn = vector("list", n_lakes) #DNN 
lake_forecast_lstm = vector("list", n_lakes) #LSTM 
pred_lakes = vector("list", n_lakes)

#Max lags in rain and lake-level data
lags = 10

#If the fitted GAM model is already known, then describe each model:
model_form = vector("list", n_lakes)
#For when the AR is fit separately i.e. fit fitGAM_ar()
model_form [[1]] = "level ~ 
  s(rn,bs=\"cr\",k=6)+s(rn1,bs=\"cr\",k=6)+
  s(rn2,bs=\"cr\",k=6)+s(rn3,bs=\"cr\",k=6)+
  te(rn,rn1,k=20)+te(rn1,rn2,k=20)+te(rn2,rn3,k=20)"

model_form [[2]] = "level ~ 
  s(rn,bs=\"cr\",k=6)+s(rn1,bs=\"cr\",k=6)+
  s(rn2,bs=\"cr\",k=6)+s(rn3,bs=\"cr\",k=6)+
  te(rn,rn1,k=20)+te(rn1,rn2,k=20)+te(rn2,rn3,k=20)"

model_form [[3]] = "level ~ 
  s(rn,bs=\"cr\",k=6)+s(rn1,bs=\"cr\",k=6)+
  s(rn2,bs=\"cr\",k=6)+s(rn3,bs=\"cr\",k=6)+
  te(rn,rn1,k=20)+te(rn1,rn2,k=20)+te(rn2,rn3,k=20)"

model_form [[4]] = "level ~ 
  s(rn,bs=\"cr\",k=6)+s(rn1,bs=\"cr\",k=6)+
  s(rn2,bs=\"cr\",k=6)+s(rn3,bs=\"cr\",k=6)+
  te(rn,rn1,k=20)+te(rn1,rn2,k=20)+te(rn2,rn3,k=20)"

#Versions from the RF fitting
model_form [[1]] = "level ~ 
  s(rn,bs=\"cr\",k=6)+s(rn1,bs=\"cr\",k=6)+
  s(rn2,bs=\"cr\",k=6)+s(rn3,bs=\"cr\",k=6)+
  s(mon1,bs=\"cr\",k=6)+s(mon2,bs=\"cr\",k=6)+
  s(mon3,bs=\"cr\",k=6)+s(mon4,bs=\"cr\",k=6)+s(mon5,bs=\"cr\",k=6)+
  s(wau1,bs=\"cr\",k=6)+s(wau2,bs=\"cr\",k=6)+
  s(keg1,bs=\"cr\",k=6)+s(keg2,bs=\"cr\",k=6)+
  s(keg3,bs=\"cr\",k=6)+s(keg4,bs=\"cr\",k=6)+
  s(keg5,bs=\"cr\",k=6)+s(keg6,bs=\"cr\",k=6)+
  te(rn,rn1,k=20)+te(rn1,rn2,k=20)+te(rn2,rn3,k=20)"

model_form [[2]] = "level ~ 
  s(rn,bs=\"cr\",k=6)+s(rn1,bs=\"cr\",k=6)+
  s(rn2,bs=\"cr\",k=6)+s(rn3,bs=\"cr\",k=6)+
  s(men1,bs=\"cr\",k=6)+s(men2,bs=\"cr\",k=6)+
  s(men3,bs=\"cr\",k=6)+s(men4,bs=\"cr\",k=6)+
  s(wau1,bs=\"cr\",k=6)+s(wau2,bs=\"cr\",k=6)+
  s(wau3,bs=\"cr\",k=6)+s(wau4,bs=\"cr\",k=6)+
  s(wau5,bs=\"cr\",k=6)+
  s(keg1,bs=\"cr\",k=6)+
  te(rn,rn1,k=20)+te(rn1,rn2,k=20)+te(rn2,rn3,k=20)"

model_form [[3]] = "level ~ 
  s(rn,bs=\"cr\",k=6)+s(rn1,bs=\"cr\",k=6)+
  s(rn2,bs=\"cr\",k=6)+s(rn3,bs=\"cr\",k=6)+
  s(men1,bs=\"cr\",k=6)+s(men2,bs=\"cr\",k=6)+
  s(men3,bs=\"cr\",k=6)+s(men4,bs=\"cr\",k=6)+
  s(men5,bs=\"cr\",k=6)+s(men6,bs=\"cr\",k=6)+
  s(mon1,bs=\"cr\",k=6)+s(mon2,bs=\"cr\",k=6)+
  s(keg1,bs=\"cr\",k=6)+s(keg2,bs=\"cr\",k=6)+
  te(rn,rn1,k=20)+te(rn1,rn2,k=20)+te(rn2,rn3,k=20)"

model_form [[4]] = "level ~ 
  s(rn,bs=\"cr\",k=6)+s(rn1,bs=\"cr\",k=6)+
  s(rn2,bs=\"cr\",k=6)+s(rn3,bs=\"cr\",k=6)+
  s(men1,bs=\"cr\",k=6)+s(men2,bs=\"cr\",k=6)+
  s(men3,bs=\"cr\",k=6)+
  s(wau1,bs=\"cr\",k=6)+s(wau3,bs=\"cr\",k=6)+
  te(rn,rn1,k=20)+te(rn1,rn2,k=20)+te(rn2,rn3,k=20)"

#Flooding thresholds from Usinowicz et al. 2016
#For Mendota, gauge at 839.96 ft. 
#			10% = 259.64m = 851.84 ft.
#			100% = 259.82m = 852.43 ft
#For Monona, gauge at 839.86 
#			10% = 258.17 = 847.01 ft
#			100% = 258.40 = 847.77 ft.

# thresh_10 = c((851.84 - 839.96), (847.01 - 839.96))
# thresh_100 = c((852.43 - 839.86), (847.77 - 839.86))

thresh_10 = c((851.84 ), (847.01 ))
thresh_100 = c((852.43 ), (847.77))


#To color code max lake level, according to flood threat level
flash_col= c("aqua", "orange", "red")

##############################################################
#Global functions 
##############################################################

###############################################################################
#This is a helper function for lake level. If the data is not up to date, do
#all of the necessary data procesing. 
###############################################################################

updateLake = function (lake_table, start_date, current_date, site_key, gh) {

  #Use the USGS address format to get data over the date range (62615, 00065)

  #Make a special case for Monona, which does not seem to have data
  #as elevation, but only relative to gauge. 
  url1 = paste(url_base[1], site_key, "&startDT=", start_date,
  "&endDT=",current_date,"&parameterCd=00060,00065&siteStatus=all",sep="" )
  lt_temp = as.data.frame(read_delim(print(url1), comment = "#", delim="\t"))
  #Add 839.86 to gauge height
  lt_temp[,5] = as.numeric(as.character(lt_temp[,5]))
  lt_temp[,5] = lt_temp[,5] + gh

  lt_temp = lt_temp[-1,]
  lt_temp[,"datetime"] = as.POSIXct(lt_temp[,"datetime"],
                       format = '%Y-%m-%d %H:%M')

  lt_temp = lt_temp[,c(3, 5)]
  colnames(lt_temp ) = c("time", "level")

  #Data seem to be at 15 min intervals by default. Aggregate these into 
  #a mean daily level
  lt_temp = lt_temp %>%
      mutate(time = as.Date(ymd_hms(time))) %>%
      group_by(time) %>%
      summarise(level = mean(level, na.rm = TRUE)) %>%
      as.data.frame()

  return(lt_temp)

}

###############################################################################
#This is a helper function for rain. If the data is not up to date, do
#all of the necessary data procesing. 
###############################################################################

updateRain = function (daily_rain, start_date, current_date) {

  #Get the matching precipitation data from the NASA POWER collection
  rn_temp = get_power(
    community = "ag",
    lonlat = c(43.0930, -89.3727),
    pars =  nasa_pars,
    dates = c(paste(start_date), paste(current_date)),
    temporal_api = "daily"
  ) %>% as.data.frame()
  rn_temp = rn_temp[,c(7,8)]
  colnames(rn_temp) = c("time", "rn")

  return(rn_temp)

}

###############################################################################
# Load the historical data and check whether it is up to date. 
###############################################################################

updateHistoric = function() {

  #Preallocate the important historical data tables 
  lake_table = vector("list", n_lakes)
  daily_precip = NULL

  #Last dates that appear in the data sets
  last_date = vector("list",n_lakes)
  last_rain = NULL

  

  for (n in 1:n_lakes){
    #file name
    fn = paste("./data/",lake_pre[n], "_hist.csv",sep="")

    #Load the current data files
    current_date = ymd( Sys.Date() ) #Current date. Could be set to other
    lake_table[[n]] = read.csv(file = paste(fn))
  
    #This is necessary because R ts objects convert time to decimal
    #format. Sometimes this pops up as an issue. 
    if (is.double(lake_table[[n]][,"time"])) {
      lake_table[[n]][,"time"] = lake_table[[n]][,"time"] %>% 
                                  as.numeric() %>% 
                                  date_decimal() %>% 
                                  as_date()
    }else{
      lake_table[[n]][,"time"] = ymd(lake_table[[n]][,"time"])
    }
  
    #Make backups of previous file:
    fn_back = paste("./data/",lake_pre[n], "_hist.csv.bck",sep="")
    file.copy(from = fn, to =fn_back)
  }
    
  daily_precip = read.csv(file = "./data/rain_hist.csv")
  daily_precip[,"time"] = ymd(daily_precip[,"time"])

  #Make backups of previous file:
  file.copy(from = "./data/rain_hist.csv", to ="./data/rain_hist.csv.bck")

  #Get the last dates entered. If they don't match to the current date
  #then update the data with the functions updateLake and updateRain. 
  #Write the new files.  

  #For the precip first (same data for all lakes)
  last_rain = daily_precip[nrow(daily_precip),"time"]
  if(last_rain != current_date  ){ 
    daily_precip = rbind(daily_precip,
        updateRain(daily_precip, start_date = last_rain, 
          current_date = current_date) )
    daily_precip = daily_precip[-( (nrow(daily_precip)-1):
      nrow(daily_precip)), ]
  }

  #Remove date duplicates
  daily_precip=daily_precip[!duplicated(daily_precip$time),]


  #For the lakes
  for ( n in 1:n_lakes){ 
   
    #file name
    fn = paste("./data/",lake_pre[n], "_hist.csv",sep="")

    site_key = site_keys[n]

    last_date[[n]] = lake_table[[n]][nrow(lake_table[[n]]),"time"]
    
    if(last_date[[n]] != current_date  ){ 
      lake_table[[n]] = rbind(lake_table[[n]],
          updateLake(lake_table[[n]], start_date = last_date[[n]], 
          current_date = current_date, site_key = site_key, 
          gh = g_h[n] ) )  
      lake_table[[n]] = lake_table[[n]][-( (nrow(lake_table[[n]])-2):
        nrow(lake_table[[n]])), ]
    }

     # lake_table[[n]] = 
     #      updateLake(lake_table[[n]], start_date = start_date, 
     #      current_date = current_date, site_key = site_key, gh = g_h[n] )  

    #Remove duplicated dates
  	lake_table[[n]] = lake_table[[n]][!duplicated(lake_table[[n]]$time),]

    lake_data[[n]] = lake_table[[n]] %>%
        inner_join(daily_precip, by = "time" ) 

    write.table(lake_data[[n]][,1:2], file = paste(fn), sep=",")

  }

  write.table(lake_data[[1]][,c(1,3)], file = "./data/rain_hist.csv", sep=",")

}

###############################################################################
# GAM Section
###############################################################################

###############################################################################
# updateModel is the function to take fit a GAM model if one does not already
# exist. It is effectively a wrapper for the fuction fitGAM in the 
# flash_functions.R file. It looks to see if models already exist. 
# It returns either (or both) an LP matrix and the R variable for the fitted
# models, depending on how the code below is commented.
#
###############################################################################

updateModel = function (lake_data, model_form){

  #First check to see if the fitted models already exist. If they don't, 
  #run the code to fit the models. This is time consuming! 
  #The standard I have chosen to employ is to name stored fitted models 
  #with suffix ".var"
  model_files = list.files("./data/")
  model_true = grepl("*GAM*.*var|*var.*GAM*", model_files)

  #Test if thefile exists
  if(  sum(model_true) >= 1 ){   
    
    #Which are the model files? 
    model_files = model_files[model_true == TRUE ]
    n_files = length(model_files)
    #Loop and load the files 
    for ( n in 1:n_files ){ 
      x_tmp = load(paste("./data/", model_files[n],sep="") )
      lake_models[[n]] = get(x_tmp)
    }
  
  }else{ 

    #We have to fit the models. We fit the AR and the GAM 
    #separately because forecasting seems to be better this way.
    #Use this function:
    lake_models = fitGAM_ar(lake_data, model_form)
    
    #Because these are so large when saved as a variable,
    #we don't save the variables but use the Lp matrix
    #and model coefficients to represent the model and 
    #make future predictions
    #model_saves = c("mendotaGAM1.var","mononaGAM1.var")
    #for(n in 1:n_lakes){ mod.tmp = new_models[[n]]; 
    #         saveRDS(mod.tmp,file = paste(model_saves[[n]]) )} 

    #These are the two model compenents we'll save:
    new_models = lake_models 
    models_Xp = vector("list", n_lakes)
    models_coef = vector("list", n_lakes)
    models_Vp = vector("list", n_lakes)
    model_smooths =  vector("list", n_lakes)

    for (n in 1:n_lakes){
      #Get the model summary to extract names of terms:
      sum_tmp = summary(new_models[[n]]$gam) 
      model_smooths[[n]] =  rownames(sum_tmp$s.table)

      #Create a new data frame that essentially creates an 
      #equally spaced grid over the range of each variable 
      #in the data set. Then get the Lp matrix from that to 
      #use for prediction. 

      models_Xp[[n]] = predict(new_models[[n]]$gam,lake_data[[n]],type="lpmatrix" )
      #Model coefficients
      models_coef[[n]] = coef(new_models[[n]]$gam)
      #Variance matrix
      models_Vp[[n]] = new_models[[n]]$gam$Vp
    }

    #save(file = "lakeGAMsLpB.var", model_smooths, models_Xp, models_coef, models_Vp )
    #save(file = "./data/lakeGAMsfull.var", lake_models) #Too big? :(

    for(n in 1:n_lakes) {
      fsave = paste("./data/lakeGAMsfull",n,".var", sep="")
      model_tmp = lake_models[[n]]
      save(file = fsave, model_tmp ) #Too big? :(
    }

  }

}

###############################################################################
# predictFlashGAM uses the fitted GAM to predict future rain
# It returns fitted data points with SE for the number of future precipitation 
# events that have been given to it. 
###############################################################################
updateGAM_pred = function(lake_data, fut_precip){

    #Where the fitted model coefficients and Lp matrix live
    model_files = list.files("./data/")
    model_true = grepl("*GAM*.*var|*var.*GAM*", model_files)

    #Which are the model files? 
    model_files = model_files[model_true == TRUE ]
    n_files = length(model_files)
    #Loop and load the files 
    for ( n in 1:n_files ){ 
      x_tmp = load(paste("./data/", model_files[n],sep="") )
      lake_models[[n]] = get(x_tmp)
    }

    predictFlashGAM(lake_data, fut_precip, lake_models)

}

###############################################################################
# ML Section
###############################################################################

###############################################################################
# updateModelLSTM is the function to fit and update the LSTM RNN with keras . 
# It is effectively a wrapper for the keras functions.
# In its current iteration, this just needs to be run daily to train the model
# on new data and generate new predictions. 
###############################################################################

###############################################################################
# 
###############################################################################

updateModelLSTM = function(lake_data_lstm, lagsp = 7 ){

  #First check to see if the fitted models already exist. If they don't, 
  #run the code to fit the models. This is time consuming! 
  #The standard I have chosen to employ is to name stored fitted models 
  #with suffix ".var"
  model_files = list.files("./LSTM/")
  model_true = grepl("*LSTM*.*tf|*tf.*LSTM*", model_files)

  #Test if the files/folders exist and have been updated today
  if(  sum(model_true) >= 1 ){   
    
    #Which are the model files? 
    model_files = model_files[model_true == TRUE ]
    n_files = length(model_files)

    #Were they created today? 
    tyes = 0 
    for (f in 1:n_files){
    	#Get the dates on model files
    	get_dates = file.info(list.files(
    		paste("./LSTM/",model_files[f],sep=""),
    		full.names=T))$mtime
    	#Find the latest date
    	latest_date = max(as_date (get_dates))
    	#Check it against today
    	if( latest_date == current_date){ tyes = tyes +1 }
    }

    #If fewer than n_files have been updated then run the updates
    if ( tyes < n_files){
		 	#We have to update each model.This is a wrapper for keras
		    fit_predLSTM(lake_data_lstm, lagsp ) 
		} 
 
  }else{ 
    #We have to fit the models.This is a wrapper for keras
	fit_predLSTM(lake_data_lstm, lagsp ) 
  }

}


###############################################################################
# 
###############################################################################

updateModelDNN = function(lake_data_lstm,  fut_precip_scaled, lagsp = 7 ){

  #First check to see if the fitted models already exist. If they don't, 
  #run the code to fit the models. This is time consuming! 
  #The standard I have chosen to employ is to name stored fitted models 
  #with suffix ".var"
  model_files = list.files("./DNN/")
  model_true = grepl("*DNN*.*tf|*tf.*DNN*", model_files)

  #Test if the files/folders exist and have been updated today
  if(  sum(model_true) >= 1 ){   
    
    #Which are the model files? 
    model_files = model_files[model_true == TRUE ]
    n_files = length(model_files)

    #Were they created today? 
    tyes = 0 
    for (f in 1:n_files){
      #Get the dates on model files
      get_dates = file.info(list.files(
        paste("./DNN/",model_files[f],sep=""),
        full.names=T))$mtime
      #Find the latest date
      latest_date = max(as_date (get_dates))
      #Check it against today
      if( latest_date == current_date){ tyes = tyes +1 }
    }

    #If fewer than n_files have been updated then run the updates
    if ( tyes < n_files){
      #We have to update each model.This is a wrapper for keras
      fit_predDNN(lake_data_lstm, fut_precip_scaled, lagsp ) 
    } 
 
  }else{ 
    #We have to fit the models.This is a wrapper for keras
    fit_predDNN(lake_data_lstm, fut_precip_scaled, lagsp ) 
  }

}


###############################################################################
# 
###############################################################################

updateModelCNNLSTM = function(lake_data_lstm, fut_precip_scaled, lagsp = 7 ){

  #First check to see if the fitted models already exist. If they don't, 
  #run the code to fit the models. This is time consuming! 
  #The standard I have chosen to employ is to name stored fitted models 
  #with suffix ".var"
  model_files = list.files("./CNNLSTM/")
  model_true = grepl("*CNNLSTM*.*tf|*tf.*CNNLSTM*", model_files)

  #Test if the files/folders exist and have been updated today
  if(  sum(model_true) >= 1 ){   
    
    #Which are the model files? 
    model_files = model_files[model_true == TRUE ]
    n_files = length(model_files)

    #Were they created today? 
    tyes = 0 
    for (f in 1:n_files){
      #Get the dates on model files
      get_dates = file.info(list.files(
        paste("./CNNLSTM/",model_files[f],sep=""),
        full.names=T))$mtime
      #Find the latest date
      latest_date = max(as_date (get_dates))
      #Check it against today
      if( latest_date == current_date){ tyes = tyes +1 }
    }

    #If fewer than n_files have been updated then run the updates
    if ( tyes < n_files){
      #We have to update each model.This is a wrapper for keras
      fit_predCNNLSTM(lake_data_lstm, fut_precip_scaled, lagsp ) 
    } 
 
  }else{ 
    #We have to fit the models.This is a wrapper for keras
    fit_predCNNLSTM(lake_data_lstm, fut_precip_scaled, lagsp ) 
  }

}