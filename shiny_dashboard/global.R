library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
#Data processing
library(nasapower) #API for NASA data, for precipitation
library(openmeteo)
#Stats
library(mgcv)
library(fGarch) #For GARCH models
#misc data processing and stats
source("./../functions/flash_functions.R")

##############################################################
#Global variables
##############################################################
#USGS site keys. Currently includes Mendota and Monona
site_keys = c("05428000", "05429000")
n_lakes = length(site_keys)

#The base urls used by the USGS for lake data
url_base = c("https://waterservices.usgs.gov/nwis/iv/?format=rdb&sites=")

#Parameters from the NASA POWER data collection
nasa_pars = c("PRECTOTCORR")

#Where the historical data should start
#How long of a data set? Currently 10 years
#real_start = list( ymd("1980-1-1"), ymd("1980-1-1")) #Too big to save
real_start = list( ymd("2013-1-1"), ymd("2013-1-1"))

#Today
current_date = ymd( Sys.Date() )
  
#Where the lake stage and rain data live: 
lake_data = vector("list", n_lakes)

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



##############################################################
#Global functions 
##############################################################

###############################################################################
#This is a helper function for lake level. If the data is not up to date, do
#all of the necessary data procesing. 
###############################################################################

updateLake = function (lake_table, start_date, current_date, site_key) {

  #Use the USGS address format to get data over the date range
  url1 = paste(url_base[1], site_key, "&startDT=", start_date,
    "&endDT=",current_date,"&parameterCd=00060,00065&siteStatus=all",sep="" )
  
  lt_temp = as.data.frame(read_delim(print(url1), comment = "#", delim="\t"))
  lt_temp = lt_temp[-1,]
  lt_temp[,"datetime"] = as.POSIXct(lt_temp[,"datetime"],
                       format = '%Y-%m-%d %H:%M')
  lt_temp[,5] = as.numeric(as.character(lt_temp[,5]))

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

  #Load the current data files
  current_date = ymd( Sys.Date() ) #Current date. Could be set to other
  lake_table[[1]] = read.csv(file = "./../data/men_hist.csv")
  
  #This is necessary because R ts objects convert time to decimal
  #format. Sometimes this pops up as an issue. 
  if (is.double(lake_table[[1]][,"time"])) {
    lake_table[[1]][,"time"] = lake_table[[1]][,"time"] %>% 
                                as.numeric() %>% 
                                date_decimal() %>% 
                                as_date()
  }else{
    lake_table[[1]][,"time"] = ymd(lake_table[[1]][,"time"])
  }


  lake_table[[2]] = read.csv(file = "./../data/mon_hist.csv")
  if (is.double(lake_table[[2]][,"time"])) {
    lake_table[[2]][,"time"] = lake_table[[2]][,"time"] %>% 
                                as.numeric() %>% 
                                date_decimal() %>% 
                                as_date()
  }else{
    lake_table[[2]][,"time"] = ymd(lake_table[[2]][,"time"])
  }

  daily_precip = read.csv(file = "./../data/rain_hist.csv")
  daily_precip[,"time"] = ymd(daily_precip[,"time"])

  #Make backups of previous file:
  file.copy(from = "./../data/men_hist.csv", to ="./../data/men_hist.csv.bck")
  file.copy(from = "./../data/mon_hist.csv", to ="./../data/mon_hist.csv.bck")
  file.copy(from = "./../data/rain_hist.csv", to ="./../data/rain_hist.csv.bck")

  # file.copy(from = "men_hist.csv", to ="men_hist.csv.bck")
  # file.copy(from = "mon_hist.csv", to ="mon_hist.csv.bck")
  # file.copy(from = "rain_hist.csv", to ="rain_hist.csv.bck")


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
    
    site_key = site_keys[n]

    last_date[[n]] = lake_table[[n]][nrow(lake_table[[n]]),"time"]
    
    if(last_date[[n]] != current_date  ){ 
      lake_table[[n]] = rbind(lake_table[[n]],
          updateLake(lake_table[[n]], start_date = last_date[[n]], 
          current_date = current_date, site_key = site_key ) )
      lake_table[[n]] = lake_table[[n]][-( (nrow(lake_table[[n]])-2):
        nrow(lake_table[[n]])), ]
    }


    #Remove duplicated dates
  	lake_table[[n]] = lake_table[[n]][!duplicated(lake_table[[n]]$time),]

    lake_data[[n]] = lake_table[[n]] %>%
        inner_join(daily_precip, by = "time" ) 

  }

  write.table(lake_data[[1]][,1:2], file = "./../data/men_hist.csv", sep=",")
  write.table(lake_data[[2]][,1:2], file = "./../data/mon_hist.csv", sep=",")
  write.table(lake_data[[1]][,c(1,3)], file = "./../data/rain_hist.csv", sep=",")

  # write.table(lake_data[[1]][,1:2], file = "men_hist.csv", sep=",")
  # write.table(lake_data[[2]][,1:2], file = "mon_hist.csv", sep=",")
  # write.table(lake_data[[1]][,c(1,3)], file = "rain_hist.csv", sep=",")
}

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
  model_files = list.files("./")
  model_true = grepl("*GAM*.*var|*var.*GAM*", model_files)

  #Test if thefile exists
  if(  sum(model_true) >= 1 ){   
    
    #Which are the model files? 
    model_files = model_files[model_true == TRUE ]
    n_files = length(model_files)
    #Loop and load the files 
    for ( n in 1:n_files ){ 
      load(paste(model_files[n]) )
    }
  
  }else{ 

    #We have to fit the models. We fit the AR and the GAM 
    #separately because forecasting seems to be better this way.
    #Use this function:
    new_models = fitGAM_ar(lake_data, model_form)
    
    #Because these are so large when saved as a variable,
    #we don't save the variables but use the Lp matrix
    #and model coefficients to represent the model and 
    #make future predictions
    #model_saves = c("mendotaGAM1.var","mononaGAM1.var")
    #for(n in 1:n_lakes){ mod.tmp = new_models[[n]]; 
    #         saveRDS(mod.tmp,file = paste(model_saves[[n]]) )} 

    #These are the two model compenents we'll save: 
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
    save(file = "lakeGAMsfull.var", lake_models ) #Too big? :(


  }

}

###############################################################################
# predictFlashGAM uses the fitted GAM to predict future rain
# It returns fitted data points with SE for the number of future precipitation 
# events that have been given to it. 
###############################################################################
predictFlashGAM = function(lake_data, fut_precip){

    #Where the fitted model coefficients and Lp matrix live
    model_files = list.files("./")
    model_true = grepl("*GAM*.*var|*var.*GAM*", model_files)

    #Which are the model files? 
    model_files = model_files[model_true == TRUE ]
    n_files = length(model_files)
    #Loop and load the files 
    for ( n in 1:n_files ){ 
      load(paste(model_files[n]) )
    }

    #How many days are we forecasting? 
    n_days = dim(fut_precip)[1]

    #Most current time step
    ntime = (tail(lake_data[[1]],1))$time

    #Predicted lake levels 
    pred_lakes = vector("list",n_lakes)
    pred_lakes_ar = vector("list",n_lakes)

    #Build the new data set for prediction and make predictions:
    for (n in 1:n_lakes){ 

      #AR order of rain and lake level
      ar_lake = grep("level", (colnames(lake_data[[n]])))
      ar_lake = ar_lake[-1]
      ar_rain = grep("rn", (colnames(lake_data[[n]]))) 
      ar_rain = ar_rain[-1]
      l_arl = length (ar_lake)
      l_arr = length (ar_rain)

      #Which AR is larger? 
      if(l_arl>l_arr){ lar = l_arl}else{lar = l_arr}
      
      #Get the last section of data table for lags
      lt = tail(lake_data[[n]], l_arl)
      
      #Look for an NA in most recent rn, this happens
      if(sum(is.na(lt$rn)) > 0){  
        lt$rn[is.na(lt$rn)] = mean(lt$rn,na.rm=T)
      }

      #Version 3: An iterative prediction approach where
      #the AR is predicted first, then the GAM, then the 
      #two are added. 

      #The start of the new data set for prediction with 
      #the first new day
      lt_tmp = as.data.frame(c(ntime+1, NA, lt[l_arl,2:(l_arl+1)],
        fut_precip[1,2],
        lt[l_arl,(l_arl+3):(l_arl+2+l_arr) ] ))
      colnames(lt_tmp) = colnames(lt)
      lt_new = rbind( lt,lt_tmp) 
      
      #Initialize new data set
      lt_use = lt_new[l_arl+1,]
      ld_use = lake_data[[n]]
      lt_save = NULL

      #Temporarily store predicted lake level and SE
      pr_tmp = matrix(0, n_days, 4 )
      pr_tmp[,1] = ntime+(1:n_days)
      pr_tmp[,2] = fut_precip$rn
      colnames(pr_tmp) = c("time", "rn", "level", "se")

      for (t in 1:n_days){

        pt = predict(lake_models[[n]]$gam, newdata=lt_use,se.fit=TRUE, type ="response")
        ll_ar = ar(ld_use$level)
        ll_tmp1 = predict(ll_ar, n.ahead = 1, se.fit=TRUE)

        pr_tmp[t,3] =pt$fit[1] + ll_tmp1$pred[1]
        pr_tmp[t,4] = pt$se[1] + ll_tmp1$se[1]

        #Now update the lags in lt_use with data for this day, 
        #but don't do this for n_days
        if (t < n_days){ 
          lt_use$level = pr_tmp[t,3] #Replace NA with prediction
          ld_use = rbind(ld_use, lt_use)
          lt_use = as.data.frame(c(ntime+t, NA, lt_use[1,2:(l_arl+1)],
          fut_precip[t+1,2],
          lt_use[1,(l_arl+3):(l_arl+2+l_arr) ] ))

          colnames(lt_use) = colnames(lt)
        }else{   }
      
      }


  	pred_lakes[[n]]  = as.data.frame(pr_tmp)
	pred_lakes[[n]]$time = fut_precip$time
  
    }

    return(pred_lakes)

}

