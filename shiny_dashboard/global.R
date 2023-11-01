library(shiny)
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
  
#Where the lake stage and rain data live: 
lake_data = vector("list", n_lakes)

#Max lags in rain and lake-level data
lags = 10

#If the fitted GAM model is already known, then describe each model:
model_form = vector("list", n_lakes)
#model_form [[1]] = "level ~ s(time, bs = \"cr\", k = 100)"
# model_form [[1]] = "level ~ s(time, bs = \"cr\", k = 100)+
#     s(level1,bs=\"cr\",k=6)+s(level2,bs=\"cr\",k=6)+s(level3,bs=\"cr\",k=6)+
#     s(level4,bs=\"cr\",k=6)+s(level5,bs=\"cr\",k=6)+
#     s(rn,bs=\"cr\",k=6)+s(rn1,bs=\"cr\",k=6)+
#     s(rn2,bs=\"cr\",k=6)+s(rn3,bs=\"cr\",k=6)+s(rn4,bs=\"cr\",k=6)+
#     te(rn,time,k=20)+te(rn1,time,k=20)+te(rn2,time,k=20)+
#     te(rn3,time,k=20)+te(rn4,time,k=20)+te(rn,rn1,k=20)+
#     te(rn1,rn2,k=20)+te(rn2,rn3,k=20)"

# model_form [[2]] = "level ~ s(time, bs = \"cr\", k = 100)+
#     s(level1,bs=\"cr\",k=6)+s(level2,bs=\"cr\",k=6)+s(level3,bs=\"cr\",k=6)+
#     s(level4,bs=\"cr\",k=6)+s(level5,bs=\"cr\",k=6)+s(level6,bs=\"cr\",k=6)+
#     s(rn,bs=\"cr\",k=6)+s(rn1,bs=\"cr\",k=6)+
#     s(rn2,bs=\"cr\",k=6)+s(rn3,bs=\"cr\",k=6)+s(rn4,bs=\"cr\",k=6)+
#     te(rn,time,k=20)+te(rn1,time,k=20)+te(rn2,time,k=20)+
#     te(rn3,time,k=20)+te(rn4,time,k=20)+te(rn,rn1,k=20)+
#     te(rn1,rn2,k=20)+te(rn2,rn3,k=20)"

model_form [[1]] = "level ~ 
    s(level1,bs=\"cr\",k=6)+s(level2,bs=\"cr\",k=6)+
    s(rn,bs=\"cr\",k=6)+s(rn1,bs=\"cr\",k=6)+
    s(rn2,bs=\"cr\",k=6)+s(rn3,bs=\"cr\",k=6)+
    te(rn,rn1,k=20)+te(rn1,rn2,k=20)+te(rn2,rn3,k=20)"

model_form [[2]] = "level ~ 
    s(level1,bs=\"cr\",k=6)+s(level2,bs=\"cr\",k=6)+s(level3,bs=\"cr\",k=6)+
    s(rn,bs=\"cr\",k=6)+s(rn1,bs=\"cr\",k=6)+
    s(rn2,bs=\"cr\",k=6)+s(rn3,bs=\"cr\",k=6)+
    te(rn,rn1,k=20)+te(rn1,rn2,k=20)+te(rn2,rn3,k=20)"

# model_form [[1]] = "level ~
#     s(level1,bs=\"cr\",k=6)+s(level2,bs=\"cr\",k=6)+s(level3,bs=\"cr\",k=6)+
#     s(level4,bs=\"cr\",k=6)+s(rn1,bs=\"cr\",k=6)+
#     s(rn2,bs=\"cr\",k=6)+s(rn3,bs=\"cr\",k=6)+s(rn4,bs=\"cr\",k=6)+
#     te(rn,time,k=20)+te(rn1,time,k=20)+te(rn2,time,k=20)+
#     te(rn3,time,k=20)+te(rn4,time,k=20)"

# model_form [[2]] = "level ~ 
#     s(level1,bs=\"cr\",k=6)+s(level2,bs=\"cr\",k=6)+s(level3,bs=\"cr\",k=6)+
#     s(level4,bs=\"cr\",k=6)+s(rn1,bs=\"cr\",k=6)+
#     s(rn2,bs=\"cr\",k=6)+s(rn3,bs=\"cr\",k=6)+s(rn4,bs=\"cr\",k=6)+
#     te(rn,time,k=20)+te(rn1,time,k=20)+te(rn2,time,k=20)+
#     te(rn3,time,k=20)+te(rn4,time,k=20)"


# model_form [[1]] = "level ~ s(time, bs = \"cr\", k = 100)+
#     s(level1,bs=\"cr\",k=6)+s(level2,bs=\"cr\",k=6)+
#     s(rn1,bs=\"cr\",k=6)+
#     s(rn2,bs=\"cr\",k=6)+s(rn3,bs=\"cr\",k=6)+
#     te(rn,time,k=20)+te(rn1,time,k=20)+te(rn2,time,k=20)+
#     te(rn3,time,k=20)"

# model_form [[2]] = "level ~ s(time, bs = \"cr\", k = 100)+
#     s(level1,bs=\"cr\",k=6)+s(level2,bs=\"cr\",k=6)+s(level3,bs=\"cr\",k=6)+
#     s(rn1,bs=\"cr\",k=6)+
#     s(rn2,bs=\"cr\",k=6)+s(rn3,bs=\"cr\",k=6)+
#     te(rn,time,k=20)+te(rn1,time,k=20)+te(rn2,time,k=20)+
#     te(rn3,time,k=20)"

# model_form [[1]] = "level ~ s(time, bs = \"cr\", k = 100)+
#     s(rn,bs=\"cr\",k=6)+ s(rn2,bs=\"cr\",k=6)+ ti(rn,time,k=20)+ ti(rn2,time,k=20)"

# model_form [[1]] = "level ~ s(time, bs = \"cr\", k = 100)+
#     s(rn,bs=\"cr\",k=6)+ ti(rn,time,k=20)"

# model_form [[1]] = "level ~ s(time, bs = \"cr\", k = 100)+
#     s(rn,bs=\"cr\",k=6)+s(rn2,bs=\"cr\",k=6)+s(rn3,bs=\"cr\",k=6)+ 
#     ti(rn,time,k=20)+ti(rn2,time,k=20)+ti(rn3,time,k=20)"

# model_form [[2]] = "level ~ s(time, bs = \"cr\", k = 100)+
#     s(rn,bs=\"cr\",k=6) + ti(rn,time,k=20)"



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
  # file.copy(from = "./../data/men_hist.csv", to ="./../data/men_hist.csv.bck")
  # file.copy(from = "./../data/mon_hist.csv", to ="./../data/mon_hist.csv.bck")
  # file.copy(from = "./../data/rain_hist.csv", to ="./../data/rain_hist.csv.bck")

  file.copy(from = "men_hist.csv", to ="men_hist.csv.bck")
  file.copy(from = "mon_hist.csv", to ="mon_hist.csv.bck")
  file.copy(from = "rain_hist.csv", to ="rain_hist.csv.bck")


  #Get the last dates entered. If they don't match to the current date
  #then update the data with the functions updateLake and updateRain. 
  #Write the new files.  

  #For the precip first (same data for all lakes)
  last_rain= daily_precip[nrow(daily_precip),"time"]
  if(last_rain != current_date  ){ 
    daily_precip = rbind(daily_precip,
        updateRain(daily_precip, start_date = last_rain, 
          current_date = current_date) )
    daily_precip = daily_precip[-( (nrow(daily_precip)-1):
      nrow(daily_precip)), ]
  }

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

    lake_data[[n]] = lake_table[[n]] %>%
        inner_join(daily_precip, by = "time" ) 

  }

  # write.table(lake_data[[1]][,1:2], file = "./../data/men_hist.csv", sep=",")
  # write.table(lake_data[[2]][,1:2], file = "./../data/mon_hist.csv", sep=",")
  # write.table(lake_data[[1]][,c(1,3)], file = "./../data/rain_hist.csv", sep=",")
  write.table(lake_data[[1]][,1:2], file = "men_hist.csv", sep=",")
  write.table(lake_data[[2]][,1:2], file = "mon_hist.csv", sep=",")
  write.table(lake_data[[1]][,c(1,3)], file = "rain_hist.csv", sep=",")
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

    #We have to fit the models. Use this function:
    new_models = fitGAM(lake_data, model_form)
    
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
      sum_tmp = summary(new_models[[n]]) 
      model_smooths[[n]] =  rownames(sum_tmp$s.table)

      #Create a new data frame that essentially creates an 
      #equally spaced grid over the range of each variable 
      #in the data set. Then get the Lp matrix from that to 
      #use for prediction. 

      models_Xp[[n]] = predict(new_models[[n]],lake_data[[n]],type="lpmatrix" )
      #Model coefficients
      models_coef[[n]] = coef(new_models[[n]])
      #Variance matrix
      models_Vp[[n]] = new_models[[n]]$Vp
    }

    save(file = "lakeGAMsLpB.var", model_smooths, models_Xp, models_coef, models_Vp )
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
    ntime = dim(lake_data[[1]])[1]

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
 
      #The start of the new data set for prediction with 
      #the first new day
      lt_tmp = as.data.frame(c(ntime+1, NA, lt[l_arl,2:(l_arl+1)],
        fut_precip[1,2],
        lt[l_arl,(l_arl+3):(l_arl+2+l_arr) ] ))
      colnames(lt_tmp) = colnames(lt)
      lt_new = rbind( lt,lt_tmp) 
      lt_use = lt_new[, -2]

      
      a1=predict(lake_models[[n]],newdata=lt_use,se.fit=TRUE)



      #Do each time-step sequentially: 
      for (f in 1:n_days){

        x0 = 1         ## intercept column

        #Loop through smooth terms:
        for ( j in 0:(n_terms-1) ) {
          dx = m_ldiff[j+2]
          #Get the set of columns for the smooth
          if(j == 0) { cst = 0 }else{
            cst = sum(model_ks[[n]][1:(j)])
          }
          lcols = 1+cst + 1:(model_ks[[n]][j+1])
          #Find the relevant rows
          lrows = floor(lt_use[ (l_arl+f),j+2]/dx) 
          w1 = (lt_use[(l_arl+f), j+2]-lrows*dx)/dx ## interpolation weights
          ## find approx. predict matrix row portion, by interpolation
          x0 = c(x0,models_Xp[[n]][lrows+2,lcols]*w1 + models_Xp[[n]][lrows+1,lcols]*(1-w1))
        }

        dim(x0)=c(1,dim(models_Xp[[n]])[2]) 
        fv = x0%*%models_coef[[n]]   ## evaluate and add offset
        se = sqrt(x0%*%models_Vp[[n]]%*%t(x0)) ## get standard error

        ## compare to normal prediction
  

   }
}

}

###############################################################################
# NOT IMPLEMENTED: 
# predictFlashGAM_LP uses the Lp Matrix of a fitted GAM to predict
#
#This is the version of predictFlashGAM that would be used if the model 
#produced in the fitting process was too massive to store and recall
#efficiently from memory. 
###############################################################################

predictFlashGAM_LP = function(lake_data, fut_precip){

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
    ntime = dim(lake_data[[1]])[1]

    #This section will break down the formula and extract two 
    #key pieces of info: the number of smooth terms and the 
    #number of knots for each 
    model_clean = vector("character",n_lakes)
    model_ks = vector("list",n_lakes)

    #a1 = ts( as.matrix(fut_precip[,2]), start=real_start[[n]], frequency=freq )
    
    # fp_table[[n]] = make.flashiness.object(lake.tmp, rn.tmp, lags)

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
 
      #The start of the new data set for prediction with 
      #the first new day
      lt_tmp = as.data.frame(c(ntime+1, NA, lt[l_arl,2:(l_arl+1)],
        fut_precip[1,2],
        lt[l_arl,(l_arl+3):(l_arl+2+l_arr) ] ))
      colnames(lt_tmp) = colnames(lt)
      lt_new = rbind( lt,lt_tmp) 
      lt_use = lt_new[, -2]

      #Figure out the smooth terms 
      n_terms = length(model_smooths[[n]])
      all_ks = colnames(models_Xp[[n]])
      model_ks[[n]] = matrix(0,n_terms,1)

      for(t in 1:n_terms){

        sm_tmp = model_smooths[[n]][t] 
        ks_tmp = grepl(
        glob2rx(paste(sm_tmp,".*",sep="")), 
        all_ks)
        model_ks[[n]][t] = sum(ks_tmp) 

      }

      n_smooth = sum(model_ks[[n]])

      #Find the average distance between samples:
      ldiffs = apply(lake_data[[n]],2,diff)  
      m_ldiff = apply(abs(ldiffs), 2, mean,na.rm=T) 
      
 
      
      #Do each time-step sequentially: 
      for (f in 1:n_days){

        x0 = 1         ## intercept column

        #Loop through smooth terms:
        for ( j in 0:(n_terms-1) ) {
          dx = m_ldiff[j+2]
          #Get the set of columns for the smooth
          if(j == 0) { cst = 0 }else{
            cst = sum(model_ks[[n]][1:(j)])
          }
          lcols = 1+cst + 1:(model_ks[[n]][j+1])
          #Find the relevant rows
          lrows = floor(lt_use[ (l_arl+f),j+2]/dx) 
          w1 = (lt_use[(l_arl+f), j+2]-lrows*dx)/dx ## interpolation weights
          ## find approx. predict matrix row portion, by interpolation
          x0 = c(x0,models_Xp[[n]][lrows+2,lcols]*w1 + models_Xp[[n]][lrows+1,lcols]*(1-w1))
        }

        dim(x0)=c(1,dim(models_Xp[[n]])[2]) 
        fv = x0%*%models_coef[[n]]   ## evaluate and add offset
        se = sqrt(x0%*%models_Vp[[n]]%*%t(x0)) ## get standard error



   }
}

}





# An empty prototype of the data frame we want to create
prototype <- data.frame(date = character(), time = character(),
  size = numeric(), r_version = character(), r_arch = character(),
  r_os = character(), package = character(), version = character(),
  country = character(), ip_id = character(), received = numeric())

# Connects to streaming log data for cran.rstudio.com and
# returns a reactive expression that serves up the cumulative
# results as a data frame
packageStream <- function(session) {
  # Connect to data source
  sock <- socketConnection("cransim.rstudio.com", 6789, blocking = FALSE, open = "r")
  # Clean up when session is over
  session$onSessionEnded(function() {
    close(sock)
  })

  # Returns new lines
  newLines <- reactive({
    invalidateLater(1000, session)
    readLines(sock)
  })

  # Parses newLines() into data frame
  reactive({
    if (length(newLines()) == 0)
      return()
    read.csv(textConnection(newLines()), header=FALSE, stringsAsFactors=FALSE,
      col.names = names(prototype)
    ) %>% mutate(received = as.numeric(Sys.time()))
  })
}

# Accumulates pkgStream rows over time; throws out any older than timeWindow
# (assuming the presence of a "received" field)
packageData <- function(pkgStream, timeWindow) {
  shinySignals::reducePast(pkgStream, function(memo, value) {
    rbind(memo, value) %>%
      filter(received > as.numeric(Sys.time()) - timeWindow)
  }, prototype)
}

# Count the total nrows of pkgStream
downloadCount <- function(pkgStream) {
  shinySignals::reducePast(pkgStream, function(memo, df) {
    if (is.null(df))
      return(memo)
    memo + nrow(df)
  }, 0)
}

# Use a bloom filter to probabilistically track the number of unique
# users we have seen; using bloom filter means we will not have a
# perfectly accurate count, but the memory usage will be bounded.
userCount <- function(pkgStream) {
  # These parameters estimate that with 5000 unique users added to
  # the filter, we'll have a 1% chance of false positive on the next
  # user to be queried.
  bloomFilter <- BloomFilter$new(5000, 0.01)
  total <- 0
  reactive({
    df <- pkgStream()
    if (!is.null(df) && nrow(df) > 0) {
      # ip_id is only unique on a per-day basis. To make them unique
      # across days, include the date. And call unique() to make sure
      # we don't double-count dupes in the current data frame.
      ids <- paste(df$date, df$ip_id) %>% unique()
      # Get indices of IDs we haven't seen before
      newIds <- !sapply(ids, bloomFilter$has)
      # Add the count of new IDs
      total <<- total + length(newIds)
      # Add the new IDs so we know for next time
      sapply(ids[newIds], bloomFilter$set)
    }
    total
  })
}



    { "keys": ["ctrl+shift+b"], "command": "toggle_terminus_panel" },
      {
      "keys": ["ctrl+shift+enter"],
      "command": "send_selection_to_terminus"
    }
