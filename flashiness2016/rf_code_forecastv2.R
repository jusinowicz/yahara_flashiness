#############################################################
# Analyze the flashiness of the Madison Lakes region, in the
# Yahara watershed. Flashiness -- the tendency of a body of 
# water to change its stage level in response to a given 
# amount of precipitation -- is a strong correlate for 
# flooding potential. This program is designed to use several
# freely available public data sets to both 1) model and 
# understand historical changes in lake flashines and, 
# 2) forecast future flashiness in response to precipitation. 
#
# Lake stage and precipitation data come from the USGS: 
# (reference here)
#
#See Usinowicz et al. 2016 for more motivation and background 
#on this work. 
#
#This version uses a Random Forests ML model to explore the
#impact of predictors (features)


##############################################################
#load libraries
##############################################################
library(tidyverse)
library(lubridate)
library(curl)
library(shiny)
library(rvest) #To parse table from webpage
library(nasapower) #API for NASA data, for precipitation
library(openmeteo)
source("./functions/flash_functions.R")
#For random forests:
library(randomForest)
library(caret)

##############################################################
#Key user-defined variables
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
lake_pre = c("men","mon","keg","wau")
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
lake_data_lstm = vector("list", n_lakes) #LSTM formatted

#Forecasts:
lake_models_forecast = vector("list", n_lakes) 
lake_forecast_dnn = vector("list", n_lakes) #DNN 
lake_forecast_lstm = vector("list", n_lakes) #LSTM 
pred_lakes = vector("list", n_lakes)

#Max lags in rain and lake-level data
lags = 10

##############################################################
#PART 1: Data processing
##############################################################
#Import lake-level time series as the response. These data for
#Lake Stage Level are from the USGS data base, e.g. for Lake
#Mendota:  
#https://waterservices.usgs.gov/nwis/iv/?format=rdb&sites=05427718&startDT=2013-09-21&endDT=2014-09-21&parameterCd=00045&siteStatus=all
#Preallocate the important historical data tables 
lake_table = vector("list", n_lakes)
daily_precip = vector("list", n_lakes)
#Readable dates for plotting
lake_dates = vector("list", n_lakes)
#Scales of data
scale_ll = vector("list", n_lakes)

#Get the rain forecast data:
fut_precip = as.data.frame(weather_forecast(location =  
c(43.0930, -89.3727), daily="precipitation_sum") )
colnames(fut_precip) = c("time", "rn")
fut_precip$rn = fut_precip$rn / 25.4 #Convert mm to inches

lagsp = dim(fut_precip)[1]

#Load the historic data sets
daily_precip[[1]] = read.csv(file = ".././data/rain_hist.csv")
daily_precip[[1]][,"time"] = ymd(daily_precip[[1]][,"time"])

for (n in 1:n_lakes){
    #file name
    fn = paste(".././data/",lake_pre[n], "_hist.csv",sep="")

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


   	###DO THIS FOR RF SO OUTPUT IS NOT DOMINATED BY AR####
	#na_ind = which(is.na(lake_table[[n]][,2]))

	# Use the residuals from the GARCH model so that the trends in variance are
	# removed. Note, this version only fits the GARCH part because the AR will be
	# fit by the GAM: 
	lake_gfit1=garchFit( ~arma(0,0)+garch(1,1),
			 data=na.exclude(lake_data[[n]][,2,drop=F]), trace=F)

	# New lake-level time series based on residuals
	lake_new=as.matrix(lake_gfit1@residuals)

	#New lake-level time series based on AR residuals
	lake_new = as.matrix(ar(lake_new)$resid)

	lake_data[[n]][,2] = lake_new

	#########################################################
    
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

    years.tmp = data.frame( Year=format(lake_dates[[n]], "%Y") )
    months.tmp = data.frame(Month = format(lake_dates[[n]], "%m") )

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


}

 #Build out the final data sets with lags of other lake levels
  #Join the lake and rain data to match up dates
  lake_data_all = lake_data_temp[[1]][,1:2]
  lake_data_allG = lake_data[[1]]

  for (n in 2:n_lakes){
    lake_data_all = lake_data_all %>%
                        inner_join ( lake_data_temp[[n]][,1:2],by = "time")
    # lake_data_allG = lake_data_allG %>%
    #                     inner_join ( lake_data[[n]],by = "time")
  }

  #names
  colnames(lake_data_all) = c("time", lake_pre)
 
  #Add the rain
  lake_data_all = lake_data_all %>%
                        inner_join ( lake_data_temp[[n]][,c(1,3)],by = "time")

#Now make the data sets for each lake, with lags of all lakes
for (n in 1:n_lakes){
    l_others = 1:n_lakes
    l_others = l_others[-n]

    #Make the lake specific data frame 
    lake_data_temp[[n]] = data.frame(lake_data_all$time, 
                          level = lake_data_all[,(n+1)],
                          lake_data_all[1+l_others],
                          rn = lake_data_all$rn)

    #Now feed it to the function to add the lags
    lake_data_lstm[[n]] = make.flashiness.object(
      data.frame(level = lake_data_temp[[n]]$level), as.data.frame(lake_data_temp[[n]][,3:(n_lakes+2)]),
      matrix(lagsp-1,1,(n_lakes) ), 
      auto=F, orders=lagsp-1)

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

##############################################################
#PART 2: Fitting RF models
##############################################################

#Store fitted models
lake_models = vector("list", n_lakes)

#If the fitted GAM model is already known, then describe each model:
model_form = vector("list", n_lakes)

for (n in 1:n_lakes){ 
	model_form [[n]] = "level ~."
}

#Prediction and MSE 
pred_test = vector("list", n_lakes)


#Loop over lakes and fit models. 
#The RF models will primarily pick up on AR when it is present so remove this.
train = vector("list", n_lakes)
test = vector("list", n_lakes)

for(n in 1:n_lakes){ 

	#Chop off time 
	ld_tmp = lake_data_lstm[[n]][,-1]

	#Chop off the first lagged rows with NAs:
	ld_tmp = ld_tmp[-(1:(lags+1)), ]
	ntime_full = dim(ld_tmp)[1]

	#Need to convert ld_tmp to a matrix
	ld_tmp = as.matrix(ld_tmp)

	lake_r = ld_tmp

	#Split data for training and testing: 
	ind = base::sample(2, nrow(lake_r), replace = TRUE, prob = c(0.7, 0.3))
	train[[n]] = lake_r [ind==1,]
	test[[n]] = lake_r [ind==2,]

	#Tuning the model: 
	t = tuneRF(train[[n]][,-5], train[[n]][,5],
       stepFactor = 0.5,
       plot = TRUE,
       ntreeTry = 150,
       trace = TRUE,
       improve = 0.05)

	#Get mtry with the lowest OOB Error
	# t[ as.numeric(t[,2]) < 0 ] = 1
	mtry_use = as.numeric(t[which(t == min(t),arr.ind=T)[1],1])  

	#Basic RF fitting
	lake_models[[n]] = randomForest (as.formula(model_form [[n]] ),
		data=train[[n]], proximity=TRUE, mtry = mtry_use)

	#Prediction
	pred_test[[n]] = predict(lake_models[[n]], test[[n]])



}

##############################################################
#PART 3: Look at the importance of variables
##############################################################

par(mfrow = c(2,2))
#n=2
for(n in 1:n_lakes){ 
	plot(lake_models[[n]])
	print(mean((pred_test[[n]]-test[[n]][,1])^2))
}

par(mfrow = c(2,2))
for(n in 1:n_lakes){ 
	hist(treesize(lake_models[[n]]),
	     main = "No. of Nodes for the Trees",
	     col = "green")
}

fig.name = paste("varImpPlot3",".pdf",sep="")
pdf(file=fig.name, height=8, width=8, onefile=TRUE, family='Helvetica', pointsize=16)

par(mfrow = c(2,2))
for(n in 1:n_lakes){ 
	#Variable Importance
	varImpPlot(lake_models[[n]],
	           sort = T,
	           n.var = 20,
	           main = paste("Lake",n,"Variable Importance",sep=" ")
	      )
}

dev.off()

#MeanDecreaseGini
importance(lake_models[[n]])[order(importance(lake_models[[n]]),decreasing = T),]
