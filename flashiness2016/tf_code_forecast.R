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
#This version uses tensorflow to explore the role of 
#predictors (features)


##############################################################
#load libraries
##############################################################
library(tidyverse)
library(lubridate)
library(rvest) #To parse table from webpage
library(nasapower) #API for NASA data, for precipitation
library(GGally)
source("./functions/flash_functions.R")
#For tensorflow:
library(tensorflow)
library(keras)
library(tidymodels)
library(recipes)
##############################################################
#Key user-defined variables
##############################################################
current_date = Sys.Date() #Current date. Could be set to other

#How long of a data set? Currently 30 years
yl1 = 30
start_date = current_date  -  years(yl1)

#USGS site keys. Currently includes Mendota and Monona
site_keys = c("05428000", "05429000")

#The base urls used by the USGS for lake data
url_base = c("https://waterservices.usgs.gov/nwis/iv/?format=rdb&sites=")

#Parameters from the NASA POWER data collection
nasa_pars = c("PRECTOTCORR")

#Lags in rain and lake-level data
lags = 10

#The days that we count as winter to exclude ice-on days
winter=c(334,120)

##############################################################
#PART 1: Data processing
##############################################################
#Import lake-level time series as the response. These data for
#Lake Stage Level are from the USGS data base, e.g. for Lake
#Mendota:  
#https://waterservices.usgs.gov/nwis/iv/?format=rdb&sites=05427718&startDT=2013-09-21&endDT=2014-09-21&parameterCd=00045&siteStatus=all

#Preallocate and get number of lakes
n_lakes = length(site_keys)
lake_table = vector("list", n_lakes)
daily_precip = vector("list", n_lakes)
real_start = vector("list", n_lakes)
#Final data set
lake_data = vector("list", n_lakes)

#Loop over lakes to get stage level and dates: 
for(n in 1:n_lakes){ 
	#Use the USGS address format to get data over the date range
	url1 = paste(url_base[1], site_keys[n], "&startDT=", start_date,
		"&endDT=",current_date,"&parameterCd=00060,00065&siteStatus=all",sep="" )
	
	lake_table[[n]]= as.data.frame(read_delim(print(url1), comment = "#", delim="\t"))
	lake_table[[n]] = lake_table[[n]][-1,]
	lake_table[[n]][,"datetime"] = as.POSIXct(lake_table[[n]][,"datetime"],
                       format = '%Y-%m-%d %H:%M')
	lake_table[[n]][,5] = as.numeric(as.character(lake_table[[n]][,5]))

	lake_table[[n]] = lake_table[[n]][,c(3, 5)]
	colnames(lake_table[[n]] ) = c("datetime", "level")

	#Data seem to be at 15 min intervals by default. Aggregate these into 
	#a mean daily level
	lake_table[[n]] = lake_table[[n]] %>%
  		mutate(day = as.Date(ymd_hms(datetime))) %>%
  		group_by(day) %>%
  		summarise(level = mean(level, na.rm = TRUE)) %>%
  		as.data.frame()
	
	#Just in case we requested back too far, what is the actual start 
	#date of the retrieved data?
	real_start[[n]] = lake_table[[n]][1,"day"]
                     
	#Get the matching precipitation data from the NASA POWER collection
	daily_precip[[n]] = get_power(
	  community = "ag",
	  lonlat = c(43.0930, -89.3727),
	  pars =  nasa_pars,
	  dates = c(paste(real_start[[n]]), paste(current_date)),
	  temporal_api = "daily"
	) %>% as.data.frame()
	daily_precip[[n]] = daily_precip[[n]][,c(7,8)]
	colnames(daily_precip[[n]]) = c("day", "rn")

	#Join the lake and rain data to match up dates
	lake_table[[n]] = lake_table[[n]] %>%
			inner_join(daily_precip[[n]], by = "day" ) 

	#Do some processing to remove ice-on days (approximately). This 
	#function automatically removes winter days and converts data 
	#table to a timeseries (ts) object 
	lake.tmp = remove.days(lake_table[[n]]$level, year(real_start[[n]] ) )
	colnames(lake.tmp) = "level"
	rn.tmp = remove.days(lake_table[[n]]$rn, year(real_start[[n]] ) )
	colnames(rn.tmp) = "rn"

	#This final step creates the full data object, with lags of 
	#lake level for autocorrelation and lags of rain for delayed
	#rain input. 
	lake_data[[n]] = make.flashiness.object(lake.tmp, rn.tmp, lags)

}


##############################################################
#PART 2: tensorflow
##############################################################

#Store fitted models
lake_models = vector("list", n_lakes)

#If the fitted GAM model is already known, then describe each model:
model_form = vector("list", n_lakes)

model_form [[1]] = "level ~."
model_form [[2]] = "level ~."

#Prediction and MSE 
pred_test = vector("list", n_lakes)


#Loop over lakes and fit models. 
#The RF models will primarily pick up on AR when it is present so remove this.

for(n in 1:n_lakes){ 

	# Use the residuals from the GARCH model so that the trends in variance are
	# removed. Note, this version only fits the GARCH part because the AR will be
	# fit by the GAM: 

	lake_gfit1=garchFit( ~arma(0,0)+garch(1,1),
				 data=na.exclude(lake_data[[n]][,2,drop=F]), trace=F)

	# New lake-level time series based on residuals
	lake_new=as.matrix(lake_gfit1@residuals)

	#New lake-level time series based on AR residuals
	lake_new = as.matrix(ar(lake_new)$resid)

	# New time series after removing NAs in the rain
	rn_new=as.matrix(lake_data[[n]]$rn[!is.na(lake_data[[n]][,"rn",drop=T])])
	lake_new = as.matrix(lake_new[!is.na(lake_data[[n]][,"rn",drop=T])])
	colnames(rn_new) = "rn"
	colnames(lake_new) = "level"

	#Combine all of the data, add the lagged data, and turn into ts
	lake_r = make.flashiness.object( lake_new , rn_new, lags)
	lake_r = na.omit(lake_r) #No NAs for RF models

	#Split data for training and testing: 
	ind = base::sample(2, nrow(lake_r), replace = TRUE, prob = c(0.7, 0.3))
	train = lake_r [ind==1,]
	test = lake_r [ind==2,]

	#Split the labels from the features: 
	train_f = select(train, -level)
	test_f = select(test, -level)

	train_l = select(train, level)
	test_l = select(test, level)

	#Normalize the data:
	normalizer = layer_normalization (axis = -1L)

	#Tuning the model: 
	t = tuneRF(train[,-5], train[,5],
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
		data=train, proximity=TRUE, mtry = mtry_use)

	#Prediction
	pred_test[[n]] = predict(lake_models[[n]], test)



}

##############################################################
#PART 3: Look at the importance of variables
##############################################################
n=2
plot(lake_models[[n]])
print(mean((pred_test[[n]]-test$level)^2))

hist(treesize(lake_models[[n]]),
     main = "No. of Nodes for the Trees",
     col = "green")

#Variable Importance
varImpPlot(lake_models[[n]],
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")

#MeanDecreaseGini
importance(lake_models[[n]])[order(importance(lake_models[[n]]),decreasing = T),]


#Useful correlation plot
lake_data[[n]] %>% select(level, rn1, rn2,rn3,rn4,rn5) %>%
 ggpairs(lake_data[[n]])

#Useful quick glimpse	  