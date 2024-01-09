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
library(keras)
library(tidymodels)
library(recipes)
#Note:ignore the instructions to load "tensorflow" directly
#Install should happen via: 
#	install.packages("keras")
#	library(keras)
#	install_keras()
##############################################################
#Key user-defined variables
##############################################################
current_date = Sys.Date() #Current date. Could be set to other

#How long of a data set? Currently 30 years
yl1 = 15
start_date = current_date  -  years(yl1)

#Where the historical data should start
#How long of a data set? Currently 10 years
#real_start = list( ymd("1980-1-1"), ymd("1980-1-1")) #Too big to save
real_start = list( ymd("2013-1-1"), ymd("2013-1-1"))

#USGS site keys. Currently includes Mendota and Monona
site_keys = c("05428000", "05429000")

#The base urls used by the USGS for lake data
url_base = c("https://waterservices.usgs.gov/nwis/iv/?format=rdb&sites=")

#Parameters from the NASA POWER data collection
nasa_pars = c("PRECTOTCORR")

#Lags in rain and lake-level data
#Note -- the wording around what lags are seems to be somewhat
#different in literature/examples. In ML literature lags seems 
#to include current timestep + past ts. So here, we pass a lags
#of x, and in the ML data objects this will result in dimension
#x+1
lags = 6

#The days that we count as winter to exclude ice-on days
winter=c(334,120)

#Number of lakes: currently 2, Mendota and Monona
n_lakes = 2

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
  #Final data sets
  lake_data = vector("list", n_lakes)
  #Readable dates for plotting
  lake_dates = vector("list", n_lakes)
  scale_ll = vector("list", n_lakes)


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
    lake.tmp = remove.days(lake_data[[n]][,c(1,2)], w.yes=F, year(real_start[[n]] ) )
    #colnames(lake.tmp) = "level"
    rn.tmp = remove.days(lake_data[[n]][,c(1,3)], w.yes=F, year(real_start[[n]] ) )
    #colnames(rn.tmp) = "rn"

    #Keep the dates
    lake_dates[[n]] = lake.tmp$time 

    #as_date(date_decimal(as.numeric(time(lake.tmp))))

	#For the RNN. The lags are the number of days into the future we 
    #wish to forecast.
    scale_ll[[n]] = c ( mean(lake.tmp$level,na.rm=T), sqrt(var(lake.tmp$level,na.rm=T)) )
    scale_rn = c ( mean(rn.tmp$rn,na.rm=T), sqrt(var(rn.tmp$rn,na.rm=T)) )
    lake_data[[n]] = make.flashiness.object(data.frame(level= 
      (lake.tmp$level - scale_ll[[n]][1])/scale_ll[[n]][2] ),
      data.frame(rn= (rn.tmp$rn - scale_rn[1])/scale_rn[2] ), lags, auto=F, orders=lags)
  
  }

lake_data2 = lake_data

##############################################################
#PART 2: keras and tensorflow
##############################################################

#Store fitted models
lake_models_lstm = vector("list", n_lakes)

#Performance and prediction 
pred_train = vector("list", n_lakes)
pred_test = vector("list", n_lakes)

lake_models_forecast = vector("list", n_lakes)
lake_models_compare = vector("list", n_lakes)

#Need to make the lake data a 3D array and to separe out the 
#lake level and rain lags into separate matrixes (along the 
#3rd array dimension):
lake_data3D = vector("list", n_lakes)


#Model definition: 
#This function is for tensorflow to create the LSTM. This is 
#analogous to the "model form"   

	build_and_compile_model = function() {
		model = keras_model_sequential() %>%
		# layer_conv_1d(filters=32, kernel_size=3, activation="relu",
    #              input_shape = list(NULL, dim(data)[[-1]])) %>%
    # layer_max_pooling_1d(pool_size=3) %>%
    # layer_conv_1d(filters = 32, kernel_size = 5, activation = "relu") %>%
		# bidirectional(
		layer_conv_lstm_1d( filters=64, kernel_size=3, units = 64, # size of the layer
			activation = 'relu',
			# batch size, timesteps, features
    	batch_input_shape = c(1, lags+1, 2), 
			return_sequences = TRUE,
			stateful = TRUE)  %>%
		# fraction of the units to drop for the linear transformation of the inputs
		layer_dropout(rate = 0.65) %>%
		layer_conv_lstm_1d(filters=64, kernel_size=3,units =64,
             return_sequences = TRUE,
             stateful = TRUE) %>%
		layer_dropout(rate = 0.65) %>%
		time_distributed(layer_dense(units = 1))

	  model %>% compile(
	    loss = 'mean_absolute_error',
	    optimizer = optimizer_adam()
	  )

	  model
	}


for(n in 1:n_lakes){ 
	##########################################################################
	#Processing section to convert each lake_data[[n]] to correct 
	#format for LSTM. This includes an X and Y data set. 
	##########################################################################
	checkpoint_path = paste("./CNNLSTM/", "lakeLSTM",n,".tf", sep="")
	checkpoint_dir = fs::path_dir(checkpoint_path)

	# Create a callback that saves the model's weights
	cp_callback = callback_model_checkpoint(
	  filepath = checkpoint_path,
	  #save_weights_only = TRUE,
	  verbose = 1
	)

	#Use a subset initially:
	s1 = 1
	s2 = 1000
	lake_data[[n]] = lake_data2[[n]][s1:s2,]

	#Chop off time 
	ld_tmp = lake_data[[n]][,-1]

	#Chop off the first lagged rows with NAs:
	ld_tmp = ld_tmp[-(1:(lags+1)), ]

	ntime_full = dim(ld_tmp)[1]

	#Separate last two rows: one for predicion data, 
	#the second for comparison: 
	lake_test = ld_tmp [(ntime_full-1), ]
	lake_compare = ld_tmp [(ntime_full), ]
	ld_tmp = ld_tmp[1:(ntime_full-2), ]

	#Split the remaining data into X and Y for training.
	#E.g. if dim(ld_tmp)[1] = 100, then X will be 1:(100-lags)
	#and Y will be lags:100. 
	ntime_tmp = dim(ld_tmp)[1]
	ld_tmp_x = ld_tmp[1:(ntime_tmp-lags),]
	ld_tmp_y = ld_tmp[(lags+1):(ntime_tmp),]
	
	#Split the features in both X and Y, as well as the test 
	#prediction matrix, into arrays as 3D objects
	lake_data3D_x = array(data = as.numeric(unlist(ld_tmp_x)), 
		dim = c(nrow(ld_tmp_x),
			ncol(ld_tmp_x)/2,2) )

	lake_data3D_y = array(data = as.numeric(unlist(ld_tmp_x)), 
		dim = c(nrow(ld_tmp_y),
			ncol(ld_tmp_y)/2,2) )

	lake_data3D_test = array(data = as.numeric(unlist(lake_test)), 
		dim = c(nrow(lake_test),
			ncol(lake_test)/2,2) )

	##########################################################################
	#Model fitting section.
	##########################################################################

	#Build the model
	lake_models_lstm[[n]]  = build_and_compile_model()
	#summary(dnn_model)

	#Fit the model to training data
	lake_models_lstm[[n]] %>% fit(
		x = lake_data3D_x,
		y = lake_data3D_y,
		batch_size = 1,
		epochs = 20,
		#verbose = 0,
		shuffle = FALSE, #Important for LSTM!
		callbacks = list(cp_callback) # Pass callback to training
	)

	#look at the forecast
	lake_forecast = lake_models_lstm[[n]]  %>%
	  predict(lake_data3D_test, batch_size = 1) %>%
	  .[, , 1]
	 
	lake_models_forecast[[n]] = lake_forecast*scale_ll[[n]][1]+scale_ll[[n]][2]
	lake_models_compare[[n]] = lake_compare 

}

##############################################################
#PART 3: Try reloading a model and resuming training 
##############################################################

n=1 

s3 = s1+1000
s4 = s2+1000

checkpoint_path = "./test1/lm.ckpt"
	checkpoint_dir = fs::path_dir(checkpoint_path)

	# Create a callback that saves the model's weights
	cp_callback = callback_model_checkpoint(
	  filepath = checkpoint_path,
	  save_weights_only = TRUE,
	  verbose = 1
	)

	lake_data[[n]] = lake_data2[[n]][s3:s4,]

	#Chop off time 
	ld_tmp = lake_data[[n]][,-1]

	#Chop off the first lagged rows with NAs:
	ld_tmp = ld_tmp[-(1:(lags+1)), ]

	ntime_full = dim(ld_tmp)[1]

	#Separate last two rows: one for predicion data, 
	#the second for comparison: 
	lake_test = ld_tmp [(ntime_full-1), ]
	lake_compare = ld_tmp [(ntime_full), ]
	ld_tmp = ld_tmp[1:(ntime_full-2), ]

	
	#Split the remaining data into X and Y for training.
	#E.g. if dim(ld_tmp)[1] = 100, then X will be 1:(100-lags)
	#and Y will be lags:100. 
	ntime_tmp = dim(ld_tmp)[1]
	ld_tmp_x = ld_tmp[1:(ntime_tmp-lags),]
	ld_tmp_y = ld_tmp[(lags+1):(ntime_tmp),]
	
	#Split the features in both X and Y, as well as the test 
	#prediction matrix, into arrays as 3D objects
	lake_data3D_x = array(data = as.numeric(unlist(ld_tmp_x)), 
		dim = c(nrow(ld_tmp_x),
			ncol(ld_tmp_x)/2,2) )

	lake_data3D_y = array(data = as.numeric(unlist(ld_tmp_x)), 
		dim = c(nrow(ld_tmp_y),
			ncol(ld_tmp_y)/2,2) )

	lake_data3D_test = array(data = as.numeric(unlist(lake_test)), 
		dim = c(nrow(lake_test),
			ncol(lake_test)/2,2) )

m1 = build_and_compile_model()
train_on_batch(m1, x= lake_data3D_x, y=lake_data3D_y)
load_model_weights_tf(m1, "./test1/lm.ckpt")

lake_forecast = m1  %>%
	  predict(lake_data3D_test, batch_size = 1) %>%
	  .[, , 1]

