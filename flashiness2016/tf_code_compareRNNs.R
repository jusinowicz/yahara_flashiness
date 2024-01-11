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
#For graphviz and pyplot:
#reticulate::conda_install(packages = "graphviz")
#reticulate::py_install("pydot", pip = TRUE)
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
lake_models_dnn = vector("list", n_lakes)
lake_models_gru = vector("list", n_lakes)
lake_models_lstm = vector("list", n_lakes)
lake_models_bilstm = vector("list", n_lakes)
lake_models_cnn = vector("list", n_lakes)
lake_models_cnnlstm = vector("list", n_lakes)

#Forecasts
lake_forecast_dnn = vector("list", n_lakes)
lake_models_compare = vector("list", n_lakes)

#Model definition: 
#This function is for tensorflow to create NNs. Comparing three
#different types here: DNN (baselin), GRU, LSTM. This is 
#analogous to the "model form"   

#DNN
	build_and_compile_dnn = function() {
	  model = keras_model_sequential() %>%
	    layer_dense(64, activation = 'relu') %>%
	    layer_dense(64, activation = 'relu') %>%
	    layer_dense(1)

	  model %>% compile(
	    loss = 'mean_absolute_error',
	    optimizer = optimizer_adam()
	  )
		  model
	}

	#GRU
	build_and_compile_gru = function() {
		model = keras_model_sequential() %>% 
  		layer_gru(units = 64, activation = "relu",
            dropout = 0.1, 
            recurrent_dropout = 0.5,
            return_sequences = TRUE,
            #stateful = TRUE,
						input_shape = list(NULL, dim(ld_tmp)[[-1]])) %>%
 		 	layer_gru(units = 64, 
            dropout = 0.1,
            recurrent_dropout = 0.5,
           	return_sequences = TRUE,
            #stateful = TRUE 
          ) %>% 
  		(layer_dense(units = 1) ) 

		model %>% compile(
		  optimizer = optimizer_rmsprop(),
		  loss = 'mean_absolute_error'
		)

}

#LSTM
	build_and_compile_lstm = function() {
		model = keras_model_sequential() %>%
			layer_lstm(
					units = 64, 
					dropout=0.1,
					activation="relu", 
					recurrent_dropout=0.5,
					return_sequences = TRUE,
					#stateful = TRUE,
					input_shape = list(NULL, dim(ld_tmp)[[-1]])) %>%
	    layer_lstm(
	    		units = 64,  
					dropout=0.1, 
					recurrent_dropout=0.5,
					return_sequences = TRUE,
					#stateful = TRUE
				) %>%
			(layer_dense(units = 1) )

	  model %>% compile(
	    loss = 'mean_absolute_error',
	    optimizer = optimizer_adam()
	  )

	  model
	}

#bi-directional LSTM
	build_and_compile_bilstm = function() {
		model = keras_model_sequential() %>%
			bidirectional( 
				layer_lstm(
					units = 64, 
					dropout=0.1,
					activation="relu", 
					recurrent_dropout=0.5,
					return_sequences = TRUE,
					#stateful = TRUE,
					input_shape = list(NULL, dim(ld_tmp)[[-1]])
				)
			) %>%
	    layer_lstm(
	    		units = 64,  
					dropout=0.1, 
					recurrent_dropout=0.5,
					return_sequences = TRUE,
					#stateful = TRUE
				) %>%
			(layer_dense(units = 1) )

	  model %>% compile(
	    loss = 'mean_absolute_error',
	    optimizer = optimizer_adam()
	  )

	  model
	}

	#Convolutional NN
	build_and_compile_cnn= function() {
		model = keras_model_sequential() %>%
			layer_conv_1d(
					filters=64, 
					kernel_size=5, 
					activation="relu",
					input_shape = list(NULL, dim(ld_tmp)[[-1]])
				) %>%
			layer_max_pooling_1d(pool_size=3) %>%
 			layer_dense(64, activation = 'relu') %>% 
			layer_dense(units = 1)

	  model %>% compile(
	    loss = 'mean_absolute_error',
	    optimizer = optimizer_adam()
	  )

	  model
	}

#CNN plus LSTM
	build_and_compile_cnnlstm = function() {
		model = keras_model_sequential() %>%
			layer_conv_1d(
					filters=64, 
					kernel_size=3, 
					activation="relu",
					input_shape = list(NULL, dim(ld_tmp)[[-1]])
				) %>%
			layer_max_pooling_1d(pool_size=3) %>%
   		layer_conv_1d(
					filters=64, 
					kernel_size=3, 
					activation="relu",
				) %>%
	    layer_lstm(
	    		units = 64,  
					dropout=0.1, 
					recurrent_dropout=0.5,
					return_sequences = TRUE,
					#stateful = TRUE
				) %>%
			layer_dense(units = 1) 

	  model %>% compile(
	    loss = 'mean_absolute_error',
	    optimizer = optimizer_adam()
	  )

	  model
	}




for(n in 1:n_lakes){ 
	
	##########################################################################
	#Create the save points for models
	##########################################################################
	dnn_checkpoint_path = paste("./DNN/", "lakeDNN",n,".tf", sep="")
	dnn_checkpoint_dir = fs::path_dir(dnn_checkpoint_path)

	dnn_log = "logs/run_dnn"

	gru_checkpoint_path = paste("./GRU/", "lakeGRU",n,".tf", sep="")
	gru_checkpoint_dir = fs::path_dir(gru_checkpoint_path)

	gru_log = "logs/run_gru"

	lstm_checkpoint_path = paste("./LSTM/", "lakeLSTM",n,".tf", sep="")
	lstm_checkpoint_dir = fs::path_dir(lstm_checkpoint_path)

	lstm_log = "logs/run_lstm"

	bilstm_checkpoint_path = paste("./BILSTM/", "lakeBILSTM",n,".tf", sep="")
	bilstm_checkpoint_dir = fs::path_dir(bilstm_checkpoint_path)

	bilstm_log = "logs/run_bilstm"

	cnn_checkpoint_path = paste("./CNN/", "lakeCNN",n,".tf", sep="")
	cnn_checkpoint_dir = fs::path_dir(cnn_checkpoint_path)

	cnn_log = "logs/run_cnn"

	cnnlstm_checkpoint_path = paste("./CNNLSTM/", "lakeCNNLSTM",n,".tf", sep="")
	cnnlstm_checkpoint_dir = fs::path_dir(cnnlstm_checkpoint_path)

	cnnlstm_log = "logs/run_cnnlstm"

	# Create a callback that saves the model's weights
	dnn_callback = callback_model_checkpoint(
	  filepath = dnn_checkpoint_path,
	  #save_weights_only = TRUE,
	  verbose = 1
	)

	gru_callback = callback_model_checkpoint(
	  filepath = gru_checkpoint_path,
	  #save_weights_only = TRUE,
	  verbose = 1
	)

	lstm_callback = callback_model_checkpoint(
	  filepath = lstm_checkpoint_path,
	  #save_weights_only = TRUE,
	  verbose = 1
	)

	bilstm_callback = callback_model_checkpoint(
	  filepath = bilstm_checkpoint_path,
	  #save_weights_only = TRUE,
	  verbose = 1
	)


	cnn_callback = callback_model_checkpoint(
	  filepath = cnn_checkpoint_path,
	  #save_weights_only = TRUE,
	  verbose = 1
	)


	cnnlstm_callback = callback_model_checkpoint(
	  filepath = cnnlstm_checkpoint_path,
	  #save_weights_only = TRUE,
	  verbose = 1
	)

	##########################################################################
	#Data processing section to make data sets: truncate, normalize, split 
	##########################################################################
	#Note: balance lookback, delay, and batch size to maximize training 
	#efficiency! 
	#How long of a series to use at a time
	lookback = 20
	#Use every time point
	step = 1
	#Number of time steps into the future to predict
	delay = 1
	#Samples
	batch_size = 20
	predser = 1 #Index of label

	#Chop off time 
	ld_tmp = lake_data[[n]][,-1]

	#Chop off the first lagged rows with NAs:
	ld_tmp = ld_tmp[-(1:(lags+1)), ]
	ntime_full = dim(ld_tmp)[1]

	#Need to convert ld_tmp to a matrix
	ld_tmp = as.matrix(ld_tmp)

	#Set variables for training, validation, and testing data sets
	#Range of training, validation, and test sets:
	min_train = 1
	max_train = floor(ntime_full*2/3)
	min_val = max_train+1
	max_val = min_val + floor(0.5*(ntime_full-max_train))
	min_test = max_val+1
	max_test = NULL

	#Validation and test steps 
	val_steps = floor( (max_val - min_val - lookback) / batch_size )
	test_steps = floor( (nrow(ld_tmp) - max_val - lookback) / batch_size)

	#Use generator function to instantiate three generators: 
	#one for training, one for validation, and one for testing. 
	#Each will look at different temporal segments of the original 
	#data: the training generator looks at the first 2/3 timesteps, 
	#the validation generator looks at the following 1/3, and the test 
	#generator looks at the last time step.

	#Training set 
	train_gen = generator(
	  ld_tmp,
	  lookback = lookback,
	  delay = delay,
	  min_index = min_train,
	  max_index = max_train,
	  #shuffle = TRUE,
	  step = step,
	  batch_size = batch_size,
	  predseries = predser  
	)

	#Validation set 
	val_gen = generator(
	  ld_tmp,
	  lookback = lookback,
	  delay = delay,
	  min_index = min_val,
	  max_index = max_val,
	  step = step,
	  batch_size = batch_size,
	  predseries = predser  
	)

	#Test set looks at remaining
	test_gen = generator(
	  ld_tmp,
	  lookback = lookback,
	  delay = delay,
	  min_index = min_test,
	  max_index = NULL,
	  step = step,
	  batch_size = batch_size,
	  predseries = predser    
	)


	##########################################################################
	#Model fitting section.
	##########################################################################

	#Build the models
	lake_models_dnn[[n]]  = build_and_compile_dnn()
	lake_models_gru[[n]]  = build_and_compile_gru()
	lake_models_lstm[[n]]  = build_and_compile_lstm()
	lake_models_bilstm[[n]]  = build_and_compile_bilstm()
	lake_models_cnn[[n]]  = build_and_compile_cnn()
	lake_models_cnnlstm[[n]]  = build_and_compile_cnnlstm()

	#Fit the model to training data
	#tensorboard(dnn_log )

	lake_models_dnn[[n]] %>% fit(
		train_gen,
		steps_per_epoch = 80, #test_steps,
  	epochs = 20,
  	validation_data = val_gen,
 		validation_steps = val_steps,
		callbacks = list(dnn_callback,callback_tensorboard(dnn_log )) # Pass callback to training
	)

	#Fit the model to training data
	#tensorboard(gru_log )

	lake_models_gru[[n]] %>% fit(
		train_gen,
		steps_per_epoch = 80, #test_steps,
  	epochs = 20,
  	validation_data = val_gen,
 		validation_steps = val_steps,
		callbacks = list(gru_callback,callback_tensorboard(gru_log )) # Pass callback to training
	)

	#Fit the model to training data
	#tensorboard(lstm_log )
	lake_models_lstm[[n]] %>% fit(
		train_gen,
		steps_per_epoch = test_steps,
  	epochs = 20,
  	validation_data = val_gen,
 		validation_steps = val_steps,
		callbacks = list(lstm_callback, 
				callback_tensorboard(lstm_log )) # Pass callback to training
	)

	#Fit the model to training data
	#tensorboard(bilstm_log )
	lake_models_bilstm[[n]] %>% fit(
		train_gen,
		steps_per_epoch = test_steps,
  	epochs = 20,
  	validation_data = val_gen,
 		validation_steps = val_steps,
		callbacks = list(bilstm_callback, 
				callback_tensorboard(bilstm_log )) # Pass callback to training
	)

	#Fit the model to training data
	#tensorboard(bilstm_log )
	lake_models_cnn[[n]] %>% fit(
		train_gen,
		steps_per_epoch = test_steps,
  	epochs = 20,
  	validation_data = val_gen,
 		validation_steps = val_steps,
		callbacks = list(cnn_callback, 
				callback_tensorboard(cnn_log )) # Pass callback to training
	)

	#Fit the model to training data
	#tensorboard(bilstm_log )
	lake_models_cnnlstm[[n]] %>% fit(
		train_gen,
		steps_per_epoch = test_steps,
  	epochs = 20,
  	validation_data = val_gen,
 		validation_steps = val_steps,
		callbacks = list(cnnlstm_callback, 
				callback_tensorboard(cnnlstm_log )) # Pass callback to training
	)


	#look at the forecasts and get RMSE for each
	#Need to restructure test data for the RNNs: 
	test_data = array( ld_tmp[min_test:ntime_full, ], 
											dim = c(1, 636,14 ) )

	lf_dnn = lake_models_dnn[[n]]  %>%
	  predict(test_data[1,,]) 

	lf_gru = lake_models_gru[[n]]  %>%  
		predict(test_data)

	lf_lstm = lake_models_lstm[[n]] %>%  
		predict(test_data)

	lf_bilstm = lake_models_bilstm[[n]]  %>%
	  predict(test_data)

	lf_cnn = lake_models_cnn[[n]]  %>%
	  predict(test_data)

	lf_cnnlstm = lake_models_bilstm[[n]]  %>%
	  predict(test_data)
	  
	nuse = length(ld_tmp[min_test:ntime_full,1])

  #Get the RMSE
  acc_dnn = sqrt(mean((lf_dnn -ld_tmp[min_test:ntime_full,1])^2,na.rm=T))
  acc_gru = sqrt(mean((lf_gru -ld_tmp[min_test:ntime_full,1])^2,na.rm=T))
  acc_lstm = sqrt(mean((lf_lstm-ld_tmp[min_test:ntime_full,1])^2,na.rm=T))
  acc_bilstm= sqrt(mean((lf_bilstm -ld_tmp[min_test:ntime_full,1])^2,na.rm=T))
  acc_cnn= sqrt(mean((lf_cnn -ld_tmp[min_test:ntime_full,1])^2,na.rm=T))
  acc_cnnlstm= sqrt(mean((lf_cnnlstm -ld_tmp[min_test:ntime_full,1])^2,na.rm=T))

  rmse_all = c( acc_dnn, acc_gru, acc_lstm, acc_bilstm)


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

