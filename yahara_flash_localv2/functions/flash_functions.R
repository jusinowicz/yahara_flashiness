#############################################################
# These are essential functions for the data processing 
# and GAM model fitting procedures. 
#
# Analyze the flashiness of the Madison Lakes region, in the
# Yahara watershed. Flashiness -- the tendency of a body of 
# water to change its stage level in response to a given 
# amount of precipitation -- is a strong correlate for 
# flooding potential. 
#
# Lake stage and precipitation data come from the USGS: 
# (reference here)
#
#See Usinowicz et al. 2016 for more motivation and background 
#on this work.

library(lubridate)
#Stats
library(mgcv)
library(fGarch) #For GARCH models
#For tensorflow:
library(keras)
library(tensorflow)
library(tidymodels)
library(recipes)

##############################################################
# Data processing function 1: Remove winter and leap days from
# a dataset with 365 daily values and make it a timeseries variable
# that, importantly, is also a matrix. 
#	variable1	A single column vector of values spanning multiple
#				years.  
#	w.yes 		If this is set to FALSE, there is no winter and this
#				function only serves to remove leap days and package
#				the variable as a ts object with frequency = 365 days.
#	winter		A 2 value vector with numerical start and end days
#				of winter. For example, winter from Dec. 1 to May 1
#				gives the default c(334, 120).
#	date.start 	The first full year of data. NOTE: This assumes that 
#				there are no partial years in dataset. 

remove.days=function(variable1, date.start, w.yes=TRUE, winter=c(334,120)  ){

	length.var=dim(variable1)[1]
	# Make a list of the years and identify leap years.
	nyears = floor(length.var/365)
	years = date.start:(date.start+nyears)
	leap.yes1 = ((years %% 4 == 0) & (years %% 100 != 0)) | (years %% 400 == 0) 

	rd=NULL
	no.leaps=0
	for(y in 1:nyears) { if (as.numeric(leap.yes1[y]) >0) { 

						rd = c(rd,365*(y-1)+60+no.leaps)
						no.leaps=no.leaps+1

				}

			}

	variable1 = variable1[-rd,]

	# Test the order of the winter dates provided then remove the winter
	# days. The first option corresponds to a wrapped date, as in the 
	# default. 
	rmove=NULL
	if (w.yes == TRUE) {

	if (winter[1]>winter[2]) { 
		r1 = winter[1]:365
		r2 = 1:winter[2]
		freq = 365 - length(c(r1,r2))
		for ( y in 1:nyears) { rmove = c(rmove, -(365*(y-1)+c(r1,r2)))}
		new.var1 = variable1[ rmove, ]
						} else { 
		
		r1 = winter[1]:winter[2]
		freq = 365 - length(r1)
		for ( y in 1:nyears) { rmove = c(rmove, -(365*(y-1)+r1))}
		new.var1 = variable1[ rmove ,]	
		}
	
		#return(ts( as.matrix(new.var1), start=date.start, frequency=freq ))
		return(new.var1)
 	} else { 

 		new.var1 = variable1
 		#return(ts( as.matrix(new.var1), start=date.start, frequency=365 ))
		return(new.var1)

	}
 	

 }


##############################################################
# Data processing function 2: Combine all variables into a single
# dataset, including those generated as lags of a particular
# variable (e.g. precipitation or lake level), and return a data frame. 
#
# response 	The variable that will be the response, i.e. lake
#			lake-level. 
# lagged_covar Variables that will be lagged, usually just precipitation.
# lags 		The number of lags for each lagged variable. The 
#			number of entries needs to match the number of columns
#			in lagged_covar. 
# covar     Additional covariates that will not be lagged. E.g.
#			impervious surface area. 
# auto 		By default, this function will test the autoregression in
#			in the response and determined the appropriate number of 
#			lags. Setting this to false requires the user to input the 
#			autoregressive order. The ar(p) order determines the number 
#			of lags of the response to add to the final data object. 
# orders	The order of the ar(p) model, which determines the number of 
#			lags of the response to add to the final data object.
#

make.flashiness.object = function (response, lagged_covar, lags, covar=NULL, auto=TRUE, orders=NULL) {

	#Make sure the lags are fully specified for lagged covars.

	if(dim(as.matrix(lagged_covar))[2]!= length(lags)) { 
		stop("Lags does not match number of lagged variables")
	}

	if( is.null(dim(response)) | is.null(dim(lagged_covar))) {
		stop ("Make sure the response and all covariates are at least 
			column vectors (remove.days() will do this automatically).")}
		
	names.list = c( colnames(response), colnames(lagged_covar),colnames(covar))
		
	#Decide how many lags of the response variable. This can 
	#be determined automatically by testing for autoregression.
	if ( auto == TRUE) { 

			ar.tmp = ar(response,na.action=na.exclude)
			r.lags = ar.tmp$order

		} else { 

			r.lags = orders
	}	
		
		#Make the matrix of lagged responses
		r.names = names.list[1]
		r.cols = response
		if (r.lags>0){ 
			for (rl in 1:r.lags) { 
				#Create the lagged matrix and pad with NAs
				lag.r =lag(as.data.frame(response),rl)  
				r.cols = cbind(r.cols, lag.r )  
				r.names = c(r.names, paste(names.list[1],rl,sep="")) 
			}
		}
		colnames(r.cols)=r.names


		#Make the matrix of lagged covariates
		all.lc.cols=NULL
		n.cov=dim(lagged_covar)[2]

		#Test that there are covariates
		if( is.null(n.cov) ) {n.cov=0}
		if(n.cov > 0 ) { 
			
			for (nc in 1:n.cov){
				lc.cols= as.matrix(lagged_covar[,nc])
				col.names = c(names.list[(nc+1)])
				n.lags = lags[nc]

				#Test that the covariate is lagged
				if (n.lags>0){

					#Make the lagged covariates
					for (nl in 1:n.lags) {
					lag.c = lag(as.data.frame(lagged_covar[,nc]),nl) 
					lc.cols = cbind(lc.cols,lag.c)	
					col.names = c(col.names, paste(names.list[(nc+1)],nl,sep=""))
					}
				
				} 
				colnames(lc.cols) = col.names
				all.lc.cols = cbind(all.lc.cols,as.matrix(lc.cols) )
			}
		
		} else { 	
		 all.lc.cols = NULL
		}

		#Make the full matrix
		full.tmp = cbind(1:(dim(all.lc.cols)[1]), as.matrix(r.cols),
			as.matrix(all.lc.cols),covar )
		colnames(full.tmp)=c("time", colnames(r.cols),colnames(all.lc.cols),colnames(covar))

	return( as.data.frame(full.tmp))
}

##############################################################
# Wrap the model fitting in a function that can be called
# by global.R and server.R
##############################################################
fitGAM = function( lake_data, model_form){ 

	n_lakes = length(model_form)
	#Store fitted models
	lake_models = vector("list", n_lakes)

	#Loop over lakes and fit models. Assuming that best-fit models have 
	#already been determined by AIC and GCV. 
	for(n in 1:n_lakes){ 

		# Use the residuals from the GARCH model so that the trends in variance are
		# removed. Note, this version only fits the GARCH part because the AR will be
		# fit by the GAM: 

		lake_gfit1=garchFit( ~arma(0,0)+garch(1,1),
					 data=na.exclude(lake_data[[n]][,2,drop=F]), trace=F)

		# New lake-level time series based on residuals
		lake_new=as.matrix(lake_gfit1@residuals)
		
		# New time series after removing NAs in the rain
		rn_new=as.matrix(lake_data[[n]]$rn[!is.na(lake_data[[n]][,"rn",drop=T])])
		lake_new = as.matrix(lake_new[!is.na(lake_data[[n]][,"rn",drop=T])])
		colnames(rn_new) = "rn"
		colnames(lake_new) = "level"

		#Combine all of the data, add the lagged data, and turn into ts
		lake_r = make.flashiness.object( lake_new , rn_new, lags)

		# The best-fit GAMs were determined in Usinowicz et al. 2016. 
		# Those are what are fit here.
		# Use bam() (instead of gam()) from mgcv because it is designed for 
		# large data sets.

		lake_models[[n]] = bam ( as.formula((model_form [[n]] )), data=lake_r)

	}

	return(lake_models)

}

##############################################################
# Wrap the model fitting in a function that can be called
# by global.R and server.R. This version uses GAMM to fit 
# the AR correlation structure
##############################################################
fitGAM_ar = function( lake_data, model_form){ 

	n_lakes = length(model_form)
	#Store fitted models
	lake_models = vector("list", n_lakes)

	#Loop over lakes and fit models. Assuming that best-fit models have 
	#already been determined by AIC and GCV. 
	for(n in 1:n_lakes){ 

		# Use the residuals from the GARCH model so that the trends in variance are
		# removed. Note, this version only fits the GARCH part because the AR will be
		# fit by the GAM: 

		lake_gfit1=garchFit( ~arma(0,0)+garch(1,1),
					 data=na.exclude(lake_data[[n]][,2,drop=F]), trace=F)

		# New lake-level time series based on residuals
		lake_new=as.matrix(lake_gfit1@residuals)
		
		#Fit the AR 
		ar_mod = ar(lake_gfit1@residuals)
		lake_new_use = ar_mod$resid

		# New time series after removing NAs in the rain
		lake_r = lake_data[[n]]
		lake_r$level = lake_new_use #Replace lake level with residuals
		lake_r = lake_r[!is.na(lake_r[,"rn",drop=T]),]

		# The best-fit GAMs were determined in Usinowicz et al. 2016. 
		# Those are what are fit here.
		# Use bam() (instead of gam()) from mgcv because it is designed for 
		# large data sets.

		# lake_models[[n]] = bam( as.formula(model_form[[n]]), method = "REML", optimizer = c("efs"),
		# 	correlation = corARMA( form = ~ 1 | time, p = ar_ord), data=lake_r )

		lake_models[[n]]$gam = bam( as.formula(model_form[[n]]), data=lake_r )
		lake_models[[n]]$ar = ar_mod


	}

	return(lake_models)

}

###############################################################################
# predictFlashGAM uses the fitted GAM to predict future rain
# It returns fitted data points with SE for the number of future precipitation 
# events that have been given to it. 
# lake_models 			Is a list with an element for each lake. Each element 
#						in the list has two components: a gam model fitted by 
#						mgcv, and an ar model fitted by ar. 
###############################################################################

predictFlashGAM = function(lake_data, fut_precip, lake_models){
		#Temporary data sets
		pr_tmp_all = vector("list", n_lakes)
		lt_use_all = vector("list", n_lakes)
		lt_use_save = vector("list", n_lakes)		
		ld_use_all = vector("list", n_lakes)
		lt_all = vector("list", n_lakes)
		l_arl = matrix(0, n_lakes,1)

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
      l_arl[n] = length (ar_lake)
      l_arr = length (ar_rain)

      #Which AR is larger? 
      if(l_arl[n]>l_arr){ lar = l_arl[n]}else{lar = l_arr}
      
      #Get the last section of data table for lags
      lt_all[[n]] = tail(lake_data[[n]], l_arl[n])
      
      #Look for an NA in most recent rn, this happens
      if(sum(is.na(lt_all[[n]]$rn)) > 0){  
        lt_all[[n]]$rn[is.na(lt_all[[n]]$rn)] = mean(lt_all[[n]]$rn,na.rm=T)
      }

      #The start of the new data set for prediction with 
      #the first new day
      all_cols = dim(lt_all[[n]])[2]
      rn_strt = min(ar_rain)-1
      lt_tmp = as.data.frame(c(ntime+1, NA, lt_all[[n]][l_arl[n],2:(l_arl[n]+1)],
      	lt_all[[n]][l_arl[n],(l_arl[n]+3):(rn_strt-1)],
        fut_precip[1,2],
        lt_all[[n]][l_arl[n],rn_strt:(all_cols-1) ] ) )
      colnames(lt_tmp) = colnames(lt_all[[n]])
      lt_new = rbind( lt_all[[n]],lt_tmp) 
      
      #Initialize new data set
      lt_use_all[[n]] = lt_new[l_arl[n]+1,]
      ld_use_all[[n]]  = lake_data[[n]] #For the AR prediction
      lt_use_save[[n]] = lt_use_all[[n]] #Record with updated preds

      #Temporarily store predicted lake level and SE
      pr_tmp_all[[n]] = matrix(0, n_days, 4 )
      pr_tmp_all[[n]][,1] = ntime+(1:n_days)
      pr_tmp_all[[n]][,2] = fut_precip$rn
      colnames(pr_tmp_all[[n]]) = c("time", "rn", "level", "se")
		}
        
    #Now make the forecasts:
   	#Version 3: An iterative prediction approach where
    #the AR is predicted first, then the GAM, then the 
    #two are added. 
    for (t in 1:n_days){
    	#First make each lake's one-day ahead forecast
  		for (n in 1:n_lakes){
        pt = predict(lake_models[[n]]$gam, newdata=lt_use_all[[n]] ,se.fit=TRUE, type ="response")
        ll_ar = ar(ld_use_all[[n]]$level)
        ll_tmp1 = predict(ll_ar, n.ahead = 1, se.fit=TRUE)

        pr_tmp_all[[n]][t,3] =pt$fit[1] + ll_tmp1$pred[1]
        pr_tmp_all[[n]][t,4] = pt$se[1] + ll_tmp1$se[1]

				#Replace NA with prediction
       	lt_use_save[[n]]$level[t] = pr_tmp_all[[n]][t,3] 
	        
      }

      #Now update the lags in lt_use with data for this day, 
      #but don't do this for n_days
      if (t < n_days){ 
      	for(n in 1:n_lakes) {
      		#Indexes of other lakes
    			other_lakes = 1:n_lakes
    			other_lakes = other_lakes[-n]

    			#Index of rain
    			ind_rain = grep("rn", (colnames(lake_data[[n]]))) 
  
      		#Create the next time step using current predictions
    			#First add current lake level and lags

	        ltu_tmp = c(ntime+1, NA, lt_use_save[[n]][t,2:(l_arl[n]+1)])

        	#Next, cycle through other lakes
    			for (q in 1:(n_lakes-1)){
    			ltu_tmp = c(ltu_tmp, NA,
    								 pr_tmp_all[[other_lakes[q]]][t,3], 
    								 lt_use_save[[other_lakes[q]]][t,2:(l_arl[n])] )
    			}

    			#Add the rain
    			ltu_tmp = c(ltu_tmp,
    							 pr_tmp_all[[n]][(t+1),2], 
    							 lt_use_save[[n]][t,ind_rain[c(- length(ind_rain) )] ] )

    			lt_use_all[[n]] = as.data.frame(ltu_tmp)
	        colnames(lt_use_all[[n]]) = colnames(lt_all[[n]])
	        lt_use_save[[n]] = as.data.frame(rbind(lt_use_save[[n]], lt_use_all[[n]]) )
	     
	      }
      }else{   }


    }


    for (n in 1:n_lakes){
			pred_lakes[[n]]  = as.data.frame(pr_tmp_all[[n]])
			pred_lakes[[n]]$time = fut_precip$time

			#This will keep adding the newest forecasts to the same file to keep
			#a rolling table of past predictions.
			tbl_file = paste("./data/gam_",n,"_forecast.csv", sep="")
			if(file.exists(tbl_file)){
				tbl_tmp = read.csv(tbl_file)
				tbl_tmp$time = as.Date(ymd(tbl_tmp$time)) 
				tbl_row = dim(tbl_tmp)[1]
				tbl_col = dim(tbl_tmp)[2]
				#Add a new row
				tbl_tmp = rbind(tbl_tmp, tbl_tmp[1,])
				#Overwrite the existing data in the window 
				#with the new predictions
				tbl_tmp[( tbl_row-(n_days-2) ):(tbl_row+1),] = pred_lakes[[n]]
				write.table(tbl_tmp, file = tbl_file, sep=",",row.names=FALSE)
			
			}else {
				#If the file does not already exist
				tbl_tmp = pred_lakes[[n]]
				write.table(tbl_tmp, file = tbl_file, sep=",",row.names=FALSE)
			}
		}

    save(file = "./data/gams_forecast.var", lake_models_forecast )
    
}

##############################################################
# Generator function
# 
# Exactly copied generator function from Chollet and Allaire 
# (p.195) but, allow which column of data is the one to be 
# predicted to be an input.
#
#	data — The original array of floating-point data
# lookback — How many timesteps back the input data should go.
# delay — How many timesteps in the future the target should be.
# min_index and max_index — Indices in the data array that 
#			delimit which timesteps to draw from. This is useful for 
#			keeping a segment of the data for validation and another for testing.
#	shuffle — Whether to shuffle the samples or draw them in 
#			chronological order.
#	batch_size — The number of samples per batch.
#	step — The period, in timesteps, at which you sample data. 
##############################################################

generator <- function(data, lookback, delay, min_index, max_index,
                      shuffle = FALSE, batch_size, step, 
                      predseries) {
  
  if (is.null(max_index)) max_index <- nrow(data) - delay - 1
  i <- min_index + lookback
  function() {

    if (shuffle) {
      rows <- sample(c((min_index+lookback):max_index), size = batch_size)
    } else {
      if (i + batch_size >= max_index)
        i <<- min_index + lookback
      	rows <- c(i:min(i+batch_size, max_index))
      	i <<- i + length(rows)
		}

    samples <- array(0, dim = c(length(rows),
                                lookback / step,
                                dim(data)[[-1]]))

    targets <- array(0, dim = c(length(rows)))
    
    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback, rows[[j]],
                     length.out = dim(samples)[[2]])
      samples[j,,] <- data[indices,]
      targets[[j]] <- data[rows[[j]] + delay,predseries]
    }
    list(samples, targets)
  }
}

##############################################################
#Machine learning section
##############################################################

##############################################################
# Wrap keras and the RNN model fitting and prediction in a 
# function that can be called by global.R and server.R. 
# This version uses keras to initialize, create lstm layer, 
# relu, adam. 
#
# Predict the next N days of lake level using historical data
# of lake level and precip. 
#
# This is a DNN with two dense layers. 
#
# Parameters for the generator function, which can 
# influence how well the models train: 
#
# lookback	How long of a series to use at a time
# step 			Which time points (1 = use every time point)
# delay			Number of time steps into the future to predict
# batch_size
# predser 	Index of label	
##############################################################

fit_predDNN = function(lake_data_lstm,fut_precip_scaled, lagsp, 
	lookback = 10, step = 1, delay = 1, batch_size = 20, predser=1 ){
	
	#Store fitted models
	lake_models_dnn = vector("list", n_lakes)

	#Forecasts
  #How many days are we forecasting? 
  n_days = dim(fut_precip_scaled)[1]

	#Iterate forecasts for uncertainty
	nits = 1
	lake_forecast_dnn = vector("list", n_lakes)
	for (n in 1:n_lakes ){
		lake_forecast_dnn[[n]] = matrix(0,lagsp,nits)
	}

	#Model definition: 
	#This function is for tensorflow to create the LSTM. This is 
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

	for(n in 1:n_lakes){ 

	##########################################################################
	#Create the save points for models
	##########################################################################

		dnn_checkpoint_path = paste("./DNN/", "lakeDNN",n,".tf", sep="")
		dnn_checkpoint_dir = fs::path_dir(dnn_checkpoint_path)
		dnn_log = "logs/run_dnn"

		# Create a callback that saves the model's weights
		dnn_callback = callback_model_checkpoint(
	  	filepath = dnn_checkpoint_path,
	  	#save_weights_only = TRUE,
	  	verbose = 1
		)
		
		##########################################################################
		#Data processing section to make data sets: truncate, normalize, split 
		##########################################################################
		#Note: balance lookback, delay, and batch size to maximize training 
		#efficiency! 

		#Chop off time 
		ld_tmp = lake_data_lstm[[n]][,-1]

		#Chop off the first lagged rows with NAs:
		ld_tmp = ld_tmp[-(1:(lags+1)), ]
		ntime_full = dim(ld_tmp)[1]

		#Need to convert ld_tmp to a matrix
		ld_tmp = as.matrix(ld_tmp)

		#Since the model fitting has already been done elsewhere, use
		#the whole set for training: 
		min_train = 1
		max_train = NULL 

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

		##########################################################################
		#Model fitting section.
		##########################################################################
		#Build the model

		#If it exists, load it. Otherwise, compile it fresh:  
		if(file.exists(dnn_checkpoint_path)){ 
			lake_models_dnn[[n]]  = load_model_tf(paste(dnn_checkpoint_path) ) 
		}else {  
			lake_models_dnn[[n]]  = build_and_compile_dnn()
		}

		#Fit the model to training data
		#tensorboard(dnn_log )

		lake_models_dnn[[n]] %>% fit(
			train_gen,
			steps_per_epoch = 80, #test_steps,
	  	epochs = 20,
			callbacks = list(dnn_callback,callback_tensorboard(dnn_log )) # Pass callback to training
		)

			##########################################################################
			#Forecasting
			##########################################################################
		 	ot = dim(lake_data_lstm[[n]])[[1]]
		 	#Get the last section of data table for lags
			lt = as.matrix(tail(as.data.frame(ld_tmp),100))
 
      #Temporarily store predicted lake level
      pr_tmp = matrix(0, n_days, 3)
      pr_tmp[,1] = fut_precip_scaled$rn
      colnames(pr_tmp) = c("rn", "level", "se")

      cur_pred = 100 #Start an index for predicted value

      for (t in 1:n_days){

				for ( p in 1:nits){
					lake_forecast_dnn[[n]][t,p] = 
							predict(lake_models_dnn[[n]], lt)[cur_pred] 
				}

        pr_tmp[t,2] = mean(lake_forecast_dnn[[n]][t,])
        pr_tmp[t,3] = sqrt(var(lake_forecast_dnn[[n]][t,] ))

        #Now update lt with the forecasted data
        if (t < n_days){ 
        	lt_add = t( c(pr_tmp[t,2],lt[cur_pred,1:(lagsp-1)],
          pr_tmp[t,1],
          lt[cur_pred,(lagsp+1):(dim(ld_tmp)[2]-1) ] ) )
          rownames(lt_add) = (ot+t)
          lt = rbind(lt, lt_add)
          cur_pred = cur_pred+1
        }else{   }
      
      }

			#This will keep adding the newest forecasts to the same file to keep
			#a rolling table of past predictions.
			tbl_file = paste("lakemodel_",n,"_DNNforecast.csv", sep="")
			if(file.exists(tbl_file)){
				tbl_tmp = read.csv(tbl_file)
				tbl_row = dim(tbl_tmp)[1]
				tbl_col = dim(tbl_tmp)[2]
				#Add a new row
				tbl_tmp = rbind(tbl_tmp, matrix(0,1,tbl_col))
				#Overwrite the existing data in the window 
				#with the new predictions
				tbl_tmp[( tbl_row-(lagsp-2) ):(tbl_row+1),] = lake_forecast_dnn[[n]]
				write.table(tbl_tmp, file = tbl_file, sep=",",row.names=FALSE)
			
			}else {
				#If the file does not already exist
				tbl_tmp = lake_forecast_dnn[[n]]
				write.table(tbl_tmp, file = tbl_file, sep=",",row.names=FALSE)
			}



	}

	save(file = "./data/todays_DNNforecast.var", lake_forecast_dnn )

}


##############################################################
# Wrap keras and the RNN model fitting and prediction in a 
# function that can be called by global.R and server.R. 
# This version uses keras to initialize, create lstm layer, 
# relu, adam. 
#
# Predict the next N days of lake level using historical data
# of lake level and precip. 
#
# This is a DNN with two dense layers. 
#
# Parameters for the generator function, which can 
# influence how well the models train: 
#
# lookback	How long of a series to use at a time
# step 			Which time points (1 = use every time point)
# delay			Number of time steps into the future to predict
# batch_size
# predser 	Index of label	
##############################################################

fit_predCNNLSTM = function(lake_data_lstm,fut_precip_scaled, lagsp, 
	lookback = 10, step = 1, delay = 1, batch_size = 20, predser=1, epochs=2 ){
	
	#Store fitted models
	lake_models_cnnlstm = vector("list", n_lakes)

	#Forecasts
  #How many days are we forecasting? 
  n_days = dim(fut_precip_scaled)[1]

	#Iterate forecasts for uncertainty
	nits = 1
	lake_forecast_cnnlstm = vector("list", n_lakes)
	for (n in 1:n_lakes ){
		lake_forecast_cnnlstm[[n]] = matrix(0,(lagsp+1),nits)
	}

	#Model definition: 
	#This function is for tensorflow to create the RNN. This is 
	#analogous to the "model form"   
#CNN plus LSTM
	build_and_compile_cnnlstm = function() {
		model = keras_model_sequential() %>%
			layer_conv_1d(
					filters=64, 
					kernel_size=4, 
					padding = "causal", 
					activation="relu",
					input_shape = list(NULL, dim(ld_tmp)[[-1]])
				) %>%
	    layer_lstm(
	    		units = 64,  
					return_sequences = TRUE,
					#stateful = TRUE
				) %>%
    	layer_dropout(rate = 0.5) %>%
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

		cnnlstm_checkpoint_path = paste("./CNNLSTM/", "lakeCNNLSTM",n,".tf", sep="")
		cnnlstm_checkpoint_dir = fs::path_dir(cnnlstm_checkpoint_path)

		cnnlstm_log = "logs/run_cnnlstm"

		# Create a callback that saves the model's weights
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

		#Chop off time 
		ld_tmp = lake_data_lstm[[n]][,-1]

		#Chop off the first lagged rows with NAs:
		ld_tmp = ld_tmp[-(1:(lags+1)), ]
		ntime_full = dim(ld_tmp)[1]

		#Need to convert ld_tmp to a matrix
		ld_tmp = as.matrix(ld_tmp)

		#Since the model fitting has already been done elsewhere, use
		#the whole set for training: 
		min_train = 1
		max_train = NULL 

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

		##########################################################################
		#Model fitting section.
		##########################################################################
		#Build the model

		#If it exists, load it. Otherwise, compile it fresh:  
		if(file.exists(cnnlstm_checkpoint_path)){ 
			lake_models_cnnlstm[[n]]  = load_model_tf(paste(cnnlstm_checkpoint_path) ) 
		}else {  
			lake_models_cnnlstm[[n]]  = build_and_compile_cnnlstm()
		}

		#Fit the model to training data
		#tensorboard(dnn_log )

		lake_models_cnnlstm[[n]] %>% fit(
			train_gen,
			steps_per_epoch = 80, #test_steps,
	  	epochs = epochs,
			callbacks = list(cnnlstm_callback,callback_tensorboard(cnnlstm_log )) # Pass callback to training
		)
	}

	##########################################################################
	#Forecasting
	##########################################################################
	#Temporarily store predicted lake level
	pr_tmp_all = vector("list", n_lakes)
	lt_tmp1_all = vector("list", n_lakes) 
	lt_tmp_all = vector("list", n_lakes)
	lt_all = vector("list", n_lakes)
 	
 	#Original ending time:
 	ot = dim(lake_data_lstm[[n]])[[1]]

	for(n in 1:n_lakes) {

		#Chop off time 
		ld_tmp = lake_data_lstm[[n]][,-1]

		#Chop off the first lagged rows with NAs:
		ld_tmp = ld_tmp[-(1:(lags+1)), ]
		ntime_full = dim(ld_tmp)[1]

		#Need to convert ld_tmp to a matrix
		ld_tmp = as.matrix(ld_tmp)

	 	#Get the last section of data table for lags
		lt_tmp1_all[[n]] = as.matrix(tail(as.data.frame(ld_tmp),100))
		lt_tmp_all[[n]] = lt_tmp1_all[[n]][1:99, ]	
		lt_all[[n]] = array( lt_tmp_all[[n]], 
											dim = c(1, dim(lt_tmp_all[[n]])[1], 
												dim(lt_tmp_all[[n]])[2] ) )

    #Temporarily store predicted lake level
    pr_tmp_all[[n]] = matrix(0, n_days+1, 3)
    pr_tmp_all[[n]][1,1] = lt_tmp1_all[[n]][100,colnames(lt_tmp_all[[n]])=="rn"] 
    pr_tmp_all[[n]][2:(n_days+1),1] = fut_precip_scaled$rn
    colnames(pr_tmp_all[[n]]) = c("rn", "level", "se")
	}
      
  cur_pred = 99 #Start an index for predicted value

  #Now make the forecasts:
  for (t in 1:(n_days+1)){
  	#First make each lake's one-day ahead forecast
  	for (n in 1:n_lakes){
				for ( p in 1:nits){
					lake_forecast_cnnlstm[[n]][t,p] = 
							predict(lake_models_cnnlstm[[n]], lt_all[[n]])[cur_pred] 
				}

        pr_tmp_all[[n]][t,2] = mean(lake_forecast_cnnlstm[[n]][t,])
        pr_tmp_all[[n]][t,3] = sqrt(var(lake_forecast_cnnlstm[[n]][t,] ))
   	}

    #Now update lt_all with the forecasted data
    if (t < (n_days+1)){ 
    	for(n in 1:n_lakes) { 
    		#Indexes of other lakes
    		other_lakes = 1:n_lakes
    		other_lakes = other_lakes[-n]

    		#Create the next time step using current predictions
    		#First add current lake level and lags
    		lt_add = c(pr_tmp_all[[n]][t,2],
    									lt_all[[n]][1,cur_pred,1:(lagsp-1)] )
    		
    		#Next, cycle through other lakes
    		for (q in 1:(n_lakes-1)){
    			lt_add = c(lt_add,
    								 pr_tmp_all[[other_lakes[q]]][t,2], 
    								 lt_all[[other_lakes[q]]][1,cur_pred,1:(lagsp-1)] )
    		}

    		#Add the rain
    		lt_add = c(lt_add,
    							 pr_tmp_all[[n]][t,1], 
    							 lt_all[[n]][1,cur_pred,(lagsp*4+1):(lagsp*4+(lagsp-1)) ] )

    		#Add the month and year encodings: 
    		lt_add = c(lt_add,
    								lt_all[[n]][1,cur_pred,(lagsp*4+(lagsp+1)):(dim(ld_tmp)[2]) ]
    								)

    		#Make it a matrix and add the current time index
    		lt_add = t(lt_add)
    		rownames(lt_add) = (ot+t)

    		#Add this new row to lt
    		lt_new = rbind(lt_all[[n]][1,,],lt_add)
				lt_all[[n]] = array( lt_new, 
									dim = c(1, dim(lt_new)[1], dim(lt_new)[2] ) )
			}
      cur_pred = cur_pred+1
    }else{   }
  
	}

		#This will keep adding the newest forecasts to the same file to keep
		#a rolling table of past predictions.
	for(n in 1:n_lakes){
		tbl_file = paste("./data/lakemodel_",n,"_CNNLSTMforecast.csv", sep="")
		if(file.exists(tbl_file)){
			tbl_tmp = read.csv(tbl_file)
			tbl_row = dim(tbl_tmp)[1]
			tbl_col = dim(tbl_tmp)[2]
			#Add a new row
			tbl_tmp = rbind(tbl_tmp, matrix(0,1,tbl_col))
			#Overwrite the existing data in the window 
			#with the new predictions
			tbl_tmp[( tbl_row-(lagsp-1) ):(tbl_row+1),] = lake_forecast_cnnlstm[[n]]
			write.table(tbl_tmp, file = tbl_file, sep=",",row.names=FALSE)
		
		}else {
			#If the file does not already exist
			tbl_tmp = lake_forecast_cnnlstm[[n]]
			write.table(tbl_tmp, file = tbl_file, sep=",",row.names=FALSE)
		}
	}

	save(file = "./data/todays_CNNLSTMforecast.var", lake_forecast_cnnlstm )

}


##############################################################
# Wrap keras and the LSTM model fitting and prediction in a 
# function that can be called by global.R and server.R. 
# This version uses keras to initialize, create lstm layer, 
# relu, adam. 
#
# Predict the next N days of lake level using historical data
# of lake level and precip. 
##############################################################

fit_predLSTM = function(lake_data_lstm, lagsp ){
	#Store fitted models
	lake_models_lstm = vector("list", n_lakes)

	#Forecasts
	#Iterate forecasts for uncertainty
	nits = 300
	lake_models_forecast = vector("list", n_lakes)
	for (n in 1:n_lakes ){
		lake_models_forecast[[n]] = matrix(0,lagsp,nits)
	}

	#Performance and prediction 
	pred_train = vector("list", n_lakes)
	pred_test = vector("list", n_lakes)

	#Need to make the lake data a 3D array and to separe out the 
	#lake level and rain lags into separate matrixes (along the 
	#3rd array dimension):
	lake_data3D = vector("list", n_lakes)

	#Model definition: 
	#This function is for tensorflow to create the LSTM. This is 
	#analogous to the "model form"   

	build_and_compile_model = function() {
		model = keras_model_sequential() %>%
		bidirectional(
		layer_lstm(units = 64, # size of the layer
			activation = 'relu',
			# batch size, timesteps, features
			batch_input_shape = c(1, lagsp, 2), 
			return_sequences = TRUE,
			stateful = TRUE) ) %>%
		# fraction of the units to drop for the linear transformation of the inputs
		layer_dropout(rate = 0.65) %>%
		layer_lstm(units =64,
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

	##########################################################################
	#Processing section to convert each lake_data[[n]] to correct 
	#format for LSTM. This includes an X and Y data set. 
	##########################################################################
	for(n in 1:n_lakes){ 
			checkpoint_path = paste("./LSTM/", "lakeLSTM",n,".tf", sep="")
			checkpoint_dir = fs::path_dir(checkpoint_path)

			# Create a callback that saves the model's weights
			cp_callback = callback_model_checkpoint(
			  filepath = checkpoint_path,
			  #save_weights_only = TRUE,
			  verbose = 1
			)

			#Chop off time 
			ld_tmp = lake_data_lstm[[n]][,-1]

			#Chop off the first lagged rows with NAs:
			ld_tmp = ld_tmp[-(1:(lagsp)), ]

			ntime_full = dim(ld_tmp)[1]

			#Use last row for prediction: 
			lake_pred = ld_tmp [(ntime_full), ]
			ld_tmp = ld_tmp[1:(ntime_full-1), ]

			#Split the remaining data into X and Y for training.
			#E.g. if dim(ld_tmp)[1] = 100, then X will be 1:(100-lags)
			#and Y will be lags:100. 
			ntime_tmp = dim(ld_tmp)[1]
			ld_tmp_x = ld_tmp[1:(ntime_tmp-lagsp),]
			ld_tmp_y = ld_tmp[(lagsp+1):(ntime_tmp),]
			
			#Split the features in both X and Y, as well as the test 
			#prediction matrix, into arrays as 3D objects
			lake_data3D_x = array(data = as.numeric(unlist(ld_tmp_x)), 
				dim = c(nrow(ld_tmp_x),
					ncol(ld_tmp_x)/2,2) )

			lake_data3D_y = array(data = as.numeric(unlist(ld_tmp_x)), 
				dim = c(nrow(ld_tmp_y),
					ncol(ld_tmp_y)/2,2) )

			lake_data3D_test = array(data = as.numeric(unlist(lake_pred)), 
				dim = c(nrow(lake_pred),
					ncol(lake_pred)/2,2) )

			##########################################################################
			#Model fitting section.
			##########################################################################
			#Build the model

			#If it exists, load it. Otherwise, compile it fresh:  
			if(file.exists(checkpoint_path)){ 
				lake_models_lstm[[n]]  = load_model_tf(paste(checkpoint_path) ) 
			}else {  
				lake_models_lstm[[n]]  = build_and_compile_model()
			}

			#lake_models_lstm[[n]]  = build_and_compile_model()

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

			for ( p in 1:nits){
				lake_models_forecast[[n]][,p] = lake_models_lstm[[n]]  %>%
						predict(lake_data3D_test, batch_size = 1, verbose =F) %>%
						.[, , 1]
			}

			#This will keep adding the newest forecasts to the same file to keep
			#a rolling table of past predictions.
			tbl_file = paste("lakemodel_",n,"_forecast.csv", sep="")
			if(file.exists(tbl_file)){
				tbl_tmp = read.csv(tbl_file)
				tbl_row = dim(tbl_tmp)[1]
				tbl_col = dim(tbl_tmp)[2]
				#Add a new row
				tbl_tmp = rbind(tbl_tmp, matrix(0,1,tbl_col))
				#Overwrite the existing data in the window 
				#with the new predictions
				tbl_tmp[( tbl_row-(lagsp-2) ):(tbl_row+1),] = lake_models_forecast[[n]]
				write.table(tbl_tmp, file = tbl_file, sep=",",row.names=FALSE)
			
			}else {
				#If the file does not already exist
				tbl_tmp = lake_models_forecast[[n]]
				write.table(tbl_tmp, file = tbl_file, sep=",",row.names=FALSE)
			}



	}

	save(file = "./data/todays_forecast.var", lake_models_forecast )

}



##############################################################
# Get the names of smooth variables from an mgcv GAM object
##############################################################
get.var.names = function (gam.model){ 

no.sm.vars=length(gam.model$smooth)
names.list=NULL
for (a in 1:no.sm.vars){ 
	names.list=c(names.list,gam.model$smooth[[a]]$label)}
	names.list=list(names.list)
return (names.list)

}

##############################################################
#Calculate proportionate variance from covariates
# 
# Each covariate or group of covariates in a linear model can be 
# be viewed as either amplifying (>1) or damping (<1) fluctuations in the response
# variable due to being multiplicative constants. This function uses that
# fact to calculate whether particular covariates are responsible for increasing
# or decreasing variance in the response relative to any other. For the flashiness
# studies, we are particularly interested in whether changing watershed characterstics
# or changing precipitation patterns are more responsible for amplifying 
# variance (i.e. driving changes in flashiness).    
# 
# This function corresponds to equation (2) in Usinowicz et al 2016. Its
# derivation is based first on the observation that the total variance in the predicted
# values of a fitted model is equal to the sum of the squared coefficients, multiplied
# by the variance in each variable. For an AR(1) model this would look something like: 
# 		
# 		var(Ri) = sum (from i to n) (B_i)^2 * var(V_i)/ (1-a1^2)
#
# In order to analyze a certain related set of variables, it is easiest to 
# refit the GAM with only those variables. In our case, we only fit the variables on 
# precipitation, including the interactions between rain and time and the interactions between
# rain lags. We then take the ratio of var(Ri)_peak/ var(Ri)_base, which effectively cancels the 
# variance arising from the AR terms regardless of the order p of AR(p) (this variance is 
# constant across a period due to the detrending of the GARCH models and is thus the 
# same in every period). 
#
# For this code, however, the approach is even simpler and more robust. First, fit a GAM
# model that does not include any of the AR terms, and instead only includes parameters fitted on
# the covariate of interest -- namely precipitation, for the flashiness. This is shown in the main
# workflow below. Then, using the fitted model containing only the precipitation lags, interactions, 
# and precip*time interactions, it is straightforward to extract the impact of the coefficients vs. 
# the impact of variance in the precipitation. 
# 
# 	model 			a fitted GAM model from mgcv
#	peaks			a 2 x n matrix designated the periods that serve as the baseline 
#					variance (first column) and variance peaks(subsequent columns).	This are specified
#					as vector indices, not as dates, etc. 
#	covar.names 	a vector with two elements: the name of the response variable, and the name
#					of the primary covariate for the variance ratio (i.e. rain for flashiness)

variance.ratios = function ( model, peaks, covar.names ) {

#Test that peaks are all within the length of the model residuals
if( min(peaks) < 1){ 
	stop("Starting peak time is too small") }
if( max(peaks) > length(model$model[[1]])){ 
	stop("Maximum peak time is outside of the range of data") }

#All of the covars in the model
covars.all=colnames(model$model)
tot.vars = length(covars.all)

#Number of periods
pers = ncol(peaks)

#Variables for calculation
rV = matrix(0,pers,1)
Xp = matrix(0,pers,1)
Vtot = matrix(0,pers,1)
t.chck = matrix(0,pers,1)

# Test that there are at least one baseline and one peak period
if(pers<2) {
	stop("Not enough periods specified! At least two must be specified (i.e. cols(peaks)>=2)")
}


#Calculate variance for all periods 

for (n in 1:pers) { 
	ns = length(peaks[1,n]:peaks[2,n])
	#Total variance of response
	rV[n] = var(model$model[covar.names[2]][peaks[1,n]:peaks[2,n],1])
	#The data for simulation
	new.dat = (model$model[covars.all[1:tot.vars]])[peaks[1,n]:peaks[2,n], ]
	#Simulated response from model
	Xp = predict(model, newdata = new.dat,type="lpmatrix") 
	#Get the names of terms
	names.list=get.var.names(model)
	names.list.keep=get.var.names(model)

	#Get the variance produced by each term

	for (a in 1:length(names.list[[1]])) { 
		names.list[[1]][a] =var(Xp[,(model$smooth[[a]]$first.para):(model$smooth[[a]]$last.para)]
							%*%coef(model)[(model$smooth[[a]]$first.para):(model$smooth[[a]]$last.para)],na.rm=T) 
		}

	all.var = as.numeric(unlist(names.list[[1]]))
	#Total variance, including covariate and the coefficients
	Vtot[n] = sum(all.var)

	#Just the impact of the coefficients. Note, this only works
	#under the assumption that all coefficients are fit to covariates
	#with the same variance! I.e. all are effects on rain at various lags,
	#or including time interactions with rain. 

	t.chck[n] = sum(all.var/rV[1])

	}

	var.rat = as.data.frame(cbind(t.chck,rV))
	colnames(var.rat) = c("t.chck","rV")

	return(var.rat)

}