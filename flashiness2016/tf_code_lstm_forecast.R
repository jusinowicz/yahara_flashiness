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
source("./../functions/flash_functions.R")
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

    #Keep the dates
    lake_dates[[n]] = lake.tmp$time 

    #as_date(date_decimal(as.numeric(time(lake.tmp))))

 	#This final step creates the full data object, with lags of 
	#lake level and lags of rain. Note, this is different than it 
    	#is for the GAM/statistical modelling stuff! For this RNN the 
     #lags are the number of days into the future we wish to forecast.

    	#Assume the future precip will have 7 days:
	lake_data[[n]] = make.flashiness.object(data.frame(level= lake.tmp$level),
	 data.frame(rn=rn.tmp$rn), lags, auto=F, orders=lags)
  }

##############################################################
#PART 2: keras and tensorflow
##############################################################

#Store fitted models
lake_models_lstm = vector("list", n_lakes)

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
	layer_lstm(units = 64, # size of the layer
		activation = 'relu',
		# batch size, timesteps, features
        	batch_input_shape = c(1, lags+1, 2), 
	   	return_sequences = TRUE,
        	stateful = TRUE) %>%
	# fraction of the units to drop for the linear transformation of the inputs
	layer_dropout(rate = 0.5) %>%
	layer_lstm(units = 64,
             return_sequences = TRUE,
             stateful = TRUE) %>%
	layer_dropout(rate = 0.5) %>%
	time_distributed(layer_dense(units = 1))

  model %>% compile(
    loss = 'mean_absolute_error',
    optimizer = optimizer_adam(0.001)
  )

  model
}

for(n in 1:n_lakes){ 
	##########################################################################
	#Processing section to convert each lake_data[[n]] to correct 
	#format for LSTM. This includes an X and Y data set. 
	##########################################################################

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

	#Standardize both of these data sets (separately!) 
	#(mean =0, var = 1)
	scale_ld = cbind(colMeans(ld_tmp), diag(var(ld_tmp)) )
	ld_tmp = scale(ld_tmp)
	
	scale_test = matrix(
				c( mean(unlist(lake_test[1:(lags+1)])),
					mean(unlist(lake_test[(lags+2):(2*lags)])),
					sqrt(var(unlist(lake_test[1:(lags+1)]))),
					sqrt(var(unlist(lake_test[(lags+2):(2*lags)])))),
				2,2)

	lake_test[1:(lags+1)] = (lake_test[1:(lags+1)] - 
		scale_test[1,1])/scale_test[2,1]
	lake_test[(lags+2):(2*lags)] = (lake_test[(lags+2):(2*lags)] - 
		scale_test[2,1])/scale_test[2,2]
	
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
		verbose = 0,
		shuffle = FALSE #Important for LSTM!
	)

	#look at the forecast
	lake_forecast = lake_models_lstm[[n]]  %>%
	  predict(lake_data3D_test, batch_size = 1) %>%
	  .[, , 1]
	 
	lake_forecast*scale_ld[1,2]+scale_ld[1,1]
	lake_compare 

}

##############################################################
#PART 3: Look at performance and importance of variables
#Not implemented yet. 
#Use LIME to plot feature importance. 
##############################################################

class( lake_models[[n]])
# Setup lime::model_type().The only input is x, the keras model. 
# The function simply returns classification, which tells LIME we 
# are classifying.
model_type.keras.engine.sequential.Sequential = function(x, ...) {
  "classification"}

# Setup the prediction function for the model type
predict_model.keras.engine.sequential.Sequential = function (x, 
	newdata, type, ...) {
  pred <- predict(object = x, x = as.matrix(newdata))
  data.frame (Positive = pred, Negative = 1 - pred) 
}

#Test that we have set this up correctly
predict_model (x       = lake_models[[n]], 
               newdata = train_f, 
               type    = 'raw')

#Create an explainer with LIME. 
#If data were categorical, set bin_continuous = FALSE
explainer = lime(x = train_f, model= lake_models[[n]], 
	bin_continuous = TRUE)  

#Run explain on the explanation:
explanation = lime::explain (
    x = test_f[1:10, ],
    explainer    = explainer, 
    n_features = 4,
    n_labels = 1,  
    kernel_width = 0.5)

#Plot the features as bar plots with the built-in plot tool
plot_features (explanation)

ggplot(data.frame( pred = as.numeric(pred1),level = test_l$level)) +
  geom_point(aes(x = pred, y = level )) +
  geom_abline(intercept = 0, slope = 1, color = "blue")


#########################################################################
# This is junk from failed attempts to get feature significance to run
# I think some of this code might be useful later so I'm saving it here.
# conda create -n py3.11 python=3.11 scikit-learn pandas numpy matplotlib
#
# So far I have been unable to get any of the various 
# feature-significance methods/packages to run successfully
# on a keras DNN. Tried setting up reticulate to export 
# directly to python but this seems fraught with its own 
# difficulties. Giving up on making this work in R for now.
# Nothing below here actually works :(

use_condaenv("py3.11", required = TRUE)
use_condaenv("py3.10_tf", required = TRUE)

reticulate::conda_install(
    packages = c("lime", "scikit-learn","numpy","pandas"),
    envname = "py3.10_tf"
)

sklearn = import("sklearn") 
tf = import("tensorflow") 
np = import("numpy")
shp = import("shap") 
lm = import("lime")
pd = import("pandas")

explainer = lime$lime_tabular$LimeTabularExplainer(train_f, feature_names=list(train_f), class_names=[0, 1], mode='classification')

explainer = lm$lime_tabular$LimeTabularExplainer(train_f, training_labels=train_l, 
										 feature_names=train,
                                                   discretize_continuous=TRUE,
                                                   #class_names=['Falling', 'Rising'],
                                                   discretizer='decile')

explainer = shp$TreeExplainer(lake_models[[n]])
shap_values = explainer.shap_values(X)


shp$initjs()


nr = 10
nr = as.integer(nr)
rs = 0
rs = as.integer(rs)
sklearn$inspection$permutation_importance(lake_models[[n]], train_f, 
	train_l, n_repeats=nr, random_state=rs)  


explainer <- shapr(train_f, lake_models[[n]])

```{python}

1+1

```

n=1
plot(lake_models[[n]])
plot(pred_train[[n]])
plot(pred_test[[n]])

ind = sample(nrow(train_f), 500)
train_f_sub = train_f[ind, ]

ks = kernelshap(
    train_f_sub, 
    pred_fun = function(X) as.numeric(predict(lake_models[[n]], X, batch_size = nrow(X))), 
    bg_X = train_f_sub
  )


)
ks


el = lime:lime(train_f, lake_models[[n]])
explain(test_f, el, n_features = 12)
