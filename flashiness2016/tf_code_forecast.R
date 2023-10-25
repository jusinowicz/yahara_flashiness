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

#Performance and prediction 
pred_train = vector("list", n_lakes)
pred_test = vector("list", n_lakes)

#This function is for tensorflow to create a DNN. This is 
#analogous to the "model form"  
build_and_compile_model = function(norm) {
  model = keras_model_sequential() %>%
    norm() %>%
    layer_dense(64, activation = 'relu') %>%
    layer_dense(64, activation = 'relu') %>%
    layer_dense(1)

  model %>% compile(
    loss = 'mean_absolute_error',
    optimizer = optimizer_adam(0.001)
  )

  model
}

for(n in 1:n_lakes){ 

	# Use the residuals from the GARCH model so that the trends in variance are
	# removed. Note, this version only fits the GARCH part because the AR will be
	# fit by the GAM: 

	lake_gfit1=garchFit( ~arma(0,0)+garch(1,1),
				 data=na.exclude(lake_data[[n]][,2,drop=F]), trace=F)

	# New lake-level time series based on residuals
	lake_new=as.matrix(lake_gfit1@residuals)

	#The ML models will primarily pick up on AR when it is present so remove this.
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
	#(This is an alternative approach to using MinMaxScaler with 
	#fit_transform and transform)
	normalizer = layer_normalization (axis = -1L)
	adapt(normalizer, as.matrix(train_f))

	#Build the model
	lake_models[[n]]  = build_and_compile_model(normalizer)
	#summary(dnn_model)

	#Fit the model to training data
	pred_train[[n]] = lake_models[[n]]  %>% fit(
	  as.matrix(train_f),
	  as.matrix(train_l),
	  validation_split = 0.2,
	  verbose = 0,
	  epochs = 100
	)

	#Evaluate the performance on test data
	pred_test[[n]] = lake_models[[n]]  %>% evaluate(
	  as.matrix(test_f),
	  as.matrix(test_l),
	  verbose = 0
	)


}

##############################################################
#PART 3: Look at performance and importance of variables
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
