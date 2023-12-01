The goal of this repository is to run a shiny app server which outputs predictions of the lake level for lakes
in the Yahara Watershed in Southern Wisconsin. At the moment it is only implemented for lakes Mendota and 
Monona, which border Madison, WI. 

##yahara_flash
The code to run the shiny app is in the folder yahara_flash. The app itself is currently hosted at
https://ecotheory.shinyapps.io/yahara_flash/. The app mostly loads data and plots it. 

The lake level data is retrieved from the USGS server 
https://waterservices.usgs.gov/nwis/iv/?format=rdb&sites=
Site keys: site_keys = c("05428000", "05429000")

Historical rain data comes from NASA with the R library nasapower. 
Coordinates: c(43.0930, -89.3727)

The precipitation forecast data uses the R library openmeteo. Same coordinates.

The fitted GAMMs are stored in the variable "lakeGAMsfull.var" and the GAMM forecast is made
on the shiny server. 

The fitted LSTMs are not stored on the shiny server (see below). The forecasts made by these models
are loaded from a file stored here in this github repository at: 
"https://raw.githubusercontent.com/jusinowicz/yahara_flashiness/master/yahara_flash_local/lakemodel_",n,"_forecast.csv"
where n is 1 for Mendota and 2 for Monona. 

#yahara_flash_local
There is a second version of the app in the folder yahara_flash_local which performs the actual updates 
of the forecasts. The ML model, which is currently built as a Long Short Term Memory (LSTM) Recurring
Neural Net (RNN), can take 10-15 minutes to run and so it does not make sense to host it on the shiny 
server itself. The models are run locally on another server, and then both of the LSTMs and the forecasts
are updated here. 

The LSTMs are stored in the folder LSTM.

The forecasts are in lakemodel_n_forecast.csv, where n has the same lake indexing as above.

#data
This is where the historical data sets get stored.

#flashines2016
This is the original code that matches the publication Usinowicz et al. 2016. 

This code is essentially designed to take basic USGS .csv data with lake level and precipitation, in 
combination with any other user-defined covariates (e.g. impervious surface) to investigate flashiness. 

This code should be viewed as a collection of useful routines, as opposed to a single coherent program. 
To summarize the types of routines: 

1. Data processing: taking individual column vectors of the
    dependenet and various independent variables, removing
    winter months and leap days, producing
    lagged covariates (i.e. of precip and lake level), 
    combining into a single object, and making a time series
    object. 

2. Analysis of heteroskedasticity: non-stationarity of the 
 		variance is used as a first test for changing flashiness.

3. Fitting of GAM models: the role of various covariates in 
		driving flashiness is investigated through the use of 
		GAM models which allow flexible, non-parametric fits.
		Model fitting is a very interactive process, which basically
		requires adding and dropping covariates in a nested 
		way while tracking changes in AIC. Thus this section should
		serve largely as an example of how to do this, as opposed
		to an automated tool. 

4. Proportion of variance explained: the GAM model can be analyzed
		for each period of differing flashiness levels to determine what 
		proportion of the variance is explained by each variable or 
		set of variables. This is used to argue that flashiness is 
		being more or less driven by any of these variables, relative
		to others. 

5. Partial flood analysis with moving window: Standard USGS 
		methods exist for calculating the probability that the stage level
 		of any waterway will exceed certain thresholds in a given period
		of time. Given that the variance in lake-level is non-stationary in
 		Mendota, we adapt the standard approach with a moving-window that simply
		applies the standard approaches to a limited 10-window. In this way,
		we can track how the probability of certain thresholds increases or decreases
		through time. For exampl, we can se that the probabiliyt of a 100-year flood
		increases through time. 

6. Visualization: plotting the fitted GAM models in various ways 
	 to ease interpretation, and visually confirming predicted lake	
	 level values relative to reality. 
#
