# flashiness2016
#############################################################
# Code to aid in the analysis of flashiness according to 
# Usinowicz et al. 2016. 
# 
# This code is essentially designed to take basic USGS .csv data
# with lake level and precipitation, in combination with any other
# user-defined covariates (e.g. impervious surface) to investigate
# flashiness. 
# 
# This code should be viewed as a collection of useful routines, 
# as opposed to a single coherent program. To summarize the types
# of routines: 
#
#	1. Data processing: taking individual column vectors of the 
#		dependenet and various independent variables, removing
#		winter months and leap days, producing
#		lagged covariates (i.e. of precip and lake level), 
#		combining into a single object, and making a time series
#		object. 
#
# 	2. Analysis of heteroskedasticity: non-stationarity of the 
# 		variance is used as a first test for changing flashiness.
#
#	3. Fitting of GAM models: the role of various covariates in 
#		driving flashiness is investigated through the use of 
#		GAM models which allow flexible, non-parametric fits.
#		Model fitting is a very interactive process, which basically
#		requires adding and dropping covariates in a nested 
#		way while tracking changes in AIC. Thus this section should
#		serve largely as an example of how to do this, as opposed
#		to an automated tool. 
#
#	4. Proportion of variance explained: the GAM model can be analyzed
#		for each period of differing flashiness levels to determine what 
#		proportion of the variance is explained by each variable or 
#		set of variables. This is used to argue that flashiness is 
#		being more or less driven by any of these variables, relative
#		to others. 
#
#	5. Partial flood analysis with moving window: Standard USGS 
#		methods exist for calculating the probability that the stage level
# 		of any waterway will exceed certain thresholds in a given period
#		of time. Given that the variance in lake-level is non-stationary in
# 		Mendota, we adapt the standard approach with a moving-window that simply
#		applies the standard approaches to a limited 10-window. In this way,
#		we can track how the probability of certain thresholds increases or decreases
#		through time. For exampl, we can se that the probabiliyt of a 100-year flood
#		increases through time. 
#
#	6. Visualization: plotting the fitted GAM models in various ways 
#	 to ease interpretation, and visually confirming predicted lake	
#	 level values relative to reality. 
#
