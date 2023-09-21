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
#############################################################


#############################################################
#
# Definition of some useful functions
#
#############################################################
#
#
#########
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

	length.var=length(variable1)
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

	variable1 = variable1[-rd]

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
		new.var1 = variable1[ rmove ]
						} else { 
		
		r1 = winter[1]:winter[2]
		freq = 365 - length(r1)
		for ( y in 1:nyears) { rmove = c(rmove, -(365*(y-1)+r1))}
		new.var1 = variable1[ rmove ]	
		}
	
		return(ts( as.matrix(new.var1), start=date.start, frequency=freq ))
 	} else { 

 		new.var1 = variable1
 		return(ts( as.matrix(new.var1), start=date.start, frequency=365 ))
	}
 	

 }


#########
# Data processing function 2: Combine all variables into a single
# dataset, including those generated as lags of a particular
# variable (e.g. precipitation or lake level), and return a data frame. 
#
#		response 	The variable that will be the response, i.e. lake
#				 	lake-level. 
#		lagged_covar Variables that will be lagged, usually just precipitation.
#		lags 		The number of lags for each lagged variable. The 
#					number of entries needs to match the number of columns
#					in lagged_covar. 
#		covar 		Additional covariates that will not be lagged. E.g.
#			 		impervious surface area. 
#		auto 		By default, this function will test the autoregression in
#					in the response and determined the appropriate number of 
#					lags. Setting this to false requires the user to input the 
#					autoregressive order. The ar(p) order determines the number 
#					of lags of the response to add to the final data object. 
#		orders		The order of the ar(p) model, which determines the number of 
#				 	lags of the response to add to the final data object.
#

make.flashiness.object = function (response, lagged_covar, lags, covar=NULL, auto=TRUE, orders=NULL) {

	#Make sure the lags are fully specified for lagged covars.

	if(dim(as.matrix(lagged_covar))[2]!= length(lags)) { 
		stop("Lags does not match number of lagged variables")
	}

	if( is.null(dim(response)) | is.null(dim(lagged_covar))) {
		stop ("Make sure the response and all covariates are at least column vectors (remove.days() will do this automatically).")}
		
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
				lag.r =as.matrix( c(r.cols[(rl+1):length(response)],matrix(NA, rl,1)))
				r.cols = cbind(r.cols, lag.r )  
				r.names = c(r.names, paste(names.list[1],rl,sep="")) }
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
					lag.c =as.matrix( c(lc.cols[(nl+1):length(response)],matrix(NA, nl,1)))
					lc.cols = cbind(lc.cols,lag.c)	
					col.names = c(col.names, paste(names.list[(nc+1)],nl,sep=""))
					}
				
				} 
				colnames(lc.cols) = col.names
				all.lc.cols = cbind(all.lc.cols,lc.cols)
			}
		
		} else { 	
		 all.lc.cols = NULL
		}

		#Make the full matrix
		full.tmp = cbind(1:(dim(all.lc.cols)[1]), r.cols,all.lc.cols,covar)
		colnames(full.tmp)=c("time", colnames(r.cols),colnames(all.lc.cols),colnames(covar))

	return( as.data.frame(full.tmp))
}


####### Function to get the names of smooth variables from an mgcv GAM object
#
get.var.names = function (gam.model){ 

no.sm.vars=length(gam.model$smooth)
names.list=NULL
for (a in 1:no.sm.vars){ 
	names.list=c(names.list,gam.model$smooth[[a]]$label)}
	names.list=list(names.list)
return (names.list)

}


####### Function for GAM prediction with autoregression
# mgcv provides functions for predicting values with a fitted model,
# but none of which are designed to work with autoregressive terms.
# This function is designed to use the existing predict.gam in a stepwise
# fashion, such that the AR terms can be replied to the response at
# each step. That is, instead of feeding the AR covariates directly to
# predict.GAM, this function first creates a new response variable by 
# using the AR terms seperately, then passing the results of this first
# prediction to predict.GAM. 
#	model 		A fitted GAM from mgcv
#	response 	The response variable
#	covars 		All of the covariates from the flashiness.object
#	ar.columns 	A vector giving the column indexes of the AR terms
#				in the flashiness.object
#	ns 			The number of time steps to predict (i.e. number of simulations)


predict_arGAMbs = function (model, response, covars, ar.columns, ns) {

model.sim=matrix(0,nrow=ns,ncol=1)

#Take the columns from covars that are used in the model and keep them in the same order
var.list=colnames(covars)[colnames(covars) %in%  names(attr(model$terms,"dataClasses"))]
covars.temp=covars[var.list]
covars.use=covars.temp
ar.list=colnames(covars)[ar.columns]
ar.columns= which(colnames(covars.temp) %in% ar.list)
order=length(ar.columns)

model.sim.lp=matrix(0,nrow=ns,ncol=sqrt(length(model$Vp)))

#ICs
for (rn in 1: order) {model.sim[rn,1]=response[rn]}

for (j in (order+1):ns) { 

k= ar.columns[1]:ar.columns[order]
covars.temp[j,k]=model.sim[j-(k-ar.columns[1])-1]
#If NaNs in simulation have real values in data set, replace them
covars.temp[j,k][is.na(covars.temp[j,k])]=covars[j,k][is.na(covars.temp[j,k])]

new=data.frame(as.matrix(covars.temp[j,]))
pred.mod=predict.gam(model, newdata=new)
pred.mod.lp=predict.gam(model, newdata=new,type="lpmatrix")
model.sim[j]=pred.mod
model.sim.lp[j,]=pred.mod.lp

}

model.sim.ret=list(model.sim, model.sim.lp)
return(model.sim.ret)
}

#######Function to calculate proportionate variance from covariates
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



##############################################################
#
# Main workflow  
#
##############################################################
library(mgcv) #GAM functions
library(fGarch) #For GARCH models


################################################
#
#######PART 1: Data processing
#Import lake-level time series as the response. These data for 
#lake Mendota downloaded from 
# https://waterdata.usgs.gov/nwis/dv/?site_no=05428000&agency_cd=USGS&amp;referred_module=sw
mendota_table=read.csv(file="mendota_example.csv")

#Import precipitation time series
rain_table = read.csv(file = "")

#Import other variables (e.g. impervious surface)
impervious_table=read.csv(file = "")


#Make the appropriate variables. This involves removing winter dates,
#then combining variables into a single matrix or frame. 
#For the Madison lakes, we estimated the safe ice-on and ice-off dates as 
#Dec. 1 and May 1., respectively.  
men = remove.days(mendota_table$level,1920)
rn = remove.days(rain_table,1920)
lags = 10 
mendota = make.flashiness.object(men, rn, lags)


#For this example I've pre-processed the data as tables that need the 
#following modifications (these are redundant and should be replaced with
#the previous code):
men=read.table("men_nw")
colnames(men) = c("stage", "elevation")
rn=read.table("pop_nw")
colnames(rn)=c("rn")
men.ts=as.matrix(ts(men, start=1916, frequency=214))
rn.ts=as.matrix(ts(rn, start=1916, frequency=214)) 
lags=10

mendota=make.flashiness.object(as.matrix(men[,2,drop=F]), rn, lags, auto = F, order=c(7))
mendota = as.data.frame(mendota)


###############################################
#
########PART 2: Analysis of heteroskedasticity
# The most straightforward test is to fit an AR model to the 
# squared residuals of an AR model. If this produces a model
# with significant terms, then the variance is non-stationary. 
men.ar1 = ar(mendota[,1,drop=F], na.action=na.exclude)
#If any of these coefficients are significantly >0, then it is an ARCH
men.arch=ar((men.ar1$resid^2),na.action=na.exclude)

# For more detailed information, and to produce the plots 
# in the paper, fit a GARCH model to the lake-level time series with
# garchFit from library fGarch. 
men.gfit=garchFit( ~arma(6,0)+garch(1,1), data=na.exclude(mendota[,2,drop=F]), trace=F)

#Looking at summary(men.gfit) gives a lot of information. 
# mu corresponds to the intercept of the regression.
# ar are the AR coefficients
# ma are the MA coefficients (non in this example)
# omega is the intercept of the modeled variance
# alpha is analogous to the autoregression of the variance
# beta is analagous to the moving average of the variance
#If alpha and/or beta is significant, then this confirms non-stationary
#variance. In this case, the actual values of alpha and beta are given.
#See ?garchFit for details about the data output. men.gfit@h.t is plotted
#in figure 2 of Usinowicz et al. 2015

#For comparison, calculate a moving window of the variance over some
#time period (e.g. 5 years). In the paper, we use this to calculate an
#average variance, then calculate the periods of high and low variance
#relative to this average. 
	
winter=c(334,120)
freq = 	abs(winter[1]-winter[2]) #Account for winter days removed
win.years = 5 #Set window in years
win1=freq*win.years
#Account for NAs being removed 
lnna=length(is.na(mendota[,1,drop=F]))-sum(is.na(mendota[,1,drop=F]))

men.var=matrix(0,(lnna-win1),1) 
#Calculate the moving window of variance
for (r in 1:(lnna-win1)){
men.var[r]=var(mendota[,1,drop=F][r:(r+win1)],na.rm=T)
}

#############################################
#
#########PART 3: Fitting GAM models
# Fitting GAM models is straightforward in that it largely follows
# the same syntax (with mgcv) and concepts as simple linear model fitting. 
# The difficult part is simply in the time spent sorting through 
# the various models and their AICs or GCV scores to determine which
# provides the best fit, and then interpreting smooth fits. 
#
# R, by defaulr, is woefully inadequate when it comes to multicore
# computers. Most R code simply chugs away on a simple core without
# attempting to distribute any processes. There are some packages
# that offer options to work with multicore processors, and mgcv
# seems to work well with certain of these. My favorite has been 
# "parallel." Here is example code for working with mgcv: 


require(parallel)  
nc <- detectCores()   ## cluster size, set for example portability
if (detectCores()>1) { ## no point otherwise
  cl <- makeCluster(nc) 
  ## could also use makeForkCluster, but read warnings first!
} else cl <- NULL

# The cluster will then be passed to gam fitting function below through
# the variable cl. Note: this doesn't necessarily produce dramatic gains
# in speed, but it can make a noticeable difference when interactions
# are being fit through tensor methods (i.e. calling te()).  


# Here is an example based on the data that are loaded so far. The 
# naming of the variable just reflects that this model is for 
# Mendota (men.), with AR(7) of lake level (ar6.), 3 rain lags (rn3.), 
# and with interactions between rain and time (wrntm), and between 
# rain lags (wrn). 
#
# Here, I use bam() (instead of the more obvious gam()) from mgcv because it
# is designed for large data sets.
# In my experience with the long time series in the Madison lakes data, 
# bam() is dramatically faster and much more likely to converge on an 
# answer, while gam() frequently runs out of memory before convergence. 
#

# Use the residuals from the GARCH model so that the trends in variance are
# removed. Note, this version only fits the GARCH part because the AR will be
# fit by the GAM: 

men.gfit=garchFit( ~arma(0,0)+garch(1,1), data=na.exclude(mendota[,2,drop=F]), trace=F)

# New mendota lake-level time series based on residuals
men.new=as.matrix(men.gfit@residuals)
colnames(men.new) = "elevation"
# New rain time series based on the size of the residuals (removing NAs will make this shorter)
rn.new=as.matrix(rn[!is.na(mendota[,2,drop=F]),1])
colnames(rn.new) = "rn"

mendota.r = make.flashiness.object( men.new, rn.new, lags)

men.ar6.rn3.wrntm.wrn2 = bam (elevation ~ s(time, bs = "cr", k = 400) 
	+s(elevation1,bs="cr",k=6)+s(elevation2,bs="cr",k=6)+s(elevation3,bs="cr",k=6)
	+s(elevation4,bs="cr",k=6)+s(elevation5,bs="cr",k=6)+s(elevation6,bs="cr",k=6)
	+s(elevation7,bs="cr",k=6)+s(rn,bs="cr",k=6)+s(rn1,bs="cr",k=6)
	+s(rn2,bs="cr",k=6)+s(rn3,bs="cr",k=6)+s(rn4,bs="cr",k=6)
	+te(rn,time,k=20)+te(rn1,time,k=20)+te(rn2,time,k=20)
	+te(rn3,time,k=20)+te(rn4,time,k=20)+te(rn,rn1,k=20)
	+te(rn1,rn2,k=20)+te(rn2,rn3,k=20), data=mendota.r, cluster=cl)


# The syntax s(variable, bs, k) creates a smooth fit of a parameter -- this 
# corresponds to the basic fitting of a variable with GAMs. The "bs" sets the
# basis, and the k sets the maximum number of nodes (i.e. inflections in the 
# curvature of the fitted smooth) that are allowed. The numbers I chose here 
# were arrived at through trial and error.

# The syntax te(variabl1, varible2, k) is the basic approach to fitting 
# interactions between variables in a GAM. The k still has the same meaning. 

# Most of what happens internally in the fitting is optimization, but
# there seems to be some sensitivity of the fitting process to specified
# parameter values. 

# In order to drop or retain covariates, keep track of whether the AIC or GCV
# change with their addition or removal. For Usinowicz et al. 2016 I did this 
# by hand, keeping careful notes on changes in AIC and GCV. DO NOT use p values 
# as an indicator of whether terms should be retained. However, p values may 
# indicate which fitted covariates can be given interpretation outside of the 
# context of the model. 


####################################################
#
#########PART 4: Proportion of variance explained
# Using the fitted model (in this case, the fitted GAM), it is possible to determine
# the amount of variance that one variable or set of variables explains relative to 
# others. Conceptually, since flashiness is defined by the variability in lake level 
# (or our response variable), we say that the proportion of variance explained is the 
# proportion of flashiness explained. 

#We first identifiend baseline and peak levels, based on analysis of the graph
#produced in section 2. The number 214 is the frequency, the number of days that 
#define the year after ice on dates are removed. 

#baseline:
10346/214
[1] 48.34579
1964
#First peak
12500/214
[1] 58.41121
1974
214*66
[1] 14124
1982
#Second peak
> 16600/214
[1] 77.57009
1993

#Calculate the proportionate variances for each specified time period
#Defining time periods is important. First entry should be the baseline.
perds = matrix(c(1,10346,12500,14125,16600,19829),2,3)

# Make a new model that only includes the terms we are interested in! That is,
# remove any AR terms, and impacts of other covariates not related to the comparison
# of interest. In this case, that leaves us with only things that act on the rain terms. 
# Also, note that the data are NOT the GARCH residuals.  

men.rn3.wrntm.wrn = bam (elevation ~ s(rn,bs="cr",k=6)+s(rn1,bs="cr",k=6)
	+s(rn2,bs="cr",k=6)+s(rn3,bs="cr",k=6)+s(rn4,bs="cr",k=6)
	+te(rn,time,k=20)+te(rn1,time,k=20)+te(rn2,time,k=20)
	+te(rn3,time,k=20)+te(rn4,time,k=20)+te(rn,rn1,k=20)
	+te(rn1,rn2,k=20)+te(rn2,rn3,k=20), data=mendota, cluster=cl)

#The variance function
covar.names=c ("elevation","rn")
t.chck.men = variance.ratios( men.rn3.wrntm.wrn, perds, covar.names ) {


####################################################
#
#########PART 5: Flooding series analysis
# This is largely based on classic flooding analysis techniques, however we have 
# adjusted the overall approach to account for the non-stationarity of lake-level
# peaks. We essentially treat the variability in lake-level response with a moving 
# 10-year (decadal) window.
# 
# These data produce figure 6 in Usinowicz et al. 2016

#####Set these variables by hand
#data 
mendota = mendota

#which column of data matrix has time index? 
tcol=1
#which column of data matrix has response? 
rcol=2

#Specify which columns of data matrix contain AR terms: 
men.ar.columns=3:9

#Change these if needed
winter=c(334,120)
days=abs(winter[1]-winter[2]) #Account for winter days removed
n.years=floor(nrow(mendota)/214) #Number of years in data set
win=10 #Window size
pksyr=3 #Number of peaks per year to take. pksyr*win should be >=30
men.i=length(ar.columns) #Based on maximum lag in GAM models

#Fooding thresholds
ep=c(0.999, 0.99,0.9) #1000, 100 and 10 year flood
######END


#Make some variables
pksyr.men=matrix(0,pksyr, n.years)
pkorder.men= matrix(0,pksyr, n.years)

peaks.dec.men=matrix(0,pksyr*win, n.years-win+1)
mom1.dec.men=matrix(0,1, n.years-win)
mom2.dec.men=matrix(0,1, n.years-win)
mom3.dec.men=matrix(0,1, n.years-win)

ca=n.years-win

exceed.dec.men=matrix(0,length(ep),ca)
exceed.dec.mon=matrix(0,length(ep),ca)


for (p in 1:(n.years)){ 
	#Take the top 3, to do a hybrid partial/annual series. 
	ord.men=order(na.omit(mendota[,rcol][((p-1)*days+1):(p*days)]), decreasing=T)
	srt.men=log(sort(na.omit(mendota[,rcol][((p-1)*days+1):(p*days)]), decreasing=T) )
	pksyr.men[1,p]=srt.men[1]
	pkorder.men[1,p]=ord.men[1]
	fill.men=2
	fill.pos=2
	
	while (fill.men < (pksyr+1) ) { 
		pkorder.men[fill.men,p]=ord.men[fill.pos]
		tmp.m=matrix(pkorder.men[1:fill.men,p],fill.men,fill.men)-t(matrix(pkorder.men[1:fill.men,p],fill.men,fill.men))
		
		if ( sum( abs(tmp.m[lower.tri(tmp.m)])<=men.i, na.rm=T) == 0 ) { 
			pksyr.men[fill.men,p]=srt.men[fill.pos]
			fill.men=fill.men+1
		}

	fill.pos=fill.pos+1
	 }
}

#Get the necessary stats on the top peaks per window. 
for (r in win:n.years) {
	peaks.men=NULL

	#Take the top peaks, to do a hybrid partial/annual series. 
	for (p in 0:(win-1)){ 
		peaks.men=c(peaks.men, pksyr.men[,r-p])
	}

	peaks.dec.men[,r-win+1]=peaks.men
	mom1.dec.men[r-win+1]=mean(peaks.men,na.rm=T)
	mom2.dec.men[r-win+1]=var(peaks.men,na.rm=T)
	mom3.dec.men[r-win+1]=mean(((peaks.men-as.numeric(mom1.dec.men[r-win+1]))/(as.numeric(mom2.dec.men[r-win+1]))^0.5)^3, na.rm=T)

}

#Now calculate the likelihood of seeing specific flood types
#These methods come largely from the USGS (1984) "Guidelines
#for determining flood flow frequency."

for (d in 1:ca){
	#chi^2 df based on calculated skew: 
	c2df.men=8/(mom3.dec.men[d])^2
	#Corresponding chi^2 statistic:
	c2stat.men=qchisq(ep,df=c2df.men)
	#The K coefficient used to calculate exceedance probability:
	Ksp.men=(c2stat.men-c2df.men)/sqrt(2*c2df.men)

	#Probability that a certain threshold is exceeded, Y=mean(Y)+K*sd(K)
	exceed.dec.men[,d]=mom1.dec.men[d]+Ksp.men*sqrt(mom2.dec.men[d])
}

############PDF Output
#pdf(file="mononaAmendota10winfld.pdf", height=6, width=6, onefile=TRUE, family='Helvetica', paper='letter', pointsize=12)

par(mfrow=c(1,1))
plot(1:ca,exp(exceed.dec.men[3,]), t="l", ylim=c(844.5,854), xlim=c(0.5,82.1),xaxt="n", xlab="Time", ylab="10% threshold (ft.)",bty="l",xaxs="i", yaxs="i")
axis(side=1, labels=c("1920","1940","1960","1980","2000"), at=seq(5,82,17))
lines(1:ca, exp(exceed.dec.men[3,])+exp(sqrt(mom2.dec.men[1:ca])/sqrt(30)),lty=3)
lines(1:ca, exp(exceed.dec.men[3,])-exp(sqrt(mom2.dec.men[1:ca])/sqrt(30)),lty=3)


#dev.off()




####################################################
#
#########PART 6: Visualizing the impacts of smooth variables
#
# Although it is possible to print plots of the smooth terms,
# this does not always provide a clear picture of the overall impact
# that any particular covariate has on the response. One way that 
# we arrived at for this is to hold a particular covariate fixed at a
# mean or median value across all time, and then examine what happens
# to the response by simulating it with the fitted model and the fixed 
# version of the covariate. To show the variation in the response, it then
# becomes useful not only to plot the response to mean/median covariate,
# but also to SD increments of the response in something like a contour
# plot. This is especially useful for demonstrating non-linear 
# effects, where the spacing between responses to SDs are not linear.  
#
# The following code creates a number of fake datasets based on the 
# means + SD of each covariate, using a fitted GAM model to generate 
# new response data. 
#
# This corresponds to Figure 4 in Usinowicz et al. 2016


###############Variables that may need to be set/renamed by user.
#Model to use: 
model = men.ar6.rn3.wrntm.wrn2

#Data set to go with it:
#Note, the variables below are written to use the example
#data set "mendota." This could be changed using find/replace,
#but simply changing the variable here would accomplish the same 
#purpose. 
mendota = mendota

#which column of data matrix has time index? 
tcol=1
#which column of data matrix has response? 
rcol=2

#Specify which columns of data matrix contain AR terms: 
men.ar.columns=3:9

#Number of values to simulate
npts=1000

#These set the values for the increments below. I call them "decades" 
#which is an artifact of once having used values per-decade, instead of 
#incrementing by the SD. Thus it would be more accurate to call these 
# "SDs" as they increment by standard deviations.  
ra=1
rb=15

#This named list must be supplied by user, and must match the ra/rb 
#increments
decs.list.full=list("dec1","dec2","dec3","dec4","dec5","dec6","dec7","dec8","dec9","dec10","dec11","dec12","dec13","dec14","dec15")

#These are the covariates whose impact we want to examine: 
param.list=list("mendota.rn","mendota.rn1","mendota.rn2","mendota.rn3","mendota.rn4")

#Change these if needed
winter=c(334,120)
days=abs(winter[1]-winter[2]) #Account for winter days removed

#Precipitation in increments of SD
rn.sdev=sqrt(var(mendota$rn)) #SD of precip

#Create the increments of SD for precip
dec.med=matrix(0, 1, ncol=ca)
for (r in 1:ca){dec.med[,r]=mean(mendota$rn, na.rm=T)+(r-1)*rn.sdev}

#END
#############

#Number of columns to calculate
ca=rb-ra+1

#Create some variables
rainlakes.decs.list=decs.list.full[ra:rb]
colnames(dec.med)=rainlakes.decs.list

#Outer loop over decades/standard deviations
for (d in ra:rb){
	print(d)
	rainlakes.param=list()
	varied=matrix(0,npts,length(param.list))
	for (pr in 1:length(param.list)) {
	#The time index 
		rainlakes.param[[paste(param.list[pr])]] =seq(min(mendota[,tcol]), max(mendota[,tcol]), (max(mendota[,tcol])-min(mendota[,tcol]))/(npts-1))
			for (cr in 2:ncol(mendota)){ 
					rainlakes.param[[paste(param.list[pr])]] =cbind(rainlakes.param[[paste(param.list[pr])]], matrix(mean(mendota[,cr],na.rm=T), npts,1))
				}
			rainlakes.param[[paste(param.list[pr])]][,(pr+12)] = matrix(dec.med[d], npts,1) 
			varied[,pr]=rainlakes.param[[paste(param.list[pr])]][,1]

	}


	rainlakes.men.sim=matrix(0,npts,length(param.list))
	rainlakes.men.simq=matrix(0,5,length(param.list))

	#This will simulation new response data based on the model of choice, and the 
	#fixed parameter (corresponding to d). 
	for (sm in 1:length(param.list)){
		gam.temp=as.data.frame(matrix(unlist(rainlakes.param[[paste(param.list[sm])]]), nrow=npts))
		colnames(gam.temp)=colnames(mendota)
		gam.sim1=predict(model, newdata=gam.temp)
		gam.sim2=predict(model, newdata=gam.temp,type="lpmatrix" )	
		rainlakes.men.sim[,sm]=gam.sim1
		br1=mvrnorm(n=1000, coef(model), model$Vp)
		Xp =gam.sim2
		mean.model= colMeans(Xp%*%t(br1))
		q1=quantile(mean.model)
		rainlakes.men.simq[,sm]=t(q1)		

		
	}
	
	ls.tmp=list(rainlakes.men.sim, rainlakes.men.simq)
	rainlakes.decs.list[[paste(decs.list.full[d])]]= list(rainlakes.men.sim, rainlakes.men.simq)

}

#########Then this is the actual code to produce the plots
#pdf(file="lakesbutoneNAwPI.pdf", height=6, width=6, onefile=TRUE, family='Helvetica', paper='letter', pointsize=12)

par(mfrow=c(4,1),mar=c(0,4,0,4),oma=c(4,4,4,4))

cvs = length(param.list) -1 #Set the number of covariates to plot.

for( r in 1:cvs){
	#plot the first decade in the list 
	#assign temporary variables purely for readability
	first.dec= rainlakes.decs.list[[paste(decs.list.full[1])]][[1]][,r]
	#Prediction intervals
	first.upper=first.dec+(rainlakes.decs.list[[paste(decs.list.full[1])]][[2]][4]-rainlakes.decs.list[[paste(decs.list.full[1])]][[2]][3])
	first.lower=first.dec-(rainlakes.decs.list[[paste(decs.list.full[1])]][[2]][3]-rainlakes.decs.list[[paste(decs.list.full[1])]][[2]][2])
	yl=matrix(0,(rb-ra+1),1)
	yu=matrix(0,(rb-ra+1),1)
	for (t in ra:rb){ 
		yl[t]= min(rainlakes.decs.list[[paste(decs.list.full[t])]][[1]][,r])
		yu[t]= max(rainlakes.decs.list[[paste(decs.list.full[t])]][[1]][,r])
	}
	ylow=min(yl)
	yup=max(yu)
	#xlim=c(214*4, max(varied))
	plot(varied[,r],first.dec, ylim=c(ylow,yup),ann=F, xaxt='n'
, yaxt='n',t="l", xaxs="i",main=paste("Rain=", as.character(r-1)))

	axis(side=1, labels=F, at=seq(214*4,214*9*10,214*10*2))
	yinc=0.2*(yup-ylow)
	axis(side=2, labels=c(format((ylow+yinc),digits=3,nsmall=2),format((yup-yinc),digits=3,nsmall=2)), at=c((ylow+yinc),(yup-yinc)))
	lines( varied[10:npts,r],first.upper[10:npts],lty=3)
	lines( varied[10:npts,r],first.lower[10:npts],lty=3)
	
	#Add intervals.This requires ra and rb still, and can be edited to change
	#the increments. 

	for (t in seq((ra+2),rb,by=3)){ 
		now.dec= rainlakes.decs.list[[paste(decs.list.full[t])]][[1]][,r]
		now.upper=now.dec+(rainlakes.decs.list[[paste(decs.list.full[t])]][[2]][4]-rainlakes.decs.list[[paste(decs.list.full[t])]][[2]][3])
		now.lower=now.dec-(rainlakes.decs.list[[paste(decs.list.full[t])]][[2]][3]-rainlakes.decs.list[[paste(decs.list.full[t])]][[2]][2]) 
		lines( varied[,r],now.dec )
		lines( varied[10:npts,r],now.upper[10:npts], col="black", lty=3)
		lines( varied[10:npts,r],now.lower[10:npts], col="black", lty=3)

	}

	if (r == cvs){axis(side=1,labels=c("1920","1940","1960","1980","2000"), at=seq(214*4,214*9*10,214*10*2))}
}

#dev.off()
