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
library(shiny)
library(tidyverse)
library(lubridate)
library(mgcv)
library(fGarch) #For GARCH models

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
		
		#Get the AR order: 
		ar_ord = ar(lake_gfit1@residuals)$order
		#phi = unname(intervals(m$lme, which = "var-cov")$corStruct[, 2])

		#Append this to the model description: 
		mf = paste(model_form[[n]], ", correlation = corARMA(value = 
			phi, fixed =TRUE, form = ~ 1 | time, p =", ar_ord, ")")

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

		# lake_models[[n]] = bam( as.formula(model_form[[n]]), method = "REML", optimizer = c("efs"),
		# 	correlation = corARMA( form = ~ 1 | time, p = ar_ord), data=lake_r )

		lake_models3 [[n]] = bam( as.formula(model_form[[n]]), method = "REML",
		 data=lake_r )


	}

	return(lake_models)

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
# GAM prediction with autoregression
#
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