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


##############################################################
#load libraries
##############################################################
library(tidyverse)
library(lubridate)
library(rvest) #To parse table from webpage
library(nasapower) #API for NASA data, for precipitation
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

#Import other variables (e.g. impervious surface)
impervious_table=read.csv(file = "")

##############################################################
#PART 2: Analysis of heteroskedasticity
##############################################################
library(fGarch) #For GARCH models
#Preallocate and get number of lakes
lake_ar1 = vector("list", n_lakes)
lake_arch = vector("list", n_lakes)
lake_gfit = vector("list", n_lakes)
lake_var = vector("list", n_lakes)

#Loop over lakes to get stage level and dates: 
for(n in 1:n_lakes){ 

	# The most straightforward test is to fit an AR model to the 
	# squared residuals of an AR model. If this produces a model
	# with significant terms, then the variance is non-stationary. 
	lake_ar1[[n]] = ar(lake_data[[n]][,1,drop=F], na.action=na.exclude)
	
	#If any of these coefficients are significantly >0, then it is an ARCH
	lake_arch[[n]] = ar((lake.ar1$resid^2),na.action=na.exclude)

	# For more detailed information, and to produce the plots 
	# in the paper, fit a GARCH model to the lake-level time series with
	# garchFit from library fGarch. 
	lake_gfit = garchFit( ~arma(6,0)+garch(1,1), data=na.exclude(lake_data[[n]][,2,drop=F]), trace=F)

	#Looking at summary(lake_gfit) gives a lot of information. 
	# mu corresponds to the intercept of the regression.
	# ar are the AR coefficients
	# ma are the MA coefficients (none in this example)
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
	freq = 	abs(winter[1]-winter[2]) #Account for winter days removed
	win_years = 5 #Set window in years
	win1=freq*win_years
	
	#Account for NAs being removed 
	lnna=length(is.na(lake_data[[n]][,1,drop=F]))-sum(is.na(lake_data[[n]][,1,drop=F]))
	lake_var[[n]]=matrix(0,(lnna-win1),1) 
	
	#Calculate the moving window of variance
	for (r in 1:(lnna-win1)){
		lake_var[[n]][r]=var(lake_data[[n]][,1,drop=T][r:(r+win1)],na.rm=T)
	}	


##############################################################
# PART 3: Flooding series analysis
##############################################################
# This is based on classic flooding analysis, modified to 
# account for the non-stationarity of lake-level over many
# decades. We treat the variability in lake-level response 
# with a moving (e.g. 10-year, decadal) window.
##############################################################
#Set important variables: 
#which column of data matrix has time index? 
tcol=1
#which column of data matrix has response? 
rcol=2

tot_days=abs(winter[1]-winter[2]) #Account for winter days removed
win=5 #Window size
pksyr=5 #Number of peaks per year to take. pksyr*win should be >=30

#Fooding thresholds
ep=c(0.999, 0.99,0.9) #1000, 100 and 10 year flood

#Make some variables
#List of top peaks each year
pksyr_lake= vector("list",n_lakes) 
#Day of year peak occurred 
pkorder_lake = vector("list", n_lakes) 

#Full decadal peak series
peaks_dec_lake = vector("list", n_lakes) 
#1st - 3rd moments of peak distribution for series analysis
#Averag peak size in window
mom1_dec_lake = vector("list", n_lakes) 
#Variance in peak size
mom2_dec_lake = vector("list", n_lakes)
#Skew of peak distribution  
mom3_dec_lake = vector("list", n_lakes) 

#The exceedence threshold: e.g. what constitutes a 
#100 yr flooding event? 
exceed_dec_lake = vector("list", n_lakes) 


#Loop over lakes 
for(n in 1:n_lakes){ 

	#Specify which columns of data matrix contain lagged lake level
	ar_columns = grep("level", colnames(lake_data[[n]]))                                                                                                          
	ar_columns = ar_columns[-1]

	n_years=floor(nrow(lake_data[[n]])/214) #Number of years in data set

	ar_i=length(ar_columns) #Based on maximum lag in GAM models

	ca=n_years-win

	#Now set the size of each of the flooding series variables from above

	#Make some variables

	pksyr_lake[[n]]=matrix(0,pksyr, n_years)
	pkorder_lake[[n]]= matrix(0,pksyr, n_years)

	peaks_dec_lake[[n]]=matrix(0,pksyr*win, n_years-win+1)
	mom1_dec_lake[[n]]=matrix(0,1, n_years-win)
	mom2_dec_lake[[n]]=matrix(0,1, n_years-win)
	mom3_dec_lake[[n]]=matrix(0,1, n_years-win)

	#Note: in the structure of this variable, the rows correspond
	#to each of the flood-type events (e.g. 1000 yr, 100 yr, and 10 yr)
	exceed_dec_lake[[n]]=matrix(0,length(ep),ca)


	for (p in 1:(n_years)){ 
		#Take the top 3 peaks, to do a hybrid partial/annual series. 
		ord_lake=order(na.omit(lake_data[[n]][,rcol][((p-1)*tot_days+1):
			(p*tot_days)]), decreasing=T)
		srt_lake=log(sort(na.omit(lake_data[[n]][,rcol][((p-1)*tot_days+1):
			(p*tot_days)]), decreasing=T) )
		pksyr_lake[[n]][1,p]=srt_lake[1]
		pkorder_lake[[n]][1,p]=ord_lake[1]
		fill_lake=2
		fill_pos=2
			
		#This picks out next peaks by size, as well as their day of occurrence	
		while (fill_lake < (pksyr+1) ) { 
			pkorder_lake[[n]][fill_lake,p]=ord_lake[fill_pos]
			tmp.m=matrix(pkorder_lake[[n]][1:fill_lake,p],fill_lake,fill_lake)-
				t(matrix(pkorder_lake[[n]][1:fill_lake,p],fill_lake,fill_lake))
			
			if ( sum( abs(tmp.m[lower.tri(tmp.m)])<=ar_i, na.rm=T) == 0 ) { 
				pksyr_lake[[n]][fill_lake,p]=srt_lake[fill_pos]
				fill_lake=fill_lake+1
			}

			fill_pos=fill_pos+1
		 }
		}

		#Get certain stats on the top peaks per window. 
		for (r in win:n_years) {
		peaks_lake=NULL

		#Take the top peaks in a window to create hybrid partial/annual series. 
		for (p in 0:(win-1)){ 
			peaks_lake=c(peaks_lake, pksyr_lake[[n]][,r-p])
		}

		#Calculate the moments
		peaks_dec_lake[[n]][,r-win+1]=peaks_lake
		mom1_dec_lake[[n]][r-win+1]=mean(peaks_lake,na.rm=T)
		mom2_dec_lake[[n]][r-win+1]=var(peaks_lake,na.rm=T)
		mom3_dec_lake[[n]][r-win+1]=mean(((peaks_lake-as.numeric(mom1_dec_lake[[n]][r-win+1]))/
			(as.numeric(mom2_dec_lake[[n]][r-win+1]))^0.5)^3, na.rm=T)

		}

	#Now calculate the likelihood of seeing specific flood types
	#These methods are adapted from the USGS (1984) "Guidelines
	#for determining flood flow frequency."

	for (d in 1:ca){
		#chi^2 df based on calculated skew: 
		c2df_lake=8/(mom3_dec_lake[[n]][d])^2
		#Corresponding chi^2 statistic:
		c2stat_lake=qchisq(ep,df=c2df_lake)
		#The K coefficient used to calculate exceedance probability:
		Ksp_lake=(c2stat_lake-c2df_lake)/sqrt(2*c2df_lake)

		#The threshold at which each rare event is met/exceeded, Y=mean(Y)+K*sd(K)
		exceed_dec_lake[[n]][,d]=mom1_dec_lake[[n]][d]+Ksp_lake*sqrt(mom2_dec_lake[[n]][d])
	}
}

############PDF Output
#pdf(file="mononaAmendota10winfld.pdf", height=6, width=6, onefile=TRUE, family='Helvetica', paper='letter', pointsize=12)
upper_ci = exp(exceed_dec_lake[[n]][2,])+exp(sqrt(mom2_dec_lake[[n]][1:ca])/sqrt(pksyr*win))
lower_ci = exp(exceed_dec_lake[[n]][2,])-exp(sqrt(mom2_dec_lake[[n]][1:ca])/sqrt(pksyr*win))
ylims = c(min(lower_ci) -2, max(upper_ci) + 2)
par(mfrow=c(1,1))
plot(1:ca,exp(exceed_dec_lake[[n]][2,]), t="l",xlab="Time", ylim = ylims, ylab="10% threshold (ft.)",bty="l",xaxs="i", yaxs="i")
#axis(side=1, labels=c("1920","1940","1960","1980","2000"), at=seq(5,82,17))
lines(1:ca, upper_ci ,lty=3)
lines(1:ca, lower_ci ,lty=3)

#dev.off()

##############################################################
#PART 3: Fitting GAMs
##############################################################
#Use the package mgcv by Simon Wood: 
library(mgcv)
	
#If the fitted model is already known, then describe each model:
model_form = vector("list", n_lakes)
model_form [[1]] = "level ~ s(time, bs = \"cr\", k = 50)"
model_form [[1]] = "level ~ s(time, bs = \"cr\", k = 50)+
		s(level1,bs=\"cr\",k=6)+s(level2,bs=\"cr\",k=6)+s(level3,bs=\"cr\",k=6)+
		s(level4,bs=\"cr\",k=6)+s(level5,bs=\"cr\",k=6)+s(level6,bs=\"cr\",k=6)+
		s(rn,bs=\"cr\",k=6)+s(rn1,bs=\"cr\",k=6)+
		s(rn2,bs=\"cr\",k=6)+s(rn3,bs=\"cr\",k=6)+s(rn4,bs=\"cr\",k=6)+
		te(rn,time,k=20)+te(rn1,time,k=20)+te(rn2,time,k=20)+
		te(rn3,time,k=20)+te(rn4,time,k=20)+te(rn,rn1,k=20)+
		te(rn1,rn2,k=20)+te(rn2,rn3,k=20)"

model_form [[2]] = "level ~ s(time, bs = \"cr\", k = 50)+
		s(level1,bs=\"cr\",k=6)+s(level2,bs=\"cr\",k=6)+s(level3,bs=\"cr\",k=6)+
		s(level4,bs=\"cr\",k=6)+s(level5,bs=\"cr\",k=6)+s(level6,bs=\"cr\",k=6)+
		s(rn,bs=\"cr\",k=6)+s(rn1,bs=\"cr\",k=6)+
		s(rn2,bs=\"cr\",k=6)+s(rn3,bs=\"cr\",k=6)+s(rn4,bs=\"cr\",k=6)+
		te(rn,time,k=20)+te(rn1,time,k=20)+te(rn2,time,k=20)+
		te(rn3,time,k=20)+te(rn4,time,k=20)+te(rn,rn1,k=20)+
		te(rn1,rn2,k=20)+te(rn2,rn3,k=20)"

#Store fitted models
lake_models = vector("list", n_lakes)

#Try to implement some parallelization for mgcv:

require(parallel)  
nc = detectCores()   ## cluster size, set for example portability
if (detectCores()>1) { ## no point otherwise
  cl = makeCluster(nc) 
  ## could also use makeForkCluster, but read warnings first!
} else cl <- NULL

# The cluster will then be passed to gam fitting function below through
# the variable cl.


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

	lake_models[[n]] = bam ( as.formula((model_form [[n]] )), data=lake_r, cluster=cl )

}