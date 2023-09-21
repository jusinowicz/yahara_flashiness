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
library(mgcv) #GAM functions
library(fGarch) #For GARCH models
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


##############################################################
#PART 1: Data processing
##############################################################
#Import lake-level time series as the response. These data for 
#lake Mendota downloaded from 
# https://waterdata.usgs.gov/nwis/dv/?site_no=05428000&agency_cd=USGS&amp;referred_module=sw
#mendota_table=read.csv(file="mendota_example.csv")
#Latest version of this for automated access: 
#https://waterservices.usgs.gov/nwis/iv/?format=rdb&sites=05428000&startDT=1980-09-21&endDT=2023-09-21&parameterCd=00060,00065&siteStatus=all
https://waterservices.usgs.gov/nwis/iv/?format=rdb&sites=05427718&startDT=2013-09-21&endDT=2014-09-21&parameterCd=00045&siteStatus=all

n_lakes = length(site_keys)
lake_table = vector("list", n_lakes)
real_start = vector("list", n_lakes)

for(n in 1:n_lakes){ 
	#Use the USGS address format to get data over the date range
	url1 = paste(url_base[1], site_keys[n], "&startDT=", start_date,
		"&endDT=",current_date,"&parameterCd=00060,00065&siteStatus=all",sep="" )
	
	lake_table[[n]]= as.data.frame(read_delim(print(url1), comment = "#", delim="\t"))
	
	lake_table[[n]] = lake_table[[n]][-1,]
	
	#Just in case we requested back too far, what is the actual start 
	#date of the retrieved data?
	real_start[[n]] = format(as.POSIXct(lake_table[[n]][1,"datetime"],
                       format = '%Y-%m-%d %H:%M'),
                		format = '%Y-%m-%d') 


}



#The USGS address has a nice format that we can take advantage of to fit the most 
#current data: 
url1 = paste("https://waterdata.usgs.gov/nwis/dv?cb_00065=on&format=html&site_no=05428000&referred_module=sw&period=&begin_date=1951-01-01&end_date=", 
	current_date, sep="") 

#Scrape the table(s) from the webpage
mendota_table= url %>% 
			   read_html() %>%
			    html_nodes("table") %>% 
  				html_table(fill = T)

#This might find more than one, but the biggest one is the one we want: 
table_sizes = unlist(lapply(mendota_table, nrow))
right_table = which(table_sizes == max(table_sizes) )
mendota_table = mendota_table[[right_table]]

#Import precipitation time series from USGS (only goes back to 1951)
url = paste("https://nwis.waterdata.usgs.gov/usa/nwis/uv/?cb_00010=on&cb_00045=on&cb_00060=on&cb_00065=on&cb_63160=on&format=html&site_no=05427718&period=&begin_date=1951-01-01&end_date=",
	current_date, sep="")
rain_table = url %>% 
			   read_html() %>%
			    html_nodes("table") %>% 
  				html_table(fill = T)


daily_single_ag <- get_power(
  community = "ag",
  lonlat = c(151.81, -27.48),
  pars = c("RH2M", "T2M", "PRECTOTCORR"),
  dates = c("1985-01-01", "1985-01-31"),
  temporal_api = "daily"
)


 #This might find more than one, but the biggest one is the one we want: 
table_sizes = unlist(lapply(rain_table, nrow))
right_table = which(table_sizes == max(table_sizes) )
rain_table = rain_table[[right_table]]



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
