library(shiny)
library(tidyverse)
library(lubridate)
library(nasapower) #API for NASA data, for precipitation
library(openmeteo)

#USGS site keys. Currently includes Mendota and Monona
site_keys = c("05428000", "05429000")
n_lakes = length(site_keys)

#The base urls used by the USGS for lake data
url_base = c("https://waterservices.usgs.gov/nwis/iv/?format=rdb&sites=")

#Parameters from the NASA POWER data collection
nasa_pars = c("PRECTOTCORR")

#This is a helper function for lake level. If the data is not up to date, do
#all of the necessary data procesing. 
updateLake = function (lake_table, start_date, current_date) {

  #Use the USGS address format to get data over the date range
  url1 = paste(url_base[1], site_keys[n], "&startDT=", start_date,
    "&endDT=",current_date,"&parameterCd=00060,00065&siteStatus=all",sep="" )
  
  lt_temp = as.data.frame(read_delim(print(url1), comment = "#", delim="\t"))
  lt_temp = lt_temp[-1,]
  lt_temp[,"datetime"] = as.POSIXct(lt_temp[,"datetime"],
                       format = '%Y-%m-%d %H:%M')
  lt_temp[,5] = as.numeric(as.character(lt_temp[,5]))

  lt_temp = lt_temp[,c(3, 5)]
  colnames(lt_temp ) = c("time", "level")

  #Data seem to be at 15 min intervals by default. Aggregate these into 
  #a mean daily level
  lt_temp = lt_temp %>%
      mutate(time = as.Date(ymd_hms(time))) %>%
      group_by(time) %>%
      summarise(level = mean(level, na.rm = TRUE)) %>%
      as.data.frame()
  return(lt_temp)

}

#This is a helper function for rain. If the data is not up to date, do
#all of the necessary data procesing. 
updateRain = function (daily_rain, start_date, current_date) {

  #Get the matching precipitation data from the NASA POWER collection
  rn_temp = get_power(
    community = "ag",
    lonlat = c(43.0930, -89.3727),
    pars =  nasa_pars,
    dates = c(paste(start_date), paste(current_date)),
    temporal_api = "daily"
  ) %>% as.data.frame()
  rn_temp = rn_temp[,c(7,8)]
  colnames(rn_temp) = c("time", "rn")
  return(rn_temp)

}


# Load the historical data and check whether it is up to date. 
updateHistoric = function( ) {

  #Preallocate the important historical data tables 
  lake_table = vector("list", n_lakes)
  daily_precip = vector("list", n_lakes)
  real_start = vector("list", n_lakes)
  #Final data set
  lake_data = vector("list", n_lakes)

  last_date = vector("list",n_lakes)
  last_rain = vector("list",n_lakes)

  #Load the current data files
  current_date = ymd( Sys.Date() ) #Current date. Could be set to other
  lake_table[[1]] = read.csv(file = "./../data/men_hist.csv")
  lake_table[[1]][,"time"] = ymd(lake_table[[1]][,"time"])
  lake_table[[2]] = read.csv(file = "./../data/mon_hist.csv")
  lake_table[[2]][,"time"] = ymd(lake_table[[2]][,"time"])
  daily_precip[[1]] = read.csv(file = "./../data/rain_hist.csv")
  daily_precip[[1]][,"time"] = ymd(daily_precip[[1]][,"time"])
  daily_precip[[2]] = daily_precip[[1]]

  #Make backups of previous file:
  file.copy(from = "./../data/men_hist.csv", to ="./../data/men_hist.csv.bck")
  file.copy(from = "./../data/mon_hist.csv", to ="./../data/mon_hist.csv.bck")
  file.copy(from = "./../data/rain_hist.csv", to ="./../data/rain_hist.csv.bck")

  #Get the last dates entered. If they don't match to the current date
  #then update the data with the functions updateLake and updateRain. 
  #Write the new files.  
  for ( n in 1:n_lakes){ 

    #For the lakes
    last_date[[n]] = lake_table[[n]][nrow(lake_table[[n]]),"time"]
    if(last_date[[n]] != current_date  ){ 
      lake_table[[n]] = rbind(lake_table[[n]],
          updateLake(lake_table[[n]], start_date = last_date[[n]], 
          current_date = current_date ) )
      lake_table[[n]] = lake_table[[n]][-( (nrow(lake_table[[n]])-2):
        nrow(lake_table[[n]])), ]
    }

    #For the precip
    last_rain[[n]] = daily_precip[[n]][nrow(daily_precip[[n]]),"time"]
    if(last_rain[[n]] != current_date  ){ 
      daily_precip[[n]] = rbind(daily_precip[[n]],
          updateRain(daily_precip[[n]], start_date = last_rain[[n]], 
            current_date = current_date) )
      daily_precip[[n]] = daily_precip[[n]][-( (nrow(daily_precip[[n]])-1):
        nrow(daily_precip[[n]])), ]
    }

    lake_data[[n]] = lake_table[[n]] %>%
        inner_join(daily_precip[[n]], by = "time" ) 

  }

  write.table(lake_data[[1]][,1:2], file = "./../data/men_hist.csv", sep=",")
  write.table(lake_data[[2]][,1:2], file = "./../data/mon_hist.csv", sep=",")
  write.table(lake_data[[1]][,c(1,3)], file = "./../data/rain_hist.csv", sep=",")

}



# An empty prototype of the data frame we want to create
prototype <- data.frame(date = character(), time = character(),
  size = numeric(), r_version = character(), r_arch = character(),
  r_os = character(), package = character(), version = character(),
  country = character(), ip_id = character(), received = numeric())

# Connects to streaming log data for cran.rstudio.com and
# returns a reactive expression that serves up the cumulative
# results as a data frame
packageStream <- function(session) {
  # Connect to data source
  sock <- socketConnection("cransim.rstudio.com", 6789, blocking = FALSE, open = "r")
  # Clean up when session is over
  session$onSessionEnded(function() {
    close(sock)
  })

  # Returns new lines
  newLines <- reactive({
    invalidateLater(1000, session)
    readLines(sock)
  })

  # Parses newLines() into data frame
  reactive({
    if (length(newLines()) == 0)
      return()
    read.csv(textConnection(newLines()), header=FALSE, stringsAsFactors=FALSE,
      col.names = names(prototype)
    ) %>% mutate(received = as.numeric(Sys.time()))
  })
}

# Accumulates pkgStream rows over time; throws out any older than timeWindow
# (assuming the presence of a "received" field)
packageData <- function(pkgStream, timeWindow) {
  shinySignals::reducePast(pkgStream, function(memo, value) {
    rbind(memo, value) %>%
      filter(received > as.numeric(Sys.time()) - timeWindow)
  }, prototype)
}

# Count the total nrows of pkgStream
downloadCount <- function(pkgStream) {
  shinySignals::reducePast(pkgStream, function(memo, df) {
    if (is.null(df))
      return(memo)
    memo + nrow(df)
  }, 0)
}

# Use a bloom filter to probabilistically track the number of unique
# users we have seen; using bloom filter means we will not have a
# perfectly accurate count, but the memory usage will be bounded.
userCount <- function(pkgStream) {
  # These parameters estimate that with 5000 unique users added to
  # the filter, we'll have a 1% chance of false positive on the next
  # user to be queried.
  bloomFilter <- BloomFilter$new(5000, 0.01)
  total <- 0
  reactive({
    df <- pkgStream()
    if (!is.null(df) && nrow(df) > 0) {
      # ip_id is only unique on a per-day basis. To make them unique
      # across days, include the date. And call unique() to make sure
      # we don't double-count dupes in the current data frame.
      ids <- paste(df$date, df$ip_id) %>% unique()
      # Get indices of IDs we haven't seen before
      newIds <- !sapply(ids, bloomFilter$has)
      # Add the count of new IDs
      total <<- total + length(newIds)
      # Add the new IDs so we know for next time
      sapply(ids[newIds], bloomFilter$set)
    }
    total
  })
}
