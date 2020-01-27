#'Calculates the average of an irregular time series
#'
#'
#'@param timeseries dataframe; first column POSIXct, second column numeric
#'@param start POSIXct; start of the averaging window
#'@param end POSIXct; end of the averaging window
#'@param n integer; number of blocks
#'@keywords methods
#'@examples
#'
#'
#'@importFrom stats approx
#'@importFrom zoo na.approx
#'
#'@export

time_average <- function(timeseries, start, end, n=1000){
  
  # Fix time zone
  original_tz = Sys.getenv("TZ")
  
  # this way if the function exits for any reason, success or failure, these are reset:
  on.exit({
    Sys.setenv(TZ=original_tz)
  })
  
  Sys.setenv(TZ="GMT")
  
  # Instructions partially followed from: 
  # https://dzone.com/articles/working-with-irregular-time-series
  
  # If start is after the end of the time series, or the end is before the start of the time series,
  # throw an error
  
  if(start > timeseries[nrow(timeseries),1] | end < timeseries[1,1]){
    stop("Start and end of time series are out of range of time series data")
  }
  
  # If start or end are outside the time series, add them to it
  # Or if start and end are within the time series and there is no value for start of end,
  # get the value by linear interpolation
  if(start < timeseries[1,1]){
    timeseries <- rbind.data.frame(timeseries[1,],timeseries)
    timeseries[1,1] <- start
  }else if(start > timeseries[1,1] & !(start %in% timeseries[,1])){
    # Interpolate to get this value and add to timeseries
    start_value <- approx(x=timeseries[,1], y=timeseries[,2],xout = start)$y
    df_add <- data.frame(start, start_value)
    names(df_add) <- names(timeseries)
    timeseries <- rbind.data.frame(timeseries, df_add)
  }
  
  if(end > timeseries[nrow(timeseries),1]){
    timeseries <- rbind.data.frame(timeseries,timeseries[nrow(timeseries),])
    timeseries[nrow(timeseries),1] <- end
  }else if(end < timeseries[nrow(timeseries),1] & !(end %in% timeseries[,1])){
    # Interpolate to get this value and add to timeseries
    end_value <- approx(x=timeseries[,1], y=timeseries[,2],xout = end)$y
    df_add <- data.frame(end, end_value)
    names(df_add) <- names(timeseries)
    timeseries <- rbind.data.frame(timeseries, df_add)
  }
  
  # Sort by datetime
  timeseries <- timeseries[order(timeseries[,1]), ]
  
  # Cut the data frame between start to end
  timeseries <- timeseries[timeseries[,1] >= start & 
               timeseries[,1] <= end,]
  
  # Cut the timeseries into n equal parts and calculate means for each part
  regular_timeseries <- aggregate(timeseries[,2],
                   by = list(cut(timeseries[,1], 
                               c(seq(from = timeseries[1,1],
                                                   to = timeseries[nrow(timeseries),1],
                                                   length.out = n, include.lowest=T),Inf))),
                   FUN = mean,
                   drop = FALSE)
  
  # Linearly interpolate the values
  regular_timeseries[,2] <- na.approx(regular_timeseries[,2])
  
  return(mean(regular_timeseries[,2]))
  
}
