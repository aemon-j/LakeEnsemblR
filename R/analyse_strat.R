#' Returns stratification statstics
#'
#'Returns stratification statstics: annual mean, max and total
#'length of summer, winter stratification and ice duration.
#'NOTE: summer strat periods are allocated to the year in which the period starts. Winter stratification and ice periods are allocated to the year in which they end.
#'
#' @param data dataframe; water temperature data in long format with date, depths, value
#' @param H_ice vector; of ice thickness which corresponds to date vector, set to NULL if analysis not required. Defaults to NULL
#' @param drho numeric; density difference between top and bottom indicating stratificaiton [kg m^-3]
#' @param NH boolean; northern hemisphere? TRUE or FALSE. Defaults to true
#' @author Tom Shatwell
#'
#' @examples
#' \dontrun{
#' strat <- analyse_strat(Ts = df[,2], Tb = df[,ncol(df)], dates = df[,1])
#' }
#'
#' @export

analyse_strat <- function(data, H_ice = NULL, drho = 0.1, NH = TRUE){

  data[,2] <- abs(data[,2])
  depths <- unique(data[,2])
  depths <- depths[order(depths)]

  # Find closest depth near the surface without NA
  for(i in 1:length(depths)){
    Ts = data[data[,2] == depths[i],3]
    if(sum(is.na(Ts))/length(Ts) < 0.25){
      if(i != 1){
        message('Warning: Using ', depths[i], ' as the surface.')
      }
      break
    }
  }
  # Find closest depth near the bottom without NA
  for(i in length(depths):1){
    Tb = data[data[,2] == depths[i],3]
    if(sum(is.na(Tb))/length(Tb) < 0.25){
      if(i != 1){
        message('Warning: Using ', depths[i], ' as the bottom.')
      }
      break
    }
  }



  dates = unique(data[,1])

  # Put into data frame and remove NA's
  if(!is.null(H_ice)){
    df <- data.frame(dates, Ts, Tb, H_ice)
  }else{
    df <- data.frame(dates, Ts, Tb)
  }
  df <- na.exclude(df)
  if(nrow(df) == 0){
    message('Not enough data to calculate statification and/or ice statistics')
    return()
  }
  dates <- df$dates
  Ts <- df$Ts
  Tb <- df$Tb
  if(!is.null(H_ice)){
    H_ice <- df$H_ice
  }



  the_years <- as.POSIXlt(dates)$year+1900
  yrs <- unique(the_years)
  doys <- as.POSIXlt(dates)$yday # day of the year [0..364]
  alt_doys <- doys # alternative counting from [-182 .. 182] for ice in northern hemisphere or strat in southern hemisphere
  alt_doys[doys>182] <- doys[doys>182] - (365 + leap(the_years[doys>182])) # Jan 1 is day 0, correct for leap years
  alt_years <- the_years
  alt_years[alt_doys<0] <- the_years[alt_doys<0] +1 # alternative counting of years (shifted forward by half a year)

  if(NH) { # NH ice and SH stratification use alternative doy and year counts to adjust for ice and stratification events that span more than one calendar year
    ice_yrs <- alt_years
    ice_doys <- alt_doys
    strat_yrs <- the_years
    strat_doys <- doys
  } else {
    ice_yrs <- the_years
    ice_doys <- doys
    strat_yrs <- alt_years
    strat_doys <- alt_doys
  }

  s_strat <- (rho_water(t=Tb) - rho_water(t=Ts)) >= drho & Ts > Tb # logical whether stratified at each time step
  # s_strat <- Ts - Tb  > dT # logical whether stratified at each time step

  i_s_st <- diff(c(s_strat[1],s_strat))==1 # indices of stratification onset
  i_s_en <- diff(c(s_strat[1],s_strat))==-1 # indices of stratification end
  if(s_strat[1]) i_s_st <- c(NA, i_s_st) # if stratified at beginning of simulation, make first date NA
  if(s_strat[length(s_strat)]) i_s_en <- c(i_s_en, NA) # if stratified at end of sim, set last strat date to NA
  s_start <- dates[i_s_st] # summer strat start dates
  s_end   <- dates[i_s_en] # summer strat end dates
  # if(sum(s_strat)==0) s_start <- s_end <- dates[1] # if never stratifies, set to time=0
  s_dur   <- as.double(difftime(s_end, s_start, units="days")) # duration of summer strat periods

  a1 <- data.frame(year=strat_yrs[i_s_st],
                   start=s_start, end=s_end, dur=s_dur,
                   startday = strat_doys[i_s_st],
                   endday = strat_doys[i_s_en])
  a1 <- subset(a1, year %in% yrs)

  s.max <- s.mean <- s.tot <- s.on <- s.off <-
    s.first <- s.last <- yr <- NULL
  for(mm in unique(a1$year[!is.na(a1$year)])) { # remove NAs which are generated when the lake is stratified at the satrt or end of the simulation
    a2 <- subset(a1, year==mm)
    ind <- which.max(a2$dur)
    if(nrow(a2)==1) if(is.na(a2$dur)) ind <- NA # fixes issue if stratified at end of data period
    yr <- c(yr,mm)
    s.max <- c(s.max,max(a2$dur))
    s.mean <- c(s.mean,mean(a2$dur))
    s.tot <- c(s.tot,sum(a2$dur))
    s.on <- c(s.on, as.POSIXlt(a2$start[ind])$yday)
    s.off <- c(s.off, as.POSIXlt(a2$end[ind])$yday)
    s.first <- c(s.first, min(a2$startday))
    s.last <- c(s.last, max(a2$endday))
  }

  # maximum surface temperature
  # loop thru years to find Tmax and its day of year
  TsMax <- NULL
  for(ii in unique(strat_yrs)) {
    Ts_maxi <- which.max(Ts[strat_yrs == ii])
    TsMaxOut <- data.frame(year=ii,
                           TsMax       = Ts[strat_yrs == ii][Ts_maxi],
                           TsMaxDay    = strat_doys[strat_yrs==ii][Ts_maxi],
                           TsMaxDate   = dates[strat_yrs == ii][Ts_maxi]
    )

    TsMax <- rbind(TsMax, TsMaxOut)
  }

  # minimum surface temperature
  # loop thru years to find Tmax and its day of year
  TsMin <- NULL
  for(ii in unique(strat_yrs)) {
    Ts_mini <- which.min(Ts[strat_yrs == ii])
    TsMinOut <- data.frame(year=ii,
                           TsMin       = Ts[strat_yrs == ii][Ts_mini],
                           TsMinDay    = strat_doys[strat_yrs==ii][Ts_mini],
                           TsMinDate   = dates[strat_yrs == ii][Ts_mini]
    )

    TsMin <- rbind(TsMin, TsMinOut)
  }

  # maximum bottom temperature
  # loop thru years to find Tbmax and its day of year
  TbMax <- NULL
  for(ii in unique(strat_yrs)) {
    Tb_maxi <- which.max(Tb[strat_yrs == ii])
    TbMaxOut <- data.frame(year=ii,
                           TbMax       = Tb[strat_yrs == ii][Tb_maxi],
                           TbMaxDay    = strat_doys[strat_yrs==ii][Tb_maxi],
                           TbMaxDate   = dates[strat_yrs == ii][Tb_maxi]
    )

    TbMax <- rbind(TbMax, TbMaxOut)
  }

  # minimum bottom temperature
  # loop thru years to find Tbmax and its day of year
  TbMin <- NULL
  for(ii in unique(strat_yrs)) {
    Tb_mini <- which.min(Tb[strat_yrs == ii])
    TbMinOut <- data.frame(year=ii,
                           TbMin       = Tb[strat_yrs == ii][Tb_mini],
                           TbMinDay    = strat_doys[strat_yrs==ii][Tb_mini],
                           TbMinDate   = dates[strat_yrs == ii][Tb_mini]
    )

    TbMin <- rbind(TbMin, TbMinOut)
  }

  # create empty data frame to fill with data (not all years may have strat or ice)
  out <- data.frame(year=yrs, TsMax=NA, TsMaxDay=NA, TsMin=NA, TsMinDay=NA, TbMax=NA, TbMaxDay=NA, TbMin=NA, TbMinDay=NA,
                    MaxStratDur=NA, MeanStratDur=NA, TotStratDur=NA,
                    StratStart=NA, StratEnd=NA,
                    StratFirst=NA, StratLast=NA)

  out[match(TsMax$year, yrs), c("TsMax","TsMaxDay")] <-
    TsMax[,c("TsMax","TsMaxDay")]

  out[match(TsMin$year, yrs), c("TsMin","TsMinDay")] <-
    TsMin[,c("TsMin","TsMinDay")]

  out[match(TbMax$year, yrs), c("TbMax","TbMaxDay")] <-
    TbMax[,c("TbMax","TbMaxDay")]

  out[match(TbMin$year, yrs), c("TbMin","TbMinDay")] <-
    TbMin[,c("TbMin","TbMinDay")]

  out[match(yr, yrs), -1:-9] <-
    data.frame(s.max,s.mean,s.tot,s.on,s.off,s.first,s.last)


  # ice cover
  if(!is.null(H_ice)) { # only do this if ice data provided
    ice <- H_ice > 0
    i_i_st <- diff(c(ice[1],ice))==1 # indices of ice cover onset
    i_i_en <- diff(c(ice[1],ice))==-1 #  # indices of ice cover end
    if(ice[1]) i_i_st <- c(NA, i_i_st) # if initially frozen, set first start date to NA
    if(ice[length(ice)]) i_i_en <- c(i_i_en, NA) # if frozen at end, set last thaw date to NA
    ice_st  <- dates[i_i_st] # ice start dates
    ice_en  <- dates[i_i_en] # ice end dates
    # if(sum(ice)==0)  # if there is no ice at all, set start and end to time=0

    # maximum ice thickness
    IceMax <- NULL
    for(ii in unique(ice_yrs)) {
      Hice_maxi <- which.max(H_ice[ice_yrs == ii])
      IceMaxOut <- data.frame(year=ii,
                              HiceMax     = H_ice[ice_yrs == ii][Hice_maxi],
                              HiceMaxDay  = ice_doys[ice_yrs==ii][Hice_maxi],
                              HiceMaxDate = dates[ice_yrs == ii][Hice_maxi])
      if(sum(H_ice[ice_yrs == ii])==0) IceMaxOut[1,c("HiceMaxDay","HiceMaxDate")] <- NA
      IceMax <- rbind(IceMax, IceMaxOut)
    }

    ice_start_doys <- ice_doys[i_i_st] # day of year of start of ice cover events
    ice_end_doys <- ice_doys[i_i_en]   # day of year of end of ice cover events
    ice_event_yrs <- ice_yrs[i_i_en]   # the years assigned to each ice event

    # if there is no ice, set values to NA ...
    if(sum(ice)==0) {
      ice_start_doys <- ice_end_doys <- ice_event_yrs <- ice_st <- ice_en <- NA
    }
    ice_dur <- as.double(difftime(ice_en, ice_st, units="days")) # duration of ice periods


    # summary of ice cover events
    ice.summary <- data.frame(year = ice_event_yrs,
                              start = ice_st,
                              end = ice_en,
                              dur = ice_dur,
                              startday = ice_start_doys,
                              endday = ice_end_doys)

    ice_out <- NULL
    for(mm in unique(ice.summary$year[!is.na(ice.summary$year)])) {
      ice2 <- subset(ice.summary, year==mm)
      ice2_on <- ice2[which.max(ice2$dur),"startday"]
      ice2_off <- ice2[which.max(ice2$dur),"endday"]
      if(anyNA(ice2$dur)) ice2_on <- ice2_off <- NA

      ice_out <- rbind(ice_out,
                       data.frame(year=mm,
                                  MeanIceDur=mean(ice2$dur),
                                  MaxIceDur=max(ice2$dur),
                                  TotIceDur=sum(ice2$dur),
                                  ice_on=ice2_on,
                                  ice_off=ice2_off,
                                  firstfreeze=min(ice2$startday),
                                  lastthaw=max(ice2$endday)))
    }

    ice_out <- ice_out[ice_out$year %in% yrs,] # trim years outside the simulation range (eg ice that forms at the end of the last year, which should be assigned to the following year outside the simulation period)

    ice_out1 <- data.frame(year=yrs, MeanIceDur=NA, MaxIceDur=NA,
                           TotIceDur=NA, IceOn=NA, IceOff=NA, FirstFreeze=NA,
                           LastThaw=NA, HiceMax=NA, HiceMaxDay=NA)
    ice_out1[match(ice_out$year, yrs),
             c("MeanIceDur","MaxIceDur","TotIceDur",
               "IceOn","IceOff","FirstFreeze","LastThaw")] <- ice_out[,-1]
    ice_out1[which(IceMax$year %in% ice_out1$year),c("HiceMax","HiceMaxDay")] <- IceMax[which(IceMax$year %in% ice_out1$year),c("HiceMax","HiceMaxDay")]

    out <- data.frame(out, ice_out1[,-1])

  }

  # adjust some exceptions where stratification or ice extend longer than the cutoff period
  i6 <- out$StratEnd < out$StratStart
  i6[is.na(i6)]<-FALSE # this gets rid of any NAs
  if(sum(i6, na.rm=TRUE)>0) out[i6, "StratEnd"] <- out[i6,"StratStart"] + out[i6,"MaxStratDur"]
  i7 <- out$StratLast < out$StratStart & out$TotStratDur < 365
  i7[is.na(i7)]<-FALSE # this gets rid of any NAs
  if(sum(i7, na.rm=TRUE)>0) out[i7, "StratLast"] <- out[i7,"StratLast"] + 364
  i8 <- out$IceOff < out$IceOn
  i8[is.na(i8)]<-FALSE # this gets rid of any NAs
  if(sum(i8, na.rm=TRUE)>0) out[i8, "IceOff"] <- out[i8,"IceOn"] + out[i8,"MaxIceDur"]
  i9 <- out$LastThaw < out$IceOn & out$TotIceDur < 365
  i9[is.na(i9)]<-FALSE # this gets rid of any NAs
  if(sum(i9, na.rm=TRUE)>0) out[i9, "LastThaw"] <- out[i9,"LastThaw"] + 364

  return(out)
}

# is it a leap year?
leap <- function(yr) ((yr%%4)==0) - ((yr%%100)==0) + ((yr%%400)==0)

# Calculate density from temperature using the formula (Millero & Poisson, 1981):
# this is the method stated in the isimip 2b protocol in July 2019
rho_water <- function(t) {
  999.842594 + (6.793952e-2 * t) - (9.095290e-3 * t^2) +
    (1.001685e-4 * t^3) - (1.120083e-6 * t^4) + (6.536336e-9 * t^5)
}

