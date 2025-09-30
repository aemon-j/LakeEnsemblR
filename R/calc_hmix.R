#' @title Estimate mixed-layer depth from temperature profiles
#' @description This function estimates the mixed depth. It finds the minimum curvature
#' in the profile (maximum for winter stratification) as the border of the surface mixed
#' layer. Then it finds the thermocline as the depth of maximum T-gradient. It 
#' estimates the mixed layer depth as the depth where the regression line through
#' the surface layer temperatures intersects with the regression line through the
#' thermocline temperatures. It performs some other checks, like whether 
#' stratification exists (Ts-Tb) > threshold min.dT, and whether the thermocline 
#' is above the surface layer, or whether the lake is completely isothermal and
#' there is no intersection (and returns NA). If mixed, it assumes the mixed 
#' layer depth is the maximum depth. It also plots the profile if desired 
#' (plot=TRUE), marking the surface layer and inflection point in red and the 
#' thermocline in blue.
#' @name calc_hmix
#' @param temps a numeric vector of water temperature in degC.
#' @param depths a numeric vector corresponding to the depths (in m) of the temps
#'  measurements
#' @param min.dT numeric; minimum change in water temperature
#' @param therm.dz numeric; depth resolution for calculating thermoclime
#' @param min.hmix numeric; minimum depth of hmix otherwise returns NA. Defaults to 1.5m
#' @param plot boolean; plot temperature profile with calculated mixed depth.
#' Defaults to FALSE
#' @param ... arguments to be passed to base plot
#' @encoding UTF-8
#' @author Tom Shatwell
#'
#' @export
calc_hmix <- function(temps, depths, min.dT = 1, 
                 therm.dz = 1, min.hmix = 1.5, plot = FALSE, ...) {
  if(sum(!is.na(temps)) > 3) {
    dTdz <- diff(temps) / diff(depths)
    d2Tdz2 <- diff(temps, 1, 2) / diff(depths, 2)^2
    i1 <- which.min(dTdz) # index of strongest gradient (-ve for positive strat, +ve for winter strat)
    i2 <- which.min(d2Tdz2) # index of minimum curvature (bot epilimnion)
    while(depths[i2] <= min.hmix) {
      d2Tdz2[i2] <- NA
      i2 <- which.min(d2Tdz2)
      if(all(is.na(d2Tdz2))){
        warning("Warning: Too few temperature values below 'min.hmix'")
        return(as.numeric(NA))
      }
    }
    while(i1 <= i2) {
      dTdz[i1] <- NA
      i1 <- which.min(dTdz)
    }
    i3 <- which.max(d2Tdz2) # index of maximum curvature (top hypolimnion)
    if(min(depths) < 2) {
      Ts <- mean(temps[depths <= 2], na.rm=TRUE)
    } else {
      Ts <- mean(temps[1:i1], na.rm=TRUE)
      warning("Warning: Surface temp estimated as temps above greatest curvature")
    }
    Tb <- mean(temps[depths >= (max(depths)-2)], na.rm=TRUE)
    if(Ts < 4) {
      i1 <- which.max(dTdz) # index of maximum gradient
      i2 <- which.max(d2Tdz2) # max curvature for winter stratification
      i3 <- which.min(d2Tdz2) # min curvature for winter stratification
    }
    h1 <- mean(c(depths[i1], depths[i1+1])) # depth of maximum gradient
    h2 <- depths[i2+1] # depth of inflection
    
    thermo <- data.frame(depths=depths[depths >= depths[i1] - therm.dz & 
                                         depths <= depths[i1 + 1] + therm.dz],
                         temps = temps[depths >= depths[i1] - therm.dz & 
                                         depths <= depths[i1 + 1] + therm.dz])
    
    regs <- lm(temps[1:i2] ~ depths[1:i2]) # regression thru surface temps
    regt <- lm(temps ~ depths, thermo) # regression thru thermocline
    dTdz.s <- coef(regs)[2] # T gradient in surface layer
    if(is.na(coef(regs)[2])) dTdz.s <- 0
    
    h <- (coef(regt)[1] - coef(regs)[1]) / # depth of intersection of thermocline and surface layer regression lines
      (dTdz.s - coef(regt)[2])
    
    if(!is.na(h) & h > depths[i1 + 1]) h <- NA # ignore if intersection of thermocline and surface layer is below thermocline
    if(i1 < i2) h <- NA # ignore if minimum curvature is below the thermocline
    if(abs(Ts - Tb) < min.dT) h <- max(depths) # assume mixed to max(depths) if Ts-Tb < min.dT
    if(!is.na(h) & h < 0) h <- NA
  } else {
    h <- as.numeric(NA)
    warning("Warning: need more than 3 temperature values")
  }
  if(plot){
    plot(temps, depths, 
         ylim=c(max(depths),0), type="n", ...)
    abline(h=0:(max(depths)), col="grey")
    abline(h=h)
    lines(temps[1:i2], depths[1:i2], col="red", lwd=2)
    lines(temps[c(i1,i1+1)], depths[c(i1,i1+1)], col="blue", lwd=2)
    points(temps[i2+1], depths[i2+1], col="red",pch=16)
    box()
  }
  return(h)  
}
