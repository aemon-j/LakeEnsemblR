#' Extract FLake output as a dataframe
#'
#'Read in FLake results, interpolate to a gridded struture and return a dataframe in the format of rLakeAnalyzer
#'
#' @name get_wtemp_df
#' @param output filepath; to FLake output file
#' @param depths vector; of numeric values to extract the depths at
#' @param nml_file filepath; to FLake namelist file
#' @param long; Boolean; return data in long form

#' @return Dataframe in rLakeAnalyzer format
#' @importFrom reshape2 dcast
#' @importFrom glmtools get_nml_value
#'
#' @examples
#' \dontrun{
#' df <- get_wtemp_df(output = 'FLake/output/output.dat', folder = 'FLake', nml_file = 'FLake/feeagh.nml')
#' }
#'
#' @export
get_wtemp_df <- function(output, depths, folder, nml_file, long = FALSE){

  met_file <- suppressWarnings(get_nml_value(arg_name = 'meteofile', nml_file = nml_file))
  met_file <- file.path(folder, met_file)
  met_file <- gsub(',', '', met_file)

  met <- read.delim(met_file, header = FALSE)
  datetime <- as.POSIXct(met[,ncol(met)], tz = 'UTC')

  # Code from R. I. Woolway FLake Workshop
  flake_out <- read.table(output, header = TRUE, skip = 1)
  # calculate temperature profile
  Ts <- flake_out[['Ts']] # mixed layer (top) temperature
  Tb <- flake_out[['Tb']] # bottom temperature
  h <- flake_out[['h_ML']] # mixed layer depth
  C <- flake_out[['C_T']] # shape factor
  D <- max(depths) # mean depth
  l_wtr <- list()
  for(kk in 1:length(depths)){
    z <-  depths[kk]
    zeta <- (z - h) / (D - h)
    c1 <- 40 / 3
    c2 <- 20 / 3
    c3 <- 5 / 3
    c4 <- 10 / 3
    is.in.ML <- z <= h
    Tz <- ifelse(is.in.ML,Ts,zeta*(c1*C-c2+zeta*(18-30*C+zeta*(20*C-12+zeta*(c3-c4*C))))*(Tb-Ts)+Ts)
    dd <- data.frame(dateTime = flake_out$time,
                     depth = z,
                     wtr = Tz)
    l_wtr[[kk]] <- dd
  }
  wtr2 <- do.call("rbind",l_wtr)

  # sort in temporal order
  idx <- sort(wtr2$dateTime, index.return = TRUE)$ix
  wtr2 <- wtr2[idx,]
  if(long == TRUE){
    colnames(wtr2) <- c("datetime", "Depth_meter", "Water_Temperature_celsius")
    tims = sum((wtr2[,1] == wtr2[1,1]))
    time_long <- rep(datetime, each = tims)
    wtr2$datetime <- time_long
    return(wtr2)
  }else{
    # change data format from long to wide
    wtr4 <- dcast(wtr2, dateTime ~ depth, value.var = 'wtr')
    str_depths <- colnames(wtr4)[2:ncol(wtr4)]
    colnames(wtr4) <- c('datetime',paste('wtr_',str_depths, sep=""))
    wtr4$datetime <- datetime # Input datetime vector
    return(wtr4)
  }
}
