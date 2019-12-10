#' Extract FLake output as a dataframe
#'@description
#'Read in FLake results, interpolate to a gridded struture and return a dataframe in the format of rLakeAnalyzer
#'
#' @name get_wtemp_df
#' @param output filepath; to FLake output file
#' @param nml_file filepath; to FLake namelist file
#' 
#' @return Dataframe in rLakeAnalyzer format
#' 
#' @example 
#' df <- get_wtemp_df(output = 'FLake/output/output.dat', folder = 'FLake', nml_file = 'FLake/feeagh.nml')
#' 
#' @export

get_wtemp_df <- function(output, folder, nml_file){
  
  imeandepth <- glmtools::get_nml_value(arg_name = 'depth_w_lk', nml_file = nml_file)
  met_file <- glmtools::get_nml_value(arg_name = 'meteofile', nml_file = nml_file)
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
  D <- imeandepth
  idepths <- seq(0,imeandepth,by = 0.5)
  l_wtr <- list()
  for(kk in 1:length(idepths)){
    z <-  idepths[kk]
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
  
  # change data format from long to wide
  wtr4 <- reshape2::dcast(wtr2, dateTime ~ depth)
  str_depths <- colnames(wtr4)[2:ncol(wtr4)]
  colnames(wtr4) <- c('datetime',paste('wtr_',str_depths, sep=""))
  wtr4$datetime <- datetime # Input datetime vector
  return(wtr4)
  
}
