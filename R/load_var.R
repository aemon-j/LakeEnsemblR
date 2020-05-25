#' Extract variables from NetCDF file
#'
#' Extracts a selected parameter from the netCDF file and formats it into a dataframe.
#'
#' @param ncdf filepath; Name of the netCDF file to extract variable
#' @param var character; Name of the variable to be extracted. Must match short name in netCDF file
#' @param return character; Must be either list or array
#' @param print logical; Print the name and units of the variable extracted, defaults to TRUE
#' @return dataframe in the same format as the observation file with the surface in the top column and the bottom in the last column.
#' @importFrom ncdf4 nc_open
#' @importFrom ncdf4 nc_close
#' @importFrom ncdf4 ncvar_get
#' @importFrom ncdf4 ncatt_get

#' @export
load_var <- function(ncdf, var, return = "list", print = TRUE){
  
  match.arg(return, c("list", "array"))
  
  fid <- nc_open(ncdf) # Open netCDF
  
  # Extract the time
  tim <- ncvar_get(fid, "time")
  tunits <- ncatt_get(fid, "time")
  tustr <- strsplit(tunits$units, " ")
  # step <- tustr[[1]][1]
  tdstr <- strsplit(unlist(tustr)[3], "-")
  tmonth <- as.integer(unlist(tdstr)[2])
  tday <- as.integer(unlist(tdstr)[3])
  tyear <- as.integer(unlist(tdstr)[1])
  tdstr <- strsplit(unlist(tustr)[4], ":")
  thour <- as.integer(unlist(tdstr)[1])
  tmin <- as.integer(unlist(tdstr)[2])
  origin <- as.POSIXct(paste0(tyear, "-", tmonth,
                              "-", tday, " ", thour, ":", tmin),
                       format = "%Y-%m-%d %H:%M", tz = "UTC")
  time <- as.POSIXct(tim, origin = origin, tz = "UTC")

  # Extract model names
  mod_names <- ncatt_get(fid, "model", "Model")$value
  mod_names <- strsplit(mod_names, ", ")[[1]]
  mod_names <- substring(mod_names, 5)
  
  # Extract parameters
  pars <- ncvar_get(fid, "member")
  
  # Will need updating when parameter ensembles are added
  
  # Extract variable
  var1 <- ncvar_get(fid, var)
  tunits <- ncatt_get(fid, var)
  
  # Extract depths if dimensions are greater than 2
  if(length(dim(var1)) > 2){
    z <- ncvar_get(fid, "z")
  }
  
  nc_close(fid) # Close netCDF
  
  mat <- matrix(data = c(var, tunits$units, tunits$coordinates),
               dimnames = list(c("short_name",
                                 "units", "dimensions"), c()))
  if (print == TRUE) {
    message("Extracted ", var, " from ", ncdf)
    print(mat)
  }
  
  if(return == "array"){
    
    if(length(dim(var1)) > 2){
      dimnames(var1) <- list(mod_names, as.character(time), z)
    }
    if(length(dim(var1)) == 2){
      dimnames(var1) <- list(mod_names, as.character(time))
    }
    
    return(var1)
  }
  
  if(return == "list"){
    if(length(dim(var1)) > 2){
      var_list <- lapply(seq(dim(var1)[1]), function(x)var1[x, , ])
      names(var_list) <- mod_names
      
      # Add datetime column + columns names for rLake Analyzer
      var_list <- lapply(var_list, function(x){
        x <- as.data.frame(x)
        x <- cbind(time, x)
        colnames(x) <- c("datetime", paste0("wtr_", abs(z)))
        return(x)
      })
    }
    
    if (length(dim(var1)) == 2) {
      var_list <- lapply(seq(dim(var1)[1]), function(x)var1[x, ])
      names(var_list) <- mod_names
      
      # Add datetime column + columns names for rLake Analyzer
      var_list <- lapply(var_list, function(x){
        x <- as.data.frame(x)
        x <- cbind(time, x)
        colnames(x)[2] <- var
        return(x)
      })
    }
    
    return(var_list)
  }

}
