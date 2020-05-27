#' Extract variables from NetCDF file
#'
#' Extracts a selected parameter from the netCDF file and formats it into a dataframe.
#'
#' @param ncdf filepath; Name of the netCDF file to extract variable
#' @param var character; Name of the variable to be extracted. Must match short name in netCDF file
#' @param return character; Must be either list or array
#' @param dim character; NetCDF dimensions to extract. Must be either "member" or "model". Defaults to "model". 
#' @param dim_index numeric; Index of dimension chosen to extract from. Defaults to 1.
#' @param print logical; Print the name and units of the variable extracted, defaults to TRUE
#' @return dataframe in the same format as the observation file with the surface in the top column and the bottom in the last column.
#' @importFrom ncdf4 nc_open
#' @importFrom ncdf4 nc_close
#' @importFrom ncdf4 ncvar_get
#' @importFrom ncdf4 ncatt_get

#' @export
load_var <- function(ncdf, var, return = "list", dim = "model", dim_index = 1, print = TRUE){
  
  match.arg(return, c("list", "array"))
  match.arg(dim, c("model", "member"))
  
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
  
  # Extract members
  mem <- ncvar_get(fid, "member")
  
  # Extract variable
  var1 <- ncvar_get(fid, var)
  tunits <- ncatt_get(fid, var)
  miss_val <- tunits$missing_value 
  var1[var1 >= miss_val] <- NA # Replace large values with NAs
  var_dim <- tunits$coordinates
  
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
    
    if(length(dim(var1)) == 4) {
      dimnames(var1) <- list(as.character(mem), mod_names, as.character(time), z)
    }
    if(length(dim(var1)) == 3) {
      dimnames(var1) <- list(mod_names, as.character(time), z)
    }
    if(length(dim(var1)) == 2) {
      dimnames(var1) <- list(mod_names, as.character(time))
    }
    
    return(var1)
  }
  
  if(return == "list"){
    
    if( length(mem) == 1 ) {
      
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
    } else {
      
      if(length(dim(var1)) == 4){
        if( dim == "model" ) {
          
          if ( dim_index > dim(var1)[2] ) {
            stop("Dimension index ", dim_index, " out of bounds!\nAvailable dimensions: ",
                 paste(seq_len(dim(var1)[2]), collapse = ","))
          }
          
          var_list <- lapply(seq(dim(var1)[2]), function(x)var1[dim_index, x, , ])
          names(var_list) <- mod_names
        } else if ( dim == "member" ) {
          
          if ( dim_index > dim(var1)[1] ) {
            stop("Dimension index ", dim_index, " out of bounds!\nAvailable dimensions: ",
                 paste(seq_len(dim(var1)[1]), collapse = ","))
          }
          
          var_list <- lapply(seq(dim(var1)[1]), function(x)var1[x, dim_index, , ])
          names(var_list) <- as.character(mem)
        }
        
        
        
        # Add datetime column + columns names for rLake Analyzer
        var_list <- lapply(var_list, function(x){
          x <- as.data.frame(x)
          x <- cbind(time, x)
          colnames(x) <- c("datetime", paste0("wtr_", abs(z)))
          return(x)
        })
      }
      
      # For 2-D variables e.g. ice_height
      if (length(dim(var1)) == 3) {
        var_list <- lapply(seq(dim(var1)[1]), function(x)var1[x, , ])
        names(var_list) <- mod_names
        
        # Add datetime column + columns names for variable
        var_list <- lapply(var_list, function(x){
          x <- as.data.frame(x)
          x <- cbind(time, x)
          colnames(x)[2] <- var
          return(x)
        })
      }
      
    }

    return(var_list)
  }

}
