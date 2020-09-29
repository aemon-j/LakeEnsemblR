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

  if(!file.exists(ncdf)) {
    stop(ncdf, " does not exist. Check the filepath is correct.")
  }

  # check if variable is an allowed name
  if(!var %in% lake_var_dic$short_name) {
    stop(paste0("Variabel '", var, "' unknown. Allowed names for var: ",
                paste0(lake_var_dic$short_name, collapse = ", ")))
  }

  tryCatch({
    fid <- ncdf4::nc_open(ncdf) # Open netCDF

    # Extract the time
    tim <- ncdf4::ncvar_get(fid, "time")
    tunits <- ncdf4::ncatt_get(fid, "time")
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
    mod_names <- ncdf4::ncatt_get(fid, "model", "Model")$value
    mod_names <- strsplit(mod_names, ", ")[[1]]
    mod_names <- substring(mod_names, 5)

    # Extract members
    mem <- ncdf4::ncvar_get(fid, "member")

    # Extract variable
    var1 <- ncdf4::ncvar_get(fid, var)
    tunits <- ncdf4::ncatt_get(fid, var)
    miss_val <- tunits$missing_value
    var1[var1 >= miss_val] <- NA # Replace large values with NAs
    var_dim <- strsplit(tunits$coordinates, " ")[[1]]

    # Extract depths if dimensions are greater than 2
    if(length(dim(var1)) > 2){
      z <- ncvar_get(fid, "z")
    }

  }, warning = function(w) {
    return_val <- "Warning"
  }, error = function(e) {
    return_val <- "Error"
    warning("Error creating netCDF file!")
  }, finally = {
    ncdf4::nc_close(fid) # Close netCDF file
  })

  mat <- matrix(data = c(var, tunits$units, tunits$coordinates),
               dimnames = list(c("short_name",
                                 "units", "dimensions"), c()))
  if(print == TRUE) {
    message("Extracted ", var, " from ", ncdf)
    print(mat)
  }

  if(length(dim(var1)) == 4) {
    n_vals <- dim(var1)[2] * (dim(var1)[3]) * (dim(var1)[4])
    # Check for empty members
    for(m in seq_len(dim(var1)[1])) {
      nas <- sum(is.na(var1[m, , , ]))
      if(nas == n_vals) {
        break
      }
    }
    if(m != dim(var1)[1]) {
      var1 <- var1[(seq_len(m-1)), , , ]
    }
  } else if(length(dim(var1)) == 3) {
    n_vals <- dim(var1)[2] * (dim(var1)[3])
    # Check for empty members
    for(m in seq_len(dim(var1)[1])) {
      nas <- sum(is.na(var1[m, , ]))
      if(nas == n_vals) {
        break
      }
    }
    if(m != dim(var1)[1]) {
      var1 <- var1[(seq_len(m-1)), , ]
    }
  }



  if(return == "array"){

    if(length(dim(var1)) == 4) {
      dimnames(var1) <- list(paste0("member_", seq_len(dim(var1)[1])),
                             mod_names, as.character(time), z)
    }
    if(length(dim(var1)) == 3 & var == "temp") {
      dimnames(var1) <- list(mod_names, as.character(time), z)
    }
    if(length(dim(var1)) == 3 & var == "ice_height") {
      dimnames(var1) <- list(paste0("member_", seq_len(dim(var1)[1])),
                             mod_names, as.character(time))
    }
    if(length(dim(var1)) == 2) {
      dimnames(var1) <- list(mod_names, as.character(time))
    }

    return(var1)
  }

  if(return == "list"){
    # 3-D var w/member ----
    if("z" %in% var_dim) {
      if(length(dim(var1)) == 4){
        if(dim == "model") {

          if ( dim_index > dim(var1)[1] ) {
            stop("Dimension index ", dim_index, " out of bounds!\nAvailable dimensions: ",
                 paste(seq_len(dim(var1)[1]), collapse = ","))
          }

          var_list <- lapply(seq(dim(var1)[2]), function(x)var1[dim_index, x, , ])
          names(var_list) <- mod_names
        } else if ( dim == "member" ) {

          if (dim_index > dim(var1)[2]) {
            stop("Dimension index ", dim_index, " out of bounds!\nAvailable dimensions: ",
                 paste(seq_len(dim(var1)[2]), collapse = ","))
          }

          var_list <- lapply(seq(dim(var1)[1]), function(x)var1[x, dim_index, , ])

          n_vals <- dim(var_list[[1]])[1] * (dim(var_list[[1]])[2])

          # Check for empty members
          for(m in seq_len(length(var_list))) {
            nas <- sum(is.na(var_list[[m]]))
            if(nas == n_vals) {
              break
            }
          }
          if(m != length(var_list)) {
            var_list <- var_list[(seq_len(m-1))]
          }
          names(var_list) <- paste0(mod_names[dim_index], "_member_",
                                    seq_len(length(var_list)))

        }

        # Add datetime column + columns names for rLake Analyzer
        var_list <- lapply(var_list, function(x){
          x <- as.data.frame(x)
          x <- cbind(time, x)
          colnames(x) <- c("datetime", paste0("wtr_", abs(z)))
          return(x)
        })
        # 3-D var & no members ----
      } else if(length(dim(var1)) == 3) {
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

  # 2-D variables no members ----
    } else {
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
        # 2-D w/ members ----
      } else if (length(dim(var1)) == 3) {

        if(dim == "model") {

          if ( dim_index > dim(var1)[2] ) {
            stop("Dimension index ", dim_index, " out of bounds!\nAvailable dimensions: ",
                 paste(seq_len(dim(var1)[2]), collapse = ","))
          }

          var_list <- lapply(seq(dim(var1)[2]), function(x)var1[dim_index, x, ])
          names(var_list) <- mod_names
        } else {

          var_list <- lapply(seq(dim(var1)[1]), function(x)var1[x, dim_index, ])
          n_vals <- length(var_list[[1]])
          # Check for empty members
          for(m in seq_len(length(var_list))) {
            nas <- sum(is.na(var_list[[m]]))
            if(nas == n_vals) {
              break
            }
          }
          if(m != length(var_list)) {
            var_list <- var_list[(seq_len(m-1))]
          }
          names(var_list) <- paste0(mod_names[dim_index], "_member_",
                                    seq_len(length(var_list)))
        }

        # Add datetime column + columns names for variable
        var_list <- lapply(var_list, function(x){
          x <- as.data.frame(x)
          x <- cbind(time, x)
          colnames(x)[2] <- var
          return(x)
        })

      }
    }
  }
  return(var_list)
}
