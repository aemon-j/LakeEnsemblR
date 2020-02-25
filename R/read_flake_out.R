#' Extract FLake output
#'
#'Read in FLake results, interpolate to a gridded struture and return a dataframe in long or wide format for vars = "temp"
#'
#' @name read_flake_out
#' @param output filepath; to FLake output file
#' @param vars vector; variables to extract from FLake output. Currently just temp and ice
#' @param depths vector; of numeric values to extract the depths at. Only used if extracting water temp
#' @param nml_file filepath; to FLake namelist file
#' @param long Boolean; return data in long form
#' @param out_time vector; of output time values to subset data by.
#' @param out_hour numeric; hour of output time values to subset data. Only used for FLake if model time step is 86400s.
#' @return Dataframe if only one variable otherwise a list
#' @importFrom reshape2 dcast
#' @importFrom glmtools get_nml_value
#'
#' @examples
#' \dontrun{
#' df <- get_wtemp_df(output = "FLake/output/output.dat", folder = "FLake", nml_file = "FLake/feeagh.nml")
#' }
#'
#' @export
read_flake_out <- function(output, vars, depths,  folder = ".", nml_file, long = FALSE, out_time, out_hour = 0){

  met_file <- suppressWarnings(get_nml_value(arg_name = "meteofile", nml_file = nml_file))
  met_file <- file.path(folder, met_file)
  met_file <- gsub(",", "", met_file)

  met <- read.delim(met_file, header = FALSE)
  datetime <- as.POSIXct(met[, ncol(met)], tz = "UTC") + (out_hour * 60 * 60)

  # Code from R. I. Woolway FLake Workshop
  flake_out <- read.table(output, header = TRUE, skip = 1, stringsAsFactors = FALSE)

  out_list <- list()
  if("temp" %in% vars){
    # calculate temperature profile
    Ts <- flake_out[["Ts"]] # mixed layer (top) temperature
    Tb <- flake_out[["Tb"]] # bottom temperature
    h <- flake_out[["h_ML"]] # mixed layer depth
    C <- flake_out[["C_T"]] # shape factor
    D <- max(depths) # mean depth

    if(long){
      l_wtr <- list()
    }else{
      mat <- matrix(NA, nrow = length(datetime), ncol = length(depths))
    }

    # Make sure dates are ordered
    depths <- depths[order(depths)]


    for(kk in seq_len(length(depths))){
      z <- depths[kk]
      zeta <- (z - h) / (D - h)
      c1 <- 40 / 3
      c2 <- 20 / 3
      c3 <- 5 / 3
      c4 <- 10 / 3
      is_in_ML <- z <= h
      if(is_in_ML){
        Tz <- Ts
      }else{
        Tz <- zeta * (c1 * C - c2 + zeta *
                        (18 - 30 * C + zeta * (20 * C - 12 + zeta * (c3 - c4 * C)))) *
          (Tb - Ts) + Ts
      }
      
      if(long){
        dd <- data.frame(dateTime = seq_len(length(Tz)),
                         depth = z,
                         wtr = Tz)
        l_wtr[[kk]] <- dd
      }else{
        mat[, kk] <- Tz
      }
    }

    if(long){
      wtr2 <- do.call("rbind", l_wtr)

      # sort in temporal order
      wtr2 <- wtr2[order(wtr2$dateTime, wtr2$depth), ]
      colnames(wtr2) <- c("datetime", "Depth_meter", "Water_Temperature_celsius")
      tims <- sum((wtr2[, 1] == wtr2[1, 1]))
      time_long <- rep(datetime, each = tims)
      wtr2$datetime <- time_long
    }else{
      wtr2 <- as.data.frame(mat)
      wtr2$datetime <- datetime
      wtr2 <- wtr2[, c(ncol(wtr2), 1:(ncol(wtr2) - 1))]
      colnames(wtr2) <- c("datetime", paste("wtr_", depths, sep = ""))
    }
    wtr2 <- wtr2[which(wtr2$datetime %in% out_time$datetime), ]
    out_list[[length(out_list) + 1]] <- wtr2
    names(out_list)[length(out_list)] <- "temp"
  }

  if("ice_height" %in% vars){
    ice_height <- flake_out[["H_ice"]]
    df <- data.frame(datetime, ice_height)

    out_list[[length(out_list) + 1]] <- df
    names(out_list)[length(out_list)] <- "ice_height"
  }

  if(length(out_list) == 1){
    out_list <- out_list[[1]]
  }

  return(out_list)

}
