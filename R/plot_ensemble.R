#' Function to plot results of the ensemble run
#'
#' Plot the outcome of the ensemble run for a given depth along with the minimum, maximum, and
#' an average value.
#'
#' @param ncdf filepath; to the netcdf file created by `run_ensemble()`
#' @param model string vector; of models which should be included in the plot
#' @param var string; of variable which to plot
#' @param dim string; NetCDF dimensions to extract. Must be either "member" or
#'  "model". Defaults to "model". Only used if plotting from netCDF file.
#' @param dim_index numeric; Index of dimension chosen to extract from. Defaults to 1.
#'  Only used if plotting from netCDF file.
#' @param depth If `var` has a depth dimension, for which depth should it be plotted?
#' @param date Specific date for which depth profiles should be plotted
#' @param av_fun Averaging function to use, defaults to the arithmetic mean (`mean()`)
#' @param boxwhisker Create additional box-whisker plots for each model
#' @param residuals Create an additional plot with model residuals over time
#' @author Johannes Feldbauer, Robert Ladwig, Jorrit Mesman
#' @importFrom reshape2 melt
#' @importFrom RColorBrewer brewer.pal
#' @examples
#' \dontrun{
#' # time series
#' p1 <- plot_ensemble(ncdf = "output/ensemble_output.nc",
#'                    model = c('FLake', 'GLM',  'GOTM', 'Simstrat', 'MyLake'),
#'                    var = "temp", depth = 0.9, boxwhisker = TRUE)
#'
#'# depth profiles
#' p2 <- plot_ensemble(ncdf = "output/ensemble_output.nc",
#'                    model = c('FLake', 'GLM',  'GOTM', 'Simstrat', 'MyLake'),
#'                    var = "temp", date = as.POSIXct("2010-06-13", tz = "UTC"),
#'                    boxwhisker = TRUE)
#'
#' }
#'
#' @export
plot_ensemble <- function(ncdf, model = c("FLake", "GLM",  "GOTM", "Simstrat", "MyLake"),
                          var = "temp", dim = "model", dim_index = 1,
                          depth = NULL, date = NULL, av_fun = "mean", boxwhisker = FALSE,
                          residuals = FALSE){

  # Fix time zone
  original_tz <- Sys.getenv("TZ")

  # this way if the function exits for any reason, success or failure, these are reset:
  on.exit({
    Sys.setenv(TZ = original_tz)
  })

  Sys.setenv(TZ = "UTC")

  # check if model input is correct
  model <- check_models(model)
  # Check if netCDF exists
  if(!file.exists(ncdf)){
    stop("File '", ncdf, "' does not exist. Check you have the correct filepath.")
  }
  # Check if var is in ncdf
  vars <- gotmtools::list_vars(ncdf)
  if(!(var %in% vars)){
    stop("Variable '", var, "' is not present in the netCDF file '", ncdf, "'")
  }

  # get variable
  var_list <- load_var(ncdf, var = var, return = "list", dim = dim,
                       dim_index = dim_index, print = FALSE)
  # if members are selected, still get the observations
  if(dim == "member"){
    obs_list <- load_var(ncdf, var = var, return = "list", dim = "model",
                         dim_index = 1, print = FALSE)
    obs_list <- list("Obs" = obs_list[["Obs"]])
    var_list[["Obs"]] <- obs_list[["Obs"]]
  }else if(dim == "model") {
    # check if selected models are in the ncdf file
    if(any(!(model %in% names(var_list)))){
      stop("Model ", paste(model[!(model %in% names(var_list))], collapse = "/"),
           " not found in the ncdf file ", ncdf)
    }
    # only the selected models
    var_list <- var_list[c(model, "Obs")]
  }

  # output list
  plist <- list()
  pindex <- 1

  # colors for plotting
  colfunc <- colorRampPalette(RColorBrewer::brewer.pal(length(model), "Set2"))
  
  # Checks
  if(!is.null(depth) & !is.null(date)){
    stop("You can't enter both a depth and a date!")
  }
  
  if(ncol(var_list[[1]]) == 2 & (!is.null(depth) | !is.null(date))){
    stop("When trying to plot a 0D variable, you can't set 'depth' or 'date'!")
  }else if(ncol(var_list[[1]]) > 2 & is.null(depth) & is.null(date)){
    stop("When trying to plot a 1D variable, you need to set 'depth' or 'date'.",
         " If you are trying to make a heatmap, use plot_heatmap()")
  }
  
  # Turn var_list into 1D
  if(ncol(var_list[[1]]) > 2){
    # Extract for either depth or date
    if(!is.null(depth)){
      selec_col <- "variable"
      selec_col2 <- "datetime"
      selec_const <- paste0("wtr_", depth)
    }else if(!is.null(date)){
      selec_col <- "datetime"
      selec_col2 <- "Depth"
      selec_const <- as.POSIXct(date)
    }
    
    var_list_long <- reshape2::melt(var_list, id.vars = "datetime")
    var_list_long <- var_list_long[var_list_long[[selec_col]] == selec_const, ]
    
    colnames(var_list_long) <- c("datetime", "Depth", "value", dim)
    
    var_list_long$Depth <- -as.numeric(gsub("wtr_", "", var_list_long$Depth))
    
    obs <- var_list_long[var_list_long[[4]] == "Obs", ]
    colnames(obs) <- c("datetime", "Depth", "value", "Observed")
    
    if(nrow(var_list_long) == 0){
      stop("Modelled data is all NAs at ", selec_const, ".",
         "Please inspect the model output and re-run 'run_ensemble()' if necessary.")
    }
    
  }else{
    selec_col2 <- "datetime"
    
    var_list_long <- reshape2::melt(var_list, id.vars = "time")
    colnames(var_list_long) <- c("datetime", "Depth", "value", dim)
    var_list_long$Depth <- 0
    
    obs <- var_list_long[var_list_long[[4]] == "Obs", ]
    colnames(obs) <- c("datetime", "Depth", "value", "Observed")
  }
  
  # Calculating averages
  var_list_long_temp <- var_list_long[var_list_long[[4]] != "Obs", ]
  dat_av <- aggregate(var_list_long_temp[, "value"],
                      by = list(var_list_long_temp[[selec_col2]]),
                      FUN = get(av_fun),
                      na.rm = TRUE)
  colnames(dat_av)[2] <- "mean"
  df_temp <- aggregate(var_list_long_temp[, "value"],
                      by = list(var_list_long_temp[[selec_col2]]),
                      FUN = max,
                      na.rm = TRUE)
  dat_av$max <- df_temp[[2]]
  df_temp <- aggregate(var_list_long_temp[, "value"],
                       by = list(var_list_long_temp[[selec_col2]]),
                       FUN = min,
                       na.rm = TRUE)
  dat_av$min <- df_temp[[2]]
  dat_av$Type <- av_fun
  colnames(dat_av)[1] <- selec_col2
  rm(var_list_long_temp, df_temp)
  
  ## plot timeseries ----
  uniq_dim <- unique(var_list_long[[dim]])
  values <- c("grey42", colfunc(length(uniq_dim) - 1), "black")
  breaks <- c(av_fun, uniq_dim, "Obs")
  guide <- guide_legend(override.aes = list(
    linetype = c(rep("solid", length(uniq_dim)),
                 rep("blank", 1)),
    shape = c(rep(NA, length(uniq_dim)), 16)))
  
  # Plot main figure
  p1 <- ggplot() +
    geom_ribbon(data = dat_av, aes_string(selec_col2, ymin = "min", ymax = "max"),
                alpha = 0.2) +
    geom_line(data = var_list_long, aes_string(x = selec_col2, y = "value", col = dim),
              lwd = ifelse(dim == "member", 0.75, 1),
              alpha = ifelse(dim == "member", 0.75, 1), na.rm = TRUE) +
    geom_line(data = dat_av, aes_string(selec_col2, "mean", col = "Type"),
              lwd = 1.33, na.rm = TRUE) +
    geom_point(data = obs, aes_string(x = selec_col2, y = "value", col = "Observed"), size = 1,
               na.rm = TRUE) +
    scale_colour_manual(values = values,
                        breaks = breaks,
                        guide = guide) +
    theme(text = element_text(size = 10),
          axis.text.x = element_text(angle = 0, hjust = 0.5),
          legend.margin = margin(0, 0, 0, 0),
          legend.box.margin = margin(0, 0, 0, 0),
          legend.position = "bottom", legend.title = element_blank())
  
  if(!is.null(depth)){
    p1 <- p1 +
      ylab(var) +
      xlab("") +
      ggtitle(paste0("Time Series ", paste0("at depth = ", depth, " m")))
  }else if(!is.null(date)){
    p1 <- p1 +
      xlab(var) +
      coord_flip() +
      ggtitle(paste0("Depth profile ", paste0("at date ", format(date))))
  }else{
    p1 <- p1 +
      ylab(var) +
      xlab("") +
      ggtitle(paste0("Time Series of ", paste0(var)))
  }
  
  plist[[pindex]] <- p1
  pindex <- pindex + 1
  
  # Plot residuals
  if(residuals){
    if(sum(!is.na(obs$value)) == 0){
      warning(paste0("Residuals can not be calculated because observed data is missing."))
    }else{
      # if observations are available plot residuals
      dat_res <- var_list_long
      dat_res$value <- var_list_long$value - obs$value
      dat_res <- na.exclude(dat_res)
      if(!is.null(date)){
        dat_av2 <- dat_av[order(dat_av$Depth, decreasing = T), ]
      }else{
        dat_av2 <- dat_av
      }
      dat_resav <- dat_av2
      dat_resav$mean <- dat_av2$mean - obs$value
      dat_resav$max <- dat_av2$max - obs$value
      dat_resav$min <- dat_av2$min - obs$value
      dat_resav <- dat_resav[!is.na(dat_resav$mean), ]
      dat_resav <- na.exclude(dat_resav)
      
      p2 <- ggplot() +
        geom_ribbon(data = dat_resav, aes_string(selec_col2, ymin = "min", ymax = "max"),
                    alpha = 0.2) +
        geom_line(data = dat_res, aes_string(x = selec_col2, y = "value", col = dim),
                  na.rm = TRUE) +
        geom_line(data = na.omit(dat_res), aes_string(x = selec_col2, y = "value", col = dim),
                  na.rm = TRUE) +
        geom_line(data = dat_resav, aes_string(selec_col2, "mean", col = "Type"), lwd = 1.33,
                  na.rm = TRUE) +
        scale_colour_manual(values = c("grey42", colfunc(length(unique(dat_res[, dim])) - 1),
                                       "black"),
                            breaks = c(av_fun, unique(dat_res[, dim]), "Obs"),
                            guide = guide_legend(override.aes = list(
                              linetype = c(rep("solid", length(unique(dat_res[, dim])) + 1)),
                              shape = c(rep(NA, length(unique(dat_res[, dim])) + 1))))) +
        theme(text = element_text(size = 10),
              axis.text.x = element_text(angle = 0, hjust = 0.5),
              legend.margin = margin(0, 0, 0, 0),
              legend.box.margin = margin(0, 0, 0, 0),
              legend.position = "bottom", legend.title = element_blank())
      
      if(!is.null(depth)){
        p2 <- p2 +
          ylab(var) +
          xlab("") +
          ggtitle(paste0("Residuals ", paste0("at depth = ", depth, " m")))
      }else if(!is.null(date)){
        p2 <- p2 +
          xlab("depth") +
          ylab(var) +
          coord_flip() +
          ggtitle(paste0("Residuals ", paste0("at date ", format(date))))
      }else{
        p2 <- p2 +
          xlab("") +
          ylab(var) +
          ggtitle(paste0("Residuals ", paste0("of ", var)))
      }
      
      plist[[pindex]] <- p2
      pindex <- pindex + 1
    }
  }
  
  # Plot boxwhiskers
  if(boxwhisker){
    p3 <- ggplot(var_list_long, aes_string(x = dim, y = "value")) +
      geom_boxplot(na.rm = TRUE) +
      geom_jitter(shape = 16, position = position_jitter(0.2), alpha = 0.3, na.rm = TRUE) +
      stat_summary(fun = mean, geom = "point", shape = 10, size = 4, na.rm = TRUE) +
      ylab(var) +
      scale_colour_manual(breaks = c(av_fun, unique(var_list_long[, dim]), "Obs"),
                          guide = guide_legend(override.aes = list(
                            linetype = c(rep("solid", length(unique(var_list_long[, dim])) + 1),
                                         rep("blank", 1)),
                            shape = c(rep(NA, length(unique(var_list_long[, dim])) + 1),
                                      rep(16, 1))))) +
      theme_classic()
    
    if(!is.null(depth)){
      p3 <- p3 +
        ggtitle(paste0("Box-Whisker-Plot ", paste0("at depth = ", depth, " m")))
    }else if(!is.null(date)){
      p3 <- p3 +
        ggtitle(paste0("Box-Whisker-Plot ", paste0("at date = ", format(date))))
    }else{
      p3 <- p3 +
        ggtitle(paste0("Box-Whisker-Plot ", paste0("of ", var)))
    }
    
    plist[[pindex]] <- p3
    pindex <- pindex + 1
  }

  # Return plot if only one
  if(length(plist) == 1){
    plist <- plist[[1]]
  }
  
  return(plist)
}
