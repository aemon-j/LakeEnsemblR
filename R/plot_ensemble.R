#' Function to plot results of the ensemble run
#' 
#' Plot the outcome of the ensemble run for a given depth along with the minimum, maximum, and
#' an average value.
#' 
#' @param ncdf Path to the netcdf file created by `run_ensemble()`
#' @param model Vector of models which should be included in the plot
#' @param var Variable which to plot
#' @param dim character; NetCDF dimensions to extract. Must be either "member" or "model". Defaults to "model". Only used if plotting from netCDF file. Currently only works with "model".
#' @param dim_index numeric; Index of dimension chosen to extract from. Defaults to 1. Only used if plotting from netCDF file.
#' @param depth If `var` has a depth dimension, for which depth should it be plotted?
#' @param date Specific date for which depth profiles should be plotted
#' @param av_fun Averaging function to use, defaults to the arithmetic mean (`mean()`)
#' @author Johannes Feldbauer, Robert Ladwig
#' @importFrom rLakeAnalyzer get.offsets
#' @importFrom reshape2 melt
#' @importFrom RColorBrewer brewer.pal
#' @examples
#' \dontrun{
#' # time series
#' p1 <- plot_ensemble(ncdf = "output/ensemble_output.nc",
#'                    model = c('FLake', 'GLM',  'GOTM', 'Simstrat', 'MyLake'),
#'                    var = "watertemp", depth = 0.9, boxwhisker = TRUE)
#'
#'# depth profiles
#' p2 <- plot_ensemble(ncdf = "output/ensemble_output.nc",
#'                    model = c('FLake', 'GLM',  'GOTM', 'Simstrat', 'MyLake'),
#'                    var = "watertemp", date = as.POSIXct("2010-06-13", tz = "UTC"),
#'                    boxwhisker = TRUE)
#'                    
#' }
#'
#' @export
plot_ensemble <- function(ncdf, model = c('FLake', 'GLM',  'GOTM', 'Simstrat', 'MyLake'),
                          var, dim = "model", dim_index = 1,
                          depth = NULL, date = NULL, av_fun = "mean", boxwhisker = FALSE,
                          residuals = FALSE) {
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
  # if members are selecte still get the observations
  if(dim == "member") {
    obs_list <- load_var(ncdf, var = var, return = "list", dim = "model",
                            dim_index = 1, print = FALSE)
    obs_list <- list(Obs = obs_list[["Obs"]])
  }
  # check if selected models are in the ncdf file
  if(any(!(model %in% names(var_list)))) {
    stop(paste0("Model ", model[!(model %in% names(var_list))],
                " not found in the ncdf file ", ncdf))
  }
  
  # only the selected models
  var_list <- var_list[c(model, "Obs")]
  
  # output list
  plist <- list()
  pindex <- 1
  
  # colors for plotting
  colfunc <- colorRampPalette(RColorBrewer::brewer.pal(length(model), 'Set2'))

##-------------- plot timeseries -------------------------------------------------------------------
  # if no date is selected plot time series
  if(!is.null(depth)) {
    if(var == "watertemp" & is.null(depth)) {
      stop(paste0("When plotting water temperature depth must be specified"))
    }
    # get depths
    deps <- rLakeAnalyzer::get.offsets(var_list[[1]])
    # check if the chosen depth is available
    if(length(depth) > 0) {
      
      if(!(depth %in% deps)) {
        stop(paste0("The selected depth is not in the output. Available depths are:\n",
                    paste0(deps, collapse = ", "), "\n"))
      } 
      
      data <- var_list %>%
        reshape2::melt( id.vars = "datetime") %>%
        dplyr::filter(variable == paste0("wtr_", depth)) %>%
        dplyr::group_by(datetime)
      colnames(data) <- c("datetime", "Depth", "value", dim)
      if(dim == "member") {
        obs <- obs_list %>%
          reshape2::melt( id.vars = "datetime") %>%
          dplyr::filter(variable == paste0("wtr_", depth)) %>%
          dplyr::group_by(datetime)
        colnames(obs) <- c("datetime", "Depth", "value", "Observed")
      } else {
        obs <- data %>% 
          dplyr::filter(model == "Obs")
        colnames(obs) <- c("datetime", "Depth", "value", "Observed")
      }
      
      # remove NAs
      #data <- data[!is.na(data$value), ] # Remove NAs
      
      if(nrow(data) == 0) {
        stop("Modelled data is all NAs at ", depth, " m.
         Please inspect the model output and re-run 'run_ensemble()' if necessary.")
      }
      
      #obs <- obs[!is.na(obs$value), ] # Remove NAs
      
      dat <- data %>% 
        dplyr::filter(sym(dim) != "Obs")
      dat_av <- dat %>% 
        dplyr::filter(sym(dim) != "Obs") %>%
        dplyr::summarize(mean = get(av_fun)(value, na.rm = TRUE),
                  max = max(value, na.rm = TRUE),
                  min = min(value, na.rm = TRUE)) 
      dat_av$Type = av_fun
     dat <- data.frame(dat)
      p1 <- ggplot() +  
        geom_ribbon(data = dat_av, aes(datetime, ymin=min, ymax=max),
                    alpha=0.2) + 
        geom_line(data = dat, aes_string(x = "datetime", y = "value", col = dim),
                  lwd = ifelse(dim == "member", 0.75, 1),
                  alpha = ifelse(dim == "member", 0.75, 1)) +
        geom_line(data = dat_av, aes(datetime, mean, col = Type), lwd = 1.33) +
        geom_point(data = obs, aes(x = datetime, y = value, col = Observed), size = 1) +
        ylab(var) +
        xlab("") +
        ggtitle(paste0("Time Series ",paste0("at depth = ", depth, " m"))) +
        scale_colour_manual(values = c("grey42", colfunc(length(unique(dat[, dim]))), "black"),
                            breaks= c(av_fun, unique(dat[, dim]), "Obs"),
                            guide = guide_legend(override.aes = list(
                            linetype = c(rep("solid", length(unique(dat[, dim])) + 1),
                                         rep("blank", 1)),
                            shape = c(rep(NA, length(unique(dat[, dim])) + 1), rep(16, 1))))) +
        theme(text = element_text(size=10),
              axis.text.x = element_text(angle=0, hjust= 0.5),
              legend.margin=margin(0,0,0,0),
              legend.box.margin=margin(0,0,0,0),
              legend.position="bottom",legend.title=element_blank()) 
      plist[[pindex]] <- p1
      pindex <- pindex + 1
      
      if (residuals){
        if (sum(!is.na(obs$value)) == 0){
          warning(paste0("Residuals can not be calculated because observed data is missing."))
        }
        # if observations are available plot residuals
        if(sum(!is.na(obs$value)) > 0 ) {
          dat_res <- dat
          dat_res$value <- dat$value - obs$value
          dat_res <- dat_res[!is.na(dat_res$value), ]
          dat_resav <- dat_av 
          dat_resav$mean <- dat_av$mean - obs$value
          dat_resav$max <- dat_av$max - obs$value
          dat_resav$min <- dat_av$min - obs$value
          dat_resav <- dat_resav[!is.na(dat_resav$mean), ]
          
          p2 <- ggplot() +  
            geom_ribbon(data = dat_resav, aes(datetime, ymin=min, ymax=max),
                        alpha=0.2) + 
            geom_line(data = dat_res, aes_string(x = "datetime", y = "value", col = dim)) +
            geom_line(data = dat_resav, aes(datetime, mean, col = Type), lwd = 1.33) +
            ylab(var) +
            xlab("") +
            ggtitle(paste0("Residuals ",paste0("at depth = ", depth, " m"))) +
            scale_colour_manual(values = c("grey42", colfunc(length(unique(dat[, dim]))), "black"),
                                breaks= c(av_fun, unique(dat_res[, dim]), "Obs"),
                                guide = guide_legend(override.aes = list(
                                  linetype = c(rep("solid", length(unique(dat_res[, dim])) + 1)),
                                  shape = c(rep(NA, length(unique(dat_res[, dim])) + 1))))) +
      
            theme(text = element_text(size=10),
                  axis.text.x = element_text(angle=0, hjust= 0.5),
                  legend.margin=margin(0,0,0,0),
                  legend.box.margin=margin(0,0,0,0),
                  legend.position="bottom",legend.title=element_blank()) 
          plist[[pindex]] <- p2
          pindex <- pindex + 1
        }
      }
      
      if (boxwhisker){
        dat <- var_list %>% 
          melt( id.vars = "datetime") %>% 
          dplyr::filter(variable == paste0("wtr_", depth)) %>%
          dplyr::group_by(datetime)
        colnames(dat) <- c("datetime", "Depth", "value", dim)
        if(dim == "model") {
          dat[, dim] <- factor(dat[, dim])
          dat[, dim] <- factor(dat[, dim], levels=c("Obs", levels(dat[, dim])
                                                          [-c(which(levels(dat[, dim]) == "Obs"))]))
        } else {
          dat <- rbind(dat, set_colnames(obs, c("datetime", "Depth", "value", dim)))
          dat <- data.frame(dat)
          dat$member <- as.factor(dat$member)
          }
        p3 <- ggplot(dat, aes_string(x = dim, y = "value")) +
          geom_boxplot() +
          geom_jitter(shape=16, position=position_jitter(0.2), alpha = 0.3) +
          stat_summary(fun = mean, geom = "point", shape = 10, size = 4) +
          ylab(var) +
          ggtitle(paste0("Box-Whisker-Plot ",paste0("at depth = ", depth, " m"))) +
          scale_colour_manual(breaks= c(av_fun, unique(dat[, dim]), "Obs"),
                              guide = guide_legend(override.aes = list(
                                linetype = c(rep("solid", length(unique(dat[, dim])) + 1),
                                             rep("blank", 1)),
                                shape = c(rep(NA, length(unique(dat[, dim])) + 1), rep(16, 1))))) +
          theme_classic()
        
        # if (residuals){
        #   plist[[3]] <- p3
        # } else {
        #   plist[[2]] <- p3
        # }
        plist[[pindex]] <- p3
        pindex <- pindex + 1
      }
  
    } 
##------------------- plot timeseries of non depth depending variable (ice) ------------------------
  } else if(is.null(date)) {
    
    
    dat <- var_list %>%
      reshape2::melt( id.vars = "time") %>%
      dplyr::group_by(time)
    obs <- dat %>% dplyr::filter(L1 == "Obs")
    dat <- dat %>% dplyr::filter(L1 != "Obs")
    dat_av <- dat %>% dplyr::filter(L1 != "Obs") %>%
      summarize(mean = get(av_fun)(value, na.rm = TRUE),
                max = max(value, na.rm = TRUE),
                min = min(value, na.rm = TRUE)) 
    dat_av$Type = av_fun
    
    
    p1 <- ggplot() +  
      geom_ribbon(data = dat_av, aes(time, ymin=min, ymax=max),
                  alpha=0.2) + 
      geom_line(data = dat, aes(x = time, y = value, col = L1)) +
      geom_line(data = dat_av, aes(time, mean, col = Type), lwd = 1.33) +
      geom_point(data = obs, aes(x = time, y = value, col = L1), size = 1) +
      ylab(var) +
      xlab("") +
      ggtitle(paste0("Time Series of ",paste0(var))) +
      scale_colour_manual(values = c("grey42", colfunc(length(unique(dat$L1))), "black"),
                          breaks= c(av_fun, unique(dat$L1), "Obs"),
                          guide = guide_legend(override.aes = list(
                            linetype = c(rep("solid", length(unique(dat$L1)) + 1),
                                         rep("blank", 1)),
                            shape = c(rep(NA, length(unique(dat$L1)) + 1), rep(16, 1))))) +
      theme(text = element_text(size=10),
            axis.text.x = element_text(angle=0, hjust= 0.5),
            legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(0,0,0,0),
            legend.position="bottom",legend.title=element_blank()) 
    
    # plist[[1]] <- p1
    plist[[pindex]] <- p1
    pindex <- pindex + 1
  }
##--------------------- plot depth profile at a certain date ---------------------------------------
  if(!is.null(date)) {
    
    # if a specific date is selected plot a depth profile
    dat <- var_list %>% reshape2::melt( id.vars = "datetime") %>% 
      dplyr::filter(datetime == date) %>%
      dplyr::mutate(variable = -as.numeric(gsub("wtr_", "", variable)))
    colnames(dat) <- c("datetime", "Depth", "value", dim)
    if(dim == "member") {
      obs <- obs_list %>% reshape2::melt( id.vars = "datetime") %>% 
        dplyr::filter(datetime == date) %>%
        dplyr::mutate(variable = -as.numeric(gsub("wtr_", "", variable)))
      colnames(obs) <- c("datetime", "Depth", "value", "Observed")
    } else {
      obs <- dat %>% dplyr::filter(Model == "Obs")
      colnames(obs) <- c("datetime", "Depth", "value", "Observed")
      dat <- dat %>% dplyr::filter(Model != "Obs")
    }
    
    dat_av <- dat %>% dplyr::filter(sym(dim) != "Obs") %>%
      dplyr::group_by(Depth) %>%
      dplyr::summarize(mean = get(av_fun)(value, na.rm = TRUE),
                       max = max(value, na.rm = TRUE),
                       min = min(value, na.rm = TRUE))
    dat_av <- dat_av[order(dat_av$Depth, decreasing = TRUE), ]
    dat_av$Type = av_fun
    
    p1 <- ggplot() +
      geom_ribbon(data = dat_av, aes(ymin=min, ymax=max, x = Depth),
                  alpha=0.2) + geom_line(data = dat,
                                         aes_string(y = "value", x = "Depth", col = dim)) +
      geom_line(data = dat_av, aes(y = mean, x = Depth, col = Type), lwd = 1.33) +
      geom_point(data = obs, aes(y = value, x = Depth, col = Observed), size = 1) +
      xlab(var) + coord_flip() +
      ggtitle(paste0("Depth profile ", paste0("at date ", format(date)))) +
      scale_colour_manual(values = c("grey42", colfunc(length(unique(dat[, dim]))), "black"),
                          breaks= c(av_fun, unique(dat[, dim]), "Obs"),
                          guide = guide_legend(override.aes = list(
                            linetype = c(rep("solid", length(unique(dat[, dim])) + 1),
                                         rep("blank", 1)),
                            shape = c(rep(NA, length(unique(dat[, dim])) + 1), rep(16, 1))))) +
      theme(text = element_text(size=10),
            axis.text.x = element_text(angle=0, hjust= 0.5),
            legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(0,0,0,0),
            legend.position="bottom",legend.title=element_blank()) 
    
    plist[[pindex]] <- p1
    pindex <- pindex + 1
    
    
    if (residuals){
      if (sum(!is.na(obs$value)) == 0){
        warning(paste0("Residuals can not be calculated because observed data is missing."))
      }
    # if observations are available plot residuals
      if(sum(!is.na(obs$value)) > 0 ) {
        dat_res <- dat
        dat_res$value <- dat$value - obs$value
        dat_res <- na.exclude(dat_res)
        dat_resav <- dat_av 
        dat_resav$mean <- dat_av$mean - obs$value
        dat_resav$max <- dat_av$max - obs$value
        dat_resav$min <- dat_av$min - obs$value
        dat_resav <- na.exclude(dat_resav)
        
        p2 <- ggplot() +  
          geom_ribbon(data = na.exclude(dat_resav), aes(ymin=min, ymax=max, x = Depth),alpha=0.2) + 
          geom_point(data = dat_resav, aes(Depth, mean, col = Type), lwd = 1.33) +
          geom_line(data = na.exclude(dat_resav), aes(Depth, mean, col = Type),
                    lwd = 1.33) +
          geom_point(data = dat_res, aes_string(x = "Depth", y = "value", col = dim)) + 
          geom_line(data = na.exclude(dat_res), aes_string(x = "Depth", y = "value", col = dim)) +
          coord_flip() + ylab(var) + xlab("") +
          ggtitle(paste0("Residuals over depth ", paste0("at date ", format(date)))) +
          scale_colour_manual(values = c("grey42", colfunc(length(unique(dat[, dim]))), "black"),
                              breaks= c(av_fun, unique(dat_res[, dim]), "Obs"),
                              guide = guide_legend(override.aes = list(
                                linetype = c(rep("solid", length(unique(dat_res[, dim])) + 1)),
                                shape = c(rep(NA, length(unique(dat_res[, dim])) + 1))))) +
          
          theme(text = element_text(size=10),
                axis.text.x = element_text(angle=0, hjust= 0.5),
                legend.margin=margin(0,0,0,0),
                legend.box.margin=margin(0,0,0,0),
                legend.position="bottom",legend.title=element_blank()) 
        
        plist[[pindex]] <- p2
        pindex <- pindex + 1
      }
    }
    if (boxwhisker){
      
      dat <- var_list %>% reshape2::melt( id.vars = "datetime") %>% 
        dplyr::filter(datetime == date) %>%
        dplyr::mutate(variable = -as.numeric(gsub("wtr_", "", variable)))
      colnames(dat) <- c("datetime", "Depth", "value", dim)
      
      if(dim == "model") {
        dat$Model <- factor(dat$Model)
        dat$Model <- factor(dat$Model, levels=c("Obs", levels(dat$Model)
                                                [-c(which(levels(dat$Model) == "Obs"))]))
      } else {
        dat <- rbind(dat, set_colnames(obs, c("datetime", "Depth", "value", dim)))
        dat <- data.frame(dat)
        dat$member <- as.factor(dat$member)
      }
      
      p3 <- ggplot(dat, aes_string(x = dim, y = "value")) +
        geom_boxplot() +
        geom_jitter(shape=16, position=position_jitter(0.2), alpha = 0.3) +
        stat_summary(fun.y = mean, geom = "point", shape = 10, size = 4) +
        ylab(var) +
        ggtitle(paste0("Box-Whisker-Plot for depth profile ", paste0("at date ", format(date)))) +
        scale_colour_manual(breaks= c(av_fun, unique(dat[, dim]), "Obs"),
                            guide = guide_legend(override.aes = list(
                              linetype = c(rep("solid", length(unique(dat[, dim])) + 1),
                                           rep("blank", 1)),
                              shape = c(rep(NA, length(unique(dat[, dim])) + 1), rep(16, 1))))) +
        theme_classic()

      plist[[pindex]] <- p3
      pindex <- pindex + 1
    }
    
  }

  return(plist)
}
