#' Function to plot residuals for each model of the ensemble run
#' 
#' Plot residual diagnostic plots. Residuals are calculated by (sim - obs)
#'  for each corresponding depth and time step there is an observed value.
#' 
#' @param ncdf filepath; to the netcdf file created by `run_ensemble()`
#' @param var string; of variable which to plot. Defaults to "watertemp"
#' @param dim character; NetCDF dimensions to extract. Must be either "member" or "model". Defaults to "model". Only used if plotting from netCDF file. Currently only works with "model".
#' @param dim_index numeric; Index of dimension chosen to extract from. Defaults to 1. Only used if plotting from netCDF file.
#' @param var_list list; of variables in the format when loaded using `load_var()`. Defaults to NULL 
#' @param model string vector; of models which should be included in the plot. If NULL all models in the netCDF/list are plotted. Defaults to NULL.
#' @return list with four ggplot objects: "obs_res" = Observations versus residuals,
#'  "res_depth" = Residuals versus depth,
#'  "yday_res" = residuals for day of year,
#'  "res_dist" = distribution of residuals
#' @author Tadhg Moore, Johannes Feldbauer
#' @importFrom rLakeAnalyzer get.offsets
#' @importFrom reshape2 melt
#' @import ggplot2
#' @import dplyr
#' @import RColorBrewer
#' @examples
#' \dontrun{
#' plist <- plot_resid(ncdf = "output/ensemble_output.nc",var = "watertemp",
#'                    model = c('FLake', 'GLM',  'GOTM', 'Simstrat', 'MyLake'))
#' plist[['obs_res']]+
#' theme_classic()
#' }
#' 
#'
#' @export
plot_resid <- function(ncdf = NULL, var =  "watertemp", dim = "model", dim_index = 1,
                       var_list = NULL, model = NULL) {
  
  # check if model input is correct
  model <- check_models(model)
  if(!is.null(ncdf)){
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
    var_list <- load_var(ncdf, var = var, dim = dim,
                         dim_index = dim_index, return = "list")
  }else{
    var_list <- var_list
  }
  
  # only the selected models
  if(is.null(model)){
    model <- names(var_list)
    model <- model[-c(length(model))] # Remove last name "Obs"
  }
  var_list <- var_list[c(model, "Obs")]
  
  data <- var_list %>%
    reshape2::melt(id.vars = "datetime") %>%
    dplyr::group_by(datetime)
  colnames(data) <- c("datetime", "Depth", "value", "Model")
  obs <- data %>% 
    dplyr::filter(Model == "Obs")
  colnames(obs) <- c("datetime", "Depth", "obs", "Observed")
  dat <- data %>% 
    dplyr::filter(Model != "Obs")
  colnames(dat)[3] <- "mod"
  
    # Check for observed values
  if(sum(is.na(obs$obs)) == nrow(obs)){
    stop("There are no observations in netCDF/list provided.
         Please inspect the model output and re-run 'run_ensemble() if necessary.'")
  }
  
  # remove NAs
  dat <- dat[!is.na(dat$mod), ] # Remove NAs

  if(nrow(dat) == 0) {
    stop("Modelled data is all NAs.
         Please inspect the model output and re-run 'run_ensemble()' if necessary.")
  }
  
  obs <- obs[!is.na(obs$obs), ] # Remove NAs
  if(nrow(obs) == 0) {
    stop("Observed data is all NAs.
         Please inspect the model output and re-run 'run_ensemble()' if necessary.")
  }
  
  # Colours
  spec <- RColorBrewer::brewer.pal(11, "Spectral")
  cols <- RColorBrewer::brewer.pal(8, "Set2")
  dep_cols <- RColorBrewer::brewer.pal(3, "Paired")
  

  df <- merge(dat, obs, by = 1:2)
  df$depth <- -as.numeric(gsub("wtr_", "", df$Depth))
  df$fdepth <- factor(df$depth)
  df$res <- df$mod - df$obs
  df$yday <- lubridate::yday(df$datetime)
  
  pred <- seq(min(df$res, na.rm = TRUE), max(df$res, na.rm = TRUE), length.out = 100)
  
  
  # Calculate densities for p5
  dens <- lapply(model, FUN = function(x){
    idx <- which(df$Model == x)
    mean <- mean(df$res[idx], na.rm = TRUE)
    sd <- sd(df$res[idx], na.rm = TRUE)
    data.frame(pred,
               density = dnorm(pred, mean, sd),
               Model = x)
  })
  dens <- do.call("rbind", dens)
  
  
  # # Obs v Mod
  # p1 <- ggplot(df, aes(mod, obs, colour = depth))+
  #   # geom_abline(slope = 1, intercept = 0, linetype = "dashed")+
  #   # geom_hex()+
  #   geom_point(alpha = 0.1)+
  #   # scale_colour_viridis_c()+
  #   facet_wrap(~Model)+
  #   coord_fixed(xlim = rnge, ylim = rnge)
  
  # Obs v Res
  p2 <- ggplot(df, aes(obs, res, colour = depth)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_colour_gradientn(colours = c("black", dep_cols[2], dep_cols[1])) +
    geom_point(alpha = 0.1) +
    coord_equal() +
    ggtitle("Observed vs. Residuals")+
    facet_wrap(~Model)
  p2
  # Res v Depth
  p3 <- ggplot(df, aes(res, depth, colour = obs)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey") +
    geom_point(alpha = 0.1) +
    scale_colour_gradientn(colours = rev(spec)) +
    labs(colour = "temp") +
    ggtitle('Residuals vs. Depth') +
    facet_wrap(~Model)
  
  # Time v Res
  p4 <- ggplot(df, aes(yday, res, colour = depth)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point(alpha = 0.1) +
    scale_colour_gradientn(colours = c("black", dep_cols[2], dep_cols[1])) +
    ggtitle("Year day vs. Residuals") +
    facet_wrap(~Model)
  

  p5 <- ggplot(df) +
    geom_histogram(aes(x = res, y = ..density..),
                   fill = cols[8], binwidth = 0.5) +
    geom_line(data = dens, aes(x = pred, y = density), colour = cols[2], size = 1) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_x_continuous("Residuals (\u00B0C)") +
    ggtitle("Density distribution of Residuals") +
    facet_wrap(~Model)
  
  plist <- list("obs_res" = p2, "res_depth" = p3,
                "yday_res" = p4, "res_dist" = p5)
  

  
  return(plist)
}
