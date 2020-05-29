#' Plot heat map of ensemble model output
#' 
#' Plot a heat map of ensemble output data. It can either plot directly from the netCDF file or a list in the format when loaded in with `load_var()`.
#' 
#' @param ncdf Path to the netCDF file created by `run_ensemble()`
#' @param var Variable which to plot. Defaults to "watertemp"
#' @param var_list list of variables in the format when loaded using `load_var()`. Defaults to NULL 
#' @param model Vector of models which should be included in the plot
#' @param dim character; NetCDF dimensions to extract. Must be either "member" or "model". Defaults to "model". Only used if plotting from netCDF file. Currently only works with "model".
#' @param dim_index numeric; Index of dimension chosen to extract from. Defaults to 1. Only used if plotting from netCDF file.
#' @return ggplot object of heatmaps
#' @author Tadhg Moore, Johannes Feldbauer
#' @importFrom reshape2 melt
#' @import ggplot2
#' @import dplyr
#' @import RColorBrewer
#' @examples
#' \dontrun{
#' ncdf <- 'output/ensemble_output.nc'
#' # Plot ensemble mean at 0.9m
#' plot_ensemble(ncdf = ncdf, model = model, var = 'watertemp', depth = 0.9)
#' }
#' @export
plot_heatmap <- function(ncdf = NULL, var = "watertemp", dim = "model", dim_index = 1,
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
    var_list <- load_var(ncdf, var = var, return = "list",
                         dim = dim, dim_index = dim_index)
  }else{
    var_list <- var_list
  }
  
  # only the selected models
  if(!is.null(model)){
    var_list <- var_list[c(model, "Obs")]
  }
  
  mod_names <- names(var_list)
  
  # Melt list down into long dataframe
  data <- var_list %>%
    reshape2::melt(id.vars = "datetime") %>%
    dplyr::group_by(datetime)
  colnames(data) <- c("datetime", "Depth", "value", "Model")
  data$depth <- -as.numeric(gsub("wtr_", "", data$Depth))
  data <- as.data.frame(data)
  data$Model <- factor(data$Model)
  data$Model <- factor(data$Model, levels = mod_names)
  

  
  spec <- RColorBrewer::brewer.pal(11, "Spectral")
  
  # Remove NAs
  data <- data[!is.na(data$value), ] # Remove NAs
  if(nrow(data) == 0) {
    stop("Modelled  and observed data is all NAs.
         Please inspect the model output and re-run 'run_ensemble()' if necessary.")
  }
  
  
  
  p1 <- ggplot(data) +
    geom_point(aes(datetime, depth, colour = value), shape = 15) +
    scale_colour_gradientn(colours = rev(spec)) +
    facet_wrap(~Model, ncol = 2)
    
  return(p1)
}
