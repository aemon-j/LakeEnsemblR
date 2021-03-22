#' Get a value from the loaded yaml file
#'
#' Get a value from a loaded yaml file.
#' @param yaml list; loaded using `read_yaml()`
#' @param ... character string with the keys from the yaml file. They need to be in consecutive order
#' @export
#' @return list with updated yaml parameters
#' @author
#' Tadhg Moore
#' @examples
#'
#' \dontrun{
#' config_file <- system.file("extdata/feeagh/LakeEnsemblR.yaml", package = "LakeEnsemblR")
#' 
#' yaml <- read_yaml(config_file)
#' 
#' lat <- get_yaml_value(yaml, "location", "latitude")
#' kmin_init <- get_yaml_value(yaml, "calibration", "GOTM", "turb_param/k_min", "initial")
#' }

get_yaml_value <- function(yaml, ...) {
  
  if(!is.list(yaml)) {
    stop("yaml is not in the correct format. Load the yaml file using 'LakeEnsemblR::read_yaml()'")
  }
  
  all_args <- list(...)
  
  nams1 <- names(yaml)
  if(!(all_args[[1]] %in% nams1)) {
    stop(paste0(all_args[[1]], " is not found in the first level in the yaml object. Options include: '", paste0(nams1, collapse = "', '"), "'."))
  }
  
  
  if(length(all_args) == 1) {
    if(length(names(yaml[[all_args[[1]]]])) > 1) {
      stop(paste0("There are multiple keys on this level: '", paste0(names(yaml[[all_args[[1]]]]), collapse = "', '"), "'\n
                  You will need to add another argument"))
    }
    value <- yaml[[all_args[[1]]]]
  } else if(length(all_args) == 2) {
    nams2 <- names(yaml[[all_args[[1]]]])
    
    if(!(all_args[[2]] %in% nams2)) {
      stop(paste0(all_args[[2]], " is not found in the second level in the yaml object. Options include: '", paste0(nams2, collapse = "', '"), "'."))
    }
    if(length(names(yaml[[all_args[[1]]]][[all_args[[2]]]])) > 1) {
      stop(paste0("There are multiple keys on this level: '", paste0(names(yaml[[all_args[[1]]]][[all_args[[2]]]]), collapse = "', '"), "'\n
                  You will need to add another argument"))
    }
    value <- yaml[[all_args[[1]]]][[all_args[[2]]]]
  } else if(length(all_args) == 3) {
    nams2 <- names(yaml[[all_args[[1]]]])
    
    if(!(all_args[[2]] %in% nams2)) {
      stop(paste0(all_args[[2]], " is not found in the second level in the yaml object. Options include: '", paste0(nams2, collapse = "', '"), "'."))
    }
    
    nams3 <- names(yaml[[all_args[[1]]]][[all_args[[2]]]])
    
    if(!(all_args[[3]] %in% nams3)) {
      stop(paste0(all_args[[3]], " is not found in the third level in the yaml object. Options include: '", paste0(nams3, collapse = "', '"), "'."))
    }
    if(length(names(yaml[[all_args[[1]]]][[all_args[[2]]]][[all_args[[3]]]])) > 1) {
      stop(paste0("There are multiple keys on this level: '", paste0(names(yaml[[all_args[[1]]]][[all_args[[2]]]][[all_args[[3]]]]), collapse = "', '"), "'\n
                  You will need to add another argument"))
    }
    
    
    value <- yaml[[all_args[[1]]]][[all_args[[2]]]][[all_args[[3]]]]
  } else if(length(all_args) == 4) {
    nams2 <- names(yaml[[all_args[[1]]]])
    
    if(!(all_args[[2]] %in% nams2)) {
      stop(paste0(all_args[[2]], " is not found in the first level in the yaml object. Options include: '", paste0(nams2, collapse = "', '"), "'."))
    }
    
    nams3 <- names(yaml[[all_args[[1]]]][[all_args[[2]]]])
    
    if(!(all_args[[3]] %in% nams3)) {
      stop(paste0(all_args[[3]], " is not found in the second level in the yaml object. Options include: '", paste0(nams3, collapse = "', '"), "'."))
    }
    
    nams4 <- names(yaml[[all_args[[1]]]][[all_args[[2]]]][[all_args[[3]]]])
    
    if(!(all_args[[4]] %in% nams4)) {
      stop(paste0(all_args[[3]], " is not found in the third level in the yaml object. Options include: '", paste0(nams4, collapse = "', '"), "'."))
    }
    if(length(names(yaml[[all_args[[1]]]][[all_args[[2]]]][[all_args[[3]]]][[all_args[[4]]]])) > 1) {
      stop(paste0("There are multiple keys on this level: '", paste0(names(yaml[[all_args[[1]]]][[all_args[[2]]]][[all_args[[3]]]][[all_args[[4]]]]), collapse = "', '"), "'\n
                  You will need to add another argument"))
    }
    value <- yaml[[all_args[[1]]]][[all_args[[2]]]][[all_args[[3]]]][[all_args[[4]]]]
  }
  return(value)
}