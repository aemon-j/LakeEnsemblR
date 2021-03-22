#' Set a value within the loaded yaml file
#'
#' Set the value within a yaml file.
#' @param yaml list; loaded using `read_yaml()`
#' @param value string; to be input into the the yaml file.
#'   Note boolean values must be input as "true"/"false" as per the json format
#' @param ... string key1, key2, etc.: multiple keys pointing toward the line
#'   that you want to edit in the yaml file. Keys must be listed consecutively,
#'   without skipping numbers.
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
#' yaml <- set_yaml(yaml, value = 23, key1 = "location", key2 = "latitude")
#' yaml <- set_yaml(yaml, value = "2010-06-01 00:00:00", key1 = "time", key2 = "start")
#' yaml <- set_yaml(yaml, value = "meteo.csv", key1 = "input", key2 = "meteo", key3 = "file")
#' yaml <- set_yaml(yaml, value = TRUE, key1 = "calibration", key2 = "GOTM", key3 = "turb_param/k_min", key4 = "log")
#' yaml <- set_yaml(yaml, value = c("temp", "salt"), key1 = "output", key2 = "variables")
#' 
#' write_yaml(yaml, "LakeEnsemblR.yaml")
#' }

set_yaml <- function(yaml, value, ...) {
  
  if(!is.list(yaml)) {
    stop("yaml is not in the correct format. Load the yaml file using 'LakeEnsemblR::read_yaml()'")
  }
  
  # Users can provide multiple keys, named key1, key2, key3, etc.
  all_args <- list(...)
  # all_keys <- all_args[grepl("key", names(all_args))]
  
  nams1 <- names(yaml)
  if(!(all_args[[1]] %in% nams1)) {
    stop(paste0(all_args[[1]], " is not found in the first level in the yaml object. Options include: '", paste0(nams1, collapse = "', '"), "'."))
  }
  
  if(length(all_args) == 1) {
    
    if(length(names(yaml[[all_args[[1]]]])) > 1) {
      stop(paste0("There are multiple keys on this level: '", paste0(names(yaml[[all_args[[1]]]]), collapse = "', '"), "'\n
                  You will need to add a key2 to your argument"))
    }
    
    # Check classes
    c1 <- class(yaml[[all_args[[1]]]])
    c2 <- class(value)
    if(c1 != c2 & c1 != "NULL") {
      stop(paste0(value, " (", c2, ") is not the same class as ", yaml[[all_args[[1]]]], " (", c1, ")."))
    }
    yaml[[all_args[[1]]]] <- value
  } else if(length(all_args) == 2) {
    
    if(length(names(yaml[[all_args[[1]]]][[all_args[[2]]]])) > 1) {
      stop(paste0("There are multiple keys on this level: '", paste0(names(yaml[[all_args[[1]]]][[all_args[[2]]]]), collapse = "', '"), "'\n
                  You will need to add a key3 to your argument"))
    }
    
    # Check if second key is under the first key
    nams1 <- names(yaml[[all_args[[1]]]])
    if(!(all_args[[2]] %in% nams1)) {
      stop(paste0("'", all_args[[2]], "' is not nested under '", all_args[[1]], "'. Please select one of '", paste0(nams1, collapse = "', '"), "'."))
    }
    # Check classes
    c1 <- class(yaml[[all_args[[1]]]][[all_args[[2]]]])
    c2 <- class(value)
    if(c1 != c2 & c1 != "NULL") {
      stop(paste0(value, " (", c2, ") is not the same class as ", yaml[[all_args[[1]]]][[all_args[[2]]]], " (", c1, ")."))
    }
    yaml[[all_args[[1]]]][[all_args[[2]]]] <- value
  } else if(length(all_args) == 3) {
    
    if(length(names(yaml[[all_args[[1]]]][[all_args[[2]]]][[all_args[[3]]]])) > 1) {
      stop(paste0("There are multiple keys on this level: '", paste0(names(yaml[[all_args[[1]]]][[all_args[[2]]]][[all_args[[3]]]]), collapse = "', '"), "'\n
                  You will need to add a key4 to your argument"))
    }
    
    # Check if second key is under the first key
    nams1 <- names(yaml[[all_args[[1]]]])
    if(!(all_args[[2]] %in% nams1)) {
      stop(paste0("'", all_args[[2]], "' is not nested under '", all_args[[1]], "'. Please select one of '", paste0(nams1, collapse = "', '"), "'."))
    }
    # Check if second key is under the first key
    nams2 <- names(yaml[[all_args[[1]]]][[all_args[[2]]]])
    
    if(!(all_args[[3]] %in% nams2)) {
      stop(paste0("'", all_args[[3]], "' is not nested under '", all_args[[2]], "'. Please select one of '", paste0(nams2, collapse = "', '"), "'."))
    } else {
      c1 <- class(yaml[[all_args[[1]]]][[all_args[[2]]]][[all_args[[3]]]])
      c2 <- class(value)
      if(c1 != c2 & c1 != "NULL") {
        stop(paste0(value, " (", c2, ") is not the same class as ", yaml[[all_args[[1]]]][[all_args[[2]]]][[all_args[[3]]]], " (", c1, ")."))
      }
      yaml[[all_args[[1]]]][[all_args[[2]]]][[all_args[[3]]]] <- value
    }
  } else if(length(all_args) == 4) {
    
    if(length(names(yaml[[all_args[[1]]]][[all_args[[2]]]][[all_args[[3]]]][[all_args[[4]]]])) > 1) {
      stop(paste0("There are multiple keys on this level: '", paste0(names(yaml[[all_args[[1]]]][[all_args[[2]]]][[all_args[[3]]]][[all_args[[4]]]]), collapse = "', '"), "'\n
                  You will need to add a key5 to your argument"))
    }
    
    # Check if second key is under the first key
    nams1 <- names(yaml[[all_args[[1]]]])
    if(!(all_args[[2]] %in% nams1)) {
      stop(paste0("'", all_args[[2]], "' is not nested under '", all_args[[1]], "'. Please select one of '", paste0(nams1, collapse = "', '"), "'."))
    }
    # Check if second key is under the first key
    nams2 <- names(yaml[[all_args[[1]]]][[all_args[[2]]]])
    
    if(!(all_args[[3]] %in% nams2)) {
      stop(paste0("'", all_args[[3]], "' is not nested under '", all_args[[2]], "'. Please select one of '", paste0(nams2, collapse = "', '"), "'."))
    }
    # Check if second key is under the first key
    nams3 <- names(yaml[[all_args[[1]]]][[all_args[[2]]]][[all_args[[3]]]])
    
    if(!(all_args[[4]] %in% nams3)) {
      stop(paste0("'", all_args[[4]], "' is not nested under '", all_args[[3]], "'. Please select one of '", paste0(nams3, collapse = "', '"), "'."))
    } else {
      c1 <- class(yaml[[all_args[[1]]]][[all_args[[2]]]][[all_args[[3]]]][[all_args[[4]]]])
      c2 <- class(value)
      if(c1 != c2 & c1 != "NULL") {
        stop(paste0(value, " (", c2, ") is not the same class as ", yaml[[all_args[[1]]]][[all_args[[2]]]][[all_args[[3]]]][[all_args[[4]]]], " (", c1, ")."))
      }
      yaml[[all_args[[1]]]][[all_args[[2]]]][[all_args[[3]]]][[all_args[[4]]]] <- value
    }
  }
  
  return(yaml)

}
