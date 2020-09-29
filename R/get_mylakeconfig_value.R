#' Extracts values from the MyLake config file
#'
#' Extracts values from the MyLake config file by locating the label and key within the file.
#' @param file filepath; to R object (loaded Rdata file)
#' @param label string; which corresponds to section where the key is located
#' @param key string; name of key in which to extract the value
#' @export

get_mylakeconfig_value <- function(file, label, key){
  
  load(file)
  # filename is hard-coded: mylake_config
  
  if(is.null(label)){
    # Check if label occurs in the list
    if(is.null(mylake_config[[key]])){
      stop(key, " not found in mylake_config")
    }
    return(mylake_config[[key]])
    
  }else{
    label_names <- paste0(label, ".names")
    
    # Check if label occurs in the list
    if(is.null(mylake_config[[label_names]])){
      stop(label_names, " not found in mylake_config")
    }
    # Check if the key occurs in the label
    if(!(key %in% unlist(mylake_config[[label_names]]))){
      stop(key, " not found in ", label_names, " in mylake_config")
    }
    
    ind_key <- which(unlist(mylake_config[[label_names]]) == key)
    
    return(mylake_config[[label]][ind_key])
  }
}
