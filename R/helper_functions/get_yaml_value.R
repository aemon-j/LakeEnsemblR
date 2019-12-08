#' @title Extract values frpm yaml file
#' @description
#'Inputs values into yaml file by locating the label and key within the yaml file. Preserves comments (#) if present. NOTE: this does not use a yaml parser so if there are yaml formatting errors this function will not pick them up.
#' @param file filepath; to .yaml which you wish to edit
#' @param label string; which corresponds to section where the key is located
#' @param key string; name of key in which to input the value
#' @param value string; to be input into the key/value pair. Note boolean values must be input as 'true'/'false' as per the yaml format
#' @param out_file filepath; to write the output yaml file (optional); defaults to overwriting file if not specified
#' @export
#' @author
#'Tadhg Moore
#' @examples
#'get_yaml_value(file = 'gotm.yaml', label = 'airp', key = 'file'')

get_yaml_value <- function(file = 'gotm.yaml', label, key){
  yml <- readLines(file)

  #Find index of label
  label_id <- paste0(label,':')
  ind_label <- grep(label_id, yml)

  if(length(ind_label) == 0){
    stop(label, ' not found in ', file)
  }

  #Find index of key to replace
  key_id <- paste0(' ',key, ':')
  ind_key = grep(key_id, yml)
  if(length(ind_key) == 0){
    stop(key, ' not found in ', label, ' in ', file)
  }
  ind_key = ind_key[ind_key > ind_label]
  ind_map <- ind_key[which.min(ind_key - ind_label)]
  if(length(ind_map) == 0){
    stop(key, ' not found in ', label, ' in ', file)
  }

  #Split to extract comment
  spl1 <- strsplit(yml[ind_map], c('#'))[[1]]
  spl2 <- strsplit(spl1[1], ': ')[[1]][2]
  val <- gsub(" ", "", spl2, fixed = TRUE)

  val2 <- NULL

  if(val == 'false'){
    val2 = FALSE
  }
  if(val == 'true'){
    val2 = TRUE
  }
  flg <- !is.na(as.numeric(val))
  if(flg){
    val2 = as.numeric(val)
  }
  if(is.null(val2)){
    val2 <- gsub('\"',"", val)
    val2 <- as.character(val2)
  }
  return(val2)
}
