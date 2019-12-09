#' @title Extracts values from a json file
#' @description
#'Extracts values from a json file by locating the label and key within the json file.
#' @param file filepath; to json file
#' @param label string; which corresponds to section where the key is located
#' @param key string; name of key in which to extract the value
#' @export
#' @author
#'Tadhg Moore, Jorrit Mesman
#' @examples
#'input_json(file = 'samp.par', label = 'ModelParameters', key = 'f_wind', value = 1.2, out_file = NULL)

get_json_value <- function (file, label, key) 
{
  yml <- readLines(file)
  if (is.null(out_file)) {
    out_file = file
  }
  label_id <- paste0('"',label, '"')
  ind_label <- grep(label_id, yml)
  if (length(ind_label) == 0) {
    stop(label, " not found in ", file)
  }
  key_id <- paste0('"', key, '"')
  ind_key = grep(key_id, yml)
  if (length(ind_key) == 0) {
    stop(key, " not found in ", label, " in ", 
         file)
  }
  ind_key = ind_key[ind_key > ind_label]
  ind_map <- ind_key[which.min(ind_key - ind_label)]
  if (length(ind_map) == 0) {
    stop(key, " not found in ", label, " in ", 
         file)
  }
  spl1 <- strsplit(yml[ind_map], c("!"))[[1]]
  spl2 <- strsplit(spl1[1], ": ")[[1]][2]
  val <- gsub(" ", "", spl2, fixed = TRUE)
  val <- gsub(",", "", val, fixed = TRUE)
  
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
