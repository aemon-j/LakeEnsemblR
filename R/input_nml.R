#' @title Input values into nml file
#' @description
#'Inputs values into nml file by locating the label and key within the nml file. Preserves comments (!) if present. NOTE: this does not use a nml parser so if there are nml formatting errors this function will not pick them up.
#' @param file filepath; to nml file which you wish to edit
#' @param label string; which corresponds to section where the key is located
#' @param key string; name of key in which to input the value
#' @param value string; to be input into the key/value pair. Note boolean values must be input as 'true'/'false' as per the nml format
#' @param out_file filepath; to write the output nml file (optional); defaults to overwriting file if not specified
#' @export
#' @author
#'Tadhg Moore
#' @examples
#'input_nml(file = 'samp.nml', label = "LAKE_PARAMS", key = "depth_w_lk", value = 14, out_file = NULL)

input_nml <- function (file, label, key, value, out_file = NULL){
  yml <- readLines(file)
  if (is.null(out_file)) {
    out_file = file
  }
  label_id <- paste0('&',label)
  ind_label <- grep(label_id, yml)
  if (length(ind_label) == 0) {
    stop(label, " not found in ", file)
  }
  key_id <- key # paste0('"', key, '"')
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
  if (length(spl1) == 2) {
    comment <- spl1[2]
  }
  spl2 <- strsplit(spl1[1], " = ")[[1]][2]
  sub = paste0(value,',' )
  yml[ind_map] <- gsub(pattern = spl2, replacement = sub, x = yml[ind_map])
  writeLines(yml, out_file)
  old_val <- gsub(" ", "", spl2, fixed = TRUE)
  message("Replaced ", label, " ", key, " ", 
          old_val, " with ", value)
}
