#' Write a yaml object to file
#'
#' Write the YAML representation of an R object (list) to a file. Taken from the `yaml` package but added catch to replace 'yes' with 'true' and 'no' with 'false'
#' @param yaml list; loaded using `read_yaml()`
#' @param file filepath; to yaml file which you wish to edit
#'   Note boolean values must be input as "true"/"false" as per the json format
#' @export
#' @importFrom yaml write_yaml
#' @author
#' Jeremy Stephens <jeremy.f.stephens@vumc.org>, Tadhg Moore
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
 
write_yaml <- function(x, file, fileEncoding = "UTF-8", ...) {
  yaml::write_yaml(x, file, fileEncoding, ...)
  lins <- readLines(file)
  lins <- gsub("\\byes", "true", lins)
  lins <- gsub("\\bno", "false", lins)
  writeLines(lins, file)
}