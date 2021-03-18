#' Read a YAML file
#'
#' Read a YAML document from a file and create an R object from it. This is from the `yaml` package and is built to replicate the functionality used in the `glmtools`
#' package.
#' @param file filepath; to yaml file which you wish to edit
#' @param fileEncoding character string: if non-empty declares the encoding used on a file (not a connection) so the character data can be re-encoded. See file.
#' @param text character string: if file is not supplied and this is, then data are read from the value of text via a text connection. Notice that a literal string can be used to include (small) data sets within R code.
#' @param error.label a label to prepend to error messages (see Details).
#' @param ... arguments to pass to yaml.load
#' @return A list with the yaml file
#' @export
#' @author
#' Jeremy Stephens <jeremy.f.stephens@vumc.org>
#' @importFrom yaml read_yaml
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

read_yaml <- function(file, fileEncoding = "UTF-8", text, error.label, ...) {
  yaml::read_yaml(file, fileEncoding = fileEncoding, text = text, error.label = error.label, ...)
}
