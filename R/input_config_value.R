#' Inputs value into any LER model config file
#'
#' Wrapper function for functions that input values into model config files
#' @param model string; name of a model in the LER package
#' @param file filepath; to model-specific config file
#' @param label string; which corresponds to section where the key is located
#' @param key string; name of key in which to extract the value
#' @param value string; name of value to input into config file
#' @param out_file filepath; to write the output json file (optional);
#'   defaults to overwriting file if not specified
#' 
#' @importFrom gotmtools input_yaml input_nml
#' 
#' @examples
#' \dontrun{
#' input_config_value(model = "GOTM", file = "gotm.yaml",
#'   label = "turb_param", key = "k_min", value = 0.00001)
#' }
#' 
#' @export

input_config_value <- function(model, file, label, key, value, out_file = NULL){
  if(model == "FLake" | model == "GLM"){
    return(gotmtools::input_nml(file = file, label = label, key = key,
                                value = value, out_file = out_file))
  }else if(model == "GOTM"){
    return(gotmtools::input_yaml(file = file, label = label, key = key,
                                 value = value, out_file = out_file))
  }else if(model == "Simstrat"){
    return(input_json(file = file, label = label, key = key,
                      value = value, out_file = out_file))
  }else if(model == "MyLake"){
    return(input_mylakeconfig(file = file, label = label, key = key,
                              value = value, out_file = out_file))
  }else{
    stop("\"",model, "\" is not recognised as a valid model argument by input_config_value")
  }
}
