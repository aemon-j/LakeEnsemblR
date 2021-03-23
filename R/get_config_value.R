#' Extract value from any LER model config file
#'
#' Wrapper function for functions that extract values from model config files
#' @param model string; name of a model in the LER package
#' @param file filepath; to model-specific config file
#' @param label string; which corresponds to section where the key is located
#' @param key string; name of key in which to extract the value
#' 
#' @importFrom gotmtools get_yaml_value
#' @importFrom glmtools read_nml get_nml_value
#' 
#' @examples
#' \dontrun{
#' get_config_value(model = "GOTM", file = "gotm.yaml", label = "turb_param", key = "k_min")
#' }
#' 
#' @export

get_config_value <- function(model, file, label, key){
  if(model == "FLake" | model == "GLM"){
    # "label" not used in glmtools::get_nml_value function
    nml_file <- glmtools::read_nml(nml_file = file)
    return(glmtools::get_nml_value(glm_nml = nml_file, arg_name = key))
  }else if(model == "GOTM"){
    yaml <- gotmtools::read_yaml(file)
    return(gotmtools::get_yaml_value(yaml = yaml, "surface", label = label, "u10", key = key))
  }else if(model == "Simstrat"){
    return(get_json_value(file = file, label = label, key = key))
  }else if(model == "MyLake"){
    return(get_mylakeconfig_value(file = file, label = label, key = key))
  }else{
    stop("\"", model, "\" is not recognised as a valid model argument by get_config_value.")
  }
}
