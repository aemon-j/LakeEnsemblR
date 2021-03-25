#' Inputs value into any LER model config file
#'
#' Wrapper function for functions that input values into model config files
#' @param model string; name of a model in the LER package
#' @param file filepath; to model-specific config file
#' @param label string; which corresponds to section where the key is located
#' @param key string; name of key in which to extract the value
#' @param value string; name of value to input into config file
#' @param ... string; all required yaml labels
#'   defaults to overwriting file if not specified
#' 
#' @importFrom gotmtools read_yaml input_nml set_yaml write_yaml
#' 
#' @examples
#' \dontrun{
#' input_config_value(model = "GOTM", file = "gotm.yaml",
#'   label = "turb_param", key = "k_min", value = 0.00001)
#' }
#' 
#' @export

input_config_value <- function(model, file, label = NULL, key = NULL, value, ...){
  # Collect all arguments
  all_args <- list(...)
  
  if(model == "FLake"){
    nml <- glmtools::read_nml(nml_file = file)
    nml <- glmtools::set_nml(glm_nml = nml, arg_name = key, arg_val = unlist(value))
    return(glmtools::write_nml(glm_nml = nml, file = file))
  }else if(model == "GLM"){
    nml <- glmtools::read_nml(nml_file = file)
    nml <- glmtools::set_nml(glm_nml = nml, arg_name = key, arg_val = unlist(value))
    return(glmtools::write_nml(glm_nml = nml, file = file))
  }else if(model == "GOTM"){
    got_yaml <- gotmtools::read_yaml(file)
    arg_list <- list(yaml = got_yaml, value = value)
    for( i in seq_len(length(all_args))) {
      arg_list[[length(arg_list) + 1]] <- all_args[[i]]
    }
    got_yaml <- do.call(set_yaml, args = arg_list)
    return(gotmtools::write_yaml(got_yaml, file))
  }else if(model == "Simstrat"){
    return(input_json(file = file, label = label, key = key,
                      value = value))
  }else if(model == "MyLake"){
    return(input_mylakeconfig(file = file, label = label, key = key,
                              value = value))
  }else{
    stop("\"", model, "\" is not recognised as a valid model argument by input_config_value")
  }
}
