#' Inputs value into any LER model config file
#'
#' Wrapper function for functions that input values into model config files
#' @inheritParams export_config
#' @inheritParams gotmtools::set_yaml
#'
#' @importFrom gotmtools read_yaml write_yaml
#' @importFrom glmtools read_nml set_nml write_nml
#'
#' @examples
#' \dontrun{
#' input_config_value(model = "GOTM", file = "gotm.yaml",
#'   label = "turb_param", key = "k_min", value = 0.00001)
#' }
#'
#' @export

input_config_value <- function(model, yaml, folder) {

  for(m in model){
    # Only continue if model-specific parameters are specified for this model
    if(is.null(yaml[["model_parameters"]][[m]])) next

    par_list <- yaml[["model_parameters"]][[m]]
    model_config <- file.path(folder, yaml[["config_files"]][[m]])

    if(m %in% c("GLM", "FLake")) {
      pars <- strsplit(names(par_list), "/")
      par_names <- sapply(pars, "[", 2)
      arg_list <- setNames(par_list, par_names)
      nml <- glmtools::read_nml(nml_file = model_config)
      nml <- glmtools::set_nml(nml, arg_list = arg_list)
      glmtools::write_nml(nml, model_config)
    } else if(m == "GOTM") {
      gotm_yaml <- gotmtools::read_yaml(model_config)
      for(p in seq_len(length(par_list))) {
        pars <- strsplit(names(par_list[p]), "/")[[1]]
        if(length(pars) == 1) {
          gotm_yaml[[pars[1]]] <- par_list[[p]]
        } else if(length(pars) == 2) {
          gotm_yaml[[pars[1]]][[pars[2]]] <- par_list[[p]]
        } else if(length(pars) == 3) {
          gotm_yaml[[pars[1]]][[pars[2]]][[pars[3]]] <- par_list[[p]]
        } else if(length(pars) == 4) {
          gotm_yaml[[pars[1]]][[pars[2]]][[pars[3]]][[pars[4]]] <- par_list[[p]]
        }
      }
      gotmtools::write_yaml(gotm_yaml, model_config)
    } else if(m == "Simstrat") {
      pars <- strsplit(names(par_list), "/")
      par_names <- sapply(pars, "[", 2)
      arg_list <- setNames(par_list, par_names)
      input_json(file = model_config, arg_list = arg_list)
    } else if(m == "MyLake") {
      for(p in seq_len(length(par_list))) {
        pars <- strsplit(names(par_list)[p], "/")[[1]]
        input_mylakeconfig(file = model_config, label = pars[1], key = pars[2],
                           value = par_list[[p]])
      }
    } else {
      stop("\"", model, "\" is not recognised as a valid model argument by input_config_value")
    }
  }
}
