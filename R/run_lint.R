#' Runs lint for LER script
#'
#'Runs lintr::lint function, using the settings agreed upon for LakeEnsemblR
#' @param file filepath; to R script that you want to run the lintr::lint function on
#' @examples
#'run_lint(file = "export_config.R")
#'
#' @import lintr

run_lint <- function (file)
{
  LER_linters <- lintr::with_defaults(line_length_linter = line_length_linter(100), 
                               commented_code_linter = NULL, 
                               cyclocomp_linter = NULL, 
                               paren_brace_linter = NULL, 
                               spaces_left_parentheses_linter = NULL)
  
  file_location <- system.file(paste0("R/",file), package = packageName())
  
  # Call to the function, shows lines in "Markers" tab
  lintr::lint(file_location, linters = LER_linters)
}





