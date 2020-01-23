#' Load parameter dictionary
#'
#' Load parameter dictionary
#'
#'
#' @export
load_dic <- function(){
  # Load dictionary
  # dic <- readRDS(system.file('data/met_var_dic.rda', package = packageName()))
  dic <- read.csv(system.file('extdata/met_var_dic.csv', package = packageName()), stringsAsFactors = FALSE)
  return(dic)
}
