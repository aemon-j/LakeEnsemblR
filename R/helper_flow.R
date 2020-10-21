#' douplicate section in gotm.yaml
#'@description
#' doublicate a section in the gotm.yaml file. typically used for in- or outflow
#'
#' @name doubl_yaml_sec
#' @param yaml_file filepath; to gotm.yaml
#' @param sec_name name of the section to doublicate
#' @param ap appendix for the name of the section
#' @noRd


doubl_yaml_sec <- function(yaml_file, sec_name, ap) {
  
  # read yaml file
  yml <- readLines(yaml_file)
  
  # Prevent from finding labels/keys in comments
  yml_no_comments <- unname(sapply(yml, function(x) strsplit(x, "#")[[1]][1]))
  
  #Find index of section
  sec_id <- paste0(sec_name,":")
  ind_sec <- grep(paste0("\\b", sec_id), yml_no_comments)
  
  if(length(ind_sec) == 0){
    stop(sec_name, " not found in ", yaml_file)
  }

  # find index of next section
  find <- TRUE
  # index
  i <- 1
  # number of whitespace in section
  lws <- nchar(strsplit(yml_no_comments[ind_sec], sec_id)[[1]][1])
  while(find) {
    
    ws <- nchar(strsplit(yml_no_comments[ind_sec + i], "\\w+\\:")[[1]][1])
    if(ws > lws) {
      i <- i +1
    } else {
      find <- FALSE
    }
    
  }
  
  # whole sdction
  sec_yml <- yml[ind_sec:(ind_sec + i - 1)]
  # change name by adding a number
  sec_yml[1] <- gsub(pattern = sec_name, replacement = paste0(sec_name, ap), sec_yml[1])
  
  # insert section
  yml_out <- c(yml[1:(ind_sec + i -1)], sec_yml, yml[(ind_sec + i):length(yml)])
  
  #Write to file
  writeLines(yml_out, yaml_file)
}


#' remove section in gotm.yaml
#'@description
#' remove a section  in the gotm.yaml file. typically used for in- or outflow
#'
#' @name doubl_yaml_sec
#' @param yaml_file filepath; to gotm.yaml
#' @param sec_name name of the section to be removed
#' @noRd


rm_yaml_sec <- function(yaml_file, sec_name) {
  
  # read yaml file
  yml <- readLines(yaml_file)
  
  # Prevent from finding labels/keys in comments
  yml_no_comments <- unname(sapply(yml, function(x) strsplit(x, "#")[[1]][1]))
  
  #Find index of section
  sec_id <- paste0(sec_name,":")
  ind_sec <- grep(paste0("\\b", sec_id), yml_no_comments)
  
  if(length(ind_sec) == 0){
    stop(sec_name, " not found in ", yaml_file)
  }
  
  # find index of next section
  find <- TRUE
  # index
  i <- 1
  # number of whitespace in section
  lws <- nchar(strsplit(yml_no_comments[ind_sec], sec_id)[[1]][1])
  while(find) {
    
    ws <- nchar(strsplit(yml_no_comments[ind_sec + i], "\\w+\\:")[[1]][1])
    if(ws > lws) {
      i <- i +1
    } else {
      find <- FALSE
    }
    
  }
  
  # remove section
  yml_out <- yml[c(1:(ind_sec-1),(ind_sec + i):length(yml))]
  
  #Write to file
  writeLines(yml_out, yaml_file)
}
