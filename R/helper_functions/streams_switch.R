#' @title Switch on/off streams in gotm.yaml file
#' @description
#'Places '#' in front of the streams within the file or removes them, effectively switching off in/outflows.
#' @param file filepath; to .yaml which you wish to edit
#' @param method; character; Either 'on' or 'off'
#' @export
#' @author
#'Tadhg Moore
#' @examples
#'streams_switch(file = 'gotm.yaml', method = 'off')

streams_switch <- function(file = 'gotm.yaml', method){
  yml <- readLines(file)
  
  #Find index of label
  label_id <- paste0('streams:')
  ind_start <- grep(label_id, yml)
  ind_stop <- grep('physical_constants:', yml)
  
  if(method == 'off'){
    for(i in (ind_start+1):(ind_stop-1)){
      yml[i] <- paste0('#', yml[i])
    }
    message('Switched off streams!')
  }else if (method == 'on'){
    for(i in (ind_start+1):(ind_stop-1)){
      n.char <- nchar(yml[i])
      hash_ind = gregexpr(pattern ='#',yml[i])[[1]]
      del_ind = hash_ind[(length(hash_ind)-1)]
      yml[i] <- substring(yml[i],(del_ind+1), n.char)
    }
    message('Switched on streams!')
  }
  
  #Write to file
  writeLines(yml, file)
}
