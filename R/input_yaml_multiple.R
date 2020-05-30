#' Inputs values into yaml file
#'
#' Inputs values into yaml file, like gotmtools::input_yaml
#'  However, an unlimited amount of keys can be provided.
#'  Preserves comments (#) if present.
#'  NOTE: this does not use a yaml parser so if there are yaml formatting errors
#'  this function will not pick them up.
#' @param file filepath; to yaml file which you wish to edit
#' @param value string; to be input into the the yaml file.
#'   Note boolean values must be input as 'true'/'false' as per the json format
#' @param out_file filepath; to write the output json file (optional);
#'   defaults to overwriting file if not specified
#' @param ... string key1, key2, etc.: multiple keys pointing toward the line
#'   that you want to edit in the yaml file. Keys must be listed consecutively,
#'   without skipping numbers. 
#' @export
#' @author
#' Jorrit Mesman
#' @examples
#' 
#' \dontrun{
#' input_yaml_multiple(file = "example.yaml", value = "something",
#'   key1 = "streams", key2 = "inflow", key3 = "file")
#' }

input_yaml_multiple <- function(file = "gotm.yaml", value, 
                                out_file = NULL, ...){
  
  if(is.null(out_file)){
    out_file = file
  }
  
  yml = readLines(file, warn = F)
  
  # Users can provide multiple keys, named key1, key2, key3, etc.
  allArgs = list(...)
  allKeys = allArgs[grepl("key", names(allArgs))]
  
  nrOfSpaces = -1
  previousKey = 0
  for(i in 1:length(allKeys)){
    key = allKeys[[paste0("key",i)]]
    
    #Find index of label
    key_id <- paste0(key,":")
    ind_key <- grep(key_id, yml)
    
    if(length(ind_key) == 0){
      stop("Key number ",i,": ",key, " not found in ", file)
    }else if(length(ind_key) > 1){
      # ind_key needs to be higher than previous key
      ind_key = ind_key[ind_key > previousKey]
      
      # If still multiple, calculate number of spaces at the start
      # and select the one with the least amount of spaces,
      # but still more than the previous key. 
      # Then pick the first one of this series
      if(length(ind_key) > 1){
        spacesKeys = rep(NA,length(ind_key))
        for(j in 1:length(ind_key)){
          spacesKeys[j] = attr(regexpr("\\s+", yml[ind_key[j]]), "match.length")
        }
        
        # Only keep keys that have more spaces than the previous
        ind_key = ind_key[spacesKeys > nrOfSpaces]
        spacesKeys = spacesKeys[spacesKeys > nrOfSpaces]
        
        # Keep the keys that have least spaces
        ind_key = ind_key[which(spacesKeys == min(spacesKeys))]
        
        # Pick the first as ind_key
        ind_key = ind_key[1]
      }
    }
    # Set previousKey and nrOfSpaces
    # Need to calculate spacesKeys again, in case it wasn't calculated before
    
    previousKey = ind_key
    nrOfSpaces = attr(regexpr("\\s+", yml[ind_key]), "match.length")
    if(nrOfSpaces == -1){nrOfSpaces=0}
  }
  # This is the line with the value you want to change
  ind_key = previousKey
  
  
  # Replace the value (with the right amount of spaces)
  #Split to extract comment
  spl1 <- strsplit(yml[ind_key], c("#"))[[1]]
  if(length(spl1) == 2){
    comment <- spl1[2]
  }
  
  #Split to extract current value and identify pattern to sub in for
  spl2 <- strsplit(spl1[1], ": ")[[1]][2]
  
  # if(!is.na(comment)){
  #   sub = paste0(" ", value," #", comment)
  # }else{
  sub = paste0(value," ")
  # }
  
  # Sub in new value
  # Addition of \Q and \E is to avoid errors in case spl2 contains
  # characters that could be interpreted as regular expressions
  # see ?base::regex
  yml[ind_key] <- gsub(pattern = paste0("\\Q", spl2, "\\E"),
                       replacement = sub,
                       x = yml[ind_key])
  
  #Write to file
  writeLines(yml, out_file)
  old_val <- gsub(" ", "", spl2, fixed = TRUE) #remove white space for printing
  
  # Display message
  messageString = ""
  for(i in allKeys){
    messageString = paste(messageString, i)
  }
  
  message("Replaced", messageString, ": ", old_val, " with ", value)
}
