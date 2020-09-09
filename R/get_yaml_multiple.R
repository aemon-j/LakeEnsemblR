#' Extract values from yaml file
#'
#' Extract values from yaml file, like gotmtools::get_yaml_value
#'  However, an unlimited amount of keys can be provided.
#'  NOTE: this does not use a yaml parser so if there are yaml formatting errors
#'  this function will not pick them up.
#' @param file filepath; to yaml file which you wish to edit
#' @param ... string key1, key2, etc.: multiple keys pointing toward the line
#'   that you want to edit in the yaml file. Keys must be listed consecutively,
#'   without skipping numbers.
#' @export
#' @author
#' Jorrit Mesman
#' @examples
#'
#' \dontrun{
#' get_yaml_multiple(file = "example.yaml", key1 = "streams", key2 = "inflow", key3 = "file")
#' }

get_yaml_multiple <- function(file = "gotm.yaml", ...){
  
  yml <- readLines(file, warn = FALSE)
  
  # Prevent from finding labels/keys in comments
  yml_no_comments <- unname(sapply(yml, function(x) strsplit(x, "#")[[1]][1]))
  
  # Users can provide multiple keys, named key1, key2, key3, etc.
  all_args <- list(...)
  all_keys <- all_args[grepl("key", names(all_args))]
  
  nr_of_spaces <- -1
  previous_key <- 0
  for(i in seq_len(length(all_keys))){
    key <- all_keys[[paste0("key", i)]]
    
    #Find index of label
    key_id <- paste0(key, ":")
    ind_key <- grep(key_id, yml_no_comments)
    
    if(length(ind_key) == 0){
      stop("Key number ", i, ": '", key, "' not found in ", file)
    }else if(length(ind_key) > 1){
      # ind_key needs to be higher than previous key
      ind_key <- ind_key[ind_key > previous_key]
      
      # If still multiple, calculate number of spaces at the start
      # and select the one with the least amount of spaces,
      # but still more than the previous key. 
      # Then pick the first one of this series
      if(length(ind_key) > 1){
        spaces_keys <- rep(NA, length(ind_key))
        for(j in seq_len(length(ind_key))){
          spaces_keys[j] <- find_spaces(yml_no_comments[ind_key[j]], key_id)
        }
        
        # Only keep keys that have more spaces than the previous
        ind_key <- ind_key[spaces_keys > nr_of_spaces]
        spaces_keys <- spaces_keys[spaces_keys > nr_of_spaces]
        
        # Keep the keys that have least spaces
        ind_key <- ind_key[which(spaces_keys == min(spaces_keys))]
        
        # Pick the first as ind_key
        ind_key <- ind_key[1]
      }
    }
    
    if(ind_key < previous_key){
      stop("'", key, "' occurs at a lower line number than the previous key!")
    }
    
    # Make sure that key is in the section of previous_key
    # If any of the lines in between has the same number or less spaces than nr_of_spaces
    # key is in another section
    if(ind_key - previous_key > 1 & previous_key != 0){
      
      spaces <- sapply((previous_key + 1L):(ind_key - 1L),
                       function(x) find_spaces(line = yml_no_comments[x],
                                               key = key_id))
      spaces[spaces < 0] <- 0
      spaces[is.na(spaces)] <- nr_of_spaces + 1 # Empty lines should not cause errors
      
      if(any(spaces <= nr_of_spaces)){
        stop("'", key, "' is not in the same section as the previous key!")
      }
    }
    
    # Set previous_key and nr_of_spaces
    previous_key <- ind_key
    nr_of_spaces <- find_spaces(yml_no_comments[ind_key], key_id)
    if(nr_of_spaces == -1){
      nr_of_spaces <- 0
    }
  }
  # This is the line with the value you want to change
  ind_key <- previous_key
  
  
  # Replace the value (with the right amount of spaces)
  #Split to extract comment
  spl1 <- strsplit(yml[ind_key], c("#"))[[1]]
  spl2 <- strsplit(spl1[1], ": ")[[1]][2]
  val <- gsub(" ", "", spl2, fixed = TRUE)
  
  # check if item is a list
  if(length(grep("  - ",yml[ind_key+1]))>0){
    lst <- list(yml[ind_key + 1])
    k <- 2
    while (length(grep("  - ",yml[ind_key + k]))>0) {
      lst[[k]] <- yml[ind_key + k]
      k <- k+1
    }
    val <- unlist(lapply(lst,function(x){strsplit(x,"- ")[[1]][2]}))
  }
  
  
  if(length(val)==1){
    val2 <- NULL
    
    if(val == "false"){
      val2 <- FALSE
    }else if(val == "true"){
      val2 <- TRUE
    }
    
    flg <- TRUE
    flg <- tryCatch({!is.na(as.numeric(val))},
                    warning = function(x)return(FALSE))
    if(flg){
      val2 <- as.numeric(val)
    }
    if(is.null(val2)){
      val2 <- gsub('\"', "", val)
      val2 <- as.character(val2)
    }
  }else{
    val2 <- NULL
    
    if(all(val %in% c("false", "true"))){
      val2 <- rep(TRUE, length(val))
      val2[val %in% "false"] = FALSE
    }
    
    flg <- TRUE
    flg <- tryCatch({!is.na(as.numeric(val))},
                    warning = function(x)return(FALSE))
    if(all(flg)){
      val2 <- as.numeric(val)
    }
    if(is.null(val2)){
      val2 <- gsub('\"', "", val)
      val2 <- as.character(val2)
    }
  }
  
  return(val2)
}

# Find the number of spaces before a key in a line.
#' @keywords internal
find_spaces <- function(line, key){
  attr(regexpr("\\s+", strsplit(line, key)[[1]][1]), "match.length")
}

