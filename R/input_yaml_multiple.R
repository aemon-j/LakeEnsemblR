#' Inputs values into yaml file
#'
#' Inputs values into yaml file, like gotmtools::input_yaml
#'  However, an unlimited amount of keys can be provided.
#'  Preserves comments (#) if present.
#'  NOTE: this does not use a yaml parser so if there are yaml formatting errors
#'  this function will not pick them up.
#' @param file filepath; to yaml file which you wish to edit
#' @param value string; to be input into the the yaml file.
#'   Note boolean values must be input as "true"/"false" as per the json format
#' @param out_file filepath; to write the output json file (optional);
#'   defaults to overwriting file if not specified
#' @param ... string key1, key2, etc.: multiple keys pointing toward the line
#'   that you want to edit in the yaml file. Keys must be listed consecutively,
#'   without skipping numbers.
#' @param verbose Logical; output a message which values were replaced
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
                                out_file = NULL, verbose = TRUE, ...){

  if(is.null(out_file)){
    out_file <- file
  }

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
      stop("Key number ", i, ": ", key, " not found in ", file)
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

  #Split to extract current value and identify pattern to sub in for
  spl_tmp <- strsplit(spl1[1], ": ")[[1]]
  if(length(spl_tmp) == 1){
    spl2 <- ""
  }else{
    spl2 <- spl_tmp[2]
  }

  sub <- paste0(value, " ")

  if(length(sub) == 1) {
    # Sub in new value
    # Addition of \Q and \E is to avoid errors in case spl2 contains
    # characters that could be interpreted as regular expressions
    # see ?base::regex
    yml[ind_key] <- gsub(pattern = paste0("\\Q", spl1[1], "\\E"),
                         replacement = paste0(spl_tmp[1], ": ", sub),
                         x = yml[ind_key])

    old_val <- gsub(" ", "", spl2, fixed = TRUE) #remove white space for printing
  } else if (length(sub) > 1) {

    # find how many elements the variable had
    stll <- TRUE
    k <- 1
    while (stll) {
      if(grepl("\\s*\\-\\s+\\S+", yml[ind_key + k])) {
        k <- k+1
      } else {
        stll <- FALSE
        k <- k-1
      }
    }
    if(k == 0) {
      warning(paste0("The replaced parameter was not in yaml vector format, ",
                     "this will probably lead to errors"))
    }
    old_val <- paste0(gsub("\\s*\\-\\s", "", yml[(ind_key + 1):(ind_key + k)]),
                      collapse = ", ")
    spc <- gsub("\\S", "", spl1[1])
    new_l <- paste0(spc, "   - ", value)
    yml <- c(yml[seq_len(length(yml)) <= ind_key], new_l, yml[seq_len(length(yml)) > (ind_key + k)])
    value <- paste0(value, collapse = ", ")
  }
  #Write to file
  writeLines(yml, out_file)

  # Display message
  message_string <- ""
  for(i in all_keys){
    message_string <- paste(message_string, i)
  }

  if(verbose) {
    message("Replaced", message_string, ": ", old_val, " with ", value)
  }
}

# Find the number of spaces before a key in a line.
#' @keywords internal
find_spaces <- function(line, key){
  attr(regexpr("\\s+", strsplit(line, key)[[1]][1]), "match.length")
}

