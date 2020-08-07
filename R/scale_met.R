#' Scale meteorological data
#'@description
#'Scale meteorological parameters.
#'
#' @name scale_met
#' @param met dataframe; in the format for that particular model with the column headers
#' @param pars dataframe; scaling factors for meteorological data with column names corresponding
#'    to variable dictionary
#' @param model character; Model for which scaling parameters will be applied. Options include
#'    c('GOTM', 'GLM', 'Simstrat', 'FLake')
#' @param out_file filepath; For scaled meteorlogical data
#' @export
scale_met <- function(met, pars, model, out_file = NULL) {
  
  ## list with long standard names
  l_names <- as.list(met_var_dic$standard_name)
  names(l_names) <- met_var_dic$short_name

  if("FLake" %in% model) {
    par_nams <- names(pars)

    if("wind_speed" %in% par_nams) {
      met[[l_names$wind_speed]] <- met[[l_names$wind_speed]] * pars$wind_speed
    }

    if("swr" %in% par_nams) {
      met[[paste0("!", l_names$swr)]] <- met[[paste0("!", l_names$swr)]] * pars$swr
    }
    
    #Reduce number of digits
    met[, -ncol(met)] <- signif(met[, -ncol(met)], digits = 8)

    # Write to file
    write.table(met, out_file, sep = "\t", quote = FALSE, col.names = FALSE, row.names = FALSE)
  }

  if("GLM" %in% model) {

    par_nams <- names(pars)

    if("wind_speed" %in% par_nams) {
      met$WindSpeed <- met$WindSpeed * pars$wind_speed
    }

    if("swr" %in% par_nams) {
      met$ShortWave <- met$ShortWave * pars$swr
    }

    if("lwr" %in% par_nams) {
      met$LongWave <- met$LongWave * pars$lwr
    }
    
    #Reduce number of digits
    met[, -1] <- signif(met[, -1], digits = 8)

    # Write to file
    write.csv(met, out_file, quote = FALSE, row.names = FALSE)
  }

  if("GOTM" %in% model) {

    par_nams <- names(pars)

    if("wind_speed" %in% par_nams) {
      met[[l_names$u10]] <- met[[l_names$u10]] * pars$wind_speed
      met[[l_names$v10]] <- met[[l_names$v10]] * pars$wind_speed
    }

    if("swr" %in% par_nams) {
      met[[l_names$swr]] <- met[[l_names$swr]] * pars$swr
    }
    
    #Reduce number of digits
    met[, -1] <- signif(met[, -1], digits = 8)

    # Write to file
    write.table(met, out_file, quote = FALSE, row.names = FALSE, sep = "\t", col.names = TRUE)

  }

  if("Simstrat" %in% model) {

    par_nams <- names(pars)

    if("wind_speed" %in% par_nams) {
      met[[l_names$u10]] <- met[[l_names$u10]] * pars$wind_speed
      met[[l_names$v10]] <- met[[l_names$v10]] * pars$wind_speed
    }

    if("swr" %in% par_nams) {
      met[[l_names$swr]] <- met[[l_names$swr]] * pars$swr
    }

    if("lwr" %in% par_nams) {
      met[[l_names$lwr]] <- met[[l_names$lwr]] * pars$lwr
    }
    
    #Reduce number of digits
    met[, -1] <- signif(met[, -1], digits = 8)

    # Write to file
    write.table(met, file = out_file, sep = "\t", quote = FALSE, row.names = FALSE)

  }


  if("MyLake" %in% model) {

    par_nams <- names(pars)
    
    if("wind_speed" %in% par_nams) {
      met[[l_names$wind_speed]] <- met[[l_names$wind_speed]] * pars$wind_speed
    }

    if("swr" %in% par_nams) {
      met[[l_names$swr]] <- met[[l_names$swr]] * pars$swr
    }
    
    #Reduce number of digits
    met[, -1] <- signif(met[, -1], digits = 8)

    # Write to file
    write.table(met, file = out_file, sep = "\t", quote = FALSE, row.names = FALSE,
                col.names = FALSE)

  }

}
