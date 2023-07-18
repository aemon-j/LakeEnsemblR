#' Create temporary directories from model folders.
#'
#' @param model string; for which model. Options are c("dy_cd", "glm_aed" and
#'  "gotm_wet")
#' @param folder filepath; to directory which contains the model
#' configuration.
#' @param n integer; number of directories to create.
#'
#' @return vector of temporary directories.
#' @export
make_temp_dir <- function(model, folder = ".", n = 2, tmp_dir = NULL) {
  
  oldwd <- getwd()
  cfg_file <- list.files(oldwd, pattern = "LER_CNFG_TMP", full.names = TRUE)
  model_dir <- file.path(oldwd, folder, model)
  setwd(model_dir)
  on.exit(setwd(oldwd))
  fils <- list.files(model_dir)
  fils <- fils[fils != "output"]
  fils <- fils[!grepl("restart", fils)]
  fils <- fils[!grepl("nc", fils)]

  temp_dirs <- sapply(1:n, \(n) {
    if(is.null(tmp_dir)) {
      tmp_dir <- tempdir()
    } else {
      tmp_dir <- file.path(tmp_dir, tempdir())
    }
    dir <- file.path(tmp_dir, paste0("n_", n), model)
    unlink(dir, recursive = TRUE, force = TRUE)
    dir.create(dir, recursive = TRUE)
    dir.create(file.path(dir, "output"), recursive = TRUE)
    file.copy(cfg_file, file.path(tempdir(), paste0("n_", n)), recursive = TRUE)
    file.copy(fils, dir, recursive = TRUE)
    return(file.path(tmp_dir, paste0("n_", n)))
  })
  return(temp_dirs)
}
