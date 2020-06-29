#' @importFrom utils packageDescription

.onAttach <- function(...) {
  pkgname <- "LakeEnsemblR"
  ver <- packageDescription(pkgname)$Version
  builddate <- packageDescription(pkgname)$Date
  system.file("start_mess", package = "LakeEnsemblR") %>% readLines() %>% cat(sep = "\n") 
  mess <- paste(pkgname, " version ", ver, " (", builddate,") is loaded", sep = "")
  packageStartupMessage(mess)
  url <- paste0("https://raw.githubusercontent.com/aemon-j/", pkgname, "/master/DESCRIPTION")
  b <- tryCatch(suppressWarnings(readLines(url)), error = function(er) {
    er <- NULL
    return(er)
  })
  if (!is.null(b)) {
    latest.ver <- package_version(gsub("Version: ", "", b[grep("^Version", b)]))
    if (ver < latest.ver) {
      ver.mess1 <- paste0("WARNING: Your current version of ", pkgname, " (v", ver, ") is not up-to-date")
      ver.mess <- paste0("Get the latest stable version (", latest.ver,
                         ") using <devtools::install_github('aemon-j/", pkgname, "')>")
      packageStartupMessage(ver.mess1)
      packageStartupMessage(ver.mess)
    } else if (ver > latest.ver) {
      ver.mess1 <- paste0("WARNING: Your current version of ", pkgname, " (v", ver, 
                          ") is ahead of the master branch version (", latest.ver, ")")
      ver.mess <- paste0("Development version may have an unexpected behaviour")
      packageStartupMessage(ver.mess1)
      packageStartupMessage(ver.mess)
    }
  }
  # packageStartupMessage("Please see 'citation(\"", pkgname, "\")' to cite this package.")
}
# End

#' @importFrom utils globalVariables

# if (getRversion() >= "3.1.0") utils::globalVariables(c("node.list", "l1"))
