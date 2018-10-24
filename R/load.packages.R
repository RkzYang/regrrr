#' load multiple packages
#'
#' @param pkg_name_vec a string vector of package names
#' 
#' @export
#'
#' @examples
#' load.pkgs(c("dplyr", "purrr"))
load.pkgs <- function(pkg_name_vec){invisible(suppressMessages(lapply(pkg_name_vec, require, character.only = TRUE)))}
  
  

