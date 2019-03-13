#' Scale a vector into the 0-1 scale
#'
#' @param x a vector
#' @examples 
#' scale_01(rnorm(100, 1, 1))
#' 
scale_01 <- function(x){
  tryCatch({
  (x-min(x))/(max(x)-min(x))
  }, error=function(e){cat("ERROR :", conditionMessage(e), "\n")})
  }
