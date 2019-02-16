#' Scale a vector into a 0-1 scale
#'
#' @param x a vector
#' @examples 
#' scale_01(rnorm(100, 1, 1))
#' 
#' @export
scale_01 <- function(x){(x-min(x))/(max(x)-min(x))}
