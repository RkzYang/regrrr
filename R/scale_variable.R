#' Scale a vector into a 0-1 scale
#'
#' @param x a vector
#' @export
scale_01 <- function(x){(x-min(x))/(max(x)-min(x))}
