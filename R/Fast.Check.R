#' quickly check the vifs in a regression model; for checking multi-collinearity 
#'
#' @param data a data.frame used in regression model
#' @examples
#' data(mtcars)
#' model <- lm(mpg ~ vs + carb + hp + wt + wt * hp , data = mtcars)
#' check_vif(data = model$model)
#' 
#' @import usdm
#' @export
check_vif <- function(data){
  
  tryCatch({
    
  usdm::vif(data[, which(sapply(data, class) %in% c("numeric","integer","AsIs"))][,-1])
    
  }, error=function(e){cat("ERROR :", conditionMessage(e), "\n")})
}

#' quickly check correlation matrix, or the correlation between a particular X and all other vars 
#' could be useful for looking for relevant instrument
#'
#' @param data a data.frame used in regression model
#' @param var_name_select to specify the variable names to be included in the table, default is NULL--all variables are included
#' @param d number of digits retained after the decimal point
#' @examples 
#' data(mtcars)
#' check_cor(mtcars)
#' 
#' @importFrom stats cor
#' @export
check_cor <- function(data, var_name_select = NULL, d = 3){
  
  tryCatch({
  
  cor.matrix <- round(cor(data[,which(sapply(data, class) %in% c("numeric","integer","AsIs"))]), d)
  if(is.null(var_name_select)){
    result <- cor.matrix
  }else{
    seleted <- which(row.names(cor.matrix) %in% var_name_select)
    result <- cor.matrix[seleted, seleted]
  }
  
  }, error=function(e){cat("ERROR :", conditionMessage(e), "\n")})
  
  return(result)
}

#' quickly check the proportion of NAs in each columns of a dataframe 
#'
#' @param data a data.frame
#' @param true_total FALSE to show the percentage, TRUE to show the true number of missing values
#' @examples 
#' data(mtcars)
#' check_na_in(mtcars)
#' 
#' @importFrom scales percent
#' @export
check_na_in <- function(data, true_total = FALSE){
  
  tryCatch({
  
  if(true_total == FALSE){
    result <- scales::percent(sapply(data, function(x) sum(is.na(x)))/nrow(data))
    names(result) <- names(data)}else{
      result <- sapply(data, function(x) sum(is.na(x)))  
    }
  
  }, error=function(e){cat("ERROR :", conditionMessage(e), "\n")})
    
  return(result)
}