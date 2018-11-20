#' quickly check the vifs in a regression model; for checking multi-collinearity 
#'
#' @param model_df a data.frame used in regression model
#' @export
reg.Vif <- function(model_df){
  usdm::vif(model_df[,which(sapply(model_df, class) %in% c("numeric","integer","AsIs"))][,-1])
}

#' quickly check correlaiton matrix, or the correlation between a particular X and all other vars 
#' can be useful for looking for relavant instrument
#'
#' @param model_df a data.frame used in regression model
#' @export
reg.Cor <- function(model_df, var_name_select = NULL, d = 3){
  cor.matrix <- round(cor(model_df[,which(sapply(model_df, class) %in% c("numeric","integer","AsIs"))]), d)
  if(is.null(X_name)){
    result <- cor.matrix
  }else{
    seleted <- which(row.names(cor.matrix) %in% var_name_select)
    result <- cor.matrix[seleted, seleted]
  }
  return(result)
}

#' quickly check the proportion of NAs in each columns of a dataframe 
#'
#' @param df a data.frame
#' @export
check_na_in <- function(df, true_total = FALSE){
  if(true_total == TRUE){
result <- scales::percent(sapply(df, function(x) sum(is.na(x)))/nrow(df))
names(result) <- names(df)}else{
result <- sapply(df, function(x) sum(is.na(x)))  
}
return(result)
}