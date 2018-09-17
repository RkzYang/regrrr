# quickly check the vifs of a data.frame, which can be from a model #
reg.Vif <- function(model_df){
  usdm::vif(model_df[,which(sapply(model_df, class) %in% c("numeric","integer","AsIs"))])
}

# quickly check the complete correlaiton matrix, or the correlation between a variable X and all other vars #
# can be useful for finding instrument #
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


