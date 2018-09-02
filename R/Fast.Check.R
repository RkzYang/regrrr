# quickly check the vifs of a data.frame, which can be from a model #
reg.Vif <- function(model_df){
  usdm::vif(model_df[,which(sapply(model_df, class) %in% c("numeric","integer","AsIs"))])
}

# quickly check the complete correlaiton matrix, or the correlation between a variable X and all other vars #
# can be useful for finding instrument #
reg.Cor <- function(model_df, X_name = NULL){
  cor.matrix <- round(cor(model_df[,which(sapply(model_df, class) %in% c("numeric","integer","AsIs"))]),3)
  if(is.null(X_name)){
    result <- cor.matrix
  }else{
    result <- cor.matrix[which(row.names(cor.matrix)==X_name),]
  }
  return(result)
}


