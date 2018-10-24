#' make the correlation matrix from the data.frame used in regression
#'
#' @param model_df a data.frame used in regression model
#' @param model_df_to_combine another data.frame used for regression model, e.g. when you have similar set of X's but different Y's
#' @param var_name_select specify the names of regression variables to be included in the correlation matrix
#' @param all.var.names all variable names, a string vector
#' @param d number of decimal places to retain
#' @import purrr 
#' 
#' @import dplyr
#' 
#' @import tidyr
#' 
#' @export
reg.Cor.Table <- function(model_df, model_df_to_combine = NULL, var_name_select = NULL, all.var.names = NULL , d = 2){
  # allow addtional data.frame (same structured) from addtional models  
  if(!is.null(model_df_to_combine) & all.equal(dim(model_df), dim(model_df_to_combine))){
    intersec <- dplyr::intersect(names(model_df), names(model_df_to_combine))
    union_ <- dplyr::union(names(model_df), names(model_df_to_combine))
    y1_y1 <- union_[! union_ %in% intersec]
    model_df <- dplyr::bind_cols(model_df, model_df_to_combine)
    model_df <- model_df[,c(y1_y1, intersec)] # re-order vars
  }
  # filter numeric values
  df <- model_df[, which(sapply(model_df, class) %in% c("numeric","integer","AsIs"))]
  cor.matrix <- cor(df)
  # allow user to select variabels
  if(!is.null(var_name_select)){
    seleted <- which(row.names(cor.matrix) %in% var_name_select)
    cor.matrix <- cor.matrix[seleted, seleted]
  }
  # add mean and s.d.
  cor.matrix[upper.tri(cor.matrix, diag = FALSE)] <- NA
  means <- colMeans(df, na.rm=TRUE) 
  sds   <- sapply(df, FUN=sd, na.rm=TRUE)
  c.matrix <- do.call(cbind, list(means, sds, cor.matrix)) %>% formatC(format='f', digits = d)
  # add colnames 
  colnames(c.matrix) <- c(c("Mean", "S.D."), 1:nrow(c.matrix))
  # delete "NA"
  result <- apply(c.matrix, MARGIN = 2, FUN = stringr::str_replace, pattern = paste0(paste(rep(" ", d - 1), collapse = ""), "NA"), replacement = "") %>% as.data.frame()
  # add rownames
  if(!is.null(all.var.names)){
    col_names <- paste(1:nrow(c.matrix), all.var.names, sep = ".")
  }else{
    col_names <- paste(1:nrow(c.matrix), rownames(c.matrix), sep = ".")
  }
  rownames(result) <- col_names
  return(result)}