#' make the correlation matrix from the data.frame used in regression
#'
#' @param data a data.frame used in regression model, e.g. model$model
#' @param data_to_combine another data.frame used for regression model, e.g. when you have similar set of X's but different Y's
#' @param var_name_select optional: to specify the variable names used in regression to be included in the correlation matrix
#' @param all.var.names optional: to rename all variable names, a string vector
#' @param d number of decimal places to retain
#' 
#' @examples 
#' data(mtcars)
#' model <- lm(mpg ~ vs + carb + hp + wt + wt * hp , data = mtcars)
#' cor.table(data = model$model)
#' 
#' @import dplyr
#' @import tidyr
#' 
#' @export
cor.table <- function(data, data_to_combine = NULL, var_name_select = NULL, all.var.names = NULL , d = 2){
  
  tryCatch({
  # allow addtional data.frame (same structured) from addtional models
  if(!is.null(data_to_combine) && all.equal(dim(data), dim(data_to_combine))){
    intersec <- dplyr::intersect(names(data), names(data_to_combine))
    union_ <- dplyr::union(names(data), names(data_to_combine))
    y1_y1 <- union_[! union_ %in% intersec]
    data <- dplyr::bind_cols(data, data_to_combine)
    data <- data[,c(y1_y1, intersec)] # re-order vars
  }
  # filter numeric values
  df <- data[, which(sapply(data, class) %in% c("numeric","integer","AsIs"))]
  cor.matrix <- cor(df)
  # allow user to select variabels
  if(!is.null(var_name_select)){
    seleted <- which(row.names(cor.matrix) %in% var_name_select)
    cor.matrix <- cor.matrix[seleted, seleted]
  }
  # add mean and s.d.
  cor.matrix[upper.tri(cor.matrix, diag = FALSE)] <- NA
  means <- colMeans(df[colnames(cor.matrix)], na.rm=TRUE) 
  sds   <- sapply(df[colnames(cor.matrix)], FUN=sd, na.rm=TRUE)
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
  
  }, error=function(e){cat("ERROR :", conditionMessage(e), "\n")})

  return(result)}