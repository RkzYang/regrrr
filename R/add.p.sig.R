
#' Add row numbers to regression result data.frame
#'
#' @param df a data.frame of regression result
#' @export
add.n.r <- function(df){
  tryCatch({
  data.frame(n.r=1:nrow(df), df)
  }, error=function(e){cat("ERROR :", conditionMessage(e), "\n")})
}

#' Add approximate p-value based on t score or z score, when sample size is large
#'
#' @param df a data.frame of regression result
#' @param z.col the column number of t score or z score
#' @param p.already whether the regression result already contains p.value
#' @importFrom stats pnorm
#' @export
add.pr <- function(df, z.col = 3, p.already=FALSE){
          tryCatch({  
                              if(p.already==FALSE){
                data.frame(n.r=1:nrow(df), df, prob.=2 * (1 - pnorm(abs(df[,z.col]))))}else{
                data.frame(n.r=1:nrow(df), df, prob.=df[,3])
                }
  
          }, error=function(e){cat("ERROR :", conditionMessage(e), "\n")})
              }

#' Add significance level marks to the regression result
#'
#' @param df a data.frame of regression result, e.g. summary(a_lm_model)$coefficients
#' @param Pr.col the column number of p.value
#' @export
add.sig <- function(df, Pr.col = 5){
                          tryCatch({
                                    data.frame(df, sig=ifelse(df[,Pr.col]<0.001, paste0(rep("\x2a", 3), collapse = ""),
                                                             ifelse(df[,Pr.col]<0.01, paste0(rep("\x2a", 2), collapse = ""),
                                                                   ifelse(df[,Pr.col]<0.05, paste0(rep("\x2a", 1), collapse = ""),
                                                                          ifelse(df[,Pr.col]<0.1,"\xe2\x80\xa0","")))))
  
                                    }, error=function(e){cat("ERROR :", conditionMessage(e), "\n")})
                                    }

# format.reg.table.survival <- function(df, d=d){
#   df <- data.frame(n.r=1:nrow(df),df)
#   test <- cbind(var=rownames(df),df[,c(1,3:4,6)])
#   digits <- function(x,d){ if(class(x)=="numeric") {formatC(x, format = "f", digits = d)} else{x} }
#   test <- data.frame(purrr::map(test,digits,d))
#   test <- tidyr::unite(test, coef,3,5,sep="")
#   test[,4]  <- paste0("(",test[,4],")",sep="")
#   reg.table <- dplyr::arrange(tidyr::gather(test,key,beta,-c(var,n.r)),n.r) %>% dplyr::select(2,1,4)
#   even.row <- rep(c(FALSE,TRUE),nrow(reg.table)/2)
#   reg.table$var <- as.character(reg.table$var)
#   reg.table$var[even.row] <- ""
#   return(reg.table)
# }
# suppressWarnings(warning("format.reg.table"))  

# # Convert a regression result into a clean data.frame
# #
# # @param lm.obj a regression result object
# # @importFrom stats coef
# reg.table <- function(lm.obj){
# 
#   add.p.z <- function(df, z.col = 3, p.already=FALSE){
#     if(p.already==FALSE){
#       data.frame(n.r=1:nrow(df), df, p.z=2 * (1 - pnorm(abs(df[,z.col]))))}else{
#         data.frame(n.r=1:nrow(df), df, p.z=df[,3])
#       }
#   }
# 
#   add.sig <- function(df, Pr.col = 5){data.frame(df, sig=ifelse(df[,Pr.col]<0.001, paste0(rep("\x2a", 3), collapse = ""),
#                                                                 ifelse(df[,Pr.col]<0.01, paste0(rep("\x2a", 2), collapse = ""),
#                                                                        ifelse(df[,Pr.col]<0.05, paste0(rep("\x2a", 1), collapse = ""),
#                                                                               ifelse(df[,Pr.col]<0.1,"\xe2\x80\xa0","")))))}
# 
#   return(add.sig(add.p.z(as.data.frame(stats::coef(summary(lm.obj))))))}