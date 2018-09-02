suppressMessages(lapply(c("dplyr","tidyr","lme4","purrr","gee","MuMIn","car","xts"), require, character.only = TRUE))

#' Title
#'
#' @param df
#' @param z.col
#' @param p.already
#'
#' @return
#' @export
#'
#' @examples

add.n.r <- function(df){
  data.frame(n.r=1:nrow(df), df)
}

add.p.z <- function(df, z.col = 3, p.already=FALSE){
                              if(p.already==FALSE){
                data.frame(n.r=1:nrow(df), df, p.z=2 * (1 - pnorm(abs(df[,z.col]))))}else{
                data.frame(n.r=1:nrow(df), df, p.z=df[,3])
                                                }
           }

add.sig <- function(df, Pr.col = 5){data.frame(df,sig=ifelse(df[,Pr.col]<0.001,"***",
                                                            ifelse(df[,Pr.col]<0.01,"**",
                                                                   ifelse(df[,Pr.col]<0.05,"*",
                                                                          ifelse(df[,Pr.col]<0.1,"†","")))))}

format.reg.table.survival <- function(df, d=d){
  df <- data.frame(n.r=1:nrow(df),df)
  test <- cbind(var=rownames(df),df[,c(1,3:4,6)])
  digits <- function(x,d){ if(class(x)=="numeric") {formatC(x, format = "f", digits = d)} else{x} }
  test <- data.frame(purrr::map(test,digits,d))
  test <- tidyr::unite(test, coef,3,5,sep="")
  test[,4]  <- paste0("(",test[,4],")",sep="")
  reg.table <- dplyr::arrange(tidyr::gather(test,key,beta,-c(var,n.r)),n.r) %>% dplyr::select(2,1,4)
  even.row <- rep(c(FALSE,TRUE),nrow(reg.table)/2)
  reg.table$var <- as.character(reg.table$var)
  reg.table$var[even.row] <- ""
  return(reg.table)
}
suppressWarnings(warning("format.reg.table"))

format.reg.table <- function(df, d=3){
  test <- cbind(var=rownames(df),df[,c(1:3,6)])
  digits <- function(x,d){ if(class(x)=="numeric") {formatC(x, format = "f", digits = d)} else{x} }
  test <- data.frame(purrr::map(test,digits,d)) %>% tidyr::unite(Estimate,3,5,sep="")
  test[,4]  <- paste0("(",test[,4],")",sep="")
  reg.table <- dplyr::arrange(tidyr::gather(test,key,beta,-c(var,n.r)),n.r) %>% select(2,1,4)
  even.row <- rep(c(FALSE,TRUE),nrow(reg.table)/2)
  reg.table$var <- as.character(reg.table$var)
  reg.table$var[even.row] <- ""
  return(reg.table)
}
