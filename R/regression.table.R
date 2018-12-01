#' Convert a regression result into a clean data.frame
#'
#' @param lm.obj a regression result object
#' @importFrom stats coef
#' @export
reg.table <- function(lm.obj){
  
  add.p.z <- function(df, z.col = 3, p.already=FALSE){
    if(p.already==FALSE){
      data.frame(n.r=1:nrow(df), df, p.z=2 * (1 - pnorm(abs(df[,z.col]))))}else{
        data.frame(n.r=1:nrow(df), df, p.z=df[,3])
      }
  }
  
  add.sig <- function(df, Pr.col = 5){data.frame(df, sig=ifelse(df[,Pr.col]<0.001, paste0(rep("\x2a", 3), collapse = ""),
                                                                ifelse(df[,Pr.col]<0.01, paste0(rep("\x2a", 2), collapse = ""),
                                                                       ifelse(df[,Pr.col]<0.05, paste0(rep("\x2a", 1), collapse = ""),
                                                                              ifelse(df[,Pr.col]<0.1,"\xe2\x80\xa0","")))))}
  
  return(summary(lm.obj) %>% stats::coef %>% as.data.frame %>% add.p.z %>% add.sig)}

#' Combine regression results from different models by columns # updated 9/2/2018 #
#'
#' @param tbl_1  the 1st  data.frame of regression result
#' @param tbl_2  the 2nd  data.frame of regression result
#' @param tbl_3  the 3rd  data.frame of regression result
#' @param tbl_4  the 4th  data.frame of regression result
#' @param tbl_5  the 5th  data.frame of regression result
#' @param tbl_6  the 6th  data.frame of regression result
#' @param tbl_7  the 7th  data.frame of regression result
#' @param tbl_8  the 8th  data.frame of regression result
#' @param tbl_9  the 9th  data.frame of regression result
#' @param tbl_10 the 10th data.frame of regression result
#' @import purrr
#' @export
reg.combine <- function(tbl_1, tbl_2, tbl_3=NULL, tbl_4=NULL, tbl_5=NULL, tbl_6=NULL, tbl_7=NULL, tbl_8=NULL, tbl_9=NULL, tbl_10=NULL) {
  all_tbls <- list(tbl_1,tbl_2,tbl_3,tbl_4,tbl_5,tbl_6,tbl_7,tbl_8,tbl_9,tbl_10)
  non_empty <- 10 - sum(unlist(purrr::map(all_tbls, is.null)))
  list_tbls <- all_tbls[1:non_empty]
  for(i in 1:length(list_tbls)){
    list_tbls[[i]]$var_[seq(2, nrow(list_tbls[[i]]), by = 2)] <- paste0(list_tbls[[i]]$var_[seq(1, nrow(list_tbls[[i]]), by = 2)],"s.e.")
    list_tbls[[i]]$n.r <- NULL
  }
  main.table <- list_tbls %>%
    purrr::reduce(dplyr::full_join,by=c("var_"))
  main.table[is.na(main.table)] <- ""
  main.table$var_[seq(2, nrow(main.table), by = 2)] <- ""
  names(main.table) <- c("Variables", paste0("Model ", 0:(non_empty-1))) 
  return(main.table)}


#' Compare regression models, which is compatible with the reg.table output # updated 9/13/2018 #
#'
#' @param model1  the 1st  regression model
#' @param model2  the 2nd  regression model
#' @param model3  the 3rd  regression model
#' @param model4  the 4th  regression model
#' @param model5  the 5th  regression model
#' @param model6  the 6th  regression model
#' @param model7  the 7th  regression model
#' @param model8  the 8th  regression model
#' @param model9  the 9th  regression model
#' @param model10 the 10th regression model
#' @param likelihood.only whether or not to output the likelihood
#' @param round.digit number of decimal places to retain
#' @param main.effect.only specify col number of alternative main-effect models, if any
#' @param intn.effect.only specify col number of alternative moderator models, if any
#' @importFrom stats anova logLik
#' @import purrr 
#' @import MuMIn
#' @export
mod.compare <- function(model1, model2, model3=NULL, model4=NULL, model5=NULL, model6=NULL, model7=NULL, model8=NULL, model9=NULL, model10=NULL,
                        likelihood.only = FALSE, round.digit = 3, 
                        main.effect.only = NULL,
                        intn.effect.only = NULL){
  
  list_all <- list(model1, model2, model3, model4, model5, model6, model7, model8, model9, model10)
  non_empty <- 10 - sum(unlist(purrr::map(list_all, is.null)))
  model_list <- list_all[1:non_empty]
  
  if(likelihood.only == TRUE){
    compare.df <- c("LogLikelihood", round(unlist(purrr::map(model_list, logLik)), round.digit))
  }else
  {
  
  n <- non_empty
  compare <- if(n==2){
    suppressWarnings(suppressMessages(anova(model1,model2)))}else if(n==3){
      suppressWarnings(suppressMessages(anova(model1,model2,model3)))}else if(n==4){
        suppressWarnings(suppressMessages(anova(model1,model2,model3,model4)))}else if(n==5){
          suppressWarnings(suppressMessages(anova(model1,model2,model3,model4,model5)))}else if(n==6){
            suppressWarnings(suppressMessages(anova(model1,model2,model3,model4,model5,model6)))}else if(n==7){
              suppressWarnings(suppressMessages(anova(model1,model2,model3,model4,model5,model6,model7)))}else if(n==8){
                suppressWarnings(suppressMessages(anova(model1,model2,model3,model4,model5,model6,model7,model8)))}else if(n==9){
                  suppressWarnings(suppressMessages(anova(model1,model2,model3,model4,model5,model6,model7,model8,model9)))}else if(n==10){
                    suppressWarnings(suppressMessages(anova(model1,model2,model3,model4,model5,model6,model7,model8,model9,model10)))}
  
    if(!is.null(main.effect.only)){
      for(i in main.effect.only){
        compare[i,] <- anova(model_list[[main.effect.only[1]-2]], model_list[[i]])[2,]
      }
    }
    if(!is.null(intn.effect.only)){
      for(i in intn.effect.only){
        compare[i,] <- if(!is.null(main.effect.only)){anova(model_list[[i-length(main.effect.only)]], model_list[[i]])[2,]
        }else{
          anova(model_list[[intn.effect.only[1] - 1]], model_list[[i]])[2,]
        }
      }
    }
  
  ch          <- function(x){as.character(x)}
  convert.sig <- function(df){ifelse(df<0.001, paste0(rep("\x2a", 3), collapse = ""),
                                     ifelse(df<0.01, paste0(rep("\x2a", 2), collapse = ""),
                                            ifelse(df<0.05, paste0(rep("\x2a", 1), collapse = ""),
                                                   ifelse(df<0.1,"\xe2\x80\xa0",""))))}
  dg          <- function(x)formatC(x, format = "f", digits = 3)
  
  
  compare.df <- if(class(model1)[1]=="negbin"){
    data.frame(AIC=ch(t(dg(unlist(purrr::map(model_list, MuMIn::AICc))))),
               Log_Likelihood=ch(t(dg(compare[3][[1]]))), Chisq=ch(t(dg(compare[7][[1]]))),
               sig=ch(t(convert.sig(compare[8][[1]])))) %>% unite(Chisq,Chisq,sig,sep="") %>% t()
  }else if(class(model1)[1]=="lmerMod"){
    data.frame(AIC=ch(t(dg(unlist(purrr::map(model_list, MuMIn::AICc))))),
               Log_Likelihood=ch(t(dg(compare[4][[1]]))), Chisq=ch(t(dg(compare[6][[1]]))),
               sig=ch(t(convert.sig(compare[8][[1]])))) %>% unite(Chisq,Chisq,sig,sep="") %>% t()
  }else if(class(model1)[1]=="lm"){
    data.frame(R_squared=ch(t(dg(unlist(purrr::map(model_list, function(df){summary(df)$r.squared}))))),
               Adj_R_squared=ch(t(dg(unlist(purrr::map(model_list, function(df){summary(df)$adj.r.squared}))))),
               Delta_F=ch(t(dg(compare[5][[1]]))),
               sig=ch(t(convert.sig(compare[6][[1]])))) %>% tidyr::unite(Delta_F,Delta_F,sig,sep="") %>% t()
  }else{
    data.frame(AIC=ch(t(dg(unlist(purrr::map(model_list, MuMIn::AICc))))),
               Log_Likelihood=ch(t(dg(compare[1][[1]]))),Chisq=ch(t(dg(compare[2][[1]]))),
               sig=ch(t(convert.sig(compare[4][[1]])))) %>% unite(Chisq,Chisq,sig,sep="") %>% t() }
  
  compare.df[nrow(compare.df),1] <- ""
  compare.df <- data.frame(Variables=row.names(compare.df),compare.df) 
  }
  names(compare.df) <- c("Variables", paste0("Model ", 0:(non_empty-1))) 
  return(compare.df)
}