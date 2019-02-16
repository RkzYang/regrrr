#' Convert the regression result to the long format: the standard errors are in parentheses and beneath the betas
#'
#' @param reg.coef a data.frame of regression result, e.g. summary(lm_model)$coef
#' @param d number of decimal places to retain
#' @param t.value.col col number of the t-score in the reg.coef data.frame
#' @param Pr.col col number of the Prob.(>|t|)) in the reg.coef data.frame
#' 
#' @examples
#' data(mtcars)
#' m1 <- lm(mpg ~ vs + carb + hp + wt + wt * hp , data = mtcars)
#' to_long_tab(reg.coef = summary(m1)$coef) 
#' 
#' @import purrr 
#' 
#' @import dplyr 
#' 
#' @import tidyr
#' 
#' @export
to_long_tab <- function(reg.coef, d = 3, t.value.col = 3, Pr.col = 4){
  if(! "n.r" %in% colnames(reg.coef)){
    reg.coef <- data.frame(n.r = 1:nrow(reg.coef), reg.coef)
  }
  
  if(! "prob." %in% colnames(reg.coef)){
    t.value.col <- t.value.col + 1
    reg.coef <- data.frame(reg.coef, prob. = 2 * (1 - pnorm(abs(reg.coef[ , t.value.col]))))
  }
  
  if(! "sig." %in% colnames(reg.coef)){
    df <- reg.coef 
    Pr.col <- Pr.col + 1
    reg.coef <- data.frame(df, sig.=ifelse(df[,Pr.col]<0.001, paste0(rep("\x2a", 3), collapse = ""),
                                           ifelse(df[,Pr.col]<0.01, paste0(rep("\x2a", 2), collapse = ""),
                                                  ifelse(df[,Pr.col]<0.05, paste0(rep("\x2a", 1), collapse = ""),
                                                         ifelse(df[,Pr.col]<0.1,"\xe2\x80\xa0","")))))
  }
  
  Estimate <- NULL
  n.r <- NULL
  var_ <- NULL
  key <- NULL
  
  test <- cbind(var_ = rownames(df), reg.coef[,c(1:3, ncol(reg.coef))])
  digits <- function(x,d){ if(class(x)=="numeric") {formatC(x, format = "f", digits = d)} else{x} }
  test <- data.frame(purrr::map(test, digits, d)) 
  test <- tidyr::unite(test, Estimate, 3, 5, sep="") # this joins the beta_hat values with sig. marks
  test[,4]  <- paste0("(",test[,4],")") # this put the s.e.'s into parenthesis
  reg.table <- dplyr::arrange(tidyr::gather(test, key, beta, -c(var_, n.r)), n.r)[,c(2,1,4)]
  even.row <- rep(c(FALSE, TRUE), nrow(reg.table)/2)
  reg.table$var_ <- as.character(reg.table$var_)
  reg.table$var_[even.row] <- ""
  return(reg.table)
}

#' Combine regression results from different models by columns
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
#' @param tbl_11 the 10th data.frame of regression result
#' @param tbl_12 the 10th data.frame of regression result
#' @param tbl_13 the 10th data.frame of regression result
#' @param tbl_14 the 10th data.frame of regression result
#' @param tbl_15 the 10th data.frame of regression result
#' @param tbl_16 the 10th data.frame of regression result
#' @param tbl_17 the 10th data.frame of regression result
#' @param tbl_18 the 10th data.frame of regression result
#' @param tbl_19 the 10th data.frame of regression result
#' @param tbl_20 the 10th data.frame of regression result
#' 
#' @examples 
#' data(mtcars)
#' m1 <- lm(mpg ~ vs + carb + hp + wt + wt * hp , data = mtcars)
#' m2 <- update(m1, . ~ .+ wt * vs)
#' summary(m1)
#' summary(m2)
#' combine_long_tab(to_long_tab(summary(m1)$coef), 
#'                  to_long_tab(summary(m2)$coef))
#' 
#' @import purrr
#' @export
combine_long_tab <- function(tbl_1, tbl_2, tbl_3=NULL, tbl_4=NULL, tbl_5=NULL, tbl_6=NULL, tbl_7=NULL, tbl_8=NULL, tbl_9=NULL, tbl_10=NULL,
                        tbl_11=NULL, tbl_12=NULL, tbl_13=NULL, tbl_14=NULL, tbl_15=NULL, tbl_16=NULL, tbl_17=NULL, tbl_18=NULL, tbl_19=NULL, tbl_20=NULL) {
  all_tbls <- list(tbl_1,tbl_2,tbl_3,tbl_4,tbl_5,tbl_6,tbl_7,tbl_8,tbl_9,tbl_10,tbl_11,tbl_12,tbl_13,tbl_14,tbl_15,tbl_16,tbl_17,tbl_18,tbl_19,tbl_20)
  non_empty <- length(all_tbls) - sum(unlist(purrr::map(all_tbls, is.null)))
  list_tbls <- all_tbls[1:non_empty]
  for(i in 1:length(list_tbls)){
    list_tbls[[i]]$var_[seq(2, nrow(list_tbls[[i]]), by = 2)] <- paste0(list_tbls[[i]]$var_[seq(1, nrow(list_tbls[[i]]), by = 2)],"s.e.")
    list_tbls[[i]]$n.r <- NULL
  }
  main.table <- purrr::reduce(list_tbls, dplyr::full_join,by=c("var_"))
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
#' @param model11 the 10th regression model
#' @param model12 the 10th regression model
#' @param model13 the 10th regression model
#' @param model14 the 10th regression model
#' @param model15 the 10th regression model
#' @param model16 the 10th regression model
#' @param model17 the 10th regression model
#' @param model18 the 10th regression model
#' @param model19 the 10th regression model
#' @param model20 the 10th regression model
#' 
#' @param likelihood.only whether or not to output the likelihood
#' @param round.digit number of decimal places to retain
#' @param main.effect.only specify col number of alternative main-effect models, if any
#' @param intn.effect.only specify col number of alternative moderator models, if any
#' 
#' @examples 
#' data(mtcars)
#' m1 <- lm(mpg ~ vs + carb + hp + wt + wt * hp , data = mtcars)
#' m2 <- update(m1, . ~ .+ wt * vs)
#' compare_models(m1, m2)
#' 
#' @importFrom stats anova logLik
#' @import purrr 
#' @import MuMIn
#' @export
compare_models <- function(model1, model2, model3=NULL, model4=NULL, model5=NULL, model6=NULL, model7=NULL, model8=NULL, model9=NULL, model10=NULL,
                        model11=NULL, model12=NULL, model13=NULL, model14=NULL, model15=NULL, model16=NULL, model17=NULL, model18=NULL, model19=NULL, model20=NULL,
                        likelihood.only = FALSE, round.digit = 3, 
                        main.effect.only = NULL,
                        intn.effect.only = NULL){
  
  list_all <- list(model1, model2, model3, model4, model5, model6, model7, model8, model9, model10, model11, model12, model13, model14, model15, model16, model17, model18, model19, model20)
  non_empty <- length(list_all) - sum(unlist(purrr::map(list_all, is.null)))
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
                    suppressWarnings(suppressMessages(anova(model1,model2,model3,model4,model5,model6,model7,model8,model9,model10)))}else if(n==11){
                      suppressWarnings(suppressMessages(anova(model1,model2,model3,model4,model5,model6,model7,model8,model9,model10,model11)))}else if(n==12){
                        suppressWarnings(suppressMessages(anova(model1,model2,model3,model4,model5,model6,model7,model8,model9,model10,model11,model12)))}else if(n==13){
                          suppressWarnings(suppressMessages(anova(model1,model2,model3,model4,model5,model6,model7,model8,model9,model10,model11,model12,model13)))}else if(n==14){
                            suppressWarnings(suppressMessages(anova(model1,model2,model3,model4,model5,model6,model7,model8,model9,model10,model11,model12,model13,model14)))}else if(n==15){
                              suppressWarnings(suppressMessages(anova(model1,model2,model3,model4,model5,model6,model7,model8,model9,model10,model11,model12,model13,model14,model15)))}else if(n==16){
                                suppressWarnings(suppressMessages(anova(model1,model2,model3,model4,model5,model6,model7,model8,model9,model10,model11,model12,model13,model14,model15,model16)))}else if(n==17){
                                  suppressWarnings(suppressMessages(anova(model1,model2,model3,model4,model5,model6,model7,model8,model9,model10,model11,model12,model13,model14,model15,model16,model17)))}else if(n==18){
                                    suppressWarnings(suppressMessages(anova(model1,model2,model3,model4,model5,model6,model7,model8,model9,model10,model11,model12,model13,model14,model15,model16,model17,model18)))}else if(n==19){
                                      suppressWarnings(suppressMessages(anova(model1,model2,model3,model4,model5,model6,model7,model8,model9,model10,model11,model12,model13,model14,model15,model16,model17,model18,model19)))}else if(n==20){
                                        suppressWarnings(suppressMessages(anova(model1,model2,model3,model4,model5,model6,model7,model8,model9,model10,model11,model12,model13,model14,model15,model16,model17,model18,model19,model20)))}
  #################
  Chisq   <- NULL #
  sig     <- NULL #
  Delta_F <- NULL #
  #################
  
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