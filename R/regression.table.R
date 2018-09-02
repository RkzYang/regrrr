reg.table <- function(lm.obj, ...){
  return(summary(lm.obj) %>% coef %>% as.data.frame %>% add.p.z %>% add.sig)}

# updated 9/2/2018 #
reg.combine <- function(tbl_1, tbl_2, tbl_3=NULL, tbl_4=NULL, tbl_5=NULL, tbl_6=NULL, tbl_7=NULL, tbl_8=NULL, tbl_9=NULL, tbl_10=NULL) {
  all_tbls <- list(tbl_1,tbl_2,tbl_3,tbl_4,tbl_5,tbl_6,tbl_7,tbl_8,tbl_9,tbl_10)
  non_empty <- 10 - sum(unlist(purrr::map(all_tbls, is.null)))
  list_tbls <- all_tbls[1:non_empty]
  for(i in 1:length(list_tbls)){
    list_tbls[[i]]$var[seq(2, nrow(list_tbls[[i]]), by = 2)] <- paste0(list_tbls[[i]]$var[seq(1, nrow(list_tbls[[i]]), by = 2)],"s.e.")
    list_tbls[[i]]$n.r <- NULL
  }
  main.table <- list_tbls %>%
    purrr::reduce(dplyr::full_join,by=c("var"))
  main.table[is.na(main.table)] <- ""
  main.table$var[seq(2, nrow(main.table), by = 2)] <- ""
  names(main.table) <- c("Variables", paste0("Model ", 0:(non_empty-1))) 
  return(main.table)}

# updated 9/2/2018 #
mod.compare <- function(model1, model2, model3=NULL, model4=NULL, model5=NULL, model6=NULL, model7=NULL, model8=NULL, model9=NULL, model10=NULL,
                        likelihood.only = FALSE, round.digit = 3){
  
  if(likelihood.only == TRUE){
    list_all <- list(model1, model2, model3, model4, model5, model6, model7, model8, model9, model10)
    non_empty <- 10 - sum(unlist(purrr::map(list_all, is.null)))
    model_list <- list_all[1:non_empty]
    compare.df <- c("LogLikihood", round(unlist(purrr::map(model_list, logLik)), round.digit))
  }else
  {
    
  compare <- suppressWarnings(suppressMessages(anova(model1, model2, model3, model4, model5, model6, model7, model8, model9, model10)))
  ch          <- function(x){as.character(x)}
  convert.sig <- function(df){ifelse(df<0.001,"***",ifelse(df<0.01,"**",ifelse(df<0.05,"*",ifelse(df<0.1,"†",""))))}
  dg          <- function(x)formatC(x, format = "f", digits = 3)
  model.list  <- list(model1, model2, model3, model4, model5,model6,model7,model8)
  model.list  <- model.list[1:sum(!unlist((purrr::map(model.list, is.null))))]

  compare.df <- if(class(model1)=="lm"){
    data.frame(R_squared=ch(t(dg(unlist(purrr::map(purrr::map(model.list, summary), function(df){df$r.squared}))))),
               Adj_R_squared=ch(t(dg(unlist(purrr::map(purrr::map(model.list, summary), function(df){df$adj.r.squared}))))),
               F=ch(t(dg(compare[5][[1]]))),
               sig=ch(t(convert.sig(compare[6][[1]])))) %>% tidyr::unite(F,F,sig,sep="") %>% t()
  }else{
    data.frame(AIC=ch(t(dg(compare[2][[1]]))),BIC=ch(t(dg(compare[3][[1]]))),
               Log_Likelihood=ch(t(dg(compare[4][[1]]))),Chisq=ch(t(dg(compare[6][[1]]))),
               sig=ch(t(convert.sig(compare[8])))) %>% tidyr::unite(Chisq,Chisq,sig,sep="") %>% t()
  }
  compare.df[nrow(compare.df),1] <- ""
  compare.df <- data.frame(Variables=row.names(compare.df),compare.df)
  }
  names(compare.df) <- c("Variables", paste0("Model ", 0:(non_empty-1))) 
  return(compare.df)
}
