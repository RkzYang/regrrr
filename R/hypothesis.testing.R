#' significance of regression slope (the marginal effect) under moderation
#' testing restriction: the sig. of beta_x under the moderation of z1, with or without additional interaction terms, Aug 13th
#'
#' @param reg.coef a data.frame (or matrix) of regression result or a coeftest object, e.g. summary(lm_model)$coef, coeftest(lm_model, cluster.vcov(lm_model, cbind(data$group1,  data$group2)))
#' @param all.vcv the complete variance-covariance matrix
#' @param model the model object, such as a "lm" object
#' @param x_var.name main independent variable name in model, a string
#' @param moderator.name moderator name in model, a string
#' @param mod.n.sd specify the strength of the moderating effects, in the unit of s.d.s of the moderator, which can take negative values
#' @param data data used for regression
#' @param t.value.col col number of the t-score in reg.coef 
#' @param Pr.col col number of the Prob.(>|t|)) in reg.coef 
#' 
#' @examples 
#' data(mtcars)
#' m1 <- lm(mpg ~ vs + carb + hp + wt + wt * hp , data = mtcars)
#' test_tilted_slopes(reg.coef = summary(m1)$coef, model = m1, 
#'                    x_var.name = "wt", moderator.name = "hp", data = mtcars)
#' 
#' @importFrom stats pt vcov
#' @import purrr
#' @import stringr
#' @export
test_tilted_slopes <- function(reg.coef, all.vcv = NULL, model, x_var.name, moderator.name, mod.n.sd = 1, data, t.value.col = 3, Pr.col = 4){
  # m <- m2 # m is the regression result
  # mod_name <- "post.StrFoc" # moderator name in model
  # model <- H.01 # original model (such as a "lm" object)
  # data <- Event.CAR # full data set
  # extract vcov among related beta's (of main effect and the interaction terms)
  # all.vcv <- cluster.vcov(model, cbind( Event.CAR$Acquiror.fed_rssd,  Event.CAR$Quarter.Announced))
  
  if(!class(reg.coef) %in% c("matrix", "data.frame", "coeftest")){stop("reg.coef needs to be a data.frame or an coeftest object")}
  
  if(class(reg.coef) == "coeftest"){
    reg.coef <- as.data.frame(`[`(reg.coef))
  }
  
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
  m <- reg.coef
  interaction.names <- row.names(m)[stringr::str_detect(row.names(m), pattern=":")]
  main.name <- x_var.name
  mod_name <- moderator.name
  all.mod.names <- purrr::map(stringr::str_split(interaction.names, ":"), function(x){x[1]}) %>% unlist()
  other.mod.names <- all.mod.names[! all.mod.names %in% c(main.name, mod_name)]
  focal.interaction.name <- interaction.names[stringr::str_detect(interaction.names, pattern = mod_name)]
  other.interaction.name <- interaction.names[!stringr::str_detect(interaction.names, pattern = mod_name)]
  (related.var.position <- c(which(row.names(m) == main.name), which(row.names(m) == focal.interaction.name), which(row.names(m) %in% other.interaction.name)))
  if(is.null(all.vcv)){
    all.vcv <- stats::vcov(model)
  }
  betas.vcv <- all.vcv[related.var.position, related.var.position]
  # fix the intrested moderator to high and low level
  mod_vec <- unlist(data[mod_name])
  mod_high <- mean(mod_vec) + mod.n.sd*sd(mod_vec)
  mod_low <- mean(mod_vec) - mod.n.sd*sd(mod_vec)
  # fixed other moderators to mean
  other.mod.fexied.to.means <- colMeans(data[other.mod.names], na.rm = TRUE)
  # "a" vector
  (a_high <- c(1, mod_high, other.mod.fexied.to.means))
  (a_low  <- c(1, mod_low,  other.mod.fexied.to.means))
  # calculate the s.e.*
  se_star_high <- sqrt(t(a_high) %*% betas.vcv %*% a_high)
  se_star_low  <- sqrt(t(a_low)  %*% betas.vcv %*% a_low)
  # calculate the beta*
  beta_star_high <- t(a_high) %*% m[related.var.position, "Estimate"]
  beta_star_low  <- t(a_low)  %*% m[related.var.position, "Estimate"]
  # find sig for t dist.
  sig <- function(beta, se){data.frame(t_value = beta/se, 
                                       p. = 2*pt(-abs(beta/se), df = nrow(data)-1))}
  result <- purrr::pmap(list(beta=list(beta_star_high, beta_star_low), se=list(se_star_high, se_star_low)), sig)
  names(result) <- c("high_level", "low_level")
  return(result)
}

#' testing equality of two coefficients (difference between coefficients of regressors), a Wald test
#' note: if v is not alternatively specified, use car::linearHypothesis(lm_model, "X1 = X2")
#'
#' @param model the model object, such as a "lm" object
#' @param var1.name X1 name in model, a string
#' @param var2.name X2 name in model, a string
#' @param v a customized variance-covariance matrix
#' 
#' data(mtcars)
#' m1 <- lm(mpg ~ vs + carb + hp + wt + wt * hp , data = mtcars)
#' summary(m1)
#' test_coef_equality(model = m1, var1.name = "carb", var2.name = "hp")
#' 
#' @importFrom stats pnorm vcov coef
#' @import stringr
#' @export
test_coef_equality = function(model, var1.name, var2.name, v = NULL){
  
  betas <- coef(model)
  (beta_var1 <- betas[stringr::str_detect(names(betas), pattern = var1.name)][1])
  (beta_var2 <- betas[stringr::str_detect(names(betas), pattern = var2.name)][1])
  dif <- as.numeric( beta_var1 - beta_var2 )
  
  if(is.null(v)){
    v <- vcov(model)  
  }
  v.var1.p <- which(stringr::str_detect(row.names(v), pattern = var1.name))[1]
  v.var2.p <- which(stringr::str_detect(row.names(v), pattern = var2.name))[1]
  dif.se <- as.numeric( sqrt( v[v.var1.p, v.var1.p] + v[v.var2.p, v.var2.p] - 2*v[v.var1.p, v.var2.p] ) )
  p.norm <- 2*(1 - pnorm(abs(dif/dif.se)))
  names(p.norm) <- "p.norm"
  return(p.norm)}
