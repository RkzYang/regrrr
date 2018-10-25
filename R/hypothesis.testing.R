#' significance of slope under moderation
#' testing restriction: the sig. of beta_x under the moderation of z1, with or without additional interaction terms, Aug 13th
#'
#' @param m a data.frame of regression result
#' @param model the model object, such as a "lm" object
#' @param mod_name moderator name in model, a string
#' @param mod.n.sd specify the strength of the moderating effects, in the unit of s.d.s of the moderator, which can take negative values
#' @param full.data data used for regression
#' @export
slope.sig_after.mod <- function(m, model, mod_name, mod.n.sd = 1, full.data){
  # m <- m2 # m is the regression result
  # mod_name <- "post.StrFoc" # moderator name in model
  # model <- H.01 # original model (such as a "lm" object)
  # full.data <- Event.CAR # full data set
  # extract vcov among related beta's (of main effect and the interaction terms)
  all.vcv <- cluster.vcov(model, cbind( Event.CAR$Acquiror.fed_rssd,  Event.CAR$Quarter.Announced))
  interaction.names <- row.names(m)[stringr::str_detect(row.names(m), pattern=":")]
  main.name <- stringr::str_split(interaction.names, ":")[[1]][2]
  all.mod.names <- purrr::map(stringr::str_split(interaction.names, ":"), function(x){x[1]}) %>% unlist()
  other.mod.names <- all.mod.names[! all.mod.names %in% mod_name]
  focal.interaction.name <- interaction.names[stringr::str_detect(interaction.names, pattern = mod_name)]
  other.interaction.name <- interaction.names[!stringr::str_detect(interaction.names, pattern = mod_name)]
  (related.var.position <- c(which(row.names(m) == main.name), which(row.names(m) == focal.interaction.name), which(row.names(m) %in% other.interaction.name)))
  betas.vcv <- all.vcv[related.var.position,related.var.position]
  # fix the intrested moderator to high and low level
  mod_vec <- unlist(full.data[mod_name])
  mod_high <- mean(mod_vec) + mod.n.sd*sd(mod_vec)
  mod_low <- mean(mod_vec) - mod.n.sd*sd(mod_vec)
  # fixed other moderators to mean
  other.mod.fexied.to.means <- colMeans(full.data[other.mod.names],na.rm = TRUE)
  # "a" vector
  (a_high <- c(1, mod_high, other.mod.fexied.to.means))
  (a_low  <- c(1, mod_low,  other.mod.fexied.to.means))
  # calculate the s.e.*
  se_star_high <- t(a_high) %*% betas.vcv %*% a_high %>% sqrt()
  se_star_low <- t(a_low) %*% betas.vcv %*% a_low   %>% sqrt()
  # calculate the beta*
  beta_star_high <- t(a_high) %*% m[related.var.position,2]
  beta_star_low  <- t(a_low) %*% m[related.var.position,2]
  # find sig for t dist.
  sig <- function(beta, se){data.frame(t = beta/se, p. = pt(beta/se, df = nrow(full.data)-1, lower.tail = FALSE)*2)}
  result <- purrr::pmap(list(beta=list(beta_star_high, beta_star_low), se=list(se_star_high, se_star_low)), sig)
  names(result) <- c("high_level", "low_level")
  return(result)
}

#' testing equality of two coefficients, a Wald test
#'
#' @param model the model object, such as a "lm" object
#' @param var1.name X1 name in model, a string
#' @param var2.name X2 name in model, a string
#' @param v a customized variance-covariance matrix
#' @export
coef.equality.sig = function(model, var1.name, var2.name, v = NULL){
  
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
