#' plotting the marginal effect of X on Y, with or without one or multiple interaction terms
#'  
#' @param reg.coef a coefficient data.frame of regression result
#' @param data the data used in regression, a data frame
#' @param model.for.predict the model object, such as a "lm" object
#' @param by_color plot interactions by colors, otherwise by line types
#' @param x_var.name x name in the regression model, a string
#' @param y_var.name y name in the regression model, a string
#' @param moderator.name moderating variable name in the regression model, a string
#' @param min_x the min of x scale, in percentile of x
#' @param max_x the max of x scale, in percentile of x
#' @param mdrt_quantile_05 set the low level of moderator, in percentile
#' @param mdrt_quantile_50 set the middle level of moderator, in percentile
#' @param mdrt_quantile_95 set the high level of moderator, in percentile
#' @param mod.n.sd set the moderating strength, in the number of s.d. units, which can take negative values
#' @param confidence_interval if TRUE, plot confidence intervals
#' @param CI_Ribbon if TRUE, plot confidence interval ribbons, if FALSE, plot error bars
#' @param title the title of the plot
#' @param xlab label of X
#' @param ylab label of Y
#' @param moderator.lab label of moderator
#' @param mdrt.low.name the label of low-level moderator
#' @param mdrt.mid.name the label of mid-level moderator
#' @param mdrt.high.name the label of high-level moderator
#' @param y.hi.lim specify the upper limit of y
#' @param y.low.lim specify the lower limit of y
#' 
#' @examples
#' data(mtcars)
#' m1 <- lm(mpg ~ vs + carb + hp + wt + wt * hp , data = mtcars)
#' plot_effect(reg.coef = summary(m1)$coefficients, 
#'                  data = mtcars, model.for.predict = m1, 
#'                  x_var.name = "wt", y_var.name = "mpg", moderator.name = "hp", 
#'                  confidence_interval = TRUE,  CI_Ribbon = TRUE, 
#'                  xlab = "Weight", ylab = "MPG", moderator.lab = "Horsepower")
#' 
#' #' @examples
#' data(mtcars)
#' m2 <- lm(mpg ~ vs + carb + hp + wt + wt * hp + wt * vs, data = mtcars)
#' plot_effect(reg.coef = summary(m2)$coefficients, 
#'             data = mtcars, model.for.predict = m2, 
#'             x_var.name = "wt", y_var.name = "mpg", moderator.name = "hp", 
#'             confidence_interval = TRUE,  CI_Ribbon = FALSE, 
#'             xlab = "Weight", ylab = "MPG", moderator.lab = "Horsepower")
#' 
#' @examples
#'\dontrun{
#' # this shows the function is compatible with ggplot2 customization
#' library(extrafont)
#' m1 <- lm(mpg ~ vs + carb + hp + wt + wt * hp , data = mtcars)
#' plot_effect(reg.coef = summary(m1)$coefficients, 
#'                  data = mtcars, model.for.predict = m1, 
#'                  x_var.name = "wt", y_var.name = "mpg", moderator.name = "hp", 
#'                  confidence_interval = TRUE,  CI_Ribbon = TRUE, 
#'                  xlab = "Weight", ylab = "MPG", moderator.lab = "Horsepower") + 
#' ggplot2::theme(text=ggplot2::element_text(family="Times New Roman", size = 16))
#'}
#' 
#' @importFrom stats quantile median sd predict vcov model.matrix
#' @importFrom robustbase colMedians
#' @import stringr
#' @import ggplot2
#' @export
plot_effect <- function(reg.coef, data, model.for.predict, by_color=FALSE, x_var.name = NULL, y_var.name = NULL, moderator.name=NULL, 
                              min_x=0.001, max_x=0.999, 
                              mdrt_quantile_05=NULL, mdrt_quantile_50=NULL, mdrt_quantile_95=NULL, 
                              mod.n.sd=1,
                              confidence_interval = FALSE, CI_Ribbon = FALSE,
                              title=NULL, xlab="X_Var.name", ylab="Y_Var.name", moderator.lab="Moderator_name",
                              mdrt.low.name="Low", mdrt.mid.name=NULL, mdrt.high.name="High",
                              y.hi.lim=NULL, y.low.lim=NULL){
  
  tryCatch({reg.result <- as.data.frame(reg.coef)}, error=function(e){cat("ERROR :", conditionMessage(e), "\n")})
  
  if(is.null(x_var.name)){stop("what is the X variable name (x_var.name) in the model?")}
  if(is.null(y_var.name)){stop("what is the Y variable name (y_var.name) in the model?")}
  if(is.null(moderator.name)){warning("no moderating variable is specified; only plotting the main effect")}
  
  df <- as.data.frame(data)

  factor.names <- rownames(reg.result)[stringr::str_detect(rownames(reg.result), pattern = "factor")]
  factor.name <- unique(unlist(purrr::map(factor.names, function(x) stringr::str_extract(x, pattern = "(?<=\\().*?(?=\\))")[1])))

  non.factor.names <- names(df)[names(df) %in% rownames(reg.result)]

  # 1 the beta values of interested variables
  main1.r <- which(rownames(reg.result) == x_var.name)
  mdrt.r  <- which(rownames(reg.result) == moderator.name) 

  # 2 set moderator levels (low, mid, high)
  if(is.null(mdrt.r)){
    mdrt.low  <- 0
    mdrt.mid  <- 0
    mdrt.high <- 0
  }else if(!is.null(mdrt_quantile_05))
  {
    mod.name  <- rownames(reg.result)[mdrt.r]
    mdrt.low  <- quantile(unlist(df[mod.name]), probs=mdrt_quantile_05, na.rm=TRUE)
    if(!is.null(mdrt_quantile_50)){mdrt.mid  <- quantile(unlist(df[mod.name]), probs=mdrt_quantile_50, na.rm=TRUE)}
    mdrt.high <- quantile(unlist(df[mod.name]), probs=mdrt_quantile_95, na.rm=TRUE)
  }else{
    mod.name  <- rownames(reg.result)[mdrt.r]
    mdrt.low  <- mean(unlist(df[mod.name]), na.rm=TRUE) - mod.n.sd*sd(unlist(df[mod.name]), na.rm=TRUE)
    mdrt.mid  <- mean(unlist(df[mod.name]), na.rm=TRUE) 
    mdrt.high <- mean(unlist(df[mod.name]), na.rm=TRUE) + mod.n.sd*sd(unlist(df[mod.name]), na.rm=TRUE)
  }
  
  # 3 adjust the margin of x axis
  min.x <- quantile(unlist(df[rownames(reg.result)[main1.r]]), probs=min_x, rm.na=TRUE) %>% as.numeric()
  max.x <- quantile(unlist(df[rownames(reg.result)[main1.r]]), probs=max_x, rm.na=TRUE) %>% as.numeric()
  
  # 4 make a hypothetical dataset (all other variables other than x and moderator are held to median or mean) for plotting
  main.x.name <- x_var.name
  modrtr.name <- moderator.name
  
  df.factor.var <- df[, factor.name]
  other_continuous_var_names <- non.factor.names[! non.factor.names %in% c(main.x.name, modrtr.name)]
  df.other.var <- df[, other_continuous_var_names]
  
  # if there is only one "factor" variable
  if(length(factor.name) == 1){
    df.fake.factor.var <- data.frame(x = rep(df.factor.var[1],150))
    names(df.fake.factor.var) <- factor.name
  }else{
  df.fake.factor.var <- df.factor.var[rep(1,150),]}
  
  # if there is only one "other" (continuous) variable
  if(class(df.other.var) == "numeric"){
    if(!is.null(mdrt_quantile_05))
    {
      df.fake.other.var <- data.frame(x = replicate(150, robustbase::colMedians(as.matrix(df.other.var), na.rm = TRUE)))
      names(df.fake.other.var) <- other_continuous_var_names
    }else{
      df.fake.other.var <- data.frame(x = replicate(150, colMeans(as.matrix(df.other.var), na.rm = TRUE)))
      names(df.fake.other.var) <- other_continuous_var_names
         }
  }else{
    if(!is.null(mdrt_quantile_05))
    {
      df.fake.other.var <- data.frame(t(replicate(150, robustbase::colMedians(as.matrix(df.other.var), na.rm = TRUE))))
    }else{
      df.fake.other.var <- data.frame(t(replicate(150, colMeans(as.matrix(df.other.var), na.rm = TRUE))))
         }
    }
  
  if(length(factor.name) > 0){
    df.fake.other.var <- cbind(df.fake.factor.var, df.fake.other.var)
  }
  
  # contruct df.fake
  if(!is.null(mdrt.mid.name)){
      df.fake <- data.frame(x = rep(seq(min.x, max.x, length=50), 3), 
                            mod = rep(c(mdrt.low, mdrt.mid, mdrt.high), each=50), 
                            mod.level = rep(c(mdrt.low.name, mdrt.mid.name, mdrt.high.name), each=50),
                            df.fake.other.var)
  }else{
      df.fake <- data.frame(x = rep(seq(min.x, max.x, length=50), 2), 
                            mod = rep(c(mdrt.low, mdrt.high), each=50),
                            mod.level = rep(c(mdrt.low.name, mdrt.high.name), each=50),
                            df.fake.other.var[1:100,])
    }
  names(df.fake)[1:2] <- c(main.x.name, modrtr.name) 
  
  if(class(df.other.var) == "numeric" && length(factor.name) == 0){
    names(df.fake)[which(stringr::str_detect(names(df.fake), "df.fake.other.var"))] <- other_continuous_var_names
  }
  
  df.fake$y_hat <- predict(model.for.predict, df.fake, type = "response")
  names(df.fake)[length(names(df.fake))] <- y_var.name
  # add confidence intervals
  # df.fake <- cbind(df.fake, predict(model.for.predict, df.fake, interval = "confidence")[, 2:3])
  
  # 5 plot
  # requireNamespace(ggplot2)    
  # requireNamespace(extrafont)
  # requireNamespace(ggthemes)
  
  if(confidence_interval == FALSE){
  if(by_color == FALSE){
    p <-  ggplot2::ggplot(df.fake, ggplot2::aes_string(x=x_var.name, y=y_var.name)) +
      ggplot2::geom_line(ggplot2::aes(linetype = mod.level)) +
      ggplot2::scale_x_continuous(limits=c(min.x, max.x), xlab) +
      ggplot2::scale_y_continuous(limits=c(y.low.lim, y.hi.lim), ylab)
    
    if(!is.null(mdrt.mid.name)){
      p <- p + ggplot2::scale_linetype_manual(moderator.lab, values=c("solid", "dashed", "dotted"))
    }else{
      p <- p + ggplot2::scale_linetype_manual(moderator.lab, values=c("solid", "dotted"))
    }
    
  }else{
    p <-  ggplot2::ggplot(df.fake, ggplot2::aes_string(x=x_var.name, y=y_var.name)) +
      ggplot2::geom_line(ggplot2::aes(color = mod.level)) +
      ggplot2::scale_x_continuous(limits=c(min.x, max.x), xlab) +
      ggplot2::scale_y_continuous(limits=c(y.low.lim, y.hi.lim), ylab)
    
    if(!is.null(mdrt.mid.name)){
      p <- p + ggplot2::scale_colour_manual(moderator.lab, values = c("red", "blue", "black"))
    }else{
      p <- p + ggplot2::scale_colour_manual(moderator.lab, values = c("red", "black"))
    }
   }
  }else{ 
  # plotting confidence intervals (of the regression slope)
    
  # calculate CI mannually
  # ref: https://stackoverflow.com/questions/14033551/r-plotting-confidence-bands-with-ggplot
  # construct the lwr_ and upr_ into df.fake
  # V <- vcov(model.for.predict)
  # design_names <- colnames(model.matrix(model.for.predict))
  # df.fake[, design_names[1]] <- rep(1, nrow(df.fake))
  # 
  # df.fake[, paste0(x_var.name, ":", moderator.name)] <- df.fake[, x_var.name] * df.fake[, moderator.name]
  # df.fake[, paste0(moderator.name, ":", x_var.name)] <- df.fake[, moderator.name] * df.fake[, x_var.name]
  # 
  # X <- as.matrix(df.fake[, design_names])
  # 
  # df.fake$fit <- predict(model.for.predict, df.fake)
  # 
  # se.fit <- sqrt(diag(X %*% V %*% t(X)))
  # df.fake$lwr_ <- df.fake$fit - 1.96*se.fit
  # df.fake$upr_ <- df.fake$fit + 1.96*se.fit
  
  predicted <- data.frame(predict(model.for.predict, df.fake, interval = "confidence"))[, 2:3]
  names(predicted) <- c("lwr_", "upr_")
  df.fake <- cbind(df.fake, predicted)
  
  # avoid note
  lwr_ <- df.fake$lwr_ 
  upr_ <- df.fake$upr_ 
  mod.level <- df.fake$mod.level
  
  if(by_color == FALSE){
  if(CI_Ribbon == FALSE){
    p <- ggplot2::ggplot(df.fake, ggplot2::aes_string(x=x_var.name, y=y_var.name, fill = "mod.level")) +
         ggplot2::guides(fill=FALSE) +
         ggplot2::geom_line(ggplot2::aes(linetype = mod.level)) +
         ggplot2::scale_x_continuous(limits=c(min.x, max.x), xlab) +
         ggplot2::scale_y_continuous(limits=c(y.low.lim, y.hi.lim), ylab) +
         ggplot2::geom_errorbar(ggplot2::aes(ymin = lwr_, ymax = upr_), alpha = 0.16)
  }else{
    p <- ggplot2::ggplot(df.fake, ggplot2::aes_string(x=x_var.name, y=y_var.name, fill = "mod.level")) +
        ggplot2::guides(fill=FALSE) +
        ggplot2::geom_line(ggplot2::aes(linetype = mod.level)) +
        ggplot2::scale_x_continuous(limits=c(min.x, max.x), xlab) +
        ggplot2::scale_y_continuous(limits=c(y.low.lim, y.hi.lim), ylab) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = lwr_, ymax = upr_), alpha = 0.6) +
        ggplot2::scale_fill_grey()
    }
    
    if(!is.null(mdrt.mid.name)){
      p <- p + ggplot2::scale_linetype_manual(moderator.lab, values=c("solid", "dashed", "dotted"))
    }else{
      p <- p + ggplot2::scale_linetype_manual(moderator.lab, values=c("solid", "dotted"))
    }
  }else{
    if(CI_Ribbon == FALSE){
    p <- ggplot2::ggplot(df.fake, ggplot2::aes_string(x=x_var.name, y=y_var.name, fill = "mod.level")) +
      ggplot2::guides(fill=FALSE) +
      ggplot2::geom_line(ggplot2::aes(color = mod.level)) +
      ggplot2::scale_x_continuous(limits=c(min.x, max.x), xlab) +
      ggplot2::scale_y_continuous(limits=c(y.low.lim, y.hi.lim), ylab) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = lwr_, ymax = upr_), alpha = 0.16)
    }else{
    p <- ggplot2::ggplot(df.fake, ggplot2::aes_string(x=x_var.name, y=y_var.name, fill = "mod.level")) +
      ggplot2::guides(fill=FALSE) +
      ggplot2::geom_line(ggplot2::aes(color = mod.level)) +
      ggplot2::scale_x_continuous(limits=c(min.x, max.x), xlab) +
      ggplot2::scale_y_continuous(limits=c(y.low.lim, y.hi.lim), ylab) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = lwr_, ymax = upr_), alpha = 0.6)
    }
    
    if(!is.null(mdrt.mid.name)){
      p <- p + ggplot2::scale_colour_manual(moderator.lab, values = c("red", "blue", "black"))
    }else{
      p <- p + ggplot2::scale_colour_manual(moderator.lab, values = c("red", "black"))
    }
   }
  }
  # customize #
  p <- p + ggplot2::theme_light() + 
   # ggplot2::theme(text=ggplot2::element_text(family="Times New Roman", size = 16)) + # library(extrafont)
    ggplot2::theme(legend.position="bottom")
  
  if(!is.null(title)){
    p + ggplot2::ggtitle(title) 
  }
  suppressWarnings(p)
  }
  