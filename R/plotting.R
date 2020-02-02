#' plotting the marginal effect of X on Y, with or without one or multiple interaction terms
#'  
#' @param reg.coef a coefficient matrix of regression result, e.g. summary(lm_model)$coef
#' @param data the data used in regression, a data frame
#' @param model the model object, such as a "lm" object
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
#' @param v a customized variance-covariance matrix
#' @param CI_Ribbon if TRUE, plot confidence interval ribbons, if FALSE, plot error bars
#' @param title the title of the plot
#' @param xlab label of X
#' @param ylab label of Y
#' @param moderator.lab label of moderator
#' @param mdrt.low.name the label of low-level moderator
#' @param mdrt.mid.name the label of mid-level moderator
#' @param mdrt.high.name the label of high-level moderator
#' @param y.high.lim specify the upper limit of y
#' @param y.low.lim specify the lower limit of y
#' @param spline_labels label of the spline variable
#' 
#' @examples
#' data(mtcars)
#' m1 <- lm(mpg ~ vs + carb + hp + wt + wt * hp , data = mtcars)
#' plot_effect(reg.coef = summary(m1)$coefficients,
#'                  data = mtcars, model = m1,
#'                  x_var.name = "wt", y_var.name = "mpg", moderator.name = "hp",
#'                  confidence_interval = TRUE,  CI_Ribbon = TRUE,
#'                  xlab = "Weight", ylab = "MPG", moderator.lab = "Horsepower")
#' 
#' #' @examples
#' data(mtcars)
#' m2 <- lm(mpg ~ vs + carb + hp + wt + wt * hp + wt * vs, data = mtcars)
#' plot_effect(reg.coef = summary(m2)$coefficients,
#'             data = mtcars, model = m2,
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
#'                  data = mtcars, model = m1, 
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
#' @import lspline
#' @importFrom stats knots
#' @export
plot_effect <- function(reg.coef, data, model, by_color = FALSE, x_var.name = NULL, y_var.name = NULL, moderator.name = NULL, 
                        min_x=0.001, max_x=0.999, 
                        mdrt_quantile_05=NULL, mdrt_quantile_50=NULL, mdrt_quantile_95=NULL, 
                        mod.n.sd=1,
                        confidence_interval = FALSE, v=NULL, CI_Ribbon = FALSE,
                        title=NULL, xlab="X_Var.name", ylab="Y_Var.name", moderator.lab="Moderator_name",
                        mdrt.low.name="Low", mdrt.mid.name=NULL, mdrt.high.name="High",
                        y.high.lim=NULL, y.low.lim=NULL, spline_labels=c("LHS", "RHS")){
  
  if(class(reg.coef) == "coeftest"){
    reg.coef <- as.data.frame(`[`(reg.coef))
    if(is.null(v)){
      message("reminder (important): if standard errors are clustered, please specify v (a customized vcov) to plot confidencen intervals")}
  }
  
  tryCatch({reg.result <- as.data.frame(reg.coef)}, error=function(e){cat("ERROR :", conditionMessage(e), "\n")})
  
  if(is.null(x_var.name)){stop("what is the X variable name (x_var.name) in the model?")}
  if(is.null(y_var.name)){stop("what is the Y variable name (y_var.name) in the model?")}
  if(is.null(moderator.name)){warning("no moderating variable is specified; only plotting the main effect")}
  
  tryCatch({
    df <- as.data.frame(data)
    
    factor.names <- rownames(reg.result)[stringr::str_detect(rownames(reg.result), pattern = "factor")]
    factor.name <- unique(unlist(purrr::map(factor.names, function(x) stringr::str_extract(x, pattern = "(?<=\\().*?(?=\\))")[1])))
    
    lspline.names <- rownames(reg.result)[stringr::str_detect(rownames(reg.result), pattern = "lspline")]
    lspline.name_ <- unique(unlist(purrr::map(lspline.names, function(x) stringr::str_extract(x, pattern = "(?<=\\().*?(?=\\))")[1])))
    if(!is.null(lspline.name_)){
    lspline.name <- strsplit(lspline.name_, ",")[[1]][1]}else{
      lspline.name <- NULL  
    }
    
    non.factor.names <- names(df)[names(df) %in% rownames(reg.result)]
    
    # 1 the beta values of interested variables #####
    main1.r <- which(rownames(reg.result) == x_var.name)
    mdrt.r  <- which(rownames(reg.result) == moderator.name) 
    
    # 2 set moderator levels (low, mid, high) #####
    if(length(mdrt.r) == 0){
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
    
    # 3 adjust the margin of x axis #####
    min.x <- as.numeric(quantile(unlist(df[rownames(reg.result)[main1.r]]), probs=min_x, rm.na=TRUE))
    max.x <- as.numeric(quantile(unlist(df[rownames(reg.result)[main1.r]]), probs=max_x, rm.na=TRUE))
    
    # 4 make a hypothetical dataset (all other variables other than x and moderator are held to median or mean) for plotting #####
    main.x.name <- x_var.name
    modrtr.name <- moderator.name
    
    df.factor.var <- df[, factor.name]
    df.lspline.var <- df[, lspline.name]
    other_continuous_var_names <- non.factor.names[! non.factor.names %in% c(main.x.name, modrtr.name)]
    df.other.var <- df[, other_continuous_var_names]
    
    # 4.1 lspline var #####
    if(!is.null(mdrt_quantile_05))
    {
      df.fake.lspline.var <- data.frame(replicate(150, robustbase::colMedians(as.matrix(df.lspline.var), na.rm = TRUE)))
    }else{
      df.fake.lspline.var <- data.frame(replicate(150, colMeans(as.matrix(df.lspline.var), na.rm = TRUE)))
    }
    names(df.fake.lspline.var) <- lspline.name
    
    # 4.2 if there is only one "factor" variable #####
    if(length(factor.name) == 1){
      df.fake.factor.var <- data.frame(x = rep(df.factor.var[1],150))
      names(df.fake.factor.var) <- factor.name
    }else{
      df.fake.factor.var <- df.factor.var[rep(1,150),]}
    
    # 4.3 if there is only one "other" (continuous) variable #####
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
    
    if(length(lspline.name) > 0 && modrtr.name != lspline.name){
      df.fake.other.var <- cbind(df.fake.lspline.var, df.fake.other.var)
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
    
    if(!is.null(moderator.name)){
      names(df.fake)[1:2] <- c(main.x.name, modrtr.name)}else{
        names(df.fake)[1]   <-   main.x.name  
      }
    
    if(class(df.other.var) == "numeric" && length(factor.name) == 0){
      names(df.fake)[which(stringr::str_detect(names(df.fake), "df.fake.other.var"))] <- other_continuous_var_names
    }
    
    # 4.4 when modertator is lspline, construct lspline moderator #####
    if(!is.null(lspline.name) && modrtr.name == lspline.name){
      two_col <- eval(parse(text=paste0("lspline(df$",lspline.name_,")"))) %>% as.data.frame()
      eval(parse(text=strsplit(lspline.name_, ",")[[1]][2]))
      
      if(!is.null(mdrt_quantile_05)){
        mdrt.low  <- quantile(two_col[,1], probs=mdrt_quantile_05, na.rm=TRUE)
        if(!is.null(mdrt_quantile_50)){
          mdrt.mid  <- quantile(two_col[,1], probs=mdrt_quantile_50, na.rm=TRUE)}
        mdrt.high <- quantile(two_col[,1], probs=mdrt_quantile_95, na.rm=TRUE)
        
        mdrt.low_  <- quantile(two_col[,2], probs=mdrt_quantile_05, na.rm=TRUE)
        if(!is.null(mdrt_quantile_50)){
          mdrt.mid_  <- quantile(two_col[,2], probs=mdrt_quantile_50, na.rm=TRUE)}
        mdrt.high_ <- quantile(two_col[,2], probs=mdrt_quantile_95, na.rm=TRUE)
      }else{
        mdrt.low  <- mean(two_col[,1], na.rm=TRUE) - 2*sd(two_col[,1], na.rm=TRUE)
        mdrt.mid  <- mean(two_col[,1], na.rm=TRUE) 
        mdrt.high <- mean(two_col[,1], na.rm=TRUE) + 2*sd(two_col[,1], na.rm=TRUE)
        
        mdrt.low_  <- mean(two_col[,2], na.rm=TRUE) - 2*sd(two_col[,2], na.rm=TRUE)
        mdrt.mid_  <- mean(two_col[,2], na.rm=TRUE) 
        mdrt.high_ <- mean(two_col[,2], na.rm=TRUE) + 2*sd(two_col[,2], na.rm=TRUE)
      }
      
      mdrt.low  <- ifelse(mdrt.low  <= knots, mdrt.low, knots)
      mdrt.mid  <- ifelse(mdrt.mid  <= knots, mdrt.mid, knots)
      mdrt.high <- ifelse(mdrt.high <= knots, mdrt.high, knots)
      mdrt.low_ <- ifelse(mdrt.low_ >= 0, mdrt.low_, knots)
      mdrt.mid_ <- ifelse(mdrt.mid_ >= 0, mdrt.mid_, knots)
      mdrt.high_<- ifelse(mdrt.high_>= 0, mdrt.high_, knots)
      
      if(!is.null(mdrt.mid.name)){
        df.fake <- data.frame(x = rep(seq(min.x, max.x, length=25), 6), 
                              mod = rep(c(mdrt.low, mdrt.mid, mdrt.high, mdrt.low_, mdrt.mid_, mdrt.high_), each=25), 
                              mod.level = rep(c(mdrt.low.name, mdrt.mid.name, mdrt.high.name, mdrt.low.name, mdrt.mid.name, mdrt.high.name), each=25),
                              knots = rep(spline_labels, each=75),
                              df.fake.other.var)
      }else{
        df.fake <- data.frame(x = rep(seq(min.x, max.x, length=25), 4), 
                              mod = rep(c(mdrt.low, mdrt.high, mdrt.low_, mdrt.high_), each=25),
                              mod.level = rep(c(mdrt.low.name, mdrt.high.name, mdrt.low.name, mdrt.high.name), each=25),
                              knots = rep(spline_labels, each=50),
                              df.fake.other.var[1:100,])
      }
      
      if(!is.null(moderator.name)){
        names(df.fake)[1:2] <- c(main.x.name, modrtr.name)}else{
          names(df.fake)[1]   <-   main.x.name  
        }
    }
    
    # 4.5 plug in y_hat #####
    df.fake$y_hat <- predict(model, df.fake, type = "response")
    names(df.fake)[length(names(df.fake))] <- y_var.name
    # add confidence intervals
    # df.fake <- cbind(df.fake, predict(model, df.fake, interval = "confidence")[, 2:3])
    
    # 5 plot #####
    # requireNamespace(ggplot2)    
    # requireNamespace(extrafont)
    # requireNamespace(ggthemes)
    
    if(confidence_interval == FALSE){
      if(by_color == FALSE){
        p <-  ggplot2::ggplot(df.fake, ggplot2::aes_string(x=x_var.name, y=y_var.name)) +
          ggplot2::geom_line(ggplot2::aes(linetype = mod.level)) +
          ggplot2::scale_x_continuous(limits=c(min.x, max.x), xlab) +
          ggplot2::scale_y_continuous(limits=c(y.low.lim, y.high.lim), ylab)
        
        if(!is.null(mdrt.mid.name)){
          p <- p + ggplot2::scale_linetype_manual(moderator.lab, values=c("solid", "dashed", "dotted"))
        }else{
          p <- p + ggplot2::scale_linetype_manual(moderator.lab, values=c("solid", "dotted"))
        }
        
      }else{
        p <-  ggplot2::ggplot(df.fake, ggplot2::aes_string(x=x_var.name, y=y_var.name)) +
          ggplot2::geom_line(ggplot2::aes(color = mod.level)) +
          ggplot2::scale_x_continuous(limits=c(min.x, max.x), xlab) +
          ggplot2::scale_y_continuous(limits=c(y.low.lim, y.high.lim), ylab)
        
        if(!is.null(mdrt.mid.name)){
          p <- p + ggplot2::scale_colour_manual(moderator.lab, values = c("red", "blue", "black"))
        }else{
          p <- p + ggplot2::scale_colour_manual(moderator.lab, values = c("red", "black"))
        }
      }
    }else{ 
      # plotting confidence intervals (of the regression slope)
      
      if(is.null(v)){
        predicted <- data.frame(predict(model, df.fake, interval = "confidence"))[, 2:3]
        names(predicted) <- c("lwr_", "upr_")
        df.fake <- cbind(df.fake, predicted)}else{
          
          # calculate CI based on customized vcov
          # ref: https://stackoverflow.com/questions/14033551/r-plotting-confidence-bands-with-ggplot
          
          # construct the lwr_ and upr_ into df.fake
          design_names <- colnames(model.matrix(model))
          df.fake[, design_names[1]] <- rep(1, nrow(df.fake))
          
          # put interaction cols into df.fake
          all_inter_names <- design_names[which(stringr::str_detect(design_names, ":"))]
          for(itrname in all_inter_names){
            names_split <- strsplit(itrname, split = ":")[[1]]
            term_1 <- names_split[1]
            term_2 <- names_split[2]
            # print(c(term_1, term_2))
            df.fake[, itrname] <- df.fake[, term_1] * df.fake[, term_2]
          }
          
          X <- as.matrix(df.fake[, design_names])
          df.fake$fit <- predict(model, df.fake)
          se.fit <- sqrt(diag(X %*% v %*% t(X)))
          df.fake$lwr_ <- df.fake$fit - 1.96*se.fit
          df.fake$upr_ <- df.fake$fit + 1.96*se.fit
          
        }
      
      # avoid note in check()
      lwr_ <- df.fake$lwr_ 
      upr_ <- df.fake$upr_ 
      mod.level <- df.fake$mod.level
      
      if(by_color == FALSE){
        if(CI_Ribbon == FALSE){
          p <- ggplot2::ggplot(df.fake, ggplot2::aes_string(x=x_var.name, y=y_var.name, fill = "mod.level")) +
            ggplot2::guides(fill=FALSE) +
            ggplot2::geom_line(ggplot2::aes(linetype = mod.level)) +
            ggplot2::scale_x_continuous(limits=c(min.x, max.x), xlab) +
            ggplot2::scale_y_continuous(limits=c(y.low.lim, y.high.lim), ylab) +
            ggplot2::geom_errorbar(ggplot2::aes(ymin = lwr_, ymax = upr_), alpha = 0.116)
        }else{
          p <- ggplot2::ggplot(df.fake, ggplot2::aes_string(x=x_var.name, y=y_var.name, fill = "mod.level")) +
            ggplot2::guides(fill=FALSE) +
            ggplot2::geom_line(ggplot2::aes(linetype = mod.level)) +
            ggplot2::scale_x_continuous(limits=c(min.x, max.x), xlab) +
            ggplot2::scale_y_continuous(limits=c(y.low.lim, y.high.lim), ylab) +
            ggplot2::geom_ribbon(ggplot2::aes(ymin = lwr_, ymax = upr_), alpha = 0.6) +
            ggplot2::scale_fill_grey()
        }
        
        if(!is.null(mdrt.mid.name)){
          p <- p + ggplot2::scale_linetype_manual(moderator.lab, values=c("solid", "dashed", "dotted"))
        }else{
          p <- p + ggplot2::scale_linetype_manual(moderator.lab, values=c("solid", "dashed"))
        }
      }else{
        if(CI_Ribbon == FALSE){
          p <- ggplot2::ggplot(df.fake, ggplot2::aes_string(x=x_var.name, y=y_var.name, fill = "mod.level")) +
            ggplot2::guides(fill=FALSE) +
            ggplot2::geom_line(ggplot2::aes(color = mod.level)) +
            ggplot2::scale_x_continuous(limits=c(min.x, max.x), xlab) +
            ggplot2::scale_y_continuous(limits=c(y.low.lim, y.high.lim), ylab) +
            ggplot2::geom_errorbar(ggplot2::aes(ymin = lwr_, ymax = upr_), alpha = 0.116)
        }else{
          p <- ggplot2::ggplot(df.fake, ggplot2::aes_string(x=x_var.name, y=y_var.name, fill = "mod.level")) +
            ggplot2::guides(fill=FALSE) +
            ggplot2::geom_line(ggplot2::aes(color = mod.level)) +
            ggplot2::scale_x_continuous(limits=c(min.x, max.x), xlab) +
            ggplot2::scale_y_continuous(limits=c(y.low.lim, y.high.lim), ylab) +
            ggplot2::geom_ribbon(ggplot2::aes(ymin = lwr_, ymax = upr_), alpha = 0.6)
        }
        
        if(!is.null(mdrt.mid.name)){
          p <- p + ggplot2::scale_colour_manual(moderator.lab, values = c("red", "blue", "black"))
        }else{
          p <- p + ggplot2::scale_colour_manual(moderator.lab, values = c("red", "black"))
        }
      }
    }
    
    if(!is.null(lspline.name) && modrtr.name == lspline.name){
      p <- p + ggplot2::facet_wrap(~knots)
    }
    
    # customize #
    p <- p + ggplot2::theme_light() + 
      # ggplot2::theme(text=ggplot2::element_text(family="Times New Roman", size = 16)) + # library(extrafont)
      ggplot2::theme(legend.position="bottom")
    
    if(!is.null(title)){
      p + ggplot2::ggtitle(title) 
    }
    suppressWarnings(p)}, error=function(e){cat("ERROR :", conditionMessage(e), "\n")})
}