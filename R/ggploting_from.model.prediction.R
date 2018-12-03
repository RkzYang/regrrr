#' plotting the interaction, with or without multiple interactions, FROM the model prediction
#' by creating a hypothetical dataset 
#' 
#' @param reg.result a data.frame of regression result
#' @param df the data used in regression
#' @param model.for.predict the model object, such as a "lm" object
#' @param by_color plot interactions by colors, otherwise by line types
#' @param x_var.name x name in the regression model, a string
#' @param y_var.name y name in the regression model, a string
#' @param main1.r the row number of the main effect, in reg.result
#' @param mdrt.r the row number of the moderater, in reg.result
#' @param int1.r the row number of the interaction, in reg.result
#' @param min_x the min of x scale, in percentile of x
#' @param max_x the max of x scale, in percentile of x
#' @param mdrt_quantile_05 set the low level of moderator, in percentile
#' @param mdrt_quantile_50 set the middle level of moderator, in percentile
#' @param mdrt_quantile_95 set the high level of moderator, in percentile
#' @param mod.n.sd set the moderating strength, in the number of s.d. units, which can take negative values
#' @param title the title of the plot
#' @param xlab label of X
#' @param ylab label of Y
#' @param moderator.lab label of moderator
#' @param mdrt.low.name the label of low-level moderator
#' @param mdrt.mid.name the label of mid-level moderator
#' @param mdrt.high.name the label of high-level moderator
#' @param y.hi.lim specify the upper limit of y
#' @param y.low.lim specify the lower limit of y
#' @importFrom stats quantile median sd predict
#' @importFrom robustbase colMedians
#' @import stringr
#' @export
reg.gg.from.model <- function(reg.result, df, model.for.predict, by_color=FALSE, x_var.name = NULL, y_var.name = NULL, 
                              main1.r, mdrt.r=NULL, int1.r=NULL,
                              min_x=0.001, max_x=0.999, 
                              mdrt_quantile_05=NULL, mdrt_quantile_50=NULL, mdrt_quantile_95=NULL, 
                              mod.n.sd=NULL,
                              title=NULL, xlab="X_Var.name", ylab="Y_Var.name", moderator.lab="Moderator_name",
                              mdrt.low.name="Low", mdrt.mid.name=NULL, mdrt.high.name="High",
                              y.hi.lim=NULL, y.low.lim=NULL){
  
  tryCatch({reg.result <- as.data.frame(reg.result)}, error=function(e){cat("ERROR :", conditionMessage(e), "\n")})
  
  if(is.null(x_var.name)){stop("what is the X variable name (x_var.name) in the model?")}
  if(is.null(y_var.name)){stop("what is the Y variable name (y_var.name) in the model?")}
  
  df <- as.data.frame(df)
  
  factor.names <- rownames(reg.result)[stringr::str_detect(rownames(reg.result), pattern = "factor")]
  factor.name <- stringr::str_extract(factor.names, pattern = "(?<=\\().*?(?=\\))")[1]
  if(!is.na(factor.name)){factor.levels <- levels(as.factor(df[,factor.name]))}
  
  non.factor.names <- names(df)[names(df) %in% rownames(reg.result)]
  df <- df[ , non.factor.names]
  
  # 1 the beta values of interested variables
  beta.vec <- reg.result[,2]
  b.main <- beta.vec[main1.r]
  b.mdrt <- beta.vec[mdrt.r]
  
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
  
  # 4 make a fake data (all other variables other than x and moderator are held to median or mean) set for plotting
  main.x.name <- rownames(reg.result)[main1.r]
  modrtr.name <- rownames(reg.result)[mdrt.r]
  
  df.other.var <- df[, names(df)[! names(df) %in% c(main.x.name, modrtr.name, factor.name)]]
  
  if(!is.null(mdrt_quantile_05))
  {
    df.fake.other.var <- data.frame(t(replicate(150, robustbase::colMedians(as.matrix(df.other.var), na.rm = TRUE))))
  }else{
    df.fake.other.var <- data.frame(t(replicate(150, colMeans(df.other.var, na.rm = TRUE))))
  }
  
  if(!is.null(mdrt.mid.name)){
    if(!is.na(factor.name)){
      df.fake <- data.frame(x = rep(seq(min.x, max.x, length=50), 3), 
                            mod = rep(c(mdrt.low, mdrt.mid, mdrt.high), each=50), 
                            factor1 = factor.levels[length(factor.levels)], 
                            mod.level = rep(c(mdrt.low.name, mdrt.mid.name, mdrt.high.name), each=50),
                            df.fake.other.var)
      names(df.fake)[1:3] <- c(main.x.name, modrtr.name, factor.name)
    }else{
      df.fake <- data.frame(x = rep(seq(min.x, max.x, length=50), 3), 
                            mod = rep(c(mdrt.low, mdrt.mid, mdrt.high), each=50), 
                            mod.level = rep(c(mdrt.low.name, mdrt.mid.name, mdrt.high.name), each=50),
                            df.fake.other.var)
      names(df.fake)[1:2] <- c(main.x.name, modrtr.name)  
    }
  }else{
    if(!is.na(factor.name)){
      df.fake <- data.frame(x = rep(seq(min.x, max.x, length=50), 2), 
                            mod = rep(c(mdrt.low, mdrt.high), each=50), 
                            factor1 = factor.levels[length(factor.levels)], 
                            mod.level = rep(c(mdrt.low.name, mdrt.high.name), each=50),
                            df.fake.other.var[1:100,])
      names(df.fake)[1:3] <- c(main.x.name, modrtr.name, factor.name)
    }else{
      df.fake <- data.frame(x = rep(seq(min.x, max.x, length=50), 2), 
                            mod = rep(c(mdrt.low, mdrt.high), each=50),
                            mod.level = rep(c(mdrt.low.name, mdrt.high.name), each=50),
                            df.fake.other.var[1:100,])
      names(df.fake)[1:2] <- c(main.x.name, modrtr.name)  
    }
  }
  
  df.fake$y_hat <- predict(model.for.predict, df.fake, type = "response")
  names(df.fake)[length(names(df.fake))] <- y_var.name
  
  # 5 plot
  # requireNamespace(ggplot2)    
  # requireNamespace(extrafont)
  # requireNamespace(ggthemes)
  
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
  
  # customize #
  p <- p + ggplot2::theme_light() + 
    ggplot2::theme(text=ggplot2::element_text(family="Times New Roman", size=16)) +
    ggplot2::theme(legend.position="bottom")
  
  if(!is.null(title)){
    p + ggplot2::ggtitle(title) 
  }
  p
}