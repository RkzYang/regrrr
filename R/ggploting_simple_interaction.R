#' plotting the interaction, with or without multiple interaction terms
#'
#' @param reg.result a data.frame of regression result
#' @param df the data used in regression
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
#' @importFrom stats quantile median sd
#' @importFrom spatstat dummify
#' @importFrom robustbase colMedians
#' @importFrom purrr map
#' @export
reg.gg <- function(reg.result, df, by_color=FALSE, x_var.name = NULL, y_var.name = NULL, 
                   main1.r, mdrt.r=NULL, int1.r=NULL,
                   min_x=0.001, max_x=0.999, 
                   mdrt_quantile_05=NULL, mdrt_quantile_50=NULL, mdrt_quantile_95=NULL, 
                   mod.n.sd=NULL,
                   title=NULL, xlab="X_Var.name", ylab="Y_Var.name", moderator.lab="Moderator_name",
                   mdrt.low.name="Low", mdrt.mid.name=NULL, mdrt.high.name="High",
                   y.hi.lim=NULL, y.low.lim=NULL){
  
  reg.result <- as.data.frame(reg.result)
  df <- as.data.frame(df)
  
  # 0.1 dummify the df when the model estimated the fixed effects of factor variables
  factor.var.p <- which(stringr::str_detect(names(df), "factor"))
  if(length(factor.var.p) > 0){
    df <- data.frame(spatstat::dummify(df))
    df <- df[,-factor.var.p] # deleted the base-line level dummy
    names(df)[stringr::str_detect(names(df), "factor")] <- rownames(reg.result)[stringr::str_detect(rownames(reg.result), "factor")]
  }
  
  # 1.1 the beta values of interested variables
  beta.vec <- reg.result[,2]
  b.main <- beta.vec[main1.r]
  b.mdrt <- beta.vec[mdrt.r]
  b.int1 <- beta.vec[int1.r]
  
  # 1.2 
  # the row # of different variables
  para.r <- c(main1.r, mdrt.r, int1.r) # the row # of interested variables (main var, moderator, and their interaction)
  interaction.r <- which(stringr::str_detect(row.names(reg.result), patter=":")) # the row # of all interactions
  ctrl.interactions <- interaction.r[!(interaction.r %in% para.r)] # the row # control interactions
  # the row # of ctrl moderators and ctrl interactions
  ctrl.itract.names <- row.names(reg.result[ctrl.interactions,])
  ctrl.moderator.names <- stringr::str_split(ctrl.itract.names, pattern = ":") %>% purrr::map(function(x){x[1]}) %>% unlist()
  ctrl.itract.other.term.names <- stringr::str_split(ctrl.itract.names, pattern = ":") %>% purrr::map(function(x){x[2]}) %>% unlist()
  ctrl.mod.1 <- which(row.names(reg.result) == ctrl.moderator.names[1])
  ctrl.int.1 <- which(row.names(reg.result) == ctrl.itract.names[1])
  ctrl.mod.2 <- which(row.names(reg.result) == ctrl.moderator.names[2])
  ctrl.int.2 <- which(row.names(reg.result) == ctrl.itract.names[2])
  ctrl.mod.3 <- which(row.names(reg.result) == ctrl.moderator.names[3])
  ctrl.int.3 <- which(row.names(reg.result) == ctrl.itract.names[3])
  # the beta values of ctrl moderators and ctrl interactions, set to 0 if it dones't exist
  S <- length(ctrl.interactions)
  b.ctrl.mod.1 <- ifelse(S < 1, 0, beta.vec[ctrl.mod.1])
  b.ctrl.int.1 <- ifelse(S < 1, 0, beta.vec[ctrl.int.1])
  b.ctrl.mod.2 <- ifelse(S < 2, 0, beta.vec[ctrl.mod.2])
  b.ctrl.int.2 <- ifelse(S < 2, 0, beta.vec[ctrl.int.2])
  b.ctrl.mod.3 <- ifelse(S < 3, 0, beta.vec[ctrl.mod.3])
  b.ctrl.int.3 <- ifelse(S < 3, 0, beta.vec[ctrl.int.3])
  
  # 1.3 the row # of all other controls (w/o ctrl moderators and ctrl interactions) 
  other.control <- (1:nrow(reg.result))[!((1:nrow(reg.result)) %in% unique(c(para.r, interaction.r, ctrl.mod.1, ctrl.mod.2)))] 
  # the beta values of other controls (excluding ctrl moderators and ctrl interactions)
  b.othr.vec <- beta.vec[other.control] 
  
  # 2.1 holding other terms (excluding ctrl moderators and ctrl interactions) to median/mean
  intercept.position <- ifelse(stringr::str_detect(row.names(reg.result)[1], pattern = "ntercept"), 1, length(other.control)+1 ) # length(other.control)+1 is out of boundary, which won't affect selection
  otherterms <- df[row.names(reg.result)[other.control[-intercept.position]]] %>% as.matrix()
  otherTermMedians <- if(!is.null(mdrt_quantile_05)){robustbase::colMedians(otherterms, na.rm = TRUE)}else{colMeans(otherterms, na.rm = TRUE)}
  b.intercept <- ifelse(intercept.position > 0, b.othr.vec[intercept.position], 0)
  constant <- as.numeric(b.intercept + t(otherTermMedians) %*% b.othr.vec[-intercept.position])
  
  # 2.2 holding ctrl moderators to median/mean
  if(!is.null(mdrt_quantile_05)){
    M.ctrl.mod.1 <- ifelse(S < 1, 0, median(unlist(df[ctrl.moderator.names[1]]), na.rm = TRUE))
    M.ctrl.mod.2 <- ifelse(S < 2, 0, median(unlist(df[ctrl.moderator.names[2]]), na.rm = TRUE))
    M.ctrl.mod.3 <- ifelse(S < 3, 0, median(unlist(df[ctrl.moderator.names[3]]), na.rm = TRUE))
  }else{
    M.ctrl.mod.1 <- ifelse(S < 1, 0, mean(unlist(df[ctrl.moderator.names[1]]), na.rm = TRUE))
    M.ctrl.mod.2 <- ifelse(S < 2, 0, mean(unlist(df[ctrl.moderator.names[2]]), na.rm = TRUE)) 
    M.ctrl.mod.3 <- ifelse(S < 3, 0, mean(unlist(df[ctrl.moderator.names[3]]), na.rm = TRUE))
  }
  
  # 3 set moderator levels (low, mid, high)
  if(is.null(mdrt.r)){
    mdrt.low  <- 0
    mdrt.mid  <- 0
    mdrt.high <- 0
  }else if(!is.null(mdrt_quantile_05))
  {
    mod.name  <- rownames(reg.result)[mdrt.r]
    mdrt.low  <- quantile(unlist(df[mod.name]), probs=mdrt_quantile_05, na.rm=TRUE)
    mdrt.mid  <- quantile(unlist(df[mod.name]), probs=mdrt_quantile_50, na.rm=TRUE)
    mdrt.high <- quantile(unlist(df[mod.name]), probs=mdrt_quantile_95, na.rm=TRUE)
  }else{
    mod.name  <- rownames(reg.result)[mdrt.r]
    mdrt.low  <- mean(unlist(df[mod.name]), na.rm=TRUE) - mod.n.sd*sd(unlist(df[mod.name]), na.rm=TRUE)
    mdrt.mid  <- mean(unlist(df[mod.name]), na.rm=TRUE) 
    mdrt.high <- mean(unlist(df[mod.name]), na.rm=TRUE) + mod.n.sd*sd(unlist(df[mod.name]), na.rm=TRUE)
  }
  
  # 4 adjust the margin of x axis
  min.x <- quantile(unlist(df[rownames(reg.result)[main1.r]]), probs=min_x, rm.na=TRUE) %>% as.numeric()
  max.x <- quantile(unlist(df[rownames(reg.result)[main1.r]]), probs=max_x, rm.na=TRUE) %>% as.numeric()
  
  # 5 plot
  requireNamespace(ggplot2)
  requireNamespace(extrafont)
  requireNamespace(ggthemes)
  
  # function for plot | note: only x is a variable 
  fit.low  <- function(x){constant + b.main*x + b.mdrt*mdrt.low  + b.int1*x*mdrt.low + 
      b.ctrl.mod.1*M.ctrl.mod.1 + b.ctrl.int.1*x*M.ctrl.mod.1 +
      b.ctrl.mod.2*M.ctrl.mod.2 + b.ctrl.int.2*x*M.ctrl.mod.2 +
      b.ctrl.mod.3*M.ctrl.mod.3 + b.ctrl.int.3*x*M.ctrl.mod.3
  }
  
  fit.mid  <- function(x){constant + b.main*x + b.mdrt*mdrt.mid  + b.int1*x*mdrt.mid +
      b.ctrl.mod.1*M.ctrl.mod.1 + b.ctrl.int.1*x*M.ctrl.mod.1 +
      b.ctrl.mod.2*M.ctrl.mod.2 + b.ctrl.int.2*x*M.ctrl.mod.2 +
      b.ctrl.mod.3*M.ctrl.mod.3 + b.ctrl.int.3*x*M.ctrl.mod.3
  }
  
  fit.high <- function(x){constant + b.main*x + b.mdrt*mdrt.high + b.int1*x*mdrt.high +
      b.ctrl.mod.1*M.ctrl.mod.1 + b.ctrl.int.1*x*M.ctrl.mod.1 +
      b.ctrl.mod.2*M.ctrl.mod.2 + b.ctrl.int.2*x*M.ctrl.mod.2 +
      b.ctrl.mod.3*M.ctrl.mod.3 + b.ctrl.int.3*x*M.ctrl.mod.3
  }
  
  p <-  ggplot(df, aes_string(x=x_var.name, y=y_var.name)) +
    scale_x_continuous(limits=c(min.x, max.x), xlab) +
    scale_y_continuous(limits=c(y.low.lim, y.hi.lim), ylab) 
  
  if(is.null(mdrt.mid.name)){
    if(by_color==FALSE){
      p <- p + stat_function(fun=fit.low,  aes(linetype = mdrt.low.name)) +
        stat_function(fun=fit.high, aes(linetype = mdrt.high.name)) +
        scale_linetype_manual(moderator.lab, values=c("solid", "dotted"))
    }else{
      p <- p + stat_function(fun=fit.low,  aes(colour = mdrt.low.name)) +
        stat_function(fun=fit.high, aes(colour = mdrt.high.name)) +
        scale_colour_manual(moderator.lab, values = c("red", "black"))
    }
  }else{
    if(by_color==FALSE){
      p <- p +  stat_function(fun=fit.low,  aes(linetype = mdrt.low.name)) +
        stat_function(fun=fit.mid,  aes(linetype = mdrt.mid.name)) +
        stat_function(fun=fit.high, aes(linetype = mdrt.high.name)) + 
        scale_linetype_manual(moderator.lab, values=c("solid", "dashed", "dotted"))
    }else{
      p <- p +  stat_function(fun=fit.low,  aes(linetype = mdrt.low.name)) +
        stat_function(fun=fit.mid,  aes(linetype = mdrt.mid.name)) +
        stat_function(fun=fit.high, aes(linetype = mdrt.high.name)) + 
        scale_colour_manual(moderator.lab, values = c("red", "blue", "black"))
    }
  }
  
  # customize #
  p <- p + theme_light() + 
    theme(text=element_text(family="Times New Roman", size=16)) +
    theme(legend.position="bottom")
  
  if(!is.null(title)){
    p + ggtitle(title) 
  }
  p
}
