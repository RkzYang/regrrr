## add p.z and sig #######
### convert model result into {desired format} ###
## specify d = number of digits of the numeric result ##
## df format: col.1 = n.r(row number); col.2 = Estimate, col.3 = S.E., col.4 = t., col.5 = p., col.6 = sig



## compare models ##
compare.models<- function(model1,model2,model3=NULL,model4=NULL,model5=NULL,model6=NULL,model7=NULL,model8=NULL){
  compare <- suppressWarnings(suppressMessages(anova(model1,model2,model3,model4,model5,model6,model7,model8)))
  ch          <- function(x){as.character(x)}
  convert.sig <- function(df){ifelse(df<0.001,"***",ifelse(df<0.01,"**",ifelse(df<0.05,"*",ifelse(df<0.1,"†",""))))}
  dg          <- function(x)formatC(x, format = "f", digits = 3)
  model.list  <- list(model1, model2, model3, model4, model5,model6,model7,model8)
  model.list  <- model.list[1:sum(!unlist((map(model.list, is.null))))]

  compare.df <- if(class(model1)=="lm"){
    data.frame(R_squared=ch(t(dg(unlist(map(map(model.list, summary), function(df){df$r.squared}))))),
               Adj_R_squared=ch(t(dg(unlist(map(map(model.list, summary), function(df){df$adj.r.squared}))))),
               F=ch(t(dg(compare[5][[1]]))),
               sig=ch(t(convert.sig(compare[6][[1]])))) %>% unite(F,F,sig,sep="") %>% t()
  }else{
    data.frame(AIC=ch(t(dg(compare[2][[1]]))),BIC=ch(t(dg(compare[3][[1]]))),
                           Log_Likelihood=ch(t(dg(compare[4][[1]]))),Chisq=ch(t(dg(compare[6][[1]]))),
                           sig=ch(t(convert.sig(compare[8])))) %>% unite(Chisq,Chisq,sig,sep="") %>% t()
    }
  compare.df[nrow(compare.df),1] <- ""
  compare.df <- data.frame(Variables=row.names(compare.df),compare.df)
  return(compare.df)
}

## plot the result ##
## specify: df= df of the regression result, nameofdata= the data df used for analysis,
## specify: main1.r = on which row the main effect estimate is ...
## specify: find.median = TRUE, hold all other vars to median vs. mean
## specify: min.x = lower quantile of x axis, max.x= upper quantile of x axis
## specify: max.y = extra margin for y axis, in terms of the multiple of max y
## specify: mdrt_05 = quantile position of lower bound of moderater
## specify: labels

# install.packages("extrafont")
# library(extrafont)
# font_import()
# loadfonts(device="win")
par(family="Times New Roman")

plot.result <- function(df, nameofdata,
                        main1.r, main2.r=NULL, mdrt.r=NULL, mdrt.square.r=FALSE, int1.r=NULL, int2.r=NULL,
                        find.median=TRUE, min_x=0.001, max_x=0.999, max.y=1,
                        mdrt_05=.05, mdrt_50=0.5, mdrt_95=.95,n.sd=1,
                        main=NULL,xlab="name", ylab="name",
                        mdrt.05.name=NULL, mdrt.50.name=NULL, mdrt.95.name="name",
                        flip.low.high=FALSE,
                        y.hi.lim=NULL,y.low.lim=NULL,
                        adjust.fixed.effect.vec=NULL){
  df <- as.data.frame(df)
  para.r <- c(main1.r,main2.r,mdrt.r,int1.r,int2.r)
  othr.r <- (1:nrow(df))[!((1:nrow(df)) %in% para.r)]

  beta.vec <- df[,2]
  b.b <- beta.vec[main1.r]
  b.a <- ifelse(!is.null(main2.r),beta.vec[main2.r],0)
  b.mdrt <- beta.vec[mdrt.r]
  b2.mdrt <- ifelse(mdrt.square.r,beta.vec[mdrt.r],0)
  b.int1 <- ifelse(!is.null(int1.r),beta.vec[int1.r],0)
  b.int2 <- ifelse(!is.null(int2.r),beta.vec[int2.r],0)
  b.othr.vec <- beta.vec[othr.r]
  names(b.othr.vec) <- rownames(as.data.frame(df))[othr.r]

  otherterms <- as.matrix(as.data.frame(nameofdata)[rownames(as.data.frame(df))[othr.r[-1]]])
  otherTermMedians <- if(find.median==TRUE){robustbase::colMedians(otherterms,na.rm = TRUE)}else{colMeans(otherterms,na.rm = TRUE)}
  fix.effect.adjust <- ifelse(is.null(adjust.fixed.effect.vec),0,mean(adjust.fixed.effect.vec))
  constant <- as.numeric(b.othr.vec[1] + t(otherTermMedians) %*% b.othr.vec[-1] + fix.effect.adjust)

  mdrt.05 <- ifelse(!is.null(mdrt.r),ifelse(find.median==TRUE,quantile(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], probs=mdrt_05, na.rm=TRUE),
                                              mean(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], na.rm=TRUE)-
                                              n.sd*sd(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], na.rm=TRUE)),0)
  mdrt.50 <- ifelse(!is.null(mdrt.r),ifelse(find.median==TRUE,quantile(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], probs=mdrt_50, na.rm=TRUE),
                                            mean(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], na.rm=TRUE)),0)
  mdrt.95 <- ifelse(!is.null(mdrt.r),ifelse(find.median==TRUE,quantile(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], probs=mdrt_95, na.rm=TRUE),
                                            mean(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], na.rm=TRUE)+
                                              n.sd*sd(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], na.rm=TRUE)),0)
  min.x <- quantile(as.data.frame(nameofdata)[rownames(as.data.frame(df))[main1.r]][,1], probs=min_x, na.rm=TRUE)
  max.x <- quantile(as.data.frame(nameofdata)[rownames(as.data.frame(df))[main1.r]][,1], probs=max_x, na.rm=TRUE)
  x <- seq(min.x, max.x,length=100)

  mdrt.05.vals <- if(!is.null(mdrt.r)){if(mdrt.square.r){b.mdrt*mdrt.05 + b2.mdrt*mdrt.05^2 + b.int1*x*mdrt.05 + b.int2*x*mdrt.05^2}else{
                                                         b.mdrt*mdrt.05 + b.int1*x*mdrt.05 + b.int2*x^2*mdrt.05}}else{rep(0,length(x))} # only mdrt.05 related terms
  mdrt.50.vals <- if(!is.null(mdrt.r)){if(mdrt.square.r){b.mdrt*mdrt.50 + b2.mdrt*mdrt.50^2 + b.int1*x*mdrt.50 + b.int2*x*mdrt.50^2}else{
                                                         b.mdrt*mdrt.50 + b.int1*x*mdrt.50 + b.int2*x^2*mdrt.50}}else{rep(0,length(x))}
  mdrt.95.vals <- if(!is.null(mdrt.r)){if(mdrt.square.r){b.mdrt*mdrt.95 + b2.mdrt*mdrt.95^2 + b.int1*x*mdrt.95 + b.int2*x*mdrt.95^2}else{
                                                         b.mdrt*mdrt.95 + b.int1*x*mdrt.95 + b.int2*x^2*mdrt.95}}else{rep(0,length(x))}

  line1 <- constant + b.b*x + b.a*x^2 + mdrt.05.vals
  line2 <- constant + b.b*x + b.a*x^2 + mdrt.50.vals
  line3 <- constant + b.b*x + b.a*x^2 + mdrt.95.vals
  y.low.lim <- ifelse(is.null(y.low.lim),min(line1,line2,line3),y.low.lim)
  y.hi.lim  <- ifelse(is.null(y.hi.lim),max.y*max(line1,line2,line3),y.hi.lim)
  plot1 <- plot(c(min.x, max.x), c(y.low.lim, y.hi.lim), type="n",xlab=xlab, ylab=ylab,main=main)
  low  <- ifelse(flip.low.high==FALSE,1,6)
  high <- ifelse(flip.low.high==FALSE,6,1)
  lines(x, line1, lty=low)
  if(!is.null(mdrt.50.name)){lines(x, line2, lty=2)}
  lines(x, line3, lty=high)
  if(!is.null(mdrt.05.name)){if(is.null(mdrt.50.name)){legend("topright", legend = c(mdrt.05.name, mdrt.95.name),
                                                              lty = c(low,high), xjust = 1, yjust = 1,adj = c(0, 0.5))}else{legend("topright", legend = c(mdrt.05.name, mdrt.50.name, mdrt.95.name),
                                                                                                                                   lty = c(low,2,high), xjust = 1, yjust = 1,adj = c(0, 0.5))}}
}


## plot three-way interaction ##
plot.result.three.way <- function(two.graphs=TRUE, df, nameofdata,
                        main1.r, main2.r=NULL, mdrt.r=NULL, int1.r=NULL, int2.r=NULL,
                        find.median=FALSE, min_x=0.001, max_x=0.999, max.y=1,
                        mdrt_05=.05, mdrt_50=0.5, mdrt_95=.95,n.sd=1,
                        main=NULL,xlab="name", ylab="name",
                        mdrt.05.name=NULL, mdrt.50.name=NULL, mdrt.95.name="name",
                        flip.low.high=FALSE,
                        more.mdrt.r=NULL,more.int1.r=NULL,more.int2.r=NULL,more.int3.r=NULL,
                        more.median=TRUE,n.more.sd.minus=1,n.more.sd.plus=1,
                        more.mdrt_05=.05, more.mdrt_95=.95, main1=NULL, main2=NULL,
                        more.mdrt.05.name=NULL,more.mdrt.95.name=NULL,
                        more.mdrt.05.name.2=NULL,more.mdrt.95.name.2=NULL){
  df <- as.data.frame(df)
  para.r <- c(main1.r,main2.r,mdrt.r,int1.r,int2.r,more.mdrt.r,more.int1.r,more.int2.r,more.int3.r)
  othr.r <- (1:nrow(df))[!((1:nrow(df)) %in% para.r)]

  beta.vec <- df[,2]
  b.b <- beta.vec[main1.r] # linear term
  b.a <- ifelse(!is.null(main2.r),beta.vec[main2.r],0) # quadratic term
  b.mdrt <- beta.vec[mdrt.r]
  b.int1 <- beta.vec[int1.r]
  b.int2 <- ifelse(!is.null(int2.r),beta.vec[int2.r],0)
  b.more.mdrt <- ifelse(!is.null(more.mdrt.r),beta.vec[more.mdrt.r],0)
  b.more.int1 <- ifelse(!is.null(more.int1.r),beta.vec[more.int1.r],0)
  b.more.int2 <- ifelse(!is.null(more.int2.r),beta.vec[more.int2.r],0)
  b.more.int3 <- ifelse(!is.null(more.int3.r),beta.vec[more.int3.r],0)
  b.othr.vec <- beta.vec[othr.r]

  otherterms <- as.matrix(as.data.frame(nameofdata)[rownames(as.data.frame(df))[othr.r[-1]]])
  otherTermMedians <- if(find.median==TRUE){robustbase::colMedians(otherterms,na.rm = TRUE)}else{colMeans(otherterms,na.rm = TRUE)}
  constant <- as.numeric(b.othr.vec[1] + t(otherTermMedians) %*% b.othr.vec[-1])

  mdrt.05 <- ifelse(!is.null(mdrt.r),ifelse(find.median==TRUE,quantile(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], probs=mdrt_05, na.rm=TRUE),
                                            mean(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], na.rm=TRUE)-
                                              n.sd*sd(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], na.rm=TRUE)),0)
  mdrt.50 <- ifelse(!is.null(mdrt.r),ifelse(find.median==TRUE,quantile(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], probs=mdrt_50, na.rm=TRUE),
                                            mean(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], na.rm=TRUE)),0)
  mdrt.95 <- ifelse(!is.null(mdrt.r),ifelse(find.median==TRUE,quantile(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], probs=mdrt_95, na.rm=TRUE),
                                            mean(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], na.rm=TRUE)+
                                              n.sd*sd(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], na.rm=TRUE)),0)

  min.x <- quantile(as.data.frame(nameofdata)[rownames(as.data.frame(df))[main1.r]][,1], probs=min_x, na.rm=TRUE)
  max.x <- quantile(as.data.frame(nameofdata)[rownames(as.data.frame(df))[main1.r]][,1], probs=max_x, na.rm=TRUE)

  x <- seq(min.x, max.x,length=100)

  mdrt.05.vals <- if(!is.null(mdrt.r)){b.mdrt*mdrt.05 + b.int1*x*mdrt.05 + b.int2*x^2*mdrt.05}else{rep(0,length(x))}
  mdrt.50.vals <- if(!is.null(mdrt.r)){b.mdrt*mdrt.50 + b.int1*x*mdrt.50 + b.int2*x^2*mdrt.50}else{rep(0,length(x))}
  mdrt.95.vals <- if(!is.null(mdrt.r)){b.mdrt*mdrt.95 + b.int1*x*mdrt.95 + b.int2*x^2*mdrt.95}else{rep(0,length(x))}

  line1 <- constant + b.b*x + b.a*x^2 + mdrt.05.vals
  line2 <- constant + b.b*x + b.a*x^2 + mdrt.50.vals
  line3 <- constant + b.b*x + b.a*x^2 + mdrt.95.vals

  more.mdrt.05 <- ifelse(!is.null(more.mdrt.r),ifelse(more.median==TRUE,quantile(as.data.frame(nameofdata)[rownames(df)[more.mdrt.r]][,1], probs=more.mdrt_05, na.rm=TRUE),
                                                      mean(as.data.frame(nameofdata)[rownames(df)[more.mdrt.r]][,1], na.rm=TRUE)-
                                                        n.more.sd.minus*sd(as.data.frame(nameofdata)[rownames(df)[more.mdrt.r]][,1], na.rm=TRUE)),0)
  more.mdrt.95 <- ifelse(!is.null(more.mdrt.r),ifelse(more.median==TRUE,quantile(as.data.frame(nameofdata)[rownames(df)[more.mdrt.r]][,1], probs=more.mdrt_95, na.rm=TRUE),
                                                      mean(as.data.frame(nameofdata)[rownames(df)[more.mdrt.r]][,1], na.rm=TRUE)+
                                                        n.more.sd.plus*sd(as.data.frame(nameofdata)[rownames(df)[more.mdrt.r]][,1], na.rm=TRUE)),0)

  more.mdrt.05.mdrt.05.vals <- if(!is.null(more.mdrt.r)){b.more.mdrt*more.mdrt.05 + b.more.int1*more.mdrt.05*mdrt.05 + b.more.int2*more.mdrt.05*x + b.more.int3*more.mdrt.05*x*mdrt.05 }else{rep(0,length(x))}
  more.mdrt.05.mdrt.95.vals <- if(!is.null(more.mdrt.r)){b.more.mdrt*more.mdrt.05 + b.more.int1*more.mdrt.05*mdrt.95 + b.more.int2*more.mdrt.05*x + b.more.int3*more.mdrt.05*x*mdrt.95 }else{rep(0,length(x))}
  more.mdrt.95.mdrt.05.vals <- if(!is.null(more.mdrt.r)){b.more.mdrt*more.mdrt.95 + b.more.int1*more.mdrt.95*mdrt.05 + b.more.int2*more.mdrt.95*x + b.more.int3*more.mdrt.95*x*mdrt.05 }else{rep(0,length(x))}
  more.mdrt.95.mdrt.95.vals <- if(!is.null(more.mdrt.r)){b.more.mdrt*more.mdrt.95 + b.more.int1*more.mdrt.95*mdrt.95 + b.more.int2*more.mdrt.95*x + b.more.int3*more.mdrt.95*x*mdrt.95 }else{rep(0,length(x))}

  line1.more.mdrt.05.mdrt.05 <- line1 + more.mdrt.05.mdrt.05.vals
  line3.more.mdrt.05.mdrt.95 <- line3 + more.mdrt.05.mdrt.95.vals
  line1.more.mdrt.95.mdrt.05 <- line1 + more.mdrt.95.mdrt.05.vals
  line3.more.mdrt.95.mdrt.95 <- line3 + more.mdrt.95.mdrt.95.vals

  par(mfrow=c(1,2))

  plot1 <- plot(c(min.x, max.x), c(min(line1.more.mdrt.05.mdrt.05,line3.more.mdrt.05.mdrt.95,line1.more.mdrt.95.mdrt.05,line3.more.mdrt.95.mdrt.95),
                                   max.y*max(line1.more.mdrt.05.mdrt.05,line3.more.mdrt.05.mdrt.95,line1.more.mdrt.95.mdrt.05,line3.more.mdrt.95.mdrt.95)),
                type="n",xlab=xlab, ylab=ylab,main=ifelse(!is.null(more.mdrt.r),main1,main))
  low  <- ifelse(flip.low.high==FALSE,1,6)
  high <- ifelse(flip.low.high==FALSE,6,1)
  if(!is.null(more.mdrt.r)){lines(x, line1.more.mdrt.05.mdrt.05, lty=low)}else(lines(x, line1, lty=low))
  if(!is.null(mdrt.50.name)){lines(x, line2, lty=2)}
  if(!is.null(more.mdrt.r)){lines(x, line1.more.mdrt.95.mdrt.05, lty=2)}else(lines(x, line3, lty=high))
  if(!is.null(more.mdrt.r)){legend("topright", legend = c(more.mdrt.05.name, more.mdrt.95.name),
                                  lty = c(low,2), xjust = 1, yjust = 1,adj = c(0, 0.5))} else{
  if(!is.null(mdrt.05.name)){if(is.null(mdrt.50.name)){legend("topright", legend = c(mdrt.05.name, mdrt.95.name),
                                                              lty = c(low,high), xjust = 1, yjust = 1,adj = c(0, 0.5))}else{
                                                       legend("topright", legend = c(mdrt.05.name, mdrt.50.name, mdrt.95.name),
                                                              lty = c(low,2,high), xjust = 1, yjust = 1,adj = c(0, 0.5))}}}
  if(two.graphs==TRUE){if(!is.null(more.mdrt.r)){plot1 <- plot(c(min.x, max.x), c(min(line1.more.mdrt.05.mdrt.05,line3.more.mdrt.05.mdrt.95,line1.more.mdrt.95.mdrt.05,line3.more.mdrt.95.mdrt.95),
                                                             max.y*max(line1.more.mdrt.05.mdrt.05,line3.more.mdrt.05.mdrt.95,line1.more.mdrt.95.mdrt.05,line3.more.mdrt.95.mdrt.95)),
                                          type="n",xlab=xlab, ylab=ylab,main=ifelse(!is.null(more.mdrt.r),main2,main))}}
  if(!is.null(more.mdrt.r)){lines(x, line3.more.mdrt.05.mdrt.95, lty=high)}
  if(!is.null(more.mdrt.r)){lines(x, line3.more.mdrt.95.mdrt.95, lty=3)}
  if(two.graphs==TRUE){if(!is.null(more.mdrt.r)){legend("topright", legend = c(more.mdrt.05.name, more.mdrt.95.name),
                                   lty = c(high,3), xjust = 1, yjust = 1,adj = c(0, 0.5))}}else{
                       if(!is.null(more.mdrt.r)){legend("bottomright", legend = c(more.mdrt.05.name.2, more.mdrt.95.name.2),
                                                                      lty = c(high,3), xjust = 1, yjust = 1,adj = c(0, 0.5))}
                                   }
}


plot.three.way.facet.grid <- function(two.graphs=TRUE, df, nameofdata,
                                      main1.r, main2.r=NULL, mdrt.r=NULL, int1.r=NULL, int2.r=NULL,
                                      find.median=FALSE, min_x=0.001, max_x=0.999, max.y=1,
                                      mdrt_05=.05, mdrt_50=0.5, mdrt_95=.95,n.sd=1,
                                      main=NULL,xlab="name", ylab="name",
                                      mdrt.05.name=NULL, mdrt.50.name=NULL, mdrt.95.name="name",
                                      flip.low.high=FALSE,
                                      more.mdrt.r=NULL,more.int1.r=NULL,more.int2.r=NULL,more.int3.r=NULL,
                                      more.median=TRUE,n.more.sd.minus=1,n.more.sd.plus=1,
                                      more.mdrt_05=.05, more.mdrt_95=.95, main1=NULL, main2=NULL,
                                      more.mdrt.05.name=NULL,more.mdrt.95.name=NULL,
                                      more.mdrt.05.name.2=NULL,more.mdrt.95.name.2=NULL){
  df <- as.data.frame(df)
  para.r <- c(main1.r,main2.r,mdrt.r,int1.r,int2.r,more.mdrt.r,more.int1.r,more.int2.r,more.int3.r)
  othr.r <- (1:nrow(df))[!((1:nrow(df)) %in% para.r)]

  beta.vec <- df[,2]
  b.b <- beta.vec[main1.r] # linear term
  b.a <- ifelse(!is.null(main2.r),beta.vec[main2.r],0) # quadratic term
  b.mdrt <- beta.vec[mdrt.r]
  b.int1 <- beta.vec[int1.r]
  b.int2 <- ifelse(!is.null(int2.r),beta.vec[int2.r],0)
  b.more.mdrt <- ifelse(!is.null(more.mdrt.r),beta.vec[more.mdrt.r],0)
  b.more.int1 <- ifelse(!is.null(more.int1.r),beta.vec[more.int1.r],0)
  b.more.int2 <- ifelse(!is.null(more.int2.r),beta.vec[more.int2.r],0)
  b.more.int3 <- ifelse(!is.null(more.int3.r),beta.vec[more.int3.r],0)
  b.othr.vec <- beta.vec[othr.r]

  otherterms <- as.matrix(as.data.frame(nameofdata)[rownames(as.data.frame(df))[othr.r[-1]]])
  otherTermMedians <- if(find.median==TRUE){robustbase::colMedians(otherterms,na.rm = TRUE)}else{colMeans(otherterms,na.rm = TRUE)}
  constant <- as.numeric(b.othr.vec[1] + t(otherTermMedians) %*% b.othr.vec[-1])

  mdrt.05 <- ifelse(!is.null(mdrt.r),ifelse(find.median==TRUE,quantile(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], probs=mdrt_05, na.rm=TRUE),
                                            mean(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], na.rm=TRUE)-
                                              n.sd*sd(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], na.rm=TRUE)),0)
  mdrt.50 <- ifelse(!is.null(mdrt.r),ifelse(find.median==TRUE,quantile(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], probs=mdrt_50, na.rm=TRUE),
                                            mean(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], na.rm=TRUE)),0)
  mdrt.95 <- ifelse(!is.null(mdrt.r),ifelse(find.median==TRUE,quantile(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], probs=mdrt_95, na.rm=TRUE),
                                            mean(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], na.rm=TRUE)+
                                              n.sd*sd(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], na.rm=TRUE)),0)

  min.x <- quantile(as.data.frame(nameofdata)[rownames(as.data.frame(df))[main1.r]][,1], probs=min_x, na.rm=TRUE)
  max.x <- quantile(as.data.frame(nameofdata)[rownames(as.data.frame(df))[main1.r]][,1], probs=max_x, na.rm=TRUE)

  x <- seq(min.x, max.x,length=100)

  mdrt.05.vals <- if(!is.null(mdrt.r)){b.mdrt*mdrt.05 + b.int1*x*mdrt.05 + b.int2*x^2*mdrt.05}else{rep(0,length(x))}
  mdrt.50.vals <- if(!is.null(mdrt.r)){b.mdrt*mdrt.50 + b.int1*x*mdrt.50 + b.int2*x^2*mdrt.50}else{rep(0,length(x))}
  mdrt.95.vals <- if(!is.null(mdrt.r)){b.mdrt*mdrt.95 + b.int1*x*mdrt.95 + b.int2*x^2*mdrt.95}else{rep(0,length(x))}

  line1 <- constant + b.b*x + b.a*x^2 + mdrt.05.vals
  line2 <- constant + b.b*x + b.a*x^2 + mdrt.50.vals
  line3 <- constant + b.b*x + b.a*x^2 + mdrt.95.vals

  more.mdrt.05 <- ifelse(!is.null(more.mdrt.r),ifelse(more.median==TRUE,quantile(as.data.frame(nameofdata)[rownames(df)[more.mdrt.r]][,1], probs=more.mdrt_05, na.rm=TRUE),
                                                      mean(as.data.frame(nameofdata)[rownames(df)[more.mdrt.r]][,1], na.rm=TRUE)-
                                                        n.more.sd.minus*sd(as.data.frame(nameofdata)[rownames(df)[more.mdrt.r]][,1], na.rm=TRUE)),0)
  more.mdrt.95 <- ifelse(!is.null(more.mdrt.r),ifelse(more.median==TRUE,quantile(as.data.frame(nameofdata)[rownames(df)[more.mdrt.r]][,1], probs=more.mdrt_95, na.rm=TRUE),
                                                      mean(as.data.frame(nameofdata)[rownames(df)[more.mdrt.r]][,1], na.rm=TRUE)+
                                                        n.more.sd.plus*sd(as.data.frame(nameofdata)[rownames(df)[more.mdrt.r]][,1], na.rm=TRUE)),0)

  more.mdrt.05.mdrt.05.vals <- if(!is.null(more.mdrt.r)){b.more.mdrt*more.mdrt.05 + b.more.int1*more.mdrt.05*mdrt.05 + b.more.int2*more.mdrt.05*x + b.more.int3*more.mdrt.05*x*mdrt.05 }else{rep(0,length(x))}
  more.mdrt.05.mdrt.95.vals <- if(!is.null(more.mdrt.r)){b.more.mdrt*more.mdrt.05 + b.more.int1*more.mdrt.05*mdrt.95 + b.more.int2*more.mdrt.05*x + b.more.int3*more.mdrt.05*x*mdrt.95 }else{rep(0,length(x))}
  more.mdrt.95.mdrt.05.vals <- if(!is.null(more.mdrt.r)){b.more.mdrt*more.mdrt.95 + b.more.int1*more.mdrt.95*mdrt.05 + b.more.int2*more.mdrt.95*x + b.more.int3*more.mdrt.95*x*mdrt.05 }else{rep(0,length(x))}
  more.mdrt.95.mdrt.95.vals <- if(!is.null(more.mdrt.r)){b.more.mdrt*more.mdrt.95 + b.more.int1*more.mdrt.95*mdrt.95 + b.more.int2*more.mdrt.95*x + b.more.int3*more.mdrt.95*x*mdrt.95 }else{rep(0,length(x))}

  line1.more.mdrt.05.mdrt.05 <- line1 + more.mdrt.05.mdrt.05.vals
  line3.more.mdrt.05.mdrt.95 <- line3 + more.mdrt.05.mdrt.95.vals
  line1.more.mdrt.95.mdrt.05 <- line1 + more.mdrt.95.mdrt.05.vals
  line3.more.mdrt.95.mdrt.95 <- line3 + more.mdrt.95.mdrt.95.vals

  par(mfrow=c(1,2))

  plot1 <- plot(c(min.x, max.x), c(min(line1.more.mdrt.05.mdrt.05,line3.more.mdrt.05.mdrt.95,line1.more.mdrt.95.mdrt.05,line3.more.mdrt.95.mdrt.95),
                                   max.y*max(line1.more.mdrt.05.mdrt.05,line3.more.mdrt.05.mdrt.95,line1.more.mdrt.95.mdrt.05,line3.more.mdrt.95.mdrt.95)),
                type="n",xlab=xlab, ylab=ylab,main=ifelse(!is.null(more.mdrt.r),main1,main))
  low  <- ifelse(flip.low.high==FALSE,1,6)
  high <- ifelse(flip.low.high==FALSE,6,1)
  if(!is.null(more.mdrt.r)){lines(x, line1.more.mdrt.05.mdrt.05, lty=low)}else(lines(x, line1, lty=low))
  if(!is.null(mdrt.50.name)){lines(x, line2, lty=2)}
  if(!is.null(more.mdrt.r)){lines(x, line3.more.mdrt.05.mdrt.95, lty=2)}else(lines(x, line3, lty=high))
  if(!is.null(more.mdrt.r)){legend("topright", legend = c(more.mdrt.05.name, more.mdrt.95.name),
                                   lty = c(low,2), xjust = 1, yjust = 1,adj = c(0, 0.5))} else{
                                     if(!is.null(mdrt.05.name)){if(is.null(mdrt.50.name)){legend("topright", legend = c(mdrt.05.name, mdrt.95.name),
                                                                                                 lty = c(low,high), xjust = 1, yjust = 1,adj = c(0, 0.5))}else{
                                                                                                   legend("topright", legend = c(mdrt.05.name, mdrt.50.name, mdrt.95.name),
                                                                                                          lty = c(low,2,high), xjust = 1, yjust = 1,adj = c(0, 0.5))}}}
  if(two.graphs==TRUE){if(!is.null(more.mdrt.r)){plot1 <- plot(c(min.x, max.x), c(min(line1.more.mdrt.05.mdrt.05,line3.more.mdrt.05.mdrt.95,line1.more.mdrt.95.mdrt.05,line3.more.mdrt.95.mdrt.95),
                                                                                  max.y*max(line1.more.mdrt.05.mdrt.05,line3.more.mdrt.05.mdrt.95,line1.more.mdrt.95.mdrt.05,line3.more.mdrt.95.mdrt.95)),
                                                               type="n",xlab=xlab, ylab=ylab,main=ifelse(!is.null(more.mdrt.r),main2,main))}}
  if(!is.null(more.mdrt.r)){lines(x, line1.more.mdrt.95.mdrt.05, lty=high)}
  if(!is.null(more.mdrt.r)){lines(x, line3.more.mdrt.95.mdrt.95, lty=3)}
  if(two.graphs==TRUE){if(!is.null(more.mdrt.r)){legend("topright", legend = c(more.mdrt.05.name, more.mdrt.95.name),
                                                        lty = c(high,3), xjust = 1, yjust = 1,adj = c(0, 0.5))}}else{
                                                          if(!is.null(more.mdrt.r)){legend("bottomright", legend = c(more.mdrt.05.name.2, more.mdrt.95.name.2),
                                                                                           lty = c(high,3), xjust = 1, yjust = 1,adj = c(0, 0.5))}
                                                        }
}

## cor matrix ##
cor.matrix <- function(result.w.full.var, number.of.IVs, y.name.in.doc, y.name.in.reg, x.names,digits=3){
  data = if(class(result.w.full.var)=="lm"){result.w.full.var$model}else{result.w.full.var@frame}
  colnames <- coef(summary(result.w.full.var))[-1,] %>% rownames() %>% `[`(1:number.of.IVs)
  data.m   <- as.matrix(data[c(colnames,y.name.in.reg)])
  c.marix  <- apply(cor(data.m, use="complete.obs"),FUN=formatC,MARGIN=2,digits=digits,format='f')
  c.marix[upper.tri(c.marix,diag = FALSE)] <- NA
  means <- formatC(colMeans(data.m,na.rm=TRUE),digits=digits,format='f')
  sds   <- formatC(apply(data.m,FUN=sd,MARGIN=2,na.rm=TRUE),digits=digits,format='f')
  df <- data.frame(Mean=means,S.D.=sds,c.marix,stringsAsFactors = FALSE)
  colnames(df)[3:ncol(df)] <- 1:nrow(df)
  rownames(df) <- paste(seq(1:nrow(df)),c(x.names[1:number.of.IVs],y.name.in.doc),sep=". ")
  df[is.na(df)] <- ""
  return(df)
}

### pratical significance ###

practical.significance <- function(df, nameofdata,
                        main1.r, mdrt.r=NULL, int1.r=NULL,
                        find.median=TRUE, n.sd.x=1,
                        mdrt_05=.05, n.sd.mod=1){
  df <- as.data.frame(df)
  para.r <- c(main1.r,mdrt.r,int1.r)
  othr.r <- (1:nrow(df))[!((1:nrow(df)) %in% para.r)]

  beta.vec <- df[,2]
  b.b <- beta.vec[main1.r]
  b.mdrt <- beta.vec[mdrt.r]
  b.int1 <- ifelse(!is.null(int1.r),beta.vec[int1.r],0)
  b.othr.vec <- beta.vec[othr.r]
  names(b.othr.vec) <- rownames(as.data.frame(df))[othr.r]

  otherterms <- as.matrix(as.data.frame(nameofdata)[rownames(as.data.frame(df))[othr.r[-1]]])
  otherTermMedians <- if(find.median==TRUE){robustbase::colMedians(otherterms,na.rm = TRUE)}else{colMeans(otherterms,na.rm = TRUE)}
  constant <- as.numeric(b.othr.vec[1] + t(otherTermMedians) %*% b.othr.vec[-1])

  mdrt.05 <- ifelse(!is.null(mdrt.r),ifelse(find.median==TRUE,quantile(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], probs=mdrt_05, na.rm=TRUE),
                                            mean(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], na.rm=TRUE)+
                                              n.sd.mod*sd(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], na.rm=TRUE)),0)


  x <- n.sd.x*sd(as.data.frame(nameofdata)[rownames(as.data.frame(df))[main1.r]][,1], na.rm=TRUE)
  line1 <- constant + b.b*x + b.mdrt*mdrt.05 + b.int1*x*mdrt.05
  result <- c(positional_shift = x,
              starting_position = mdrt.05,
              size = exp(otherTermMedians["size.log"]),
              car = line1)
  return(result)
 }

practical.significance.int <- function(   df, nameofdata,
                                      main1.r, mdrt.r=NULL, int1.r=NULL,
                                      find.median=FALSE, n.sd.x=1,
                                      mdrt_05=.05, n.sd.mod=1,
                                      more.mdrt.r=NULL,more.int1.r=NULL,more.int2.r=NULL,more.int3.r=NULL,
                                      more.median=TRUE,n.more.sd=1){
  df <- as.data.frame(df)
  para.r <- c(main1.r,mdrt.r,int1.r,more.mdrt.r,more.int1.r,more.int2.r,more.int3.r)
  othr.r <- (1:nrow(df))[!((1:nrow(df)) %in% para.r)]

  beta.vec <- df[,2]
  b.b <- beta.vec[main1.r] # linear term
  b.mdrt <- beta.vec[mdrt.r]
  b.int1 <- beta.vec[int1.r]
  b.more.mdrt <- ifelse(!is.null(more.mdrt.r),beta.vec[more.mdrt.r],0)
  b.more.int1 <- ifelse(!is.null(more.int1.r),beta.vec[more.int1.r],0)
  b.more.int2 <- ifelse(!is.null(more.int2.r),beta.vec[more.int2.r],0)
  b.more.int3 <- ifelse(!is.null(more.int3.r),beta.vec[more.int3.r],0)
  b.othr.vec <- beta.vec[othr.r]

  otherterms <- as.matrix(as.data.frame(nameofdata)[rownames(as.data.frame(df))[othr.r[-1]]])
  otherTermMedians <- if(find.median==TRUE){robustbase::colMedians(otherterms,na.rm = TRUE)}else{colMeans(otherterms,na.rm = TRUE)}
  constant <- as.numeric(b.othr.vec[1] + t(otherTermMedians) %*% b.othr.vec[-1])

  mdrt.05 <- ifelse(!is.null(mdrt.r),ifelse(find.median==TRUE,quantile(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], probs=mdrt_05, na.rm=TRUE),
                                            mean(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], na.rm=TRUE)+
                                              n.sd.mod*sd(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], na.rm=TRUE)),0)

  x <- n.sd.x*sd(as.data.frame(nameofdata)[rownames(as.data.frame(df))[main1.r]][,1], na.rm=TRUE)

  mdrt.05.vals <- if(!is.null(mdrt.r)){b.mdrt*mdrt.05 + b.int1*x*mdrt.05}else{rep(0,length(x))}

  line1 <- constant + b.b*x + mdrt.05.vals

  more.mdrt.05 <- ifelse(!is.null(more.mdrt.r),ifelse(more.median==TRUE,quantile(as.data.frame(nameofdata)[rownames(df)[more.mdrt.r]][,1], probs=more.mdrt_05, na.rm=TRUE),
                                                      mean(as.data.frame(nameofdata)[rownames(df)[more.mdrt.r]][,1], na.rm=TRUE)+
                                                        n.more.sd*sd(as.data.frame(nameofdata)[rownames(df)[more.mdrt.r]][,1], na.rm=TRUE)),0)

  more.mdrt.05.mdrt.05.vals <- if(!is.null(more.mdrt.r)){b.more.mdrt*more.mdrt.05 + b.more.int1*more.mdrt.05*mdrt.05 + b.more.int2*more.mdrt.05*x + b.more.int3*more.mdrt.05*x*mdrt.05 }else{rep(0,length(x))}

  line1.more.mdrt.05.mdrt.05 <- line1 + more.mdrt.05.mdrt.05.vals

  result <- c(positional_shift = x,
              starting_position = mdrt.05,
              size = exp(more.mdrt.05),
              car = line1.more.mdrt.05.mdrt.05)
  return(result)
}

plot.full.model.select <- function(df, nameofdata,
                        main1.r, main2.r=NULL, mdrt.r=NULL, int1.r=NULL, int2.r=NULL,
                        no.dr.mdrt1.r=NULL, no.dr.int1.r=NULL, no.dr.mdrt2.r=NULL, no.dr.int2.r=NULL,
                        find.median=TRUE, min_x=0.001, max_x=0.999, max.y=1,
                        mdrt_05=.05, mdrt_50=0.5, mdrt_95=.95,n.sd=1,
                        main=NULL,xlab="name", ylab="name",
                        mdrt.05.name=NULL, mdrt.50.name=NULL, mdrt.95.name="name",
                        flip.low.high=FALSE){
  df <- as.data.frame(df)
  para.r <- c(main1.r,main2.r,mdrt.r,int1.r,int2.r)
  othr.r <- (1:nrow(df))[!((1:nrow(df)) %in% c(para.r))]
  othr.exlude_interaction.r <- (1:nrow(df))[!((1:nrow(df)) %in% c(no.dr.int1.r,no.dr.int2.r,para.r))]
  beta.vec <- df[,2]
  b.b <- beta.vec[main1.r]
  b.a <- ifelse(!is.null(main2.r),beta.vec[main2.r],0)
  b.mdrt <- beta.vec[mdrt.r]
  b.int1 <- ifelse(!is.null(int1.r),beta.vec[int1.r],0)
  b.int2 <- ifelse(!is.null(int2.r),beta.vec[int2.r],0)
  b.othr.vec <- beta.vec[othr.r]

  otherterms.1 <- as.matrix(as.data.frame(nameofdata)[rownames(as.data.frame(df))[othr.exlude_interaction.r[-1]]])
  no.drow.col1 <- as.matrix(as.data.frame(nameofdata)[rownames(as.data.frame(df))[no.dr.mdrt1.r]]) * as.matrix(as.data.frame(nameofdata)[,rownames(as.data.frame(df))[main1.r]])
  no.drow.col2 <- if(!is.null(no.dr.mdrt2.r)){as.matrix(as.data.frame(nameofdata)[rownames(as.data.frame(df))[no.dr.mdrt2.r]]) * as.matrix(as.data.frame(nameofdata)[,rownames(as.data.frame(df))[main1.r]])}
  otherterms <- cbind(otherterms.1, no.drow.col1, no.drow.col2)
  otherTermMedians <- if(find.median==TRUE){robustbase::colMedians(otherterms,na.rm = TRUE)}else{colMeans(otherterms,na.rm = TRUE)}
  constant <- as.numeric(b.othr.vec[1] + t(otherTermMedians) %*% b.othr.vec[-1])

  mdrt.05 <- ifelse(!is.null(mdrt.r),ifelse(find.median==TRUE,quantile(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], probs=mdrt_05, na.rm=TRUE),
                                            mean(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], na.rm=TRUE)-
                                              n.sd*sd(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], na.rm=TRUE)),0)
  mdrt.50 <- ifelse(!is.null(mdrt.r),ifelse(find.median==TRUE,quantile(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], probs=mdrt_50, na.rm=TRUE),
                                            mean(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], na.rm=TRUE)),0)
  mdrt.95 <- ifelse(!is.null(mdrt.r),ifelse(find.median==TRUE,quantile(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], probs=mdrt_95, na.rm=TRUE),
                                            mean(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], na.rm=TRUE)+
                                              n.sd*sd(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], na.rm=TRUE)),0)
  min.x <- quantile(as.data.frame(nameofdata)[rownames(as.data.frame(df))[main1.r]][,1], probs=min_x, na.rm=TRUE)
  max.x <- quantile(as.data.frame(nameofdata)[rownames(as.data.frame(df))[main1.r]][,1], probs=max_x, na.rm=TRUE)
  x <- seq(min.x, max.x,length=100)
  mdrt.05.vals <- if(!is.null(mdrt.r)){b.mdrt*mdrt.05 + b.int1*x*mdrt.05 + b.int2*x^2*mdrt.05}else{rep(0,length(x))}
  mdrt.50.vals <- if(!is.null(mdrt.r)){b.mdrt*mdrt.50 + b.int1*x*mdrt.50 + b.int2*x^2*mdrt.50}else{rep(0,length(x))}
  mdrt.95.vals <- if(!is.null(mdrt.r)){b.mdrt*mdrt.95 + b.int1*x*mdrt.95 + b.int2*x^2*mdrt.95}else{rep(0,length(x))}
  line1.linear <- constant + b.b*x + b.a*x^2 + mdrt.05.vals
  line2.linear <- constant + b.b*x + b.a*x^2 + mdrt.50.vals
  line3.linear <- constant + b.b*x + b.a*x^2 + mdrt.95.vals
   line1 <- line1.linear
   line2 <- line2.linear
   line3 <- line3.linear
  plot1 <- plot(c(min.x, max.x), c(min(line1,line2,line3), max.y*max(line1,line2,line3)), type="n",xlab=xlab, ylab=ylab,main=main)
  low  <- ifelse(flip.low.high==FALSE,1,6)
  high <- ifelse(flip.low.high==FALSE,6,1)
  lines(x, line1, lty=low)
  if(!is.null(mdrt.50.name)){lines(x, line2, lty=2)}
  lines(x, line3, lty=high)
  if(!is.null(mdrt.05.name)){if(is.null(mdrt.50.name)){legend("topleft", legend = c(mdrt.05.name, mdrt.95.name),
                                                              lty = c(low,high), xjust = 1, yjust = 1,adj = c(0, 0.5))}else{legend("topright", legend = c(mdrt.05.name, mdrt.50.name, mdrt.95.name),
                                                                                                                                   lty = c(low,2,high), xjust = 1, yjust = 1,adj = c(0, 0.5))}}
}

