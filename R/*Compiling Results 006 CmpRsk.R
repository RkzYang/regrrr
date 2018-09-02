suppressMessages(lapply(c("dplyr","tidyr","lme4","purrr","gee","MuMIn","car"), require, character.only = TRUE))


compare.models<- function(model1,model2,model3=NULL,model4=NULL,model5=NULL,model6=NULL,model7=NULL,model8=NULL,n=5){
  compare <- if(n==5){suppressWarnings(suppressMessages(anova(model1,model2,model3,model4,model5)))}else if(n==6){
    suppressWarnings(suppressMessages(anova(model1,model2,model3,model4,model5,model6)))}else if(n==7){
      suppressWarnings(suppressMessages(anova(model1,model2,model3,model4,model5,model6,model7)))
    }else if(n==8){
      suppressWarnings(suppressMessages(anova(model1,model2,model3,model4,model5,model6,model7,model8)))
    }
  ch          <- function(x){as.character(x)}
  convert.sig <- function(df){ifelse(df<0.001,"***",ifelse(df<0.01,"**",ifelse(df<0.05,"*",ifelse(df<0.1,"†",""))))}
  dg          <- function(x)formatC(x, format = "f", digits = 2)
  model.list  <- list(model1, model2, model3, model4, model5, model6, model7, model8)
  model.list  <- model.list[1:sum(!unlist((map(model.list, is.null))))]

  compare.df <- if(class(model1)[1]=="negbin"){
               data.frame(AIC=ch(t(dg(unlist(purrr::map(model.list, MuMIn::AICc))))),
               Log_Likelihood=ch(t(dg(compare[3][[1]]))), Chisq=ch(t(dg(compare[7][[1]]))),
               sig=ch(t(convert.sig(compare[8][[1]])))) %>% unite(Chisq,Chisq,sig,sep="") %>% t()

  }else{
               data.frame(AIC=ch(t(dg(unlist(purrr::map(model.list, MuMIn::AICc))))),
               Log_Likelihood=ch(t(dg(compare[1][[1]]))),Chisq=ch(t(dg(compare[2][[1]]))),
               sig=ch(t(convert.sig(compare[4][[1]])))) %>% unite(Chisq,Chisq,sig,sep="") %>% t() }
  compare.df[nrow(compare.df),1] <- ""
  compare.df <- data.frame(Variables=row.names(compare.df),compare.df)
  return(compare.df)
}

combine.result <- function(tbl_1,tbl_2, n.tbl=2, tbl_3=NULL,tbl_4=NULL,tbl_5=NULL,tbl_6=NULL,tbl_7=NULL,tbl_8=NULL,tbl_9=NULL) {
  list_tbls <- list(tbl_1,tbl_2,tbl_3,tbl_4,tbl_5,tbl_6,tbl_7,tbl_8,tbl_9)[1:n.tbl]
  main.table <- list_tbls %>%
    purrr::reduce(right_join,by=c("n.r","var")) %>%
    dplyr::select(-1)
  main.table[is.na(main.table)] <- ""
  return(main.table)}


cor.matrix <- function(result.w.full.var, nameofdata, number.of.IVs, y1.name.in.doc, y2.name.in.doc=NULL, y3.name.in.doc=NULL, y1.name.in.reg, y2.name.in.reg, y3.name.in.reg=NULL, x.names, digits=2){
  data =  if(class(result.w.full.var)[1]=="negbin"){nameofdata[c(names(result.w.full.var$model),y1.name.in.reg,y2.name.in.reg,y3.name.in.reg)]}else{nameofdata[c(names(unlist(result.w.full.var$assign)),y1.name.in.reg,y2.name.in.reg,y3.name.in.reg)]}
  colnames <- if(class(result.w.full.var)[1]=="negbin"){rownames(coef(summary(result.w.full.var)))[-1]}else{rownames(coef(summary(result.w.full.var)))}
  data.m   <- as.matrix(data[c(colnames,y1.name.in.reg,y2.name.in.reg,y3.name.in.reg)])
  c.marix  <- apply(cor(data.m, use="complete.obs"), FUN=formatC, MARGIN=2, digits=digits, format='f')
  c.marix[upper.tri(c.marix,diag = FALSE)] <- NA
  means <- formatC(colMeans(data.m,na.rm=TRUE),digits=digits,format='f')
  sds   <- formatC(apply(data.m,FUN=sd,MARGIN=2,na.rm=TRUE),digits=digits,format='f')
  df <- data.frame(Mean=means,S.D.=sds,c.marix,stringsAsFactors = FALSE)
  colnames(df)[3:ncol(df)] <- 1:nrow(df)
  rownames(df) <- paste(seq(1:nrow(df)),c(x.names[1:number.of.IVs],y1.name.in.doc,y2.name.in.doc,y3.name.in.doc),sep=". ")
  df[is.na(df)] <- ""
  return(df)
}

plot.hazard <- function(df, nameofdata,
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

  otherterms <- as.matrix(as.data.frame(nameofdata)[,rownames(as.data.frame(df))[othr.exlude_interaction.r]])
  otherTermMedians <- if(find.median==TRUE){robustbase::colMedians(otherterms,na.rm = TRUE)}else{colMeans(otherterms,na.rm = TRUE)}
  constant <- as.numeric(t(otherTermMedians) %*% b.othr.vec)

  mdrt.05 <- ifelse(!is.null(mdrt.r),ifelse(find.median==TRUE,quantile(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], probs=mdrt_05, na.rm=TRUE),
                                            mean(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], na.rm=TRUE)-
                                              n.sd*sd(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], na.rm=TRUE)),0)
  mdrt.50 <- ifelse(!is.null(mdrt.r),ifelse(find.median==TRUE,quantile(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], probs=mdrt_50, na.rm=TRUE),
                                            mean(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], na.rm=TRUE)),0)
  mdrt.95 <- ifelse(!is.null(mdrt.r),ifelse(find.median==TRUE,quantile(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], probs=mdrt_95, na.rm=TRUE),
                                            mean(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], na.rm=TRUE)+
                                              n.sd*sd(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], na.rm=TRUE)),0)
  min.x <- quantile(as.data.frame(nameofdata)[rownames(as.data.frame(df))[main1.r]][,1], probs=min_x, rm.na=TRUE)
  max.x <- quantile(as.data.frame(nameofdata)[rownames(as.data.frame(df))[main1.r]][,1], probs=max_x, rm.na=TRUE)
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
