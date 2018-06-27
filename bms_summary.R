bms_summary<-function(x0,step,tf,sd,degree,var='x',df=NULL){
  source("simple1dsolve.R")
  source("cubic2dsolve.R")
  require(BMS)
  require(deSolve)
  require(gridExtra)
  require(grid)
  require(ggplot2)
  numModel=5
  
  if(is.null(df)){
  if (length(x0)==1){
    df<-simple1dsolve(x0,step,tf,sd,degree)
  }
  else if (length(x0)==2){
    df<-cubic2dsolve(x0,step,tf,sd,degree)
    # pick variable to regress against
    if(var=='x'){
      df$y2=NULL
    }
    else {
      df$y1=NULL
    }
  }
  }
  numGraphs=ncol(df)-1
  
  fit<-bms(df,user.int=F,force.full.ols=F)
  
  means<-lapply(1:numGraphs,function(x) coef(fit,order.by.pip=F)[x,2])
  sdev<-lapply(1:numGraphs,function(x) coef(fit,order.by.pip=F)[x,3])
  
  means[[1]]
  coefs<-tableGrob(format(coef(fit)[,1:3],digits=3))
  tops<-format(topmodels.bma(fit)[,1:numModel],digits=2); colnames(tops)<-paste(rep("M",times=numModel),1:numModel,sep="")
  tops<-tableGrob(tops)

  p<-lapply(1:numGraphs,function(y) density(x=fit,reg=y,plot=F))
  p<-lapply(1:numGraphs,function(z) data.frame(as.numeric(p[[z]]$x),as.numeric(p[[z]]$y)))
  names<-lapply(1:numGraphs,function(x) c(paste0("X",x),"Density"))
  p <- Map(setNames, p, names)
  p<-lapply(1:numGraphs,function(z) ggplot(p[[z]],aes_string(x=paste0("X",z),y="Density"))+
              geom_area(color="darkblue",fill="lightblue")+geom_vline(xintercept=means[[z]])+
              geom_vline(xintercept = c(means[[z]]-sdev[[z]],means[[z]]+sdev[[z]]),linetype="dotted")+
              annotate("label", x = Inf, y = Inf, label = paste0("SD=",format(sdev[[z]],digits = 3)),vjust=1,hjust=1))
  
  p<-c(list(coefs,tops),p)

  nRow=floor(sqrt(numGraphs+2))
  do.call("grid.arrange",c(p,nrow=nRow))
}
