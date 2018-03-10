bms_summary<-function(x0,step,tf,sd,degree,var=1){
  source("simple1dsolve.R")
  source("cubic2dsolve.R")
  require(BMS)
  require(deSolve)
  require(gridExtra)
  require(grid)
  require(ggplot2)
  if (length(x0)==1){
    df<-simple1dsolve(x0,step,tf,sd,degree)
  }
  else if (length(x0)==2){
    df<-cubic2dsolve(x0,step,tf,sd,degree)
    # pick variable to regress against
    if(var==1){
      df$y2=NULL
    }
    else {
      df$y1=NULL
    }
  }
  
  reg<-bms(df,user.int=F)
  coefs<-tableGrob(format(coef(reg)[,1:3],digits=3))
  # grid.newpage()
  # grid.draw(coefs)
  tops<-format(topmodels.bma(reg)[,1:5],digits=2); colnames(tops)<-paste(rep("M",times=5),1:5,sep="")
  tops<-tableGrob(tops)
  p1<-density(reg, reg = "X1",plot=F)
  p2<-density(reg, reg = "X2",plot=F)
  p3<-density(reg, reg = "X3",plot=F)
  p4<-density(reg, reg = "X4",plot=F)
  
  p1<-data.frame(as.numeric(p1$x),as.numeric(p1$y)); colnames(p1)<-c("X1","Density")
  p2<-data.frame(as.numeric(p2$x),as.numeric(p2$y)); colnames(p2)<-c("X2","Density")
  p3<-data.frame(as.numeric(p3$x),as.numeric(p3$y)); colnames(p3)<-c("X3","Density")
  p4<-data.frame(as.numeric(p4$x),as.numeric(p4$y)); colnames(p4)<-c("X4","Density")
  
  p1<-ggplot(p1,aes(x=X1,y=Density))+geom_line()
  p2<-ggplot(p2,aes(x=X2,y=Density))+geom_line()
  p3<-ggplot(p3,aes(x=X3,y=Density))+geom_line()
  p4<-ggplot(p4,aes(x=X4,y=Density))+geom_line()
  # 
  lay <- rbind(c(1,3,4),
               c(2,5,6))
  # lay <- rbind(c(1,3,4,5,6),
  #              c(2,3,4,5,6))
  # 
  grid.arrange(coefs,tops,p1,p2,p3,p4,layout_matrix=lay)
}
