cubic2dsolve<-function(x0,step,tf,sd,degree){
  
  require(deSolve)
  
  times<-seq(0,tf,by=step)
  
  ##### SIMPLE 2-D SYSTEM
  
  parameters<-c(a=-0.1,b=2,c=-2,d=-0.1)
  
  # state<-x0
  sol1<-ode(y=x0,times=times,func=simple2d,parms=parameters)
  
  x<-sol1[,2:ncol(sol1)]
  # print(x)
  y<-apply(x,1,simple2d,t=0,parameters=parameters)
  y<-unlist(y)
  idx<-seq(1,length(y),by=2)
  y1<-y[idx]
  y2<-y[-idx]
  y<-data.frame(y1,y2)
  
  x<-polym(x[,1],x[,2],degree=degree,raw=TRUE)
  
  noise<-rnorm(n=length(y1),sd=sd)
  y<-y+noise
  
  data1<-data.frame(y,x)
  
  return(data1)
}

simple2d<-function(t,state,parameters){
  a<-parameters[[1]]
  b<-parameters[[2]]
  c<-parameters[[3]]
  d<-parameters[[4]]
  X<-state[[1]]
  Y<-state[[2]]
  dX<-a*X**3+b*Y**3
  dY<-c*X**3+d*Y**3
  list(c(dX,dY))
}