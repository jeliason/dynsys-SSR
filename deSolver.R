library(deSolve)

parameters<-c(a=1,b=-0.2,c=-0.1)
step=0.01
simple1d<-function(t,state,parameters){
  a<-parameters[[1]]
  b<-parameters[[2]]
  c<-parameters[[3]]
  x<-state
  dx<-a*x+b*x**3+c*x**4
  list(dx)
}
times<-seq(0,5,by=step)