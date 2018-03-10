simple1dsolve<-function(x0,step,sd,degree){

require(deSolve)

times<-seq(0,5,by=step)

##### SIMPLE 1-D SYSTEM

parameters<-c(a=1,b=-0.2,c=-0.1)
simple1d<-function(t,state,parameters){
  a<-parameters[[1]]
  b<-parameters[[2]]
  c<-parameters[[3]]
  x<-state
  dx<-a*x+b*x**3+c*x**4
  list(dx)
}

state<-c(x=x0)
sol1<-ode(y=state,times=times,func=simple1d,parms=parameters)

x<-sol1[,2]

y<-sapply(x,simple1d,t=0,parameters=parameters)
y1<-unlist(y)
print(nrow(y1))
x<-poly(x,degree=degree,raw=TRUE)

noise<-rnorm(n=nrow(y1),sd=sd)
y1<-y1+noise

data1<-data.frame(y1,x)

return(data1)
}