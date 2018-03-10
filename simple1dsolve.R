simple1dsolve<-function(x0,step,tf,sd,degree){

require(deSolve)

times<-seq(0,tf,by=step)

##### SIMPLE 1-D SYSTEM

parameters<-c(a=1,b=-0.2,c=-0.1)

# state<-x0
sol1<-ode(y=x0,times=times,func=simple1d,parms=parameters)

x<-sol1[,2]
y<-sapply(x,simple1d,t=0,parameters=parameters)
y1<-unlist(y)
x<-poly(x,degree=degree,raw=TRUE)

noise<-rnorm(n=length(y1),sd=sd)
y1<-y1+noise

data1<-data.frame(y1,x)

return(data1)
}

simple1d<-function(t,X,parameters){
  a<-parameters[[1]]
  b<-parameters[[2]]
  c<-parameters[[3]]
  dx<-a*X+b*X**3+c*X**4
  list(dx)
}
