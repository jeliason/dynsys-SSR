args<-commandArgs(TRUE)
var<-as.double(args[2])
thinning<-as.double(args[3])

library(basad)
sig<-sqrt(var)
# sig<-sqrt(10)

data<-read.csv(paste("~/Desktop/research/",args[1],sep=""))
y<-data[,2]
x<-data[,3:ncol(data)]
thin_indices<-seq(1,nrow(data),by=thinning)
x<-x[thin_indices,]
y<-y[thin_indices]

xc<-scale(x)
yc<-y-mean(y)

noise=rnorm(n=nrow(x),sd=sig)
yc=yc+noise

fit<-basad(x=xc,y=yc,prior.dist="Laplace",select.cri="BIC")
print(fit)

