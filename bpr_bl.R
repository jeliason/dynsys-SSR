args<-commandArgs(TRUE)
var<-as.double(args[2])
thinning<-as.double(args[3])

library(BPrimm)
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

fit<-Bayes.Lasso(y=yc,X=xc, n.burn=10000, n.thin=10, n.iter=1000, r=0.01, s=0.01) 
print(fit$kappa)
