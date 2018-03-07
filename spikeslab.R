args<-commandArgs(TRUE)
var<-as.double(args[2])
thinning<-as.double(args[3])

library(spikeslab)
sig<-sqrt(var)
# sig<-sqrt(10)

data<-read.csv(paste("~/Desktop/research/",args[1],sep=""))
y<-data[,2]
x<-data[,3:ncol(data)]
thin_indices<-seq(1,nrow(data),by=thinning)
# Changing where we're sampling from
# thin_indices<-seq(1,nrow(data)/thinning)
x<-x[thin_indices,]
y<-y[thin_indices]

noise=rnorm(n=nrow(x),sd=sig)
y=y+noise

fit<-spikeslab(x=x,y=y)
print(fit)
fit$bma.scale
