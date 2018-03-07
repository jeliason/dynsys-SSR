args<-commandArgs(TRUE)
var<-as.double(args[2])
thinning<-as.double(args[3])
sig<-sqrt(var)

source('~/Desktop/research/sparsebayes/bcr/BayesPen.R')
library(mvtnorm)
set.seed(77)

data<-read.csv(paste("~/Desktop/research/",args[1],sep=""))
y<-data[,2]
x<-data[,3:ncol(data)]
# thin_indices<-seq(1,nrow(data),by=thinning)
# Changing where we're sampling from
thin_indices<-seq(1,nrow(data)/thinning)
x<-x[thin_indices,]
y<-y[thin_indices]

xc<-scale(x)
yc<-y-mean(y)

noise=rnorm(n=nrow(x),sd=sig)
yc=yc+noise

# Fit the model
prior = list(varE=list(df=3,S=1),varBR=list(df=3,S=1))
fit = Bayes.pen(yc, xc, prior=prior,nIter=8000)
fit$order.joint
fit$order.marg