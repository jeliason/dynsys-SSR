args<-commandArgs(TRUE)
var<-as.double(args[2])
thinning<-as.double(args[3])
sig<-sqrt(var)

library(glmnet)
df<-read.csv(paste("~/Desktop/research/",args[1],sep=""))
x = df[,4:ncol(df)] 
y = df[,1:3]
# set.seed(77)
x<-sapply(x,as.numeric)
y<-sapply(y,as.numeric)

thin_indices<-seq(1,nrow(x),by=thinning)
x<-x[thin_indices,]
y<-y[thin_indices,]

noise=rnorm(n=nrow(x),sd=sig)
y=y+noise

# Fit the model
cvfit = cv.glmnet(x, y,family="mgaussian",alpha=1)
# plot(cvfit)

coef(cvfit,s="lambda.min")