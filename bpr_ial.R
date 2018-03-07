args<-commandArgs(TRUE)
var<-as.double(args[2])
thinning<-as.double(args[3])

library(BPrimm)
sig<-sqrt(var)

data<-read.csv(paste("~/Desktop/research/",args[1],sep=""))
y<-data[,2]
x<-data[,3:ncol(data)]
thin_indices<-seq(1,nrow(data),by=thinning)
x<-x[thin_indices,]
y<-y[thin_indices]
x<-as.matrix(x)
xc<-scale(x)
yc<-y-mean(y)

noise=rnorm(n=nrow(x),sd=sig)
yc=yc+noise
y=y+noise

I1 = IAL(y=y, xc, criterion="BIC")
I1$b
I1$w
nE<-1
print("Scaled variables")
I2 = backward(y=y, X=xc, I1, pcut=0.1/nE)
# I2 = backward(y=y, X=xc, BAL, pcut=0.05/nE)




I2$b

b.scale<-I2$b
w<-I2$w
b.scale<-b.scale/attr(xc,"scaled:scale")[w]

w
b.scale

