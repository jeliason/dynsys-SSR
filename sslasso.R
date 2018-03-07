args<-commandArgs(TRUE)
var<-as.double(args[2])
thinning<-as.double(args[3])
sig<-sqrt(var)

library(SSLASSO)


data<-read.csv(paste("~/Desktop/research/",args[1],sep=""))
lambda1 <- 1 # slab penalty for Spike-and-Slab LASSO
lambda0 <- seq(lambda1, 50, length=20) # slab penalties for Spike-and-Slab LASSO
L <- length(lambda0)
y<-data[,2]
x<-data[,3:ncol(data)]
thin_indices<-seq(1,nrow(data),by=thinning)
# Changing where we're sampling from
# thin_indices<-seq(1,nrow(data)/thinning)
X<-x[thin_indices,]
y<-y[thin_indices]
p<-ncol(X)
n<-nrow(X)
noise=rnorm(n=nrow(X),sd=sig)
y=y+noise

# Oracle SSLASSO with known variance
result1 <- SSLASSO(X, y, penalty = "separable", variance = "known",
                   lambda1 = lambda1, lambda0 = lambda0,
                   theta = 3/p)
plot(result1)
# Adaptive SSLASSO with known variance
result2 <- SSLASSO(X, y, penalty = "adaptive",
                   lambda1 = lambda1, lambda0 = lambda0,
                   theta = 0.5, a = 1, b = p, counter = 10)
plot(result2)
# Adaptive SSLASSO with unknown variance
result3 <- SSLASSO(X, y, penalty = "adaptive", variance = "unknown",
                   lambda1 = lambda1, lambda0 = lambda0)
plot(result3)
# result3$sigmas[L]
result2$beta
# result2$model
# result3$model