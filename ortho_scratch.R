df<-read.csv("simple1d.csv")
x<-df[,3]

n <- length(x)   # Number of observations
d <- 4         # Degree
#
# Synthesize a regressor, its powers, and orthogonal polynomials thereof.
#
# x <- rnorm(n)
x.p <- outer(x, 0:d, `^`); colnames(x.p) <- c("Intercept", paste0("x.", 1:d))
z <- poly(x, d)
#
# Compute the orthogonal polynomials in terms of the powers via OLS.
#
xform <- lm(cbind(1, z) ~ x.p-1)
gamma <- coef(xform)
#
# Verify the transformation: all components should be tiny, certainly
# infinitesimal compared to 1.
#
if (!all.equal(as.vector(1 + crossprod(x.p %*% gamma - cbind(1,z)) - 1), 
               rep(0, (d+1)^2)))
  warning("Transformation is inaccurate.")
#
# Fit the model with orthogonal polynomials.
#
y <- x.p[,2]+0.2*x.p[,4]-0.1*x.p[,5]+rnorm(n)
fit <- lm(y ~ z)
summary(fit)
#
# As a check, fit the model with raw powers.
#
fit.p <- lm(y ~ .-1, data.frame(x.p))
summary(fit.p)
#
# Compare the results.
#
(rbind(Computed=as.vector(gamma %*% coef(fit)), Fit=coef(fit.p)))

if (!all.equal(as.vector(gamma %*% coef(fit)), as.vector(coef(fit.p))))
  warning("Results were not the same.")