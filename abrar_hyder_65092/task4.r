##########################################
# Abrar Hyder Mohammed
# 65092
# Stochastic Methods for Materials Science
# Programming project
##########################################
# Task 4

source('fcts.r')
source('libs.r')

# given parameters
lambda_1 <- 4
lambda_2 <- 8
lambda_3 <- 12
R <- 0.06
spacing<-0.01

# first step generate 100 realization for matern 3 hard disc model
rM3.est = function(lambda, R=0.06, spacing = 0.01, W = owin(c(0,10),c(0,10))) {
  AA.est = 0
  LA.est = 0
  XA.est = 0
  for (i in 1:100){
    XYR.M3 = rM3.disc.const(lambda=lambda, R=R, W=owin(c(0,10), c(0,10)))
    # second step Digitize it with pixel length 0.01
    BW.XYR.M3 = digitizeDiscSys(XYR.M3, spacing=spacing) 
    XYR.M3.est = estALX(BW.XYR.M3, spacing=spacing) # third step estimates the AA,LA,XA through estALX function
    AA.est[i] = XYR.M3.est[1]
    LA.est[i] = XYR.M3.est[2]
    XA.est[i] = XYR.M3.est[3]
  }
  return(cbind(AA.est,LA.est,XA.est))
}

rM3.est.lambda1 = rM3.est(lambda=lambda_1)

# step four determine mean and S.D. with different lambda values 

lambda_1 <- 4

AA1.mean = mean(rM3.est.lambda1[,1])
AA1.sd = sd(rM3.est.lambda1[,1])

LA1.mean = mean(rM3.est.lambda1[,2])
LA1.sd = sd(rM3.est.lambda1[,2])

XA1.mean = mean(rM3.est.lambda1[,3])
XA1.sd = sd(rM3.est.lambda1[,3])


lambda_2 <- 8

rM3.est.lambda2 = rM3.est(lambda=lambda_2)

AA2.mean = mean(rM3.est.lambda2[,1])
AA2.sd = sd(rM3.est.lambda2[,1])

LA2.mean = mean(rM3.est.lambda2[,2])
LA2.sd = sd(rM3.est.lambda2[,2])

XA2.mean = mean(rM3.est.lambda2[,3])
XA2.sd = sd(rM3.est.lambda2[,3])


lambda_3 <- 12


rM3.est.lambda3 = rM3.est(lambda=lambda_3)


AA3.mean = mean(rM3.est.lambda3[,1])
AA3.sd = sd(rM3.est.lambda3[,1])

LA3.mean = mean(rM3.est.lambda3[,2])
LA3.sd = sd(rM3.est.lambda3[,2])

XA3.mean = mean(rM3.est.lambda3[,3])
XA3.sd = sd(rM3.est.lambda3[,3])


# step five box plots for each 100 estimates with different lambda values

boxplot(rM3.est.lambda1[,1],main=expression(paste(A[A],sep="")))
boxplot(rM3.est.lambda1[,2],main=expression(paste(L[A],sep="")))
boxplot(rM3.est.lambda1[,3],main=expression(paste(chi[A],sep="")))

boxplot(rM3.est.lambda2[,1],main=expression(paste(A[A],sep="")))
boxplot(rM3.est.lambda2[,2],main=expression(paste(L[A],sep="")))
boxplot(rM3.est.lambda2[,3],main=expression(paste(chi[A],sep="")))

boxplot(rM3.est.lambda3[,1],main=expression(paste(A[A],sep="")))
boxplot(rM3.est.lambda3[,2],main=expression(paste(L[A],sep="")))
boxplot(rM3.est.lambda3[,3],main=expression(paste(chi[A],sep="")))


