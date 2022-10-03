##########################################
# Abrar Hyder Mohammed
# 65092
# Stochastic Methods for Materials Science
# Programming project
##########################################
# Task 2
########


source("libs.r")
source("fcts.r")
#######################2(a)###################################
require(EBImage)
D2 <- readImage('C:/Users/ashab/Documents/abrar_hyder_65092/image1_S03.png')
display(D2, method="raster")
C <- D2
B1 <- makeBrush(size=1, shape='disc') 
B2 <- makeBrush(size=3, shape='disc') 
B3 <- makeBrush(size=5, shape='disc') 
B4 <- makeBrush(size=7, shape='disc') 
B5 <- makeBrush(size=9, shape='disc') 
B6 <- makeBrush(size=11, shape='disc') 

# Determining the true disc diameter using morphological opening
display(opening(C, B1), method="raster", interpolate=FALSE)
display(opening(C, B2), method="raster", interpolate=FALSE)
display(opening(C, B3), method="raster", interpolate=FALSE)
display(opening(C, B4), method="raster", interpolate=FALSE)
display(opening(C, B5), method="raster", interpolate=FALSE)
display(opening(C, B6), method="raster", interpolate=FALSE)

###################2(b)######################################

# Generating the realization of boolean model with two sets model parameters for a rectangular grain
XYABP <- rBM.rect.const(lambda=4.9, a=2.5,b=0.15, W=owin(c(0,10), c(0,10))) 
BW.XYABP <- digitizeRectSys(XYABP, spacing=0.025) 

#
XYABP1 <- rBM.rect.const(lambda=4.8, a=1.8,b=0.13, W=owin(c(0,10), c(0,10))) 
BW.XYABP1 <- digitizeRectSys(XYABP1, spacing=0.025) 

# The below function generates 'nrep' repetitions of a 2D Boolean model 
# with some intensity 'lambda' and a rectangular grain which estimates the first minkowski functions

getAA.rect.const <- function(nrep = 39, lambda, a,b, W = owin(c(0,1),c(0,1)), m=0, spacing=1) {
  sapply(1:nrep, function(k) {
    XYABP <- rBM.rect.const(lambda=lambda, a=a,b=b, W=W)
    BW.XYABP <- digitizeRectSys(XYABP, spacing=spacing) 
    estALXFct(BW=BW.XYABP, m=m, spacing=spacing, ms=TRUE )[,2]
  })
}

# range for the first Minkowski function A_A(r)

m<-40 

AA.model.999.PA <- getAA.rect.const(nrep=999, lambda=4.9,a=2.5,b=0.15, W = owin(c(0,10),c(0,10)), m=m, spacing=0.025)
AA.genv.999.PA <- globalEnvelopes(AA.model.999.PA, alpha=0.05)
AA.data1 <- estALXFct(BW=BW.XYABP, m=m, spacing=0.025, ms = TRUE)[,1:2]
dim(AA.data1)
globalTest(AA.data1[,2], AA.model.999.PA)
plot(AA.data1[,1], AA.data1[,2], type="l", xlab="r", ylab=expression(paste(A[A],"(r)",sep="")), lwd=2, ylim=c(0.4,1))
lines(AA.data1[,1], AA.genv.999.PA[,1], lwd=2, col=2)
lines(AA.data1[,1], AA.genv.999.PA[,2], lwd=2, col=2)

AA.model.999.PB <- getAA.rect.const(nrep=999, lambda=4.8,a=1.8,b=0.13, W = owin(c(0,10),c(0,10)), m=m, spacing=0.025)
AA.genv.999.PB<- globalEnvelopes(AA.model.999.PB, alpha=0.05)
AA.data2 <- estALXFct(BW=BW.XYABP1, m=m, spacing=0.025, ms = TRUE)[,1:2]
globalTest(AA.data1[,2], AA.model.999.PB)
plot(AA.data2[,1], AA.data1[,2], type="l", xlab="r", ylab=expression(paste(A[A],"(r)",sep="")), lwd=2, ylim=c(0.4,1))
lines(AA.data2[,1], AA.genv.999.PB[,1], lwd=2, col=2)
lines(AA.data2[,1], AA.genv.999.PB[,2], lwd=2, col=2)











