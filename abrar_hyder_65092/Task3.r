##########################################
# Abrar Hyder Mohammed
# 65092
# Stochastic Methods for Materials Science
# Programming project
##########################################
# Task 3
########
###########3(a)##################
source('fcts.r')
A<-readImage('image3_S03.png')
display(A,method="raster")
y.ALX <- estALX(A,spacing = 0.001)
V_V = y.ALX[1]     # volume fraction
S_V =  (4/pi)*y.ALX[2]   # specific surface area

BW<- readImage('image3_S03.png')
display(BW,method="raster")

BW.labelled <- bwlabel(BW)
BW.feat = computeFeatures.moment(BW.labelled)
dim(BW.feat)
attributes(BW.feat)$dimnames[[2]] 

# Disc centers and radius can be calculated using "computefeatures.moment"
########################3(b)#######################################################
# disc diameter
d.2D = BW.feat[,3]
d.2D # unit is 1 pixel = 1 micrometer
d.2D = d.2D/1000 # We switch to unit 1 millimeter.
boxplot(d.2D)

# disc centers 
center.2D = cbind(BW.feat[,1],BW.feat[,2])

# range of diameters
range(d.2D)

# We take only those disc diameter where the disc centre is more than 30 pixels away from the boundary of the window.

d.2D.1_ <- BW.feat[BW.feat[,1]>30 & BW.feat[,1]<1371 & BW.feat[,2]>30 & BW.feat[,2]<1371, 3]
d.2D.1_
d.2D.1_<-d.2D.1_/1000

# number of discs with centre in the reduced window:

n.2D <- length(d.2D.1_)
n.2D
# absoulte frequency disc of diameter 
Delta <- 0.01
fi.2D = hist(d.2D.1_, breaks=seq(0, 0.17, by=Delta))$counts


# empirical mean and standard deviation
mean.emp = mean(d.2D.1_)
sd.emp = sd(d.2D.1_)

###############3(C)##################

# area of reduced window in square millimetre:
A.W <- prod(dim(BW)-2*30)/1000^2 
# absolute frequencies of disc diameters in 2D per per square millimetre:
ni.2D <- fi.2D/A.W
ni.2D

# Apply the Scheil-Schwartz-Saltykov method:
Ni.3D <- saltykov(Delta=Delta, ni.2D, nEM=32)
# -> 'f.3D' contains absolute frequencies of ball diameters in 3D per cubic millimetre:
Ni.3D
# comparison of relative frequencies:
barplot(rbind(ni.2D/sum(ni.2D),Ni.3D/sum(Ni.3D)), beside=TRUE, legend.text = c("2D", "3D"), args.legend = list(x = "topleft"),names.arg=c("(0,0.01]","(0.01,0.02]","(0.02,0.03]","(0.03,0.04]","(0.04,0.05]","(0.05,0.06]","(0.06,0.07]","(0.07,0.08]","(0.08,0.09]","(0.09,0.10]","(0.10,0.11]","(0.11,0.12]","(0.12,0.13]","(0.13,0.14]","(0.14,0.15]","(0.15,0.16]","(0.16,0.17]"))

##############3(d)############################


# estimates of intensity and mean ball diameter

lambdaV.2.7 = sum(Ni.3D) # estimated intensity in 3D, with formula (7)
lambdaV.2.7

muV.2.7 = n.2D/(lambdaV.2.7 * A.W) # estimated mean ball diameter in 3D, with formula (7)
muV.2.7







     
