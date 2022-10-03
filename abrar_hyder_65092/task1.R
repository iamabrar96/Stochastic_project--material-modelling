##########################################
# Abrar Hyder Mohammed
# 65092
# Stochastic Methods for Materials Science
# Programming project
##########################################
# Task 1
########

source("libs.r")
A1<-readImage("image1_S03.png")
display(A1,method="raster")
A1@.Data      # converts the given image into a matrix of binary 
dim(A1)
A2<-readImage("image2_S03.png")
display(A2,method="raster")
dim(A2)
A2@.Data
source("fcts.r")
estALX(A1@.Data, spacing=0.025)
estALX(A2@.Data, spacing=0.025)

# For both the set1 and set2 images the values of m are choosen in such a way that any further 
#increment in m values does not lead to any change in the Minkowski functions A_A(r), L_A(r) and chi_A(r)
A1.ALX <- estALXFct(BW=A1, m=40, spacing=0.025, ms=TRUE)
A1.ALX
A2.ALX <- estALXFct(BW=A2, m=40, spacing=0.025, ms=TRUE)
A2.ALX

# plotting  Minkowski function estimates for set1 image

# Show the results alltogether ('show.type=0'):
plotALXFct(BW=A1, ALX=A1.ALX, show.type=0)

# Show only estimate of A_A(r) ('show.type=1'):
plotALXFct(BW=A1, ALX=A1.ALX, show.type=1)

# Show only estimate of L_A(r) ('show.type=2'):
plotALXFct(BW=A1, ALX=A1.ALX, show.type=2)

# Show only estimate of chi_A(r) ('show.type=3'):
plotALXFct(BW=A1, ALX=A1.ALX, show.type=3)

#plotting  Minkowski function estimates for set2 image

# Show the results alltogether ('show.type=0'):
plotALXFct(BW=A2, ALX=A2.ALX, show.type=0)

# Show only estimate of A_A(r) ('show.type=1'):
plotALXFct(BW=A2, ALX=A2.ALX, show.type=1)

# Show only estimate of L_A(r) ('show.type=2'):

plotALXFct(BW=A2, ALX=A2.ALX, show.type=2)

# Show only estimate of chi_A(r) ('show.type=3'):
plotALXFct(BW=A2, ALX=A2.ALX, show.type=3)


# plotting both the sets together for comparison  
plotALXFct(BW=A1, ALX=A1.ALX, ALX2=A2.ALX, show.type=1)
plotALXFct(BW=A1, ALX=A1.ALX, ALX2=A2.ALX, show.type=2)
plotALXFct(BW=A1, ALX=A1.ALX, ALX2=A2.ALX, show.type=3)




