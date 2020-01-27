

#question 1

prodrat = read.csv("C:/Users/fb8502oa/Desktop/DSCI 415/DSCI 415 fall/ProductRatings.csv", header=TRUE)
names(prodrat)

row.names(prodrat) = prodrat$QN
prod.mat = prodrat[,-1]
prod.mat = apply(prod.mat,2,as.factor)
library(FactoMineR)
library(factoextra)
library(ggpubr)

#doing the mca
prod.MCA = MCA(prod.mat, graph = FALSE)
summary(prod.MCA)

#ploting the individual 
fviz_mca_ind(prod.MCA, col.ind = "steelblue") 
#outliyer = indv 129

View(prod.mat)# to see which column 129 is at

#part b
#removing indv 129
newprodrat = prod.mat[-c(4),]


#mca of the new data 
newprod.MCA = MCA(newprodrat)
summary(newprod.MCA)


#ploting for the individual
fviz_mca_ind(newprod.MCA, col.ind = "cos2")


#ploting for the variables
fviz_mca_var(newprod.MCA, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

#part c
fviz_mca_biplot(newprod.MCA, col.var ="#FC4E07") #this graph is terrible so imma try ploting usinf usdv
plot(newprod.MCA)

biplotprodrat = prodrat[-c(4),]
biplotprodrat.prop = as.matrix(biplotprodrat / sum(biplotprodrat))
biplotprodrat.row = apply(biplotprodrat.prop, 1, sum)
biplotprodrat.col = apply(biplotprodrat.prop, 2, sum)
rdiag = diag(1/ sqrt(biplotprodrat.row))
cdiag = diag (1/ sqrt (biplotprodrat.col))
biplotprodrat.row = as.matrix(biplotprodrat.row)
biplotprodrat.col = as.matrix(biplotprodrat.col)
E= rdiag %*% (biplotprodrat.prop - biplotprodrat.row %*%t (biplotprodrat.col)) %*% cdiag
biplotprodrat.sva = svd(E)
delta = biplotprodrat.sva$d
U= biplotprodrat.sva$u[,1:2]
V= biplotprodrat.sva$v[,1:2]

U[,1] = delta[1]* U[,1]/ sqrt (biplotprodrat.row)
U[,2] = delta[2]* U[,2]/ sqrt (biplotprodrat.row)
V[,1] = delta[1]* V[,1]/ sqrt (biplotprodrat.col)
V[,2] = delta[2]* V[,1]/ sqrt (biplotprodrat.col)

U = rbind(U,V)
inertia = sum(delta[delta^2])
per1 = delta[1]^2 / inertia
per2 = delta[2]^2 / inertia

dim1 = dim(biplotprodrat)[1]
ds =as.integer(dim1 +1)
dim2 = dim(biplotprodrat)[2]
dt = dim1 + dim2

plot(U[,1], U[,2], type= "n", xlab = paste("coord 1 - %inertia =  ", format(per1)),
     ylab = paste("coord 2 - % inertia = ", format(per2)))
text(U[1: dim1,1], U[1: dim1, 2], labels = dimnames(biplotprodrat)[[1]], col = 2)
text(U[ds : dt,1], U[ds: dt, 2], labels = dimnames(biplotprodrat)[[2]], col =4)
abline(h= 0, v=0, lty =2)
title ("biplot for indi and var")




#question 2

bobross = read.csv("C:/Users/fb8502oa/Desktop/DSCI 415/DSCI 415 fall/BobRossCommon.csv")
names(bobross)

row.names(bobross) = bobross$TITLE
bobross.mat = bobross[,-1]
bobross.mat = apply(bobross.mat,2,as.factor)
library(FactoMineR)
library(factoextra)
bobross.MCA = MCA(bobross.mat, graph = FALSE)
summary(bobross.MCA)
#ploting for indi
fviz_mca_ind(bobross.MCA, col.ind = "cos2")
# 4 outlier paints

#ploting for the vari
fviz_mca_var(bobross.MCA, col.var = "contrib", gradient.cols = c("#00BFCB", "#F2B900", "#DE6E07"))

#ploting for both
plot(bobross.MCA)
fviz_mca_biplot(bobross.MCA, col.var = "#BB5E07")


