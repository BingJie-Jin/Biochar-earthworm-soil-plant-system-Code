
library(ggplot2)
library(vegan)
library(MASS)
nmd<-read.csv(file.choose(),header=T,row.names=1)

nmds=metaMDS(nmd,distance="bray")
nmds
nmds=metaMDS(nmds,distance = "bray", k = 2,trymax=20)  
plot(nmds, mass = TRUE, what=c("none", "all"), contrib = "absolute")
plot(nmds, type = "n")
points(nmds, display = "sites", cex = 0.8, pch=21, col="red", bg="yellow")
text(nmd, display = "spec", cex=0.7, col="blue")
