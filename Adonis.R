library(vegan)  #adonis
all=read.csv(file.choose(),row.names=1)
group=read.csv(file.choose(),row.names=1)
all=t(all)
all.dist <- vegdist(all)
adonis(all.dist ~ ., group, perm=999)


#main factors by multivariate permutational analysis of variance (PERMANOVA)
library(vegan)
all=read.csv(file.choose(),row.names=1)
group=read.csv(file.choose(),row.names=1)
all=t(all)
all.dist <- vegdist(all)
adonis(all.dist~year*regime,data=group,permutations=999,method="bray",p.adjust.methods="bonferroni")




pairwise.adonis(all.dist~year*regime*group,data=group,permutations=999,method="bray",p.adjust.methods="bonferroni")


pairwise.t.test

(dune ~ Management*A1, data=dune.env, permutations=99)

library(vegan)  #adonis
all=read.csv(file.choose(),row.names=1)
group=read.csv(file.choose(),row.names=1)
all=t(all)
all.dist <- vegdist(all)
adonis(all.dist ~ ., group, perm=999)





library(vegan)  #anosim
all=read.csv(file.choose(),row.names=1)
group=read.csv(file.choose(),row.names=1)
all=t(all)
all.dist <- vegdist(all)
all.ano <- with(group, anosim(all.dist, group))
summary(all.ano)
plot(all.ano)



library(vegan)  #mrpp
all=read.csv(file.choose(),row.names=1)
group=read.csv(file.choose(),row.names=1)
all=t(all)
all.dist <- vegdist(all)
all.mrpp <- with(group, mrpp(all.dist, group))
all.mrpp


#Mantel test
library(vegan)
otu=read.csv(file.choose(),row.names=1)
envi=read.csv(file.choose())
veg.dist <- vegdist(t(otu))# Bray-Curtis
env.dist <- vegdist(scale(envi), "euclid")
mantel(veg.dist, env.dist)
mantel(veg.dist, env.dist, method="spearman")


library(mvpart)
data(spider)
mvpart(data.matrix(spider[,1:12])~herbs+reft+moss+sand+twigs+
         water,spider)       # defaults
mvpart(data.matrix(spider[,1:12])~herbs+reft+moss+sand+twigs+
         water,spider,xv="p")  
fit <- mvpart(data.matrix(spider[,1:12])~herbs+reft+moss+sand+
                twigs+water,spider,xv="1se",pca=TRUE)
rpart.pca(fit,interact=TRUE,wgt.ave=TRUE) # interactive PCA plot of saved multivariate tree


library(mvpart)
all=read.csv(file.choose(),row.names=1)
all=t(all)
group=read.csv(file.choose(),row.names=1)

library(stats)
attach(airquality)
Month <- factor(Month, labels = month.abb[5:9])
pairwise.t.test(Ozone, Month)
pairwise.t.test(Ozone, Month, p.adj = "bonf")
pairwise.t.test(Ozone, Month, pool.sd = FALSE)
detach()

