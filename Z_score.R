data=read.csv(file.choose(),header=1)
zscore=scale(data,center=F,scale=T)
zscore
write.csv(zscore,file="Standard_Earthworm_Soil_properties.csv")

a<-round(scale(data),2)
a
d<-density(a)
plot(d)

getwd()