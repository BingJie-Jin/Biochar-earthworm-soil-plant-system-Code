#计算α-多样性
#Sipoliu
#20190320
options(width=65,digits=4)
otu_table=read.csv(file.choose(),row.names=1,check.names=FALSE)
library(vegan)

head(otu_table)
otu_table <- t(otu_table)

#Chao1指数
num_otu <- specnumber(otu_table)
num_otu
write.csv(num_otu,file = "SB_rhizosphere_number_otu.csv")
index <- estimateR(otu_table)
index
chao1 <- estimateR(otu_table)[2,]
chao1
write.csv(chao1,file = "SB_rhizosphere_chao1.csv")

#Shannon 指数
shannon_index <- diversity(otu_table,index = "shannon",MARGIN = 1)
shannon_index
write.csv(shannon_index,file = "SB_rhizosphere_Shannon1.csv")
#Shannon 指数，方法2，结果一样
#otu_table_total<- decostand(otu_table, MARGIN = 1,method="total")
#otu_table_p_lnp <- otu_table_total*log(otu_table_total)
#Shannon_Winner <- rowSums(otu_table_p_lnp,na.rm = TRUE)*-1
#write.csv(Shannon_Winner,file = "Shannon2.csv")

#Simpson 指数
Simpson_index <- diversity(otu_table,"simpson")
Simpson_index
write.csv(Simpson_index,file = "SB_rhizosphere_Simpson.csv")

#Pielou’s Evenness Index
S <- specnumber(otu_table)
H <- diversity(otu_table, "shannon")
J <- H/log(S)
J
write.csv(J,file = "SB_rhizosphere_Evenness.csv")

rm(list=ls())
