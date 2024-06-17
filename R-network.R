#��ȱ�׼��
library(vegan)
otu=read.csv(file.choose(),row.names=1)
otu=t(otu)
otu.h=decostand(otu,"normalize")
write.csv(otu.h,file="normalized.csv")

#network analysis ����R��P 
library(psych)
spe=read.csv(file.choose(),row.names=1)   #normalized.csv
cor<-corr.test(spe,method="spearman",adjust="BH")
write.csv(cor[1],quote=FALSE,file="spearman.csv")
write.csv(cor[4],quote=FALSE,file="adjusted p.csv")

cor_matrix=read.csv(file.choose(),row.names=1)     #��ȡ���ɵ�spearman�ļ�
a<-data.frame(Row=rownames(cor_matrix)[row(cor_matrix)[upper.tri(cor_matrix)]],Col=colnames(cor_matrix)[col(cor_matrix)[upper.tri(cor_matrix)]],Corr=cor_matrix[upper.tri(cor_matrix)])
write.table(a,quote=FALSE,sep="\t",file="cor_matrix.txt")

p_matrix=read.csv(file.choose(),row.names=1)      #��ȡ���ɵ�adjusted p�ļ�
a<-data.frame(Row=rownames(p_matrix)[row(p_matrix)[upper.tri(p_matrix)]], Col=colnames(p_matrix)[col(p_matrix)[upper.tri(p_matrix)]],Corr=p_matrix[upper.tri(p_matrix)])
write.table(a,quote=FALSE,sep="\t",file="p_matrix.txt")

#������ɵ�pֵѡ��С��0.01����С��0.05��rֵѡȡС��0.8��0.7��0.6���ɣ�rֵ���ڸ�ֵ