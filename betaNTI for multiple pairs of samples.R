##�л�������OTU����ϵͳ���������ļ�Ŀ¼
#����������ݰ�
library(picante)
#��ȡOTU������Ȼ����Ʒ�����У�OTU��������
otu = read.table("otu_table.txt", header=T, row.names=1) 
#��ȡϵͳ������
phylo = read.tree("rep_set.tre") 
#ȷ��ϵͳ�������ϵ�������otu���е���������һ��
match.phylo.otu = match.phylo.data(phylo, t(otu)) 
#����betaMNTD������abundance.weighted=T��ʾ�������ַ�ȣ�������趨�ò�������Ĭ��abundance.weighted=F�����������ַ��
beta.mntd.weighted = as.matrix(comdistnt(t(match.phylo.otu$data),cophenetic(match.phylo.otu$phy),abundance.weighted=T)) 
#���beta.mntd.weighted�����е������Ƿ��ϵͳ�������Լ�OTU����һ�£����Ӧ��ΪT
identical(colnames(match.phylo.otu$data),colnames(beta.mntd.weighted))
#ͬ�ϣ�ֻ��һ����飬���Ӧ��ΪT
identical(colnames(match.phylo.otu$data),rownames(beta.mntd.weighted)) 
##����betaMNTD���ֵ
#���������
beta.reps = 999
rand.weighted.bMNTD.comp=array(c(-999),dim=c(ncol(match.phylo.otu$data),ncol(match.phylo.otu$data),beta.reps));
for (rep in 1: beta.reps) {
#��һ��ִ�е�������ǻ��ڶ�ϵͳ��������OTU��ǩ��������Ž��еģ��������1�еķ�����ÿһ��Ⱥ���������������ټ���betaMNTD,�����ѡ��ʹ��rand.weighted.bMNTD.comp[,,rep] = as.matrix(comdistnt(randomizeMatrix(comun, null.model = c("frequency", "richness","independentswap", "trialswap"), iterations = 1000),cophenetic(match.phylo.comun$phy),abundance.weighted=T,exclude.conspecifics = F))������ɸò�����
  rand.weighted.bMNTD.comp[,,rep] = as.matrix(comdistnt(t(match.phylo.otu$data),taxaShuffle(cophenetic(match.phylo.otu$phy)),abundance.weighted=T,exclude.conspecifics = F)); 
  print(c(date(),rep));
}

##�����NTI
weighted.bNTI = matrix(c(NA),nrow=ncol(match.phylo.otu$data),ncol=ncol(match.phylo.otu$data));
for (columns in 1:(ncol(match.phylo.otu$data)-1)) {
  for (rows in (columns+1): ncol(match.phylo.otu$data)) {
#���զ�NTI���㹫ʽ����ÿһ��������Ħ�NTIֵ
    rand.vals = rand.weighted.bMNTD.comp[rows,columns,];
    weighted.bNTI[rows,columns] = (beta.mntd.weighted[rows,columns] - mean(rand.vals)) / sd(rand.vals);
    rm("rand.vals");
  };
};
rownames(weighted.bNTI) = colnames(match.phylo.otu$data);
colnames(weighted.bNTI) = colnames(match.phylo.otu$data);
weighted.bNTI;
#�õ�ȫ�������Լ�Ħ�NTIֵ��д���ļ�
write.csv(weighted.bNTI,"weighted_bNTI.csv",quote=F);