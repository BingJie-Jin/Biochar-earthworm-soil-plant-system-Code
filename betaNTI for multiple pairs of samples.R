##切换到包含OTU表和系统发育树的文件目录
#加载这个数据包
library(picante)
#读取OTU表，依然是样品名在行，OTU种类在列
otu = read.table("otu_table.txt", header=T, row.names=1) 
#读取系统发育树
phylo = read.tree("rep_set.tre") 
#确保系统发育树上的名称与otu表中的名称排序一致
match.phylo.otu = match.phylo.data(phylo, t(otu)) 
#计算betaMNTD，这里abundance.weighted=T表示考虑物种丰度，如果不设定该参数，则默认abundance.weighted=F，不考虑物种丰度
beta.mntd.weighted = as.matrix(comdistnt(t(match.phylo.otu$data),cophenetic(match.phylo.otu$phy),abundance.weighted=T)) 
#检查beta.mntd.weighted矩阵中的列名是否和系统发育树以及OTU表中一致，结果应该为T
identical(colnames(match.phylo.otu$data),colnames(beta.mntd.weighted))
#同上，只是一个检查，结果应该为T
identical(colnames(match.phylo.otu$data),rownames(beta.mntd.weighted)) 
##计算betaMNTD随机值
#随机化次数
beta.reps = 999
rand.weighted.bMNTD.comp=array(c(-999),dim=c(ncol(match.phylo.otu$data),ncol(match.phylo.otu$data),beta.reps));
for (rep in 1: beta.reps) {
#这一步执行的随机化是基于对系统发育树中OTU标签的随机重排进行的，如果沿用1中的方法对每一个群落进行随机构建后再计算betaMNTD,则可以选择使用rand.weighted.bMNTD.comp[,,rep] = as.matrix(comdistnt(randomizeMatrix(comun, null.model = c("frequency", "richness","independentswap", "trialswap"), iterations = 1000),cophenetic(match.phylo.comun$phy),abundance.weighted=T,exclude.conspecifics = F))命令完成该步运算
  rand.weighted.bMNTD.comp[,,rep] = as.matrix(comdistnt(t(match.phylo.otu$data),taxaShuffle(cophenetic(match.phylo.otu$phy)),abundance.weighted=T,exclude.conspecifics = F)); 
  print(c(date(),rep));
}

##计算βNTI
weighted.bNTI = matrix(c(NA),nrow=ncol(match.phylo.otu$data),ncol=ncol(match.phylo.otu$data));
for (columns in 1:(ncol(match.phylo.otu$data)-1)) {
  for (rows in (columns+1): ncol(match.phylo.otu$data)) {
#按照βNTI计算公式计算每一对样本间的βNTI值
    rand.vals = rand.weighted.bMNTD.comp[rows,columns,];
    weighted.bNTI[rows,columns] = (beta.mntd.weighted[rows,columns] - mean(rand.vals)) / sd(rand.vals);
    rm("rand.vals");
  };
};
rownames(weighted.bNTI) = colnames(match.phylo.otu$data);
colnames(weighted.bNTI) = colnames(match.phylo.otu$data);
weighted.bNTI;
#得到全部样本对间的βNTI值并写入文件
write.csv(weighted.bNTI,"weighted_bNTI.csv",quote=F);