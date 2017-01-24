# Here is the one for random 1000 regions

# MUC7 : 102 33

calls <- read.table("snps_ldSNPs.txt", header=F, sep = '\t')
x <- as.numeric(calls[,2]/calls[,1])
hist(x, breaks=50)
qnorm(0.99, mean=mean(x,na.rm=TRUE),sd=sd(x,na.rm=TRUE),lower.tail = T)
# [1] 0.2798909

wilcox.test(0.3235294, x, alternative = "greater")
# p-value = 0.05008

p <- ggplot(calls, aes(x=as.numeric(calls[,2]/calls[,1])))+geom_density(alpha=0.9, col="#B4AAAA", fill="#B4AAAA")
p1<-p+geom_vline(xintercept =33/102, col="#BF666A",size=0.6)+annotate("text",x=0.43,y=5,label="P-value = 0.050",col="#BF666A",size=6)
p1