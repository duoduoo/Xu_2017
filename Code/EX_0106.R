setwd("~/Study/Gokcumen_Lab/EX_0106_Pavlos_muc7")
library(ggplot2)


#######################################################################################
# PCA
pca <- read.csv("principalComps_simulationsANDmuc7.csv", sep="\t")
p <- ggplot(pca, aes(x=firstPC, y=secondPC))+geom_point(aes(col=factor(class)),size=1.5)
p1 <- p+scale_color_manual(values=c("#F4BEBE90","#6495ED90"))+xlim(c(-6,20))+ylim(c(-9,6))
p1+annotate("point",x=-0.1428765,y=0.3692534,col='red',size=2)+annotate('text',label='MUC7',x=1,y=-0.5,col='red',size=4,fontface=2)

npca <- pca[1:1516,]
pca[1517,]
p <- ggplot(npca, aes(x=npca$firstPC))+geom_histogram(aes(fill=factor(class)), position="identity")
p1 <- p+scale_fill_manual(values=c('#6495ED','#F4BEBE99'))
pc1 <- p1+geom_vline(xintercept = -0.1428765, col='#FA8072',size=1)

p <- ggplot(npca, aes(x=npca$secondPC))+geom_histogram(aes(fill=factor(class)), position="identity")
p1 <- p+scale_fill_manual(values=c('#6495ED','#F4BEBE99'))
pc2 <- p1+geom_vline(xintercept = 0.3692534, col='#FA8072',size=1)
pc2

multiplot(pc1,pc2,cols=2)

nointro_pc1 <- subset(npca, npca$class == '0')
hist(nointro_pc1[,1], breaks=100)
x <- as.numeric(nointro_pc1[,1])

qnorm(0.9,x, sd=sd(x),mean=mean(x))
par(new=T)
plot(seq(min(x), max(x), length.out=500), dnorm(seq(min(x), max(x), length.out=500), mean(x), sd(x)), type="l", col="red", xlab="", ylab="", axes=FALSE)
axis(4) #show the other axis
lines(c(-0.1085703,-0.1085703), c(0,80), lty=4, col="red")


pnorm(0.9,x, sd=sd(x),mean=mean(x))
?qnorm


#######################################################################################

stat <- read.csv("sumstat_withmuc7raw.csv",sep="\t")
nstat <- subset(stat, stat$Class != "-1")
muc7 <- stat[10001,]

names(nstat)
ggplot(nstat, aes(y=nstat$ThetaPi,x=factor(nstat$Class)))+geom_boxplot(col="#808080")+geom_hline(yintercept=14.4065,col="#FA8072",size=1)

pn <- c()
for(i in 1:21){
    label <- names(nstat)[i]
    pn[i] <- paste0('p',i,collapse = NULL)
    print(i)
    ggplot(nstat, aes(y=nstat[,i],x=factor(nstat$Class)))+geom_boxplot(col="#808080")+geom_hline(yintercept=muc7[1,i],col="#FA8072",size=1)
}
