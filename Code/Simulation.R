## Data file can be download at https://drive.google.com/drive/folders/0B7vGA-5m4MLGMEZEeXJ6M1NNVjg?usp=sharing
## load data "IntrogressionAnalysisMUC7_February2017.RData"

names(sumstat.withmuc7.class)
names(sumstat.withmuc7.class)[21] <- 'Class'

## -1 is muc7

calls <- subset(sumstat.withmuc7.class, Class == 0 | Class == 1)

names(realSumFiles.dataframe) == names(sumstat.withmuc7.class)[1:20]

realSumFiles.dataframe[,21] <- 'realData'
names(realSumFiles.dataframe)[21] <- 'Class'

calls <- rbind(calls, realSumFiles.dataframe)
summary(as.factor(calls[,21]))

boxplot(ThetaPi ~ Class, calls)

library(ggplot2)

calls$Class <- factor(calls$Class, levels = c("realData", "0", "1"))

## remove outliers
sts <- boxplot.stats(calls$ThetaPi)$stats

thetapi <- ggplot(calls, aes(x=factor(Class),y=ThetaPi))+geom_boxplot(outlier.shape = NA, width=0.6, col="#808080", aes(fill=Class))+scale_fill_manual(values = c("#F6ABAB","#ABD2F2","#6D92A9"))+annotate("segment",x=0.3, xend=3.7,y=stats.muc7$ThetaPi, yend=stats.muc7$ThetaPi, col="#E86F75")+ theme(legend.position="none")+coord_cartesian(ylim = c(-3, 55))
thetapi
thetaw <- ggplot(calls, aes(x=factor(Class),y=ThetaW))+geom_boxplot(outlier.shape = NA, width=0.6, col="#808080", aes(fill=Class))+scale_fill_manual(values = c("#F6ABAB","#ABD2F2","#6D92A9"))+annotate("segment",x=0.3, xend=3.7,y=stats.muc7$ThetaW, yend=stats.muc7$ThetaW, col="#E86F75")+ theme(legend.position="none")+coord_cartesian(ylim = c(-3, 40))
thetaw
wallsB <- ggplot(calls, aes(x=factor(Class),y=WallsB))+geom_boxplot(outlier.shape = NA, width=0.6, col="#808080", aes(fill=Class))+scale_fill_manual(values = c("#F6ABAB","#ABD2F2","#6D92A9"))+annotate("segment",x=0.3, xend=3.7,y=stats.muc7$WallsB, yend=stats.muc7$WallsB, col="#E86F75")+ theme(legend.position="none")+coord_cartesian(ylim = c(-0.02, 0.6))
wallsB

names(patterns)
realPatFiles.dataframe[,3] <- 'realData'
names(realPatFiles.dataframe) <- c("snps","ldsnps","migr")

snpinld <- rbind(subset(patterns, migr == 0 | migr == 1), realPatFiles.dataframe)
snpinld$migr <- factor(snpinld$migr, levels = c("realData", "0", "1"))
psnpinld <- ggplot(snpinld, aes(x=factor(migr),y=ldsnps))+geom_boxplot(outlier.shape = NA, width=0.6, col="#808080", aes(fill=migr))+scale_fill_manual(values = c("#F6ABAB","#ABD2F2","#6D92A9"))+annotate("segment",x=0.3, xend=3.7,y=snps.ldsnps.muc7[2], yend=snps.ldsnps.muc7[2], col="#E86F75")+ theme(legend.position="none")+coord_cartesian(ylim = c(-0.3, 70))
psnpinld

multiplot(thetapi, thetaw, wallsB, psnpinld, cols = 4)

## supplementary figure 5
sumstat.0.1 <- subset(sumstat.withmuc7.class, `c(patterns$migr, -1)` == "0" | `c(patterns$migr, -1)` == "1")
sumstat.withmuc7.class$`c(patterns$migr, -1)`
names(sumstat.0.1)[21] <- 'classes'

pdf("SF5.pdf", height=21, width=10)
par(mar=c(4,4,2,2))
layout(matrix(1:21, ncol=3, byrow=TRUE), heights = rep.int(1000, 7), widths = rep.int(1000, 3))
for( i in 1:20)
{
  boxplot(sumstat.0.1[,i]~sumstat.0.1$classes, ylab=colnames(sumstat.0.1)[i], names=c("no-introgression", "introgression"), main=colnames(sumstat.0.1)[i], col=c("#ABD2F2","#6D92A9"),outline=FALSE)
  abline(h=stats.muc7[i], col="#E86F75", lwd=1.5)
}

snpinld <- subset(patterns, migr == 0 | migr == 1)
snpinld$migr <- factor(snpinld$migr, levels = c("0", "1"))
boxplot(snpinld$ldsnps~snpinld$migr, ylab="SNPinLD", names=c("no-introgression", "introgression"), main="SNPinLD", col=c("#ABD2F2","#6D92A9"),outline=FALSE)
abline(h=snps.ldsnps.muc7[2], col="#E86F75", lwd=1.5)

dev.off()

write.csv(sumstat.withmuc7.class, "sumstat.withmuc7.class.csv")
