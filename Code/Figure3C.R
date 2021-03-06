calls <- read.table("pairwise_differences.txt", header=T, sep="\t")
library(ggplot2)
ggplot(calls, aes(Dist, fill=type)) + geom_bar(binwidth=1) + scale_fill_manual(name="Pair Type", breaks=c("a","b","c","d","e","f"), labels=c("Human-Human","Human-NeaDen","Human-Chimps","NeaDen_NeaDen","Chimps-NeaDen","Human-Haplogroup E"), values=c("#ffe184","#ff9666","#207178","#e9f5c7","#174c4f","#ff6138")) + xlab("Pairwise differences") + theme(axis.title.x=element_text(vjust=-0.1))
