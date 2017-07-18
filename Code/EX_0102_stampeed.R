calls <- read.csv("MUC7-ILMN1M.csv")
write.table(calls$rs.,"muc7_marker_rsid.txt",col.names = F, row.names = F, quote = F)

###############################################################################
# get cluster

calls <- read.table("muc7_marker.haplotype.cluster.txt", header = T, sep = "\t")
clr <- read.csv("../../ACC/marker_info/muc7_hp.real_sorted.color.csv", header=T, sep=",")

set.seed(20)
cluster <- kmeans(calls[,2:36], 8, nstart = 20)
cluster$cluster
cluster$size


library(gplots)
mat <- as.matrix(calls[,1:37][,-1])
row.names(mat) <- calls[,1]
my_palette <- c("grey","black")

# colour by real haplotype group
legend_color = as.character(clr$color_hp)
# colour by 5/6 copy
legend_color = as.character(clr$color_cn)

# colour by 5/6 copy
dev.off()
legend_color = as.character(clr$color_cn)
heatmap.2(mat, trace="none", density.info="none",
          Rowv = cluster$cluster, Colv = F, col=my_palette,
          RowSideColors=legend_color, cexRow=1.5)

# colour by real haplotype group
dev.off()
legend_color = as.character(clr$color_hp)
heatmap.2(mat, trace="none", density.info="none",
          Rowv = cluster$cluster, Colv = F, col=my_palette,
          RowSideColors=legend_color, cexRow=1.5)



#############################################################################
# Cochran-Armitage test for trend

library("DescTools")
# add in ACC to AA
x <- matrix(c(409,41,0,1030,69,6), ncol=3, nrow=2, byrow=T)
dimnames(x) <- list(c('controls','cases'),c("6/6","5/6","5/5"))

CochranArmitageTest(x, alternative = "decreasing")
#    data:  x
#    Z = 1.1257, dim = 3, p-value = 0.1301
#    alternative hypothesis: decreasing

# only use AA data
y <- matrix(c(409,41,0,503,30,3), ncol=3, nrow=2, byrow=T)
dimnames(y) <- list(c('controls','cases'),c("6/6","5/6","5/5"))
CochranArmitageTest(y, alternative = "decreasing")
# p-value = 0.09006

##############################################################################
# Exact Max CATT test

library(MaXact)
maxact.test(x, max3 = T, alternative = "greater")

catt.test(x, theta=0, alternative = "greater")
catt.test(y, theta=0, alternative = "greater")


