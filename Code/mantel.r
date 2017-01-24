> calls <- read.table("final_correlation.txt", sep="\t", header=T)
> ggplot(calls, aes(x=main.dietary.component, y=X5_copy)) + geom_boxplot() + geom_jitter()
> ggplot(calls, aes(x=subsistence, y=X5_copy)) + geom_boxplot() + geom_jitter()
> ggplot(calls, aes(x=ecoregion, y=X5_copy)) + geom_boxplot() + geom_jitter()


> library("ade4")
> geo.dists <- dist(cbind(calls$latitude1,calls$longtitude1))  #geo distance matrix
> freq.dists <- dist(calls$X5_copy)  #5 copy freq.
> zdis <- dist(calls$distance.km..to.Africa)  #distance to africa
> wdis <- dist(calls$state_diet)  #diet

> mantel.rtest(geo.dists, 5freq.dists, nrepet=9999)
Monte-Carlo test
Observation: -0.1039814 
Call: mantel.rtest(m1 = geo.dists, m2 = 5freq.dists, nrepet = 9999)
Based on 9999 replicates
Simulated p-value: 0.8722 






> library("vegan")
> mantel.partial(geo.dists, freq.dists, zdis, method="pearson", permutations=9999)

Partial Mantel statistic based on Pearson's product-moment correlation 

Call:
mantel.partial(xdis = geo.dists, ydis = freq.dists, zdis = zdis, method = "pearson", permutations = 9999) 

Mantel statistic r: -0.1362 
      Significance: 0.9245 

Upper quantiles of permutations (null model):
  90%   95% 97.5%   99% 
0.140 0.188 0.232 0.277 
Permutation: free
Number of permutations: 9999


> mantel(geo.dists, freq.dists, method="pearson", permutations=9999)

Mantel statistic based on Pearson's product-moment correlation 

Call:
mantel(xdis = geo.dists, ydis = freq.dists, method = "pearson", permutations = 9999) 

Mantel statistic r: -0.104 
      Significance: 0.8639 

Upper quantiles of permutations (null model):
  90%   95% 97.5%   99% 
0.138 0.185 0.230 0.280 
Permutation: free
Number of permutations: 9999

> mantel.partial(freq.dists, wdis, zdis, method="pearson", permutations=9999)

Partial Mantel statistic based on Pearson's product-moment correlation 

Call:
mantel.partial(xdis = freq.dists, ydis = wdis, zdis = zdis, method = "pearson",      permutations = 9999) 

Mantel statistic r: -0.02159 
      Significance: 0.404 

Upper quantiles of permutations (null model):
  90%   95% 97.5%   99% 
0.185 0.454 0.462 0.471 
Permutation: free
Number of permutations: 9999