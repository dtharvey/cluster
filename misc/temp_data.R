# original data made into a dataframe
x = c(0, 6, 3, 9.5)
y = c(10.5, 10.5, 3.5, 0)
orig_data = data.frame(x, y)
rownames(orig_data) = c("1","2","3","4")
orig_data

od_dist = dist(orig_data, "euclidean")
od_dist

dist.euc = dist(orig_data, method = "euclidean")
hc.euc = hclust(dist.euc, method = "centroid")
dist_matrix = sim_matrix(hc.euc, dist.euc, step = NULL)
dist_matrix

plot(x = x, y = y, pch = 19)
