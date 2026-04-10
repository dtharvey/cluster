
# original data

A = c(0.9, 0.3, 0.7, 0.1, 1.0, 0.3)
B = c(0.5, 0.2, 0.2, 0.4, 0.7, 0.1)
C = c(0.2, 0.6, 0.1, 1.1, 2.0, 0.3)
D = c(1.6, 0.7, 0.9, 1.3, 2.2, 0.3)
E = c(1.5, 0.1, 0.1, 0.2, 0.4, 0.1)

original_data = data.frame(A, B, C, D, E)
rownames(original_data) = c(1,2,3,4,5,6)

# dist.euc = dist(df, method = "euclidean")
# dist.euc
# 
# dist.man = dist(df, method = "manhattan")
# dist.man
