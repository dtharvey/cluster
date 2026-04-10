
# original data made into a dataframe
A = c(0.75, -2, -0.90, -0.78, 2, -1.90)
B = c(1.25, -0.51, 0.12, -0.11, -1.4, -0.17)
original_data = data.frame(A, B)
rownames(original_data) = c("1","2","3","4","5","6")

# code to create dist and hc files needed by sim_matrix function
# dist.euc = dist(df, method = "euclidean")
# hc.euc = hclust(dist.euc, method = "average"); adjust the method for
# dist and hclust as needed

# code here allows printing a single similarity matrix
# replace get_intermediate_matrices with sim_matrix

# all matricies
# intermediate <- get_intermediate_matrices(ab.cluster, ab.dist)

# Just the initial matrix (step 0)
# get_intermediate_matrices(ab.cluster, ab.dist, step = 0)

# Matrix after the second merge (step 2)
# get_intermediate_matrices(ab.cluster, ab.dist, step = 2)
