a <- c(1.70, -1.30, -0.90, -0.78, 3.20, -1.90)
b <- c(2.00, -0.51,  0.12, -0.11, -1.40, -0.17)
ab <- data.frame(a, b)

ab.dist   <- dist(ab)
ab.cluster <- hclust(ab.dist, method = "single")

get_intermediate_matrices <- function(hc, original_dist) {
  n <- attr(original_dist, "Size")
  
  d <- as.matrix(original_dist)
  rownames(d) <- colnames(d) <- as.character(1:n)
  
  cluster_members <- list()
  matrices <- list()
  matrices[[1]] <- as.dist(d)
  
  for (i in 1:nrow(hc$merge)) {
    left  <- hc$merge[i, 1]
    right <- hc$merge[i, 2]
    
    # Resolve members of each side
    left_members  <- if (left  < 0) as.character(-left)  else cluster_members[[left]]
    right_members <- if (right < 0) as.character(-right) else cluster_members[[right]]
    new_members   <- c(left_members, right_members)
    
    # Find the current labels in d that correspond to left and right
    left_label  <- if (left  < 0) as.character(-left)  else 
      paste0("(", paste(cluster_members[[left]],  collapse = ","), ")")
    right_label <- if (right < 0) as.character(-right) else 
      paste0("(", paste(cluster_members[[right]], collapse = ","), ")")
    
    new_label     <- paste0("(", paste(new_members, collapse = ","), ")")
    merged_labels <- c(left_label, right_label)
    keep_labels   <- rownames(d)[!rownames(d) %in% merged_labels]
    
    # Compute new distances using single linkage (min)
    new_dists <- sapply(keep_labels, function(k) {
      min(d[k, merged_labels])
    })
    
    # Build updated distance matrix
    new_labels_all <- c(keep_labels, new_label)
    new_size <- length(new_labels_all)
    new_d <- matrix(0, nrow = new_size, ncol = new_size)
    rownames(new_d) <- colnames(new_d) <- new_labels_all
    
    if (length(keep_labels) > 0) {
      new_d[keep_labels, keep_labels] <- d[keep_labels, keep_labels]
      new_d[new_label, keep_labels]   <- new_dists
      new_d[keep_labels, new_label]   <- new_dists
    }
    
    d <- new_d
    cluster_members[[i]] <- new_members
    
    if (nrow(d) > 1) {
      matrices[[i + 1]] <- as.dist(d)
    }
  }
  
  return(matrices)

intermediate <- get_intermediate_matrices(hc.man, dist.man)

# Print all matrices
for (i in seq_along(intermediate)) {
  cat("\n--- Step", i - 1, 
      if (i == 1) "(initial)" else paste0("(merge ", i - 1, ")"), "---\n")
  print(round(intermediate[[i]], 3))
}
