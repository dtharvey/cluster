parse_hclust_steps <- function(hc) {
  merge  = hc$merge
  distance = hc$height
  n_steps = nrow(merge)
  
  clusters = vector("list", n_steps)
  
  for (i in 1:n_steps) {
    left  = merge[i, 1]
    right = merge[i, 2]
    
    left_members  = if (left  < 0) -left  else clusters[[left]]
    right_members = if (right < 0) -right else clusters[[right]]
    
    clusters[[i]] = sort(c(left_members, right_members))
    
    cat(sprintf("Step %d: {%s} distance = %.2f\n", i,
                paste(clusters[[i]], collapse = ","),
                distance[i]))
  }
}
