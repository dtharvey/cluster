# using R for a cluster analysis

help(hclust)

help(dist)

help(scale)

a = c(1.70, -1.30, -0.90, -0.78, 3.20, -1.90)

b = c(2.00, -0.51, 0.12, -0.11, -1.40, -0.17)

ab = data.frame(a, b)

ab.dist = dist(ab)

ab.dist

# nearest neighbor

ab.cluster = hclust(ab.dist, method = "single")

ab.cluster$height

ab.cluster$order

plot(ab.cluster)

# furthest neighbor

ab.cluster = hclust(ab.dist, method = "complete")

plot(ab.cluster)

# average

ab.cluster = hclust(ab.dist, method = "average")

plot(ab.cluster)

# elements data: working with scaled data

load("Elements.RData")

View(elements)

elem = elements[3:6]

elem.s = scale(elem)

head(elem)

head(elem.s)

elem.dist = dist(elem.s)

elem.cluster = hclust(elem.dist, method = "single")

plot(elem.cluster)

plot(elem.cluster, labels = elements$element, hang = -1)

elem.cluster = hclust(elem.dist, method = "complete")

plot(elem.cluster, labels = elements$element, hang = -1)

elem.cluster = hclust(elem.dist, method = "average")

plot(elem.cluster, labels = elements$element, hang = -1)

# elements: cluster on measured properties

property = t(elem)

head(elem)

head(property)

prop.s = scale(property)

prop.dist = dist(prop.s)

prop.cluster = hclust(prop.dist, method = "single")

plot(prop.cluster)

# cluster analysis of water data

water = read.csv("~/Box Sync/p-harvey/Teaching/Chem 351/Class Units/11.Cluster_Analysis/table1.csv")

head(water)

water$ID = seq(1:25)

water$id = paste0(water$ID, ":", water$sample, ":", water$class)

water.s = scale(water[3:16])

water.dist = dist(water.s)

water.cluster = hclust(water.dist, method = "complete")

plot(water.cluster, labels = water$id, hang = -1)
