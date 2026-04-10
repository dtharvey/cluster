# code to create the figures for the introduction tab...run slide-by-slide
# saving the images as png files with the file names intro1.png,
# intro2.png...intro7.png; the first six images show the formation of
# the clusters and the resulting dendrogram 

# choice of palette
okabe_ito = palette("Okabe-Ito")

# introduction 1: plot sample data: points in 2D space
a = c(0.75, -2, -0.90, -0.78, 2, -1.90)
b = c(1.25, -0.51, 0.12, -0.11, -1.4, -0.17)
plot(a, b, pch = 19, col = okabe_ito[1], 
     xlab = "variable a", ylab = "variable b",
     xlim = c(-2,2), ylim = c(-1.5,1.5))
text(a[1], b[1], labels = "1", pos = 4, col = okabe_ito[1])
text(a[2], b[2], labels = "2", pos = 4, col = okabe_ito[1])
text(a[3], b[3], labels = "3", pos = 2, col = okabe_ito[1])
text(a[4], b[4], labels = "4", pos = 1, col = okabe_ito[1])
text(a[5], b[5], labels = "5", pos = 1, col = okabe_ito[1])
text(a[6], b[6], labels = "6", pos = 3, col = okabe_ito[1])
text(x = -2, y = 1.5, labels = "original data", 
     pos = 4, col = okabe_ito[1])

# introduction 2: form first cluster
segments(a[3], b[3], a[4], b[4], col = okabe_ito[2], lwd = 2)
a[7] = (a[3] + a[4])/2
b[7] = (b[3] + b[4])/2
points(x = a[7], y = b[7], pch = 19, col = okabe_ito[2])
text(a[7], b[7], labels = "7", pos = 4, col = okabe_ito[2])
text(x = -2, y = 1.4, labels = "first cluster", 
     pos = 4, col = okabe_ito[2])

# introduction 3: form second cluster
segments(a[2], b[2], a[6], b[6], col = okabe_ito[3], lwd = 2)
a[8] = (a[2] + a[6])/2
b[8] = (b[2] + b[6])/2
points(x = a[8], y = b[8], pch = 19, col = okabe_ito[3])
text(a[8], b[8], labels = "8", pos = 2, col = okabe_ito[3])
text(x = -2, y = 1.3, labels = "second cluster", 
     pos = 4, col = okabe_ito[3])

# introduction 4: form third cluster
segments(a[7], b[7], a[8], b[8], col = okabe_ito[4], lwd = 2)
a[9] = (a[7] + a[8])/2
b[9] = (b[7] + b[8])/2
points(x = a[9], y = b[9], pch = 19, col = okabe_ito[4])
text(a[9], b[9], labels = "9", pos = 1, col = okabe_ito[4])
text(x = -2, y = 1.2, labels = "third cluster", 
     pos = 4, col = okabe_ito[4])

# slide 5: form fourth cluster
segments(a[1], b[1], a[9], b[9], col = okabe_ito[6], lwd = 2)
a[10] = (a[1] + a[9])/2
b[10] = (b[1] + b[9])/2
points(x = a[10], y = b[10], pch = 19, col = okabe_ito[6])
text(a[10], b[10], labels = "10", pos = 2, col = okabe_ito[6])
text(x = -2, y = 1.1, labels = "fourth cluster", 
     pos = 4, col = okabe_ito[6])

# introduction 6: form fifth cluster
segments(a[5], b[5], a[10], b[10], col = okabe_ito[7], lwd = 2)
a[11] = (a[5] + a[10])/2
b[11] = (b[5] + b[10])/2
points(x = a[11], y = b[11], pch = 19, col = okabe_ito[7])
text(a[11], b[11], labels = "11", pos=2, col = okabe_ito[7])
text(x = -2, y = 1.0, labels = "fifth cluster", 
     pos = 4, col = okabe_ito[7])

# introduction 7: calculate and draw dendrogram
ab = data.frame(a[1:6], b[1:6])
ab.dist = dist(ab)
ab.cluster = hclust(ab.dist, method = "average")
plot(ab.cluster, hang = -1, main = "", sub = "", xlab = "")
