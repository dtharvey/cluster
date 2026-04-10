# load packages, scripts, and data; run this code first
# scripts and data must be in working directory
library(readr)
library(plot3D)
library(scatterplot3d)
library(chemCal)

source("MLRScript.R")

allSpec <- read_csv("allSpec.csv")
load("co_stds.RData")
load("cr_stds.RData")
load("cu_stds.RData")
load("ni_stds.RData")

# EXTERNAL STANDARDIZATION FOR CU (3 slides)
# slide 1: examine spectra for cu stds using cu_stds.RData
matplot(cu_stds2$wavelength, cu_stds2[2:6], type = "l", lwd = 2, xlab = "wavelength (nm)", ylab = "absorbance")
legend("topleft", legend = c("0.0500 M", "0.0400 M", "0.0300 M", "0.0200 M", "0.0100 M"), lwd = 2, lty = 1:5, col = 1:5, bty = "n")
cu.id = which.max(cu_stds2$cu_std1)
abline(v = cu_stds2$wavelength[cu.id], lwd = 2, lty = 2)

# slide 2: calibration curve and equation
plot(x = cu_stdconc, y = cu_stds2[cu.id,2:6], pch = 19, col = "blue", xlab = "[Cu] (M)", ylab = "absorbance", xlim = c(0, max(cu_stdconc)), ylim = c(0, max(cu_stds2[cu.id,2])))
cu.lm = lm(as.numeric(cu_stds2[cu.id, 2:6]) ~ cu_stdconc)
abline(cu.lm, lwd = 2, col = "blue", lty = 2)

# slide 3: determine concentration in unknown
plot(x = cu_stdconc, y = cu_stds2[cu.id,2:6], pch = 19, col = "blue", xlab = "[Cu] (M)", ylab = "absorbance", xlim = c(0, max(cu_stdconc)), ylim = c(0, max(cu_stds2[cu.id,2])))
cu.lm = lm(as.numeric(cu_stds2[cu.id, 2:6]) ~ cu_stdconc)
abline(cu.lm, lwd = 2, col = "blue", lty = 2)
newdata = 0.300 
cu.unk = inverse.predict(cu.lm, newdata)
arrows(-1,0.3,cu.unk$Prediction,0.3, col = "blue", length = 0.1)
arrows(cu.unk$Prediction,0.3, cu.unk$Prediction,-0.02, col = "blue", length = 0.1)

# BINARY MIXTURES OF CU AND NI (3 slides)
# slide 1: choosing wavelengths
wavelengths = as.numeric(colnames(allSpec[8:642]))
plot(x = cu_stds2$wavelength, y = cu_stds2$cu_std1, xlab = "wavelength (nm)", ylab = "absorbance", type = "l", lwd = 2, lty = 2, col = "blue")
lines(x = ni_stds2$wavelength, y = ni_stds2$ni_std1, lty = 2, lwd = 2, col = "forestgreen")
lines(x = wavelengths, y = allSpec[62, 8:642], lty = 1, lwd = 2, col = "black")
legend("top", legend = c("Cu/Ni mixture", "Cu standard", "Ni standard"), lwd = 2, lty = c(1, 2, 2), col = c("black", "blue", "forestgreen"), bty = "n")
cu.id = which.max(cu_stds2$cu_std1)
ni.id = which.max(ni_stds2$ni_std1)
abline(v = cu_stds2$wavelength[cu.id], lwd = 2, lty = 3, col = "blue")
abline(v = ni_stds2$wavelength[ni.id], lwd = 2, lty = 3, col = "forestgreen")

# slide 2: calibration curves
# plot calibration curves for copper and nickel
plot(x = cu_stdconc, y = cu_stds2[cu.id,2:6], pch = 19, col = "blue", xlab = "[Cu] or [Ni] (M)", ylab = "absorbance", xlim = c(0, max(ni_stdconc)), ylim = c(0, max(cu_stds2[cu.id,2])))
cu.lm1 = lm(as.numeric(cu_stds2[cu.id, 2:6]) ~ cu_stdconc)
abline(cu.lm1, lwd = 2, col = "blue", lty = 2)
points(x = cu_stdconc, y = cu_stds2[ni.id,2:6], pch = 15, col = "blue")
cu.lm2 = lm(as.numeric(cu_stds2[ni.id, 2:6]) ~ cu_stdconc)
abline(cu.lm2, lwd = 2, col = "blue", lty = 2)
points(x = ni_stdconc, y = ni_stds2[cu.id,2:6], pch = 19, col = "forestgreen")
ni.lm1 = lm(as.numeric(ni_stds2[cu.id, 2:6]) ~ ni_stdconc)
abline(ni.lm1, lwd = 2, col = "forestgreen", lty = 2)
points(x = ni_stdconc, y = ni_stds2[ni.id,2:6], pch = 15, col = "forestgreen")
ni.lm2 = lm(as.numeric(ni_stds2[ni.id, 2:6]) ~ ni_stdconc)
abline(ni.lm2, lwd = 2, col = "forestgreen", lty = 2)
legend("right", legend = c("Cu: 809.1 nm", "Cu: 394.2 nm", "Ni: 809.1 nm", "Ni: 394.2 nm"), pch = c(19,15,19,15), col = c("blue", "blue", "forestgreen", "forestgreen"), bty = "n")

# slide 3: solve for concentrations
eb = matrix(data = c(eb_cu1 = cu.lm1$coefficients[2], 
                     eb_cu2 = cu.lm2$coefficients[2], 
                     eb_ni1 =  ni.lm1$coefficients[2], 
                     eb_ni2 = ni.lm2$coefficients[2]), 
            nrow = 2, ncol = 2, byrow = FALSE)
colnames(eb) = c("Cu", "Ni")
rownames(eb) = c("809.1 nm", "394.2 nm")
eb
abs = matrix(data = c(allSpec[62, cu.id], allSpec[62, ni.id]), 
             nrow = 2, ncol = 1, byrow = FALSE)
colnames(abs) = "mixture"
rownames(abs) = c("809.1 nm", "394.2 nm")
abs
solve(eb, abs)

# HOW PCA WORKS (5 slides)
# slide 1: generate random xy data with noise; center & scale data; plot data
x1 = seq(-10,10,1)
y1 = x1*2 + 8 + rnorm(21,0,7)
x1 = scale(x1)
y1 = scale(y1)
plot(x1,y1,asp = 1, pch = 19, col = "blue", xlab = "first variable", ylab = "second variable")
abline(v = 0, col = "black", lty = 2)
abline(h = 0, col = "black", lty = 2)

# slide 2: find and plot the first pc axis
xy1.lm = lm(y1 ~ x1)
abline(xy1.lm, lwd = 2, col = "red")

# slide 3: calculate and plot projections (scores) of xy onto first pc
a1 = xy1.lm$coefficients[2]
b1 = xy1.lm$coefficients[1]
c1 = y1 + x1/a1
dx1 = (c1 - b1)/(a1 + 1/a1)
dy1 = a1 * dx1 + b1
points(dx1,dy1,pch = 19, col = "red")

# slide 4 and 5: calculate and plot projections (scores) of xy onto second pc
abline(a = 0, b = -1/a1,lwd = 2, col = "forestgreen")
a2 = -1/a1
b2 = 0
c2 = y1 + x1/a2
dx2 = (c2 - b2)/(a2 + 1/a2)
dy2 = a2 * dx2 + b2
points(dx2,dy2,pch = 19, col = "forestgreen")

# PCA WORKED EXAMPLE (8 slides)

# slide 1: select wavelengths and overlay choices on spectra for metals
wavelengths = as.numeric(colnames(allSpec[8:642]))
wavelength_ids = seq(8,642,40)
plot(x = wavelengths, y = allSpec[1, 8:642], type = "l", xlab = "wavelengths (nm)", ylab = "absorbance", col = "blue", lwd = 2)
lines(x = wavelengths, y = allSpec[6, 8:642], col = "red", lwd = 2)
lines(x = wavelengths, y = allSpec[11, 8:642], col = "orange", lwd = 2)
lines(x = wavelengths, y = allSpec[16, 8:642], col = "forestgreen", lwd = 2)
abline(v = as.numeric(colnames(allSpec[wavelength_ids])), lty = 3, lwd = 1)
legend(x = 820, y = 0.4, legend = c("Cu", "Co", "Cr", "Ni"), lwd = 2, col = c("blue", "red", "orange", "forestgreen"), bty = "n")

# slide 2: subset data for Cu/Cr/Co; complete pca; plot summary
ex_one = allSpec[c(1, 6, 11, 21:25, 38:53), ]
ex_one.pca = prcomp(ex_one[ , wavelength_ids], center = TRUE, scale = TRUE)
summary(ex_one.pca)
plot(ex_one.pca)

# slide 3: plot scores
plot(ex_one.pca$x, cex = 2, pch = 19, asp = 1)

# slide 4: plot scores using dimensions to color points
plot(ex_one.pca$x, cex = 2, pch = 19, asp = 1, col = factor(ex_one$dimensions))
legend("topleft", legend = c("single component", "binary mixture", "ternary mixture"), col = c("black", "red", "green"), pch = 19, bty = "n")

# slide 5: plot loadings and scores as biplot
biplot(ex_one.pca, cex = c(2, 0.6), xlabs = rep("•",24))

# slide 6: add spectra of analytes to slide 5
wavelengths = as.numeric(colnames(allSpec[8:642]))
wavelength_ids = seq(8,642,40)
plot(x = wavelengths, y = allSpec[1, 8:642], type = "l", xlab = "wavelengths (nm)", ylab = "absorbance", col = "blue", lwd = 2)
lines(x = wavelengths, y = allSpec[6, 8:642], col = "red", lwd = 2)
lines(x = wavelengths, y = allSpec[11, 8:642], col = "orange", lwd = 2)
lines(x = wavelengths, y = allSpec[16, 8:642], col = "forestgreen", lwd = 2)
abline(v = as.numeric(colnames(allSpec[wavelength_ids])), lty = 3, lwd = 1)
legend(x = 820, y = 0.4, legend = c("Cu", "Co", "Cr", "Ni"), lwd = 2, col = c("blue", "red", "orange", "forestgreen"), bty = "n")

# slide 7: plot scores with colors by concentration of copper
cuPal = colorRampPalette(c("white", "blue"))
cuColor = cuPal(50)[as.numeric(cut(ex_one$concCu,breaks = 50))]
plot(ex_one.pca$x, pch = 21, bg = cuColor, cex = 2, asp = 1)

# slide 8: plot grid of pca results by concentration and by XXX
cuPal = colorRampPalette(c("white", "blue"))
cuColor = cuPal(50)[as.numeric(cut(ex_one$concCu, breaks = 50))]
crPal = colorRampPalette(c("white", "orange"))
crColor = crPal(50)[as.numeric(cut(ex_one$concCr, breaks = 50))]
coPal = colorRampPalette(c("white", "red"))
coColor = coPal(50)[as.numeric(cut(ex_one$concCo, breaks = 50))]
old.par = par(mfrow = c(2,2))
# plot(ex_one.pca$x, pch = 19, cex = 1.6, asp = 1)
plot(ex_one.pca$x, pch = 21, bg = cuColor, cex = 1.6, asp = 1)
plot(ex_one.pca$x, pch = 21, bg = crColor, cex = 1.6, asp = 1)
plot(ex_one.pca$x, pch = 21, bg = coColor, cex = 1.6, asp = 1)
colv = factor(ex_one$dimensions)
scatterplot3d(x = ex_one.pca$x[,1], y = ex_one.pca$x[,2], z = ex_one.pca$x[,3], pch = 19, color = colv, cex.symbols = 1.6, type = "h", xlab = "PC1", ylab = "PC2", zlab = "PC3")
par(old.par)

# HOW CLUSTER ANALYSIS WORKS (5 slides)
# slide 1: plot sample data
a = c(1.70, -1.30, -0.90, -0.78, 3.20, -1.90)
b = c(2.00, -0.51, 0.12, -0.11, -1.40, -0.17)
plot(a, b, pch = 19, col = okabe_ito[4], 
     xlab = "variable a", ylab = "variable b")
text(a + 0.1, b + 0.1, seq(1:6))
text(x = -2, y = 2, labels = "original data", 
     pos = 4, col = okabe_ito[6])

# slide 2: first cluster
segments(a[3], b[3], a[4], b[4], col = okabe_ito[3])
a[7] = (a[3] + a[4])/2
b[7] = (b[3] + b[4])/2
points(x = a[7], y = b[7], pch = 19, col = okabe_ito[3])

# slide 3: second cluster
segments(a[2], b[2], a[6], b[6], col = okabe_ito[3])
a[8] = (a[2] + a[6])/2
b[8] = (b[2] + b[6])/2
points(x = a[8], y = b[8], pch = 19, col = okabe_ito[3])

# slide 4: remaining clusters
segments(a[7], b[7], a[8], b[8], col = okabe_ito[3])
a[9] = (a[7] + a[8])/2
b[9] = (b[7] + b[8])/2
points(x = a[9], y = b[9], pch = 19, col = okabe_ito[3])
segments(a[1], b[1], a[9], b[9], col = okabe_ito[3])
a[10] = (a[1] + a[9])/2
b[10] = (b[1] + b[9])/2
points(x = a[10], y = b[10], pch = 19, col = okabe_ito[3])
segments(a[5], b[5], a[10], b[10], col = okabe_ito[3])
a[11] = (a[5] + a[10])/2
b[11] = (b[5] + b[10])/2
points(x = a[11], y = b[11], pch = 19, col = okabe_ito[3])

# slide 5: calculate and draw dendrogram
ab = data.frame(a[1:6], b[1:6])
ab.dist = dist(ab)
ab.cluster = hclust(ab.dist, method = "average")
plot(ab.cluster, hang = -1, main = "", sub = "", xlab = "")

# CLUSTER ANALYSIS WORKED EXAMPLE (4 slides)
# slide 1: calculate and examine dist matrix
ex_one.dist = dist(ex_one[ , wavelength_ids])
ex_one.dist

# slide 2: calculate and examine cluster analysis
ex_one.cluster = hclust(ex_one.dist, method = "ward.D")
plot(ex_one.cluster, hang = -1, main = "", sub = "", xlab = "", cex = 0.75)

# slide 3: highlight clusters in terms of concentration of copper
ex_one$perCu = ex_one$concCu/ex_one$concCu[1]
plot(ex_one.cluster, hang = -1, labels = ex_one$perCu, main = "copper", sub = "", xlab = "fraction of stock in sample", ylab = "", cex = 0.75)
rect.hclust(ex_one.cluster, k = 3, which = 1, border = "blue")

# slide 4: plot grid of cluster diagrams
old.par = par(mfrow = c(2,2))
ex_one$perCo = ex_one$concCo/ex_one$concCo[2]
ex_one$perCr = ex_one$concCr/ex_one$concCr[3]
# plot(ex_one.cluster, hang = -1, main = "", sub = "", xlab = "", cex = 0.75)
plot(ex_one.cluster, hang = -1, labels = ex_one$perCu, main = "copper", sub = "", xlab = "fraction of stock in sample", ylab = "", cex = 0.75)
rect.hclust(ex_one.cluster, k = 3, which = 1, border = "blue")
plot(ex_one.cluster, hang = -1, labels = ex_one$perCo, main = "cobalt", sub = "", xlab = "fraction of stock in sample", ylab = "", cex = 0.75)
rect.hclust(ex_one.cluster, k = 3, which = 3, border = "red")
plot(ex_one.cluster, hang = -1, labels = ex_one$perCr, main = "chromium", sub = "", xlab = "fraction of stock in sample", ylab = "", cex = 0.75)
rect.hclust(ex_one.cluster, k = 3, which = 2, border = "orange")
wave.dist = dist(t(ex_one[ , wavelength_ids]))
wave.dist
wave.clust = hclust(wave.dist, method = "ward.D")
plot(wave.clust, hang = -1, main = "clustering of wavelengths", sub = "", xlab = "wavelength", ylab = "")
par(old.par)

# MLR WORKED EXAMPLE (4 slides)
# slide 1: calculate and examine eb values using standards
abs_stds = allSpec[1:15,wavelength_ids]
conc_stds = as.matrix(data.frame(allSpec[1:15, 4], allSpec[1:15,5], allSpec[1:15,6]))
eb_pred = findeb(abs_stds,conc_stds)
eb_pred

# slide 2: calculate and examin predicted concentrations for samples
abs_samples = allSpec[c(21:25, 38:53), wavelength_ids]
pred_conc = findconc(abs_samples,eb_pred)
pred_conc

# slide 3: extract and examine real concentrations
real_conc = as.matrix(data.frame(allSpec[c(21:25, 38:53), 4], allSpec[c(21:25,38:53), 5], allSpec[c(21:25,38:53), 6]))
real_conc

# slide 4: calculate and report errors
conc_error = real_conc - pred_conc
conc_error
means = apply(conc_error, 2, mean)
round(means, digits = 6)
sds = apply(conc_error, 2, sd)
round(sds, digits = 6)
conf.int = abs(qt(0.05/2, 20)) * sds
round(conf.int, digits = 6)
max(abs(conc_error[,1]))
max(abs(conc_error[,2]))
max(abs(conc_error[,3]))
which.max(abs(conc_error[,1]))
which.max(abs(conc_error[,2]))
which.max(abs(conc_error[,3]))
pred_conc[9,1]
real_conc[9,1]
pred_conc[16,2]
real_conc[16,2]
pred_conc[9,3]
real_conc[9,3]
