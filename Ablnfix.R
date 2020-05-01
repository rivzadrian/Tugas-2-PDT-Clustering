abln <- read.csv("Tugas Klp #2 Data.csv")
abln <- data.frame(abln)

#ganti nama
colnames(abln) <- c("sex", "length", "diameter", "height", "wholeWeight", "shuckedWeight","visceraWeight",
                    "shellWeight","rings")

##Ngambil data numerik 
abln.numerik <- abln[2:9]
View(abln.numerik)
abln.numerikscale <- scale(abln.numerik)
View(abln.numerikscale)

#identifikasi outlier
library(dbscan)
dbscan::kNNdistplot(abln.numerikscale, k =  8)
abline(h = 1.5, lty = 2)

library(fpc)
outlier.dbscan <- dbscan(abln.numerikscale, eps=1.5, MinPts = 8)
outliers <- which(outlier.dbscan$cluster == 0)

print(outliers)
print(abln.numerikscale[outliers,])

library("factoextra")
fviz_cluster(outlier.dbscan, data = abln.numerikscale, stand = FALSE,
             ellipse = FALSE, show.clust.cent = T,
             geom = "point",palette = "jco", ggtheme = theme_classic())

##praktikum
plot(abln.numerikscale)
points(abln.numerikscale[outliers,1:8], col = "red", pch = "x", cex = 2)


## Local Outlier Factor
library(DMwR)
outlier.scores <- lofactor(abln.numerikscale, k=8)
plot(density(outlier.scores))

outlier <- order(outlier.scores, decreasing=TRUE)[1:8]
print(outlier)
print(abln.numerikscale[outlier,])

n = nrow(abln.numerikscale)
pch <- rep(".", n)
pch[outlier] <- "+"
col <- rep("black", n)
col[outlier] <- "red"
pairs(abln.numerikscale, pch=pch, col=col)
