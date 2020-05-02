abln <- read.csv("Tugas Klp #2 Data.csv")
abln <- data.frame(abln)

#ganti nama
colnames(abln) <- c("sex", "length", "diameter", "height", "wholeWeight", "shuckedWeight","visceraWeight",
                    "shellWeight","rings")

##eksplorasi data
##heatmap
library(ggcorrplot)
corr <- round(cor(abaln[,c(2,3,4,5,6,7,8,9)]), 2)
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of Abalone", 
           ggtheme=theme_bw)
##plot
plot(x=abaln$sex, y=abaln$y, xlab = "sex", ylab="Probabilitas banyaknya jenis kelamin", ylim=c(0,3))

##Ngambil data numerik 
abln.numerik <- abln[2:9]
View(abln.numerik)
abln.numerikscale <- scale(abln.numerik)
View(abln.numerikscale)

#identifikasi outlier (1.8)
library(dbscan)
dbscan::kNNdistplot(abln.numerikscale, k =  8)
abline(h = 1.8, lty = 2)

library(fpc)
outlier.dbscan <- dbscan(abln.numerikscale, eps=1.8, MinPts = 8)
outliers <- which(outlier.dbscan$cluster == 0)

print(outliers)
print(abln.numerikscale[outliers,])

#identifikasi outlier (2)
library(dbscan)
dbscan::kNNdistplot(abln.numerikscale, k =  8)
abline(h = 1.5, lty = 2)

library(fpc)
outlier.dbscan <- dbscan(abln.numerikscale, eps=2, MinPts = 8)
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

outlier <- order(outlier.scores, decreasing=TRUE)[1:5]
print(outlier)
print(abln.numerikscale[outlier,])

n = nrow(abln.numerikscale)
pch <- rep(".", n)
pch[outlier] <- "+"
col <- rep("black", n)
col[outlier] <- "red"
pairs(abln.numerikscale, pch=pch, col=col)

### Drop outlier
abln.nooutlier <- abln.numerikscale[-(outlier),]

##CLUSTERING
#K-Means 
cluster.kmeans <- kmeans(abln.nooutlier, center = 3, nstart = 25)
cluster.kmeans$cluster

fviz_cluster(Clustering, geom = "point", data = abln.nooutlier)+ggtitle("K=3")
