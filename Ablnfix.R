abln <- read.csv("Tugas Klp #2 Data.csv")
abln <- data.frame(abln)

#ganti nama
colnames(abln) <- c("sex", "length", "diameter", "height", "wholeWeight", "shuckedWeight","visceraWeight",
                    "shellWeight","rings")

##eksplorasi data####
##heatmap
library(ggcorrplot)
corr <- round(cor(abln[,c(2,3,4,5,6,7,8,9)]), 2)
ggcorrplot(corr, hc.order = TRUE,
           type = "lower",
           lab = TRUE,
           lab_size = 3,
           method="circle",
           colors = c("tomato2", "white", "springgreen3"),
           title="Correlogram of Abalone",
           ggtheme=theme_bw)
##plot
plot(x=abln$sex, y=abln$y, xlab = "sex", ylab="Probabilitas banyaknya jenis kelamin", ylim=c(0,3))

#Mencari duplikasi data
library(dplyr)
which(duplicated(abln))

# Plot box sex diameter
g <- ggplot(abln, aes(sex, diameter))
g + geom_boxplot(varwidth=T, fill="plum") + 
  labs(title="Box plot", 
       subtitle="City Mileage grouped by Class of vehicle",
       caption="Source: mpg",
       x="Sex",
       y="Diameter") + ylim(0,0.75)



##Ngambil data numerik####
abln.numerik <- abln[2:9]
View(abln.numerik)
abln.numerikscale <- scale(abln.numerik)
View(abln.numerikscale)

#identifikasi outlier (1.8)####
library(dbscan)
dbscan::kNNdistplot(abln.numerikscale, k =  8)
abline(h = 1.8, lty = 2)

library(fpc)
outlier.dbscan <- dbscan(abln.numerikscale, eps=1.8, MinPts = 8)
outliers <- which(outlier.dbscan$cluster == 0)

print(outliers)
print(abln.numerikscale[outliers,])

#identifikasi outlier (2)####
library(dbscan)
dbscan::kNNdistplot(abln.numerikscale, k =  8)
abline(h = 2, lty = 2)

library(fpc)
outlier.dbscan <- dbscan(abln.numerikscale, eps=2, MinPts = 8)
outliers.db <- which(outlier.dbscan$cluster == 0)

print(outliers)
print(abln.numerikscale[outliers,])

library("factoextra")
fviz_cluster(outlier.dbscan, data = abln.numerikscale, stand = FALSE,
             ellipse = FALSE, show.clust.cent = T,
             geom = "point",palette = "jco", ggtheme = theme_classic())

##praktikum
plot(abln.numerikscale)
points(abln.numerikscale[outliers,1:8], col = "red", pch = "x", cex = 2)


## Local Outlier Factor####
library(DMwR)
outlier.scores <- lofactor(abln.numerikscale, k=8)
plot(density(outlier.scores))

outlier.lof <- order(outlier.scores, decreasing=TRUE)[1:5]
print(outlier)
print(abln.numerikscale[outlier,])

n = nrow(abln.numerikscale)
pch <- rep(".", n)
pch[outlier] <- "+"
col <- rep("black", n)
col[outlier] <- "red"
pairs(abln.numerikscale, pch=pch, col=col)


#hapus outlier####
outlier <- c(outlier.lof, outliers.db )
outlier
n_occur <- data.frame(table(outlier))
n_occur[n_occur$Freq > 1,]
outlier.fix = 0
n_occur
outlier.fix<-subset(outliers.db, outliers.db%in% outlier.lof)#men subset yang sama dr 2 vector
abln.nooutlier <- abln.numerikscale[-(outlier.fix),]

## Sum Squared Error (SSE)
SSE <- sapply(1:9, function(k) {
  kmeans(abln.nooutlier, centers = k, nstart = 25)$tot.withinss
})

plot(1:9, SSE, type="b", xlab = "k", 
     ylab = "Within grops sum of squares")
SSE

library(factoextra)
fviz_nbclust(abln.nooutlier, FUN = kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2) + 
  labs(subtitle = "Elbow Method")

##CLUSTERING
####K-Means####
cluster.kmeans <- kmeans(abln.nooutlier, center = 4, nstart = 25)
cluster.kmeans$cluster

### Plot KMeans
plot(abln.nooutlier,
     col = cluster.kmeans$cluster)

##Mengetahui Karakteristik Cluster
abln.character = data.frame(abln.nooutlier, cluster.kmeans$cluster)
View(abln.character)
aggregate(abln.character[-1],by=list(abln.character$cluster.kmeans.cluster),FUN=mean)

###METODE HIERARKI###
#Hierarki Agglomerative Clustering
library(cluster)
cluster.agnes <- agnes(x=abln.nooutlier,
                       stand = TRUE,
                       metric = "euclidean",
                       method = "average")

#Menampilkan Hierarki Agglomerative Clustering
library(factoextra)
fviz_dend(cluster.agnes, cex = 0.4, k = 4)

#Plot Data Point Agglomerative Clustering
clust <- cutree(cluster.agnes, k = 4)
fviz_cluster(list(data = abln.nooutlier, cluster = clust))

#Hierarki Divisive Clustering
cluster.diana <- diana(x=abln.nooutlier,
                       stand = TRUE,
                       metric = "euclidean")
fviz_dend(cluster.diana, cex = 0.6, k = 4)

#Plot Data Point Divisive Clustering
clust.diana <- cutree(cluster.diana, k = 4)
fviz_cluster(list(data = abln.nooutlier, cluster = clust.diana))


####Pra proses sebelum PCA untuk VQ####
abln1 <- read.csv("Tugas Klp #2 Data.csv")
abln1 <- data.frame(abln1)

#ganti nama
colnames(abln1) <- c("sex", "length", "diameter", "height", "wholeWeight", "shuckedWeight","visceraWeight",
                    "shellWeight","rings")



abalone_sex <- abln$sex

abalone_num <- abln[2:9]
abalone_num <- scale(abalone_num)
abalone_num <- cbind(sex = abalone_sex, abalone_num )#M = 3, F = 1, I = 2

####Menghapus outlier####
outlier <- c(outlier.lof, outliers.db )
outlier
n_occur <- data.frame(table(outlier))
n_occur[n_occur$Freq > 1,]
outlier.fix = 0
n_occur
outlier.fix<-subset(outliers.db, outliers.db%in% outlier.lof)#men subset yang sama dr 2 vector
abln_num_fix <- abalone_num[-(outlier.fix),]

####PCA####
pc <- princomp(abln_num_fix)
plot(pc)
plot(pc, type='l')
summary(pc)

comp <- pc$scores[,1:4]  
comp.1 <- data.frame(comp[,])
comp
comp.1
####Vector Quantization Function####
library("ggplot2")



#Calculate distance between two points
calculateDistance <- function(pointA, pointB)
{
  d <- sqrt( (pointB[1] - pointA[1])^2 + (pointB[2] - pointA[2])^2 )
  
  return(d)
}



# Best Match Unit algorithm:
# point: point to compare
# set: pointset to compare with x
# We assume the minimun distance as BMU.

findBMU <- function(point, set)
{
  mDist <- 10^10
  index <- -1
  
  max <- nrow(set)
  
  #Loop for each set point.
  for(i in 1:max)
  {
    p <- c(set[i, 1], set[i, 2])
    distance <- calculateDistance(point, p)
    
    #Store the minimun set point by its distance to 'x' 
    if(distance < mDist)
    {
      mDist <- distance
      index <- i
    }
  }
  
  return(index)
}


#Move the prototype toward 'x'
updateRule <- function(bmu, point, alpha)
{
  vector <- c(point[1] - bmu[1], point[2] - bmu[2])
  dist <- calculateDistance(bmu, point)
  
  #get unit vector
  unitVector <- c( (vector[1]/dist), (vector[2]/dist) )
  
  #get the alpha magnitud
  mg <- (dist * alpha)
  
  aux <- c( (mg*unitVector[1]), (mg*unitVector[2]) )
  
  newBMU <- c(bmu[1]+aux[1], bmu[2]+aux[2])
  
  return(newBMU)
  
}


# Vector Quantization algorithm:
# dataset: iris dataset
# k: number of prototypes
# alphaI: Maximum alpha value (max 1)
# alphaL: Minimun alpha value (min 0)
# t: number of iterations
VQ <- function(dataset, k, alphaI, alphaL, t)
{
  #Generate random points for prototypes (k).
  x <- runif(k, 4.0, 8.0)
  y <- runif(k, 2.0, 4.5)
  
  #Store them.
  prototypes <- data.frame(x, y)
  
  #Calculate alpha ratio, this will determinate how much each prototype 
  #is going to move to a certain x (from iris dataset).
  alphaRatio <- ( (alphaI - alphaL)/t )
  alpha <- (alphaI + alphaRatio)
  
  cat("Prototypes: \n")
  print(prototypes)
  cat("\n")
  
  #We move this prototypes 't' times.
  for(i in 1:t)
  {
    selRow <- ceiling(runif(1, 0, nrow(dataset)))
    selRow <- sample(1:nrow(dataset), size=1, replace=TRUE)
    
    dsX <- dataset[selRow, 1]
    dsY <- dataset[selRow, 2]
    
    #Select a point from 'x' dataset.
    dsPoint <- c(dsX, dsY)
    
    cat("Selected x point is (",dsX,", ",dsY,"). The ",selRow," set point\n")
    
    alpha <- (alpha - alphaRatio)
    
    cat("Alpha is now:", alpha,"percent\n")
    
    #Find closest prototype from selected point.
    indexBMU <- findBMU(dsPoint, prototypes)
    
    cat("The",indexBMU,"prototype is the BMU (",prototypes[indexBMU, 1],",",prototypes[indexBMU, 2],")\n")
    
    #Check for a correct bmu
    if(indexBMU >= 0)
    {
      bmu <- c(prototypes[indexBMU, 1], prototypes[indexBMU, 2])
      nBMU <- updateRule(bmu, dsPoint, alpha)
      
      #Update the BMU prototype with the new coordinates.
      prototypes[indexBMU, 1] <- nBMU[1]
      prototypes[indexBMU, 2] <- nBMU[2]
      
      #cat("Updating the prototype, from (",bmu[1],",",bmu[2],") to (",nBMU[1],", ",nBMU[2],")\n\n\n")
    }
  }
  
  return(prototypes)
}



####VQ Implementation on diameter and length####
#Parameters
alphaInit <- 0.95
alphaLast <- 0
numLoops <- 10000
comp <- as.data.frame(abln_num_fix)
newAbln <- data.frame(comp$length,comp$diameter)
newAbln
#Saving the output into a txt file.
#out <- capture.output(  VQ(newIris, 3, alphaInit, alphaLast, numLoops)  )
#cat("Output", out, file="C:/output.txt", sep="\n", append=FALSE)
newAbln
protos <- VQ(newAbln, 3, alphaInit, alphaLast, numLoops)

cat("Prototypes: \n")
print(protos)
cat("\n")

summary(protos)
#This is just for plotting purpose
z <- 0
w <- 0
e <- 0
v <- 0
g <- 0
u <- 0
s <- "prototypes"
newData <- data.frame(length = protos$x, diameter = protos$y, height = z, wholeWeight = w, shuckedWeight = e, 
                      visceraWeight = v, shellWeight = g, rings = u, sex = s)
newData

comp

outAbln <- rbind(comp, newData)



p <- ggplot(outAbln, aes(length, diameter))
p + geom_point(aes(color = factor(sex)), size = 2) + theme(legend.position="top")






####VQ Implementation on height and shuckedWeight####
#Parameters
alphaInit <- 0.95
alphaLast <- 0
numLoops <- 10000
comp <- as.data.frame(abln_num_fix)
newAbln <- data.frame(comp$height,comp$shuckedWeight)
newAbln
#Saving the output into a txt file.
#out <- capture.output(  VQ(newIris, 3, alphaInit, alphaLast, numLoops)  )
#cat("Output", out, file="C:/output.txt", sep="\n", append=FALSE)
newAbln
protos <- VQ(newAbln, 3, alphaInit, alphaLast, numLoops)

cat("Prototypes: \n")
print(protos)
cat("\n")

summary(abln_num_fix)
#This is just for plotting purpose
z <- 0
w <- 0
e <- 0
v <- 0
g <- 0
u <- 0
s <- "prototypes"
newData <- data.frame(height = protos$x, shuckedWeight = protos$y, length = z, wholeWeight = w, diameter = e, 
                      visceraWeight = v, shellWeight = g, rings = u, sex = s)
newData

comp

outAbln <- rbind(comp, newData)



p <- ggplot(outAbln, aes(height, shuckedWeight))
p + geom_point(aes(color = factor(sex)), size = 2) + theme(legend.position="top")

####VQ Implementation on shuckedwieight and rings####
#Parameters
alphaInit <- 0.95
alphaLast <- 0
numLoops <- 10000
comp <- as.data.frame(abln_num_fix)
newAbln <- data.frame(comp$shuckedWeight,comp$rings)
newAbln
#Saving the output into a txt file.
#out <- capture.output(  VQ(newIris, 3, alphaInit, alphaLast, numLoops)  )
#cat("Output", out, file="C:/output.txt", sep="\n", append=FALSE)
newAbln
protos <- VQ(newAbln, 4, alphaInit, alphaLast, numLoops)
protos
cat("Prototypes: \n")
print(protos)
cat("\n")

summary(abln_num_fix)
#This is just for plotting purpose
z <- 0
w <- 0
e <- 0
v <- 0
g <- 0
u <- 0
s <- "prototypes"
newData <- data.frame(shuckedWeight = protos$x, rings = protos$y, length = z, wholeWeight = w, diameter = e, 
                      visceraWeight = v, shellWeight = g, height = u, sex = s)
newData

comp

newData

outAbln <- rbind(comp, newData)



p <- ggplot(outAbln, aes(rings, height))
p + geom_point(aes(color = factor(sex)), size = 2) + theme(legend.position="top")

shuck <- c(comp$shuckedWeight)
rings <- c(comp$rings)

shuck_rings <- data.frame(shuck,rings)

distance<- dist()
distance$


cluster1 <- 0
cluster2<- 0
cluster3<- 0
for (row in 1:nrow(comp)) {
  distance <- dist(shuck_rings)
  
}


####HVT Clustering (VQ) threshold 0.2####



library("muHVT")
set.seed(240)
hvt.results <- list()
hvt.results <- HVT(abln_num_fix,
                   nclust = 4,
                   depth = 1,
                   quant.err = 0.2,
                   projection.scale = 10,
                   normalize = T,
                   distance_metric = "L1_Norm",
                   error_metric = "mean")

plotHVT(hvt.results,
        line.width = c(1.2), 
        color.vec = c("#141B41"),
        maxDepth = 1)

hvt.results[[3]]$compression_summary

hvtHmap(hvt.results, 
        abln_num_fix, 
        child.level = 1,
        hmap.cols = "Quant.Error", 
        line.width = c(0.2),
        color.vec = c("#141B41"),
        palette.color = 6,
        centroid.size = 3,
        show.points = T,
        quant.error.hmap = 0.2,
        nclust.hmap = 4)

set.seed(240)
hvt.results2 <- list()
# depth=2 is used for level2 in the hierarchy
hvt.results2 <- HVT(abln_num_fix,
                    nclust = 4,
                    depth = 2,
                    quant.err = 0.2,
                    projection.scale = 10,
                    normalize = T,
                    distance_metric = "L1_Norm",
                    error_metric = "mean")
plotHVT(hvt.results2, 
        line.width = c(1.2, 0.8), 
        color.vec = c("#141B41","#0582CA"),
        maxDepth = 2)
hvt.results2[[3]]$compression_summary

set.seed(240)
hvt.results3 <- list()
# depth=3 is used for level3 in the hierarchy
hvt.results3 <- HVT(abln_num_fix,
                    nclust = 4,
                    depth = 3,
                    quant.err = 0.2,
                    projection.scale = 10,
                    normalize = T,
                    distance_metric = "L1_Norm",
                    error_metric = "mean")
hvt.results3[[3]]$compression_summary

set.seed(240)
hvt.results4 <- list()
# depth=3 is used for level4 in the hierarchy
hvt.results4 <- HVT(abln_num_fix,
                    nclust = 4,
                    depth = 4,
                    quant.err = 0.2,
                    projection.scale = 10,
                    normalize = T,
                    distance_metric = "L1_Norm",
                    error_metric = "mean")
hvt.results4[[3]]$compression_summary

hvtHmap(hvt.results4, 
        trainComputers, 
        child.level = 4,
        hmap.cols = "Quant.Error", 
        line.width = c(1.2,0.8,0.4,0.4),
        color.vec = c("#141B41","#6369D1","#D8D2E1","#3F00FF"),
        palette.color = 6,
        show.points = T,
        centroid.size = 1,
        quant.error.hmap = 0.2,
        nclust.hmap = 15)

set.seed(240)
hvt.results5 <- list()
# depth=3 is used for level5 in the hierarchy
hvt.results5 <- HVT(abln_num_fix,
                    nclust = 4,
                    depth = 5,
                    quant.err = 0.2,
                    projection.scale = 10,
                    normalize = T,
                    distance_metric = "L1_Norm",
                    error_metric = "mean")
hvt.results5[[3]]$compression_summary

####HVT Clustering (VQ) threshold 0.5####



library("muHVT")
set.seed(240)
hvt.results <- list()
hvt.results <- HVT(abln_num_fix,
                   nclust = 4,
                   depth = 1,
                   quant.err = 0.2,
                   projection.scale = 10,
                   normalize = T,
                   distance_metric = "L1_Norm",
                   error_metric = "mean")

plotHVT(hvt.results,
        line.width = c(1.2), 
        color.vec = c("#141B41"),
        maxDepth = 1)

hvt.results[[3]]$compression_summary

hvtHmap(hvt.results, 
        abln_num_fix, 
        child.level = 1,
        hmap.cols = "Quant.Error", 
        line.width = c(0.2),
        color.vec = c("#141B41"),
        palette.color = 6,
        centroid.size = 3,
        show.points = T,
        quant.error.hmap = 0.2,
        nclust.hmap = 4)

set.seed(240)
hvt.results2 <- list()
# depth=2 is used for level2 in the hierarchy
hvt.results2 <- HVT(abln_num_fix,
                    nclust = 4,
                    depth = 2,
                    quant.err = 0.2,
                    projection.scale = 10,
                    normalize = T,
                    distance_metric = "L1_Norm",
                    error_metric = "mean")
plotHVT(hvt.results2, 
        line.width = c(1.2, 0.8), 
        color.vec = c("#141B41","#0582CA"),
        maxDepth = 2)
hvt.results2[[3]]$compression_summary

set.seed(240)
hvt.results3 <- list()
# depth=3 is used for level3 in the hierarchy
hvt.results3 <- HVT(abln_num_fix,
                    nclust = 4,
                    depth = 3,
                    quant.err = 0.2,
                    projection.scale = 10,
                    normalize = T,
                    distance_metric = "L1_Norm",
                    error_metric = "mean")
hvt.results3[[3]]$compression_summary

set.seed(240)
hvt.results4 <- list()
# depth=3 is used for level4 in the hierarchy
hvt.results4 <- HVT(abln_num_fix,
                    nclust = 4,
                    depth = 4,
                    quant.err = 0.2,
                    projection.scale = 10,
                    normalize = T,
                    distance_metric = "L1_Norm",
                    error_metric = "mean")
hvt.results4[[3]]$compression_summary

hvtHmap(hvt.results4, 
        trainComputers, 
        child.level = 4,
        hmap.cols = "Quant.Error", 
        line.width = c(1.2,0.8,0.4,0.4),
        color.vec = c("#141B41","#6369D1","#D8D2E1","#3F00FF"),
        palette.color = 6,
        show.points = T,
        centroid.size = 1,
        quant.error.hmap = 0.2,
        nclust.hmap = 15)

set.seed(240)
hvt.results5 <- list()
# depth=3 is used for level5 in the hierarchy
hvt.results5 <- HVT(abln_num_fix,
                    nclust = 4,
                    depth = 5,
                    quant.err = 0.2,
                    projection.scale = 10,
                    normalize = T,
                    distance_metric = "L1_Norm",
                    error_metric = "mean")
hvt.results5[[3]]$compression_summary

## Density Based eps 0.95
library(dbscan)
dbscan::kNNdistplot(abln.nooutlier, k = 4)
abline(h=0.95, lty = 2)

library(fpc)
cluster.dbscan <- dbscan(abln.nooutlier, eps=0.95, MinPts = 8.3)

plot(cluster.dbscan, abln.nooutlier)

plot(cluster.dbscan, abln.nooutlier[c(2:9)])

### Visualisasi
library(factoextra)
fviz_cluster(cluster.dbscan, geom = "point", data = abln.nooutlier)+ggtitle("K=4")

## Density Based eps 0.9

library(dbscan)
dbscan::kNNdistplot(abln.nooutlier, k = 4)
abline(h=0.9, lty = 2)

library(fpc)
cluster.dbscan <- dbscan(abln.nooutlier, eps=0.9, MinPts = 8)

plot(cluster.dbscan, abln.nooutlier)

plot(cluster.dbscan, abln.nooutlier[c(2:9)])

### Visualisasi
library(factoextra)
fviz_cluster(cluster.dbscan, geom = "point", data = abln.nooutlier)+ggtitle("K=4")

## Density Based eps 1
library(dbscan)
dbscan::kNNdistplot(abln.nooutlier, k = 4)
abline(h=1, lty = 2)

library(fpc)
cluster.dbscan <- dbscan(abln.nooutlier, eps=1, MinPts = 8)

plot(cluster.dbscan, abln.nooutlier)

plot(cluster.dbscan, abln.nooutlier[c(2:9)])

### Visualisasi
library(factoextra)
fviz_cluster(cluster.dbscan, geom = "point", data = abln.nooutlier)+ggtitle("K=4")

