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


##CLUSTERING
####K-Means####
cluster.kmeans <- kmeans(abln.nooutlier, center = 3, nstart = 25)
cluster.kmeans$cluster
library(factoextra)
fviz_cluster(cluster.kmeans, geom = "point", data = abln.nooutlier)+ggtitle("K=3")

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
  
  #cat("Prototypes: \n")
  #print(prototypes)
  #cat("\n")
  
  #We move this prototypes 't' times.
  for(i in 1:t)
  {
    #selRow <- ceiling(runif(1, 0, nrow(dataset)))
    selRow <- sample(1:nrow(dataset), size=1, replace=TRUE)
    
    dsX <- dataset[selRow, 1]
    dsY <- dataset[selRow, 2]
    
    #Select a point from 'x' dataset.
    dsPoint <- c(dsX, dsY)
    
    #cat("Selected x point is (",dsX,", ",dsY,"). The ",selRow," set point\n")
    
    alpha <- (alpha - alphaRatio)
    
    #cat("Alpha is now:", alpha,"percent\n")
    
    #Find closest prototype from selected point.
    indexBMU <- findBMU(dsPoint, prototypes)
    
    #cat("The",indexBMU,"prototype is the BMU (",prototypes[indexBMU, 1],",",prototypes[indexBMU, 2],")\n")
    
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

summary(abln_num_fix)
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
