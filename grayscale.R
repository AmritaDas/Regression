#R5 (b)

setwd("C:/.../faces-corrected/")

file_list <- list.files(".")

library("pixmap")

allPics = c()

for(fil in file_list){
  pic <- read.pnm(fil)
  picVec <- as.vector(t(pic@grey))
  picMax <- matrix(picVec)
  allPics <- cbind(allPics, picMax)
}

ncols <- ncol(pic@grey)

Rmean <- rowMeans(allPics)
Radj <- sweep(allPics, 1, Rmean)
cov <- (t(Radj) %*% Radj) / (length(file_list)-1)

eigenVs <- eigen(cov)
PCA <- Radj %*% eigenVs$vectors

f <- function(m) t(m)[,nrow(m):1]

p <- par(mfrow=c(3,4))
for(k in 1:12){
  pca1 <- f(matrix(PCA[,k], ncol = ncols, byrow = TRUE))
  image(pca1, col = gray((0:256)/256), xaxt='n', yaxt='n')
}
