#SOLUTION

install.packages("imager")
install.packages("irlba")
library(imager)
library(irlba)

image <- load.image("~/dev/SVD/SVD_students/pele.jpg")


######################################################
#############1.0 Gray scale compression ##############
######################################################
plot(image)
megray<-grayscale(image)
plot(megray,axes=FALSE,main="Grayscale image")

svd1<-svd(scale(megray))
str(svd1)

plot(svd1$d^2/sum(svd1$d^2),pch=19,xlab="Singular vector",ylab="Variance explained")

#matrix multiplication of U,D and V*
#including only first 5 singluar vectors
approx5<-svd1$u[,1:5] %*% diag(svd1$d[1:5]) %*% t(svd1$v[,1:5])
#including only first 10 singular vectors
approx10<-svd1$u[,1:10] %*% diag(svd1$d[1:10]) %*% t(svd1$v[,1:10])
#including only first 20 singular vectors
approx25<-svd1$u[,1:25] %*% diag(svd1$d[1:25]) %*% t(svd1$v[,1:25])
#including only first 20 singular vectors
approx35<-svd1$u[,1:35] %*% diag(svd1$d[1:35]) %*% t(svd1$v[,1:35])
#including only first 20 singular vectors
approx50<-svd1$u[,1:50] %*% diag(svd1$d[1:50]) %*% t(svd1$v[,1:50])

par(mfrow=c(2,3),mar=c(1,1,1,1))
#plotting for reduced images
plot(as.cimg(approx5),main="(a) 5 singular Vectors",axes=FALSE)
plot(as.cimg(approx10),main="(b) 10 singular Vectors ",axes=FALSE)
plot(as.cimg(approx25),main="(c) 25 singular Vectors",axes=FALSE)
plot(as.cimg(approx35),main="(c) 35 singular Vectors",axes=FALSE)
plot(as.cimg(approx50),main="(c) 50 singular Vectors",axes=FALSE)
plot(as.cimg(megray),main="(d) Full image",axes=FALSE)


######################################################
#############2.Color image Compression: ##############
######################################################

R = image[,,1]
G = image[,,2]
B = image[,,3]

par(mfrow=c(1,3))

par(mfrow=c(1,3),mar=c(1,1,1,1))
cscale <- function(v) rgb(v,0,0)
grayscale(image) %>% plot(colourscale=cscale,rescale=FALSE,axes=FALSE)

cscale <- function(v) rgb(0,v,0)
grayscale(image) %>% plot(colourscale=cscale,rescale=FALSE,axes=FALSE)

cscale <- function(v) rgb(0,0,v)
grayscale(image) %>% plot(colourscale=cscale,rescale=FALSE,axes=FALSE)

svdR<-svd(R)
svdG<-svd(G)
svdB<-svd(B)
svdRGB<-list(svdR,svdG,svdB)

par(mfrow=c(2,3),mar=c(1,1,1,1))
for(j in c(5,10,25,35,50)){
comp <- sapply(svdRGB, function(i){
        compressed = i$u[,1:j] %*% diag(i$d[1:j]) %*% t(i$v[,1:j])
}, simplify = 'array')
comp<-as.cimg(comp)
plot(comp,axes=FALSE,main=paste("Rank=",j))
}

plot(image,axes=FALSE,main="orginal")

