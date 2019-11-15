# Read in files (set a working directory or provide a full path, e.g. "~/Desktop/sample_dataset/users-likes.csv")
setwd("~/dev/SVD/SVD_students")
users <- read.csv("users.csv")
likes <- read.csv("likes.csv")       #  30Sec
ul <- read.csv("users-likes.csv")    #  25Sec

#can check what's inside each object using the following set of commands:
str(users)
str(likes)
str(ul)

head(users)
head(likes)
head(ul)

tail(users)
tail(likes)
tail(ul)

dim(users)
dim(likes)
dim(ul)

# Match entries in ul with users and likes dictionaries
ul$user_row<-match(ul$userid,users$userid)
ul$like_row<-match(ul$likeid,likes$likeid)

# and inspect what happened: 
head(ul)

# Step 1.2: Construct matrix

# Load Matrix library
require(Matrix)

# Construct the sparse User-Like Matrix M
M <- sparseMatrix(i = ul$user_row, j = ul$like_row, x = 1)

# Check the dimensions of M
dim(M)

# Save user IDs as row names in M
rownames(M) <- users$userid

# Save Like names as column names in M
colnames(M) <- likes$name

# Remove ul and likes objects (they won't be needed)
rm(ul, likes)

# Step 1.3: Trimming the matrix
# Remove users/Likes occurring less than 50/150 times
repeat {                                       # repeat whatever is in the brackets
  i <- sum(dim(M))                             # check the size of M
  M <- M[rowSums(M) >= 50, colSums(M) >= 150]  # Retain only these rows/columns that meet the threshold
  if (sum(dim(M)) == i) break                  # if the size has not changed, break the loop
}

# Check the new size of M
dim(M)

# Remove the users from users object that were removed from M
users <- users[match(rownames(M), users$userid), ]

# Check the new size of users
dim(users)


# Step2: Reducing Dimensionality using SVD

# Preset the random number generator in R 
# for the comparability of the results
set.seed(seed = 68)


# Load irlba and extract 5 SVD dimensions
library(irlba)
Msvd <- irlba(M, nv = 5)

# User SVD scores are here:
u <- Msvd$u

# Like SVD scores are here:
v <- Msvd$v
# The scree plot of singular values:
plot(Msvd$d)

# Rotation
# first obtain rotated V matrix:
# (unclass function has to be used to save it as an 
# object of type matrix and not loadings)
v_rot <- unclass(varimax(Msvd$v)$loadings)

# The cross-product of M and v_rot gives u_rot:
u_rot <- as.matrix(M %*% v_rot)


# Step 3:
#Interpret clusters and dimensions

# Correlate user traits and their SVD scores
cor(u_rot, users[,-1], use = "pairwise") # users[,-1] is used to exclude the column with IDs

# Step 3.1 
#The resulting correlation matrix can be presented as a heatmap.

# Heatmap SVD
corSVD <- cor(u_rot, users[,-1], use = "pairwise")
x11()
install.packages("corrplot")
library(corrplot)
corrplot(corSVD, method = "circle")
corrplot(corSVD, method = "color")

# Step 3.2 
#You can also print the Likes with the highest and lowest varimax-rotated SVD scores:
top <- list()
bottom <-list()
for (i in 1:5) {
  f <- order(v_rot[ ,i])
  temp <- tail(f, n = 10)
  top[[i]]<-colnames(M)[temp]  
  temp <- head(f, n = 10)
  bottom[[i]]<-colnames(M)[temp]  
}

top
bottom
