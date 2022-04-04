# Code for cluster algorithm part:
#-------------------------------------------------------------------------------
# -------------------------------------------Install packages and import dataset
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               readr,
               cluster)

## import data
load("cityweather.RData")
cw <- cityweather

## use <Minmax> to scale the columns of data set
cw <- apply(cw, 2, function(x) (x - min(x)) / (max(x) - min(x)))
cw <- as.data.frame(cw)
D <- dist(cw)
D <- as.matrix(D)

#-------------------------------------------------------------------------------
# ---------------------------------------------Hierarchical cluster analysis own
#hclust_own_complete documentation
#
# Purpose:
#  Hierarchical cluster analysis on a set of dissimilarities and methods for 
#  analyzing it.
#
# Input:
#  D: matrix, containing a set of dissimilarities, processed by dist() beforehand
#
# Output:
#  M: matrix, containing merged pairs
#-------------------------------------------------------------------------------
hclust_own_complete <- function(d){
  # check number of rows in distance matrix d
  N <- nrow(d)
  # set diagonal as inf so other minimum calculation will not go to diagonal
  diag(d) <- Inf
  # make an index for tracing
  n <- -(1:N)
  # make a merge matrix to store pairs
  merge <- matrix(0,nrow=N-1, ncol=2)
  # set height
  height <- rep(0,N-1)
  
  # get in loop all elements to search
  for(j in seq(1,N-1)){
    # search for the minimized distance between attributes in the distance matrix
    height[j] <- min(d)
    # search for location when distance is the smallest
    i <- which(d == height[j],arr.ind=TRUE)
    # go to the first i since we merge two clusters into one pair
    i <- i[1,,drop=FALSE]
    # put the pair in the merge matrix
    merge[j,] <- n[i]
    group <- c(i, which(n %in% n[i[1,n[i]>0]]))
    n[group] <- j
    # to compare "complete" method with the package, so set max as inter-cluster linkage
    linkage <- apply(d[i,],2,max)
    # before going to the next loop, remove current one
    d[min(i),] <- d[,min(i)] <- linkage
    d[min(i),min(i)]        <- Inf
    d[max(i),] <- d[,max(i)] <- Inf
  }
  # for simple comparison, only return merge matrix
  return(list(merge=merge))
}
#-------------------------------------------------------------------------------
# --------------------------------------Hierarchical cluster analysis by package
D_hclust <- hclust(dist(cw), method = "complete")
merge_package <- as.matrix(D_hclust_own_complete$merge)

#-------------------------------------------------------------------------------
# -----------------------------Compare the self implemented HC to the package HC
D_hclust_own_complete <- hclust_own_complete(D)
merge_own <- as.matrix(D_hclust_own$merge)

df <- cbind(merge_package, merge_own)
view(df)
# output df
write.csv(df,"df.csv")
# check whether values are matched between package and own implementation
result <- table(which(df[,3] %in% df[,1:2]))

