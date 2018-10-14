# define the USArrests as the dataset
ds <- USArrests

# calculate the Euclidian matrix
dist_matrix <- dist(ds)

# PART A - cluster the states using hierarchical clustering w/ complete linkage
hclust.complete <- hclust(dist_matrix, method = "complete")

# PART B - plot the cluster and cut the dendogram that results in three distinct clusters
# here, at height=150, it results in three distinct clusters
plot(hclust.complete)
abline(h = 150, col = "red")

# get the states on each clusters
cutree(hclust.complete, k=1)
cutree(hclust.complete, k=2)
cutree(hclust.complete, k=3)

# PART C - heirarchically cluster the states again, this time by scaling to have a sd=1

# scale the original dataset for all of the features have standard deviation=1
ds.scaled <- scale(ds)
colMeans(ds.scaled)
apply(ds.scaled, 2, sd)

# calculate the Euclidian matrix
dist_matrix_scaled <- dist(ds.scaled)

# cluster the new scaled dataset using hclust complete linkage
hclust.scaled_complete <- hclust(dist_matrix_scaled, method="complete")
plot(hclust.scaled_complete)
